# necessary libraries
library(shiny)
library(dplyr)
library(DT)      # interactive data tables
library(readr)   # read csv files
library(readxl)  # read excel files
library(jsonlite)# read json files

# Increase upload file size limit (no limit)
options(shiny.maxRequestSize = Inf)

# UI
ui <- fluidPage(
  titlePanel("Data Cleaning, Preprocessing, and Feature Engineering"),
  
  sidebarLayout(
    sidebarPanel(
      # File upload 
      fileInput("files", "Upload Dataset(s)", 
                accept = c(".csv", ".xlsx", ".json", ".rds"), multiple = TRUE),
      uiOutput("dataset_selector"), 
      uiOutput("column_selector"), # dynamic column selection
      
      # Data cleaning options
      h4("Data Cleaning Options"),
      checkboxInput("remove_na", "Remove Missing Values", value = FALSE),
      checkboxInput("remove_duplicates", "Remove Duplicates", value = FALSE),
      
      # Outlier handling
      h4("Outlier Handling"),
      selectInput("outlier_method", "Detect Outliers Using", 
                  choices = c("None", "Z-score", "IQR")),
      checkboxInput("remove_outliers", "Remove Outliers", value = FALSE),  
      
      # Data type conversion
      h4("Data Type Conversion"),
      selectInput("convert_type", "Convert Data Type", 
                  choices = c("None", "To Character", "To Numeric", "To Date")),
      
      # Scaling
      h4("Scaling"),
      selectInput("scale_method", "Scaling Method", 
                  choices = c("None", "Standardization (Z-score)", "Normalization (0-1)")),
      
      # Categorical Encoding
      h4("Categorical Encoding"),
      selectInput("encode_method", "Categorical Encoding", 
                  choices = c("None", "One-Hot Encoding", "Label Encoding")),
      
      # Text cleaning
      h4("Text Cleaning"),
      checkboxInput("trim_whitespace", "Trim Whitespace", value = FALSE),
      checkboxInput("to_lowercase", "Convert to Lowercase", value = FALSE),
      checkboxInput("remove_punctuation", "Remove Punctuation", value = FALSE),
      
      # Apply preprocessing button
      actionButton("apply", "Apply Preprocessing"),
      
      hr(),
      # Feature Engineering Options
      h4("Feature Engineering Options"),
      radioButtons("fe_method", "Select Feature Engineering Method",
                   choices = c("None", "Log Transformation", "Polynomial Feature", 
                               "Interaction Term", "Binning", "Custom Transformation"),
                   selected = "None"),
      
      # Conditional UI for log transformation
      conditionalPanel(
        condition = "input.fe_method == 'Log Transformation'",
        uiOutput("log_col_ui")
      ),
      
      # Conditional UI for polynomial feature
      conditionalPanel(
        condition = "input.fe_method == 'Polynomial Feature'",
        uiOutput("poly_col_ui"),
        numericInput("poly_degree", "Degree", value = 2, min = 2)
      ),
      
      # Conditional UI for interaction term
      conditionalPanel(
        condition = "input.fe_method == 'Interaction Term'",
        uiOutput("interact_col1_ui"),
        uiOutput("interact_col2_ui")
      ),
      
      # Conditional UI for binning
      conditionalPanel(
        condition = "input.fe_method == 'Binning'",
        uiOutput("bin_col_ui"),
        numericInput("num_bins", "Number of Bins", value = 4, min = 2)
      ),
      
      # Conditional UI for custom transformation
      conditionalPanel(
        condition = "input.fe_method == 'Custom Transformation'",
        uiOutput("custom_col_ui"),
        textInput("custom_expr", "Enter Transformation Expression (use 'x' as the variable)", value = "log(x+1)")
        # Removed the plotOutput line here
      ),
      
      # Apply feature engineering button
      actionButton("apply_fe", "Apply Feature Engineering")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Preprocessing",
                 h4("Dataset Preview"),
                 DTOutput("preview"),
                 h4("Dataset Summary"),
                 verbatimTextOutput("summary")
        ),
        tabPanel("Feature Engineering",
                 h4("Engineered Data Preview"),
                 DTOutput("engineered_preview"),
                 h4("Engineered Data Summary"),
                 verbatimTextOutput("engineered_summary"),
                 
                 # New section for Custom Transformation Plot
                 conditionalPanel(
                   condition = "input.fe_method == 'Custom Transformation'",
                   h4("Custom Transformation Histogram"),
                   plotOutput("custom_plot", height = "400px", width = "100%")
                 )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Function to clean column names
  clean_colnames <- function(df) {
    colnames(df) <- gsub("[^[:alnum:]_]", "_", colnames(df))
    colnames(df) <- tolower(colnames(df))
    return(df)
  }
  
  # Reactive: read all datasets that have been uploaded 
  datasets <- reactive({
    req(input$files)
    files <- input$files
    result <- list()
    
    for (i in seq_along(files$name)) {
      ext <- tools::file_ext(files$name[i])
      path <- files$datapath[i]
      
      data <- switch(ext,
                     csv = read_csv(path, col_types = cols(), guess_max = 1000),
                     xlsx = read_excel(path),
                     json = {
                       json_lines <- readLines(path)
                       json_data <- lapply(json_lines, jsonlite::fromJSON)
                       do.call(rbind, lapply(json_data, as.data.frame))
                     },
                     rds = readRDS(path),
                     NULL)
      
      if (!is.null(data)) {
        result[[files$name[i]]] <- clean_colnames(as.data.frame(data))
      }
    }
    return(result)
  })
  
  # UI: dropdown for selecting dataset
  output$dataset_selector <- renderUI({
    req(datasets())
    selectInput("selected_dataset", "Select Dataset", choices = names(datasets()))
  })
  
  # UI: checkbox group for selecting columns
  output$column_selector <- renderUI({
    req(datasets(), input$selected_dataset)
    checkboxGroupInput("selected_columns", "Select Columns", 
                       choices = names(datasets()[[input$selected_dataset]]),
                       selected = names(datasets()[[input$selected_dataset]]))
  })
  
  # Data cleaning and preprocessing 
  cleaned_data <- reactive({
    req(datasets(), input$selected_dataset)
    df <- datasets()[[input$selected_dataset]]
    
    # Select only the chosen columns
    if (!is.null(input$selected_columns)) {
      df <- df[, input$selected_columns, drop = FALSE]
    }
    # Remove missing values
    if (input$remove_na) {
      df <- na.omit(df)
    }
    
    # Remove duplicates
    if (input$remove_duplicates) {
      df <- df %>% distinct()
    }
    
    # Outlier handling
    if (input$outlier_method != "None") {
      numeric_cols <- df %>% select(where(is.numeric))
      if (input$outlier_method == "Z-score") {
        z_scores <- scale(numeric_cols)
        outliers <- abs(z_scores) > 3
      } else if (input$outlier_method == "IQR") {
        Q1 <- apply(numeric_cols, 2, quantile, 0.25)
        Q3 <- apply(numeric_cols, 2, quantile, 0.75)
        IQR <- Q3 - Q1
        outliers <- numeric_cols < (Q1 - 1.5 * IQR) | numeric_cols > (Q3 + 1.5 * IQR)
      }
      # Remove outliers
      if (input$remove_outliers) {
        df <- df[!apply(outliers, 1, any), ]
      }
    }
    
    # Data type conversion
    if (input$convert_type == "To Character") {
      df <- df %>% mutate(across(where(is.numeric), as.character))
    } else if (input$convert_type == "To Numeric") {
      df <- df %>% mutate(across(where(is.character), as.numeric))
    } else if (input$convert_type == "To Date") {
      df <- df %>% mutate(across(where(is.character), as.Date))
    }
    
    # Scaling
    if (input$scale_method == "Standardization (Z-score)") {
      df <- df %>% mutate(across(where(is.numeric), scale))
    } else if (input$scale_method == "Normalization (0-1)") {
      df <- df %>% mutate(across(where(is.numeric), ~ (.-min(.))/(max(.)-min(.))))
    }
    
    # Categorical Encoding
    if (input$encode_method == "One-Hot Encoding") {
      df <- as.data.frame(model.matrix(~ . - 1, data = df))
    } else if (input$encode_method == "Label Encoding") {
      df <- df %>% mutate(across(where(is.character), as.factor)) %>%
        mutate(across(where(is.factor), as.numeric))
    }
    
    # Text cleaning
    if (input$trim_whitespace) {
      df <- df %>% mutate(across(where(is.character), trimws))
    }
    if (input$to_lowercase) {
      df <- df %>% mutate(across(where(is.character), tolower))
    }
    if (input$remove_punctuation) {
      df <- df %>% mutate(across(where(is.character), ~ gsub("[[:punct:]]", "", .)))
    }
    
    df
  })
  
  # Display preprocessed data preview 
  output$preview <- renderDT({
    req(cleaned_data())
    datatable(cleaned_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Display preprocessed data summary 
  output$summary <- renderPrint({
    req(cleaned_data())
    summary(cleaned_data())
  })
  
  # Feature Engineering Section
  
  # For log transformation: only numeric columns
  output$log_col_ui <- renderUI({
    req(cleaned_data())
    numeric_vars <- names(cleaned_data()[, sapply(cleaned_data(), is.numeric), drop = FALSE])
    selectInput("log_col", "Select Numeric Column for Log Transformation", choices = numeric_vars)
  })
  
  # For polynomial feature: only numeric columns
  output$poly_col_ui <- renderUI({
    req(cleaned_data())
    numeric_vars <- names(cleaned_data()[, sapply(cleaned_data(), is.numeric), drop = FALSE])
    selectInput("poly_col", "Select Numeric Column for Polynomial Feature", choices = numeric_vars)
  })
  
  # For interaction term: select two numeric columns
  output$interact_col1_ui <- renderUI({
    req(cleaned_data())
    numeric_vars <- names(cleaned_data()[, sapply(cleaned_data(), is.numeric), drop = FALSE])
    selectInput("interact_col1", "Select First Numeric Column", choices = numeric_vars)
  })
  
  output$interact_col2_ui <- renderUI({
    req(cleaned_data())
    numeric_vars <- names(cleaned_data()[, sapply(cleaned_data(), is.numeric), drop = FALSE])
    selectInput("interact_col2", "Select Second Numeric Column", choices = numeric_vars)
  })
  
  # For binning: select one numeric column
  output$bin_col_ui <- renderUI({
    req(cleaned_data())
    numeric_vars <- names(cleaned_data()[, sapply(cleaned_data(), is.numeric), drop = FALSE])
    selectInput("bin_col", "Select Numeric Column for Binning", choices = numeric_vars)
  })
  
  # For custom transformation: only numeric columns
  output$custom_col_ui <- renderUI({
    req(cleaned_data())
    numeric_vars <- names(cleaned_data()[, sapply(cleaned_data(), is.numeric), drop = FALSE])
    selectInput("custom_col", "Select Numeric Column for Custom Transformation", choices = numeric_vars)
  })
  
  # Reactive expression to generate engineered data
  engineered_data <- eventReactive(input$apply_fe, {
    req(cleaned_data())
    df <- cleaned_data()
    
    if (input$fe_method == "Log Transformation") {
      req(input$log_col)
      new_col <- paste0("log_", input$log_col)
      df[[new_col]] <- log(df[[input$log_col]] + 1)
      
    } else if (input$fe_method == "Polynomial Feature") {
      req(input$poly_col, input$poly_degree)
      new_col <- paste0(input$poly_col, "_poly_", input$poly_degree)
      df[[new_col]] <- df[[input$poly_col]]^input$poly_degree
      
    } else if (input$fe_method == "Interaction Term") {
      req(input$interact_col1, input$interact_col2)
      new_col <- paste0(input$interact_col1, "_x_", input$interact_col2)
      df[[new_col]] <- df[[input$interact_col1]] * df[[input$interact_col2]]
      
    } else if (input$fe_method == "Binning") {
      req(input$bin_col, input$num_bins)
      new_col <- paste0(input$bin_col, "_binned")
      df[[new_col]] <- cut(df[[input$bin_col]], breaks = input$num_bins, include.lowest = TRUE)
      
    } else if (input$fe_method == "Custom Transformation") {
      req(input$custom_col, input$custom_expr)
      new_col <- paste0(input$custom_col, "_custom")
      x <- df[[input$custom_col]]
      transformed <- tryCatch(eval(parse(text = input$custom_expr), envir = list(x = x)),
                              error = function(e) {
                                showNotification("Error in custom transformation. Please check your expression.", type = "error")
                                rep(NA, length(x))
                              })
      df[[new_col]] <- transformed
    }
    # If "None" is selected, return the data unchanged
    df
  })
  
  # Display engineered data preview
  output$engineered_preview <- renderDT({
    req(engineered_data())
    datatable(engineered_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Display engineered data summary
  output$engineered_summary <- renderPrint({
    req(engineered_data())
    summary(engineered_data())
  })
  
  # Real-time visual feedback for custom transformation: plot original vs. transformed variable
  output$custom_plot <- renderPlot({
    req(cleaned_data(), input$custom_col, input$custom_expr)
    df <- cleaned_data()
    x <- df[[input$custom_col]]
    
    transformed <- tryCatch(eval(parse(text = input$custom_expr), envir = list(x = x)),
                            error = function(e) NA)
    
    if (!all(is.na(transformed))) {
      par(mfrow = c(1,2))
      hist(x, main = paste("Original:", input$custom_col), xlab = input$custom_col, col = "lightblue")
      hist(transformed, main = "Transformed", xlab = "Transformed Values", col = "lightgreen")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
