# necessary libraries
library(shiny)
library(readr) # read csv files
library(readxl) # read excel files
library(jsonlite) # read json files
library(DT) # interactive data tables

# increase upload limit (5MB -> 50MB)
options(shiny.maxRequestSize = 50 * 1024^2)

# UI
ui <- fluidPage(
  titlePanel("Uplodoad & Analyze Multiple Datasets"),
  sidebarLayout(
    sidebarPanel(
      fileInput("files", "Upload Dataset(s)",
                accept = c(".csv", ".xlsx", ".json", ".rds"),
                multiple = TRUE
      ),
      uiOutput("dataset_selector"),
      uiOutput("column_selector"), # dynamic column selection
      verbatimTextOutput("summary") # basic summary stats
    ),
    mainPanel(
      tableOutput("metadata"), # dataset metadata
      DTOutput("preview") # nteractive Data Table
    )
  )
)

# SERVER
server <- function(input, output) {
  # function to clean column names (remove spaces & special chars)
  clean_colnames <- function(df) {
    colnames(df) <- gsub("[^[:alnum:]_]", "_", colnames(df)) # replace special characters
    colnames(df) <- tolower(colnames(df)) # convert to lowercase
    return(df)
  }
  
  # function to detect column types
  detect_column_types <- function(df) {
    data.frame(
      Column = names(df),
      Type = sapply(df, function(col) {
        if (all(sapply(col, is.numeric))) {
          return("Numeric")
        } else if (all(sapply(col, function(x) is.character(x) || is.factor(x)))) {
          return("Categorical")
        } else if (all(sapply(col, function(x) inherits(x, "Date") || inherits(x, "POSIXt")))) {
          return("Date/Time")
        } else {
          return("Mixed")
        }
      })
    )
  }
  
  # reactive: read all datasets that have been uploaded
  datasets <- reactive({
    req(input$files) # ensure files are uploaded
    
    files <- input$files
    result <- list()
    
    for (i in seq_along(files$name)) {
      ext <- tools::file_ext(files$name[i]) # get file extension
      path <- files$datapath[i]
      
      # read file based on type and optimize large files
      data <- switch(ext,
                     # read first 1000 rows for type guessing
                     csv = read_csv(path, col_types = cols(), guess_max = 1000),
                     xlsx = read_excel(path),
                     json = {
                       json_lines <- readLines(path)
                       json_data <- lapply(json_lines, fromJSON)
                       do.call(rbind, lapply(json_data, as.data.frame))
                     },
                     rds = readRDS(path),
                     NULL
      )
      
      # store dataset with cleaned column names
      if (!is.null(data)) {
        result[[files$name[i]]] <- clean_colnames(data)
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
                       selected = names(datasets()[[input$selected_dataset]])
    ) # all columns by default
  })
  
  # display dataset metadata (Rows, Columns, Missing %)
  output$metadata <- renderTable({
    req(datasets(), input$selected_dataset)
    df <- datasets()[[input$selected_dataset]]
    data.frame(
      Rows = nrow(df),
      Columns = ncol(df),
      Missing_Percentage = sum(is.na(df)) / (nrow(df) * ncol(df)) * 100,
      Duplicates = sum(duplicated(df))
    )
  })
  
  # display first 5 rows (by default) of selected dataset with selected columns
  output$preview <- renderDT({
    req(datasets(), input$selected_dataset, input$selected_columns)
    datatable(datasets()[[input$selected_dataset]][, input$selected_columns, drop = FALSE],
              options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  # basic summary statistics
  output$summary <- renderPrint({
    req(datasets(), input$selected_dataset)
    summary(datasets()[[input$selected_dataset]])
  })
}

shinyApp(ui = ui, server = server)

library(shiny)
library(DT)
library(dplyr)

# Increase upload file size limit (no limit)
options(shiny.maxRequestSize = Inf)

# UI
ui <- fluidPage(
  titlePanel("Data Cleaning and Preprocessing"),
  
  sidebarLayout(
    sidebarPanel(
      # File upload (reuse your friend's file upload logic)
      fileInput("files", "Upload Dataset(s)", accept = c(".csv", ".xlsx", ".json", ".rds"), multiple = TRUE),
      uiOutput("dataset_selector"),
      uiOutput("column_selector"),
      
      # Data cleaning options (your code)
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
      
      # Apply button
      actionButton("apply", "Apply Preprocessing")
    ),
    
    mainPanel(
      h4("Dataset Preview"),
      DTOutput("preview"),  # Data preview
      h4("Dataset Summary"),
      verbatimTextOutput("summary")  # Data summary
    )
  )
)

# Server
server <- function(input, output) {
  
  # Function to clean column names (reuse your friend's function)
  clean_colnames <- function(df) {
    colnames(df) <- gsub("[^[:alnum:]_]", "_", colnames(df))
    colnames(df) <- tolower(colnames(df))
    return(df)
  }
  
  # Reactive: read all datasets that have been uploaded (reuse your friend's logic)
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
        result[[files$name[i]]] <- clean_colnames(data)
      }
    }
    return(result)
  })
  
  # UI: dropdown for selecting dataset (reuse your friend's logic)
  output$dataset_selector <- renderUI({
    req(datasets())
    selectInput("selected_dataset", "Select Dataset", choices = names(datasets()))
  })
  
  # UI: checkbox group for selecting columns (reuse your friend's logic)
  output$column_selector <- renderUI({
    req(datasets(), input$selected_dataset)
    checkboxGroupInput("selected_columns", "Select Columns", 
                       choices = names(datasets()[[input$selected_dataset]]),
                       selected = names(datasets()[[input$selected_dataset]]))
  })
  
  # Data cleaning and preprocessing (your code)
  cleaned_data <- reactive({
    req(datasets(), input$selected_dataset)
    df <- datasets()[[input$selected_dataset]]
    
    # Remove missing values
    if (input$remove_na) {
      df <- na.omit(df)
    }
    
    # Remove duplicates
    if (input$remove_duplicates) {
      df <- df %>% distinct()
    }
    
    # Outlier handling
    if (input$outlier_method == "Z-score") {
      numeric_cols <- df %>% select(where(is.numeric))
      z_scores <- scale(numeric_cols)
      outliers <- abs(z_scores) > 3  # Z-score > 3 or < -3 is outlier
    } else if (input$outlier_method == "IQR") {
      numeric_cols <- df %>% select(where(is.numeric))
      Q1 <- apply(numeric_cols, 2, quantile, 0.25)
      Q3 <- apply(numeric_cols, 2, quantile, 0.75)
      IQR <- Q3 - Q1
      outliers <- numeric_cols < (Q1 - 1.5 * IQR) | numeric_cols > (Q3 + 1.5 * IQR)
    }
    
    # Remove outliers
    if (input$remove_outliers && input$outlier_method != "None") {
      df <- df[!apply(outliers, 1, any), ]
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
      df <- model.matrix(~ . - 1, data = df)
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
  
  # Display data preview (reuse your friend's logic)
  output$preview <- renderDT({
    req(cleaned_data())
    datatable(cleaned_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Display data summary (reuse your friend's logic)
  output$summary <- renderPrint({
    req(cleaned_data())
    summary(cleaned_data())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
