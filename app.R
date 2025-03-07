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
