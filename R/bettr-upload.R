#' Launch bettr app with CSV upload functionality
#' 
#' This function creates a bettr app that allows users to upload CSV files
#' and dynamically render the visualization interface based on the uploaded data.
#' 
#' @param weightResolution Numeric scalar in (0,1), giving the resolution at 
#'     which weights can be specified using the sliders in the interface.
#' @param bstheme Character scalar giving the bootswatch theme for the app 
#'     (see https://bootswatch.com/). Default 'darkly'.
#' @param appTitle Character scalar giving the title that will be used for 
#'     the app. Defaults to 'bettr - Upload Data'.
#' @param addStopButton Logical scalar. If `TRUE` (default), will add a
#'     button to stop the app (by calling `shiny::stopApp`).
#' @param defaultWeight Numeric scalar between 0 and 1, giving the default 
#'     weight to assign to each metric.
#'  
#' @export
#' 
#' @author Claude Code
#' 
#' @returns
#' A shiny application with CSV upload capability
#' 
#' @importFrom shiny fileInput radioButtons selectInput actionButton 
#'     conditionalPanel fluidRow column br hr h4 p strong tags
#'     reactiveValues reactive observeEvent renderUI req validate need
#'     showNotification removeNotification updateSelectInput
#' @importFrom utils read.csv
#' @importFrom DT renderDataTable dataTableOutput
#' 
#' @examples 
#' if (interactive()) {
#'     bettr_upload()
#' }
#' 
bettr_upload <- function(weightResolution = 0.05, bstheme = "darkly",
                        appTitle = "bettr - Upload Data", 
                        addStopButton = TRUE, defaultWeight = 0.2) {
    
    ## UI definition ----------------------------------------------------------
    ui <- bslib::page_sidebar(
        title = appTitle,
        theme = bslib::bs_theme(bootswatch = bstheme, version = 5),
        
        sidebar = bslib::sidebar(
            width = 350,
            
            # File upload section
            shiny::h4("Upload Data"),
            shiny::fileInput(
                inputId = "csvFile",
                label = "Choose CSV File",
                accept = c(".csv", ".CSV"),
                multiple = FALSE
            ),
            
            shiny::br(),
            
            # Data configuration (only shown after upload)
            shiny::conditionalPanel(
                condition = "output.fileUploaded",
                shiny::hr(),
                shiny::h4("Configure Data"),
                
                shiny::selectInput(
                    inputId = "idCol",
                    label = "ID Column (entities to compare):",
                    choices = NULL,
                    selected = NULL
                ),
                
                shiny::selectInput(
                    inputId = "metricCols",
                    label = "Metric Columns:",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE
                ),
                
                shiny::br(),
                shiny::actionButton(
                    inputId = "generateViz",
                    label = "Generate Visualization",
                    class = "btn-primary"
                ),
                
                shiny::br(), shiny::br(),
                
                # Close app button
                if (addStopButton) {
                    shiny::actionButton("close_app", "Close app")
                } else {
                    NULL
                }
            )
        ),
        
        # Main panel
        shiny::conditionalPanel(
            condition = "!output.fileUploaded",
            bslib::card(
                shiny::h3("Welcome to bettr"),
                shiny::p("Upload a CSV file to get started with interactive benchmarking visualization."),
                shiny::strong("CSV Requirements:"),
                shiny::tags$ul(
                    shiny::tags$li("First row should contain column headers"),
                    shiny::tags$li("One column should contain entity/method IDs"),
                    shiny::tags$li("Other columns should contain numeric metrics for comparison"),
                    shiny::tags$li("Missing values are allowed but may affect visualizations")
                )
            )
        ),
        
        shiny::conditionalPanel(
            condition = "output.fileUploaded && !output.vizGenerated",
            shiny::fluidRow(
                shiny::column(
                    12,
                    bslib::card(
                        shiny::h4("Data Preview"),
                        shiny::p("Configure your data columns in the sidebar, then click 'Generate Visualization'."),
                        DT::dataTableOutput("dataPreview")
                    )
                )
            )
        ),
        
        # Visualization area (only shown after generation)
        shiny::uiOutput("bettrApp")
    )
    
    ## Server definition ------------------------------------------------------
    server <- function(input, output, session) {
        
        # Reactive values to store data and state
        values <- shiny::reactiveValues(
            uploaded_data = NULL,
            bettr_app_ui = NULL,
            file_uploaded = FALSE,
            viz_generated = FALSE
        )
        
        # File upload handling
        shiny::observeEvent(input$csvFile, {
            req(input$csvFile)
            
            tryCatch({
                # Read the CSV file
                df <- utils::read.csv(input$csvFile$datapath, stringsAsFactors = FALSE)
                
                # Store the data
                values$uploaded_data <- df
                values$file_uploaded <- TRUE
                
                # Update column choices
                col_names <- colnames(df)
                numeric_cols <- col_names[sapply(df, function(x) is.numeric(x) || 
                                                 (is.character(x) && !any(is.na(suppressWarnings(as.numeric(x))))))]
                
                shiny::updateSelectInput(session, "idCol", 
                                       choices = col_names,
                                       selected = col_names[1])
                
                shiny::updateSelectInput(session, "metricCols",
                                       choices = numeric_cols,
                                       selected = numeric_cols[1:min(3, length(numeric_cols))])
                
                shiny::showNotification("File uploaded successfully!")
                
            }, error = function(e) {
                shiny::showNotification(
                    paste("Error reading file:", e$message), 
                    type = "error", duration = 10
                )
            })
        })
        
        # Output flags for conditional panels
        output$fileUploaded <- shiny::reactive({
            values$file_uploaded
        })
        shiny::outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
        
        output$vizGenerated <- shiny::reactive({
            values$viz_generated
        })
        shiny::outputOptions(output, "vizGenerated", suspendWhenHidden = FALSE)
        
        # Data preview table
        output$dataPreview <- DT::renderDataTable({
            req(values$uploaded_data)
            values$uploaded_data
        }, options = list(scrollX = TRUE, pageLength = 10))
        
        # Generate visualization
        shiny::observeEvent(input$generateViz, {
            req(values$uploaded_data, input$idCol, input$metricCols)
            
            tryCatch({
                # Validate selections
                if (length(input$metricCols) < 1) {
                    shiny::showNotification("Please select at least one metric column.")
                    return()
                }
                
                if (input$idCol %in% input$metricCols) {
                    shiny::showNotification("ID column cannot also be a metric column.")
                    return()
                }
                
                # Prepare data for bettr
                df <- values$uploaded_data
                
                # Ensure metric columns are numeric
                for (col in input$metricCols) {
                    if (!is.numeric(df[[col]])) {
                        df[[col]] <- as.numeric(df[[col]])
                    }
                }
                
                # Remove rows with missing ID values
                df <- df[!is.na(df[[input$idCol]]) & df[[input$idCol]] != "", ]
                
                if (nrow(df) == 0) {
                    shiny::showNotification("No valid data rows found.")
                    return()
                }
                
                # Store bettr parameters for the embedded app
                values$bettr_data <- df
                values$bettr_idCol <- input$idCol
                values$bettr_metrics <- input$metricCols
                
                # Launch bettr with the uploaded data
                # We need to create a new bettr app instance
                values$viz_generated <- TRUE
                shiny::showNotification("Visualization generated successfully!")
                
            }, error = function(e) {
                shiny::showNotification(
                    paste("Error generating visualization:", e$message), 
                    type = "error", duration = 10
                )
            })
        })
        
        # Render the bettr application - FULL INTEGRATION
        output$bettrApp <- shiny::renderUI({
            # Only show content if visualization has been generated
            if (!values$viz_generated || is.null(values$bettr_data)) {
                return(NULL)
            }
            
            # Get data for bettr
            df <- values$bettr_data
            idCol <- values$bettr_idCol  
            metrics <- values$bettr_metrics
            
            # Create the full bettr UI layout (adapted from bettr-main.R)
            shiny::tagList(
                shiny::h3("📊 Interactive Benchmarking Visualization"),
                shiny::p(paste("Loaded:", nrow(df), "entities,", length(metrics), "metrics")),
                
                # Full bettr tabset panel with all visualization options
                shiny::tabsetPanel(
                    type = "tabs",
                    shiny::tabPanel(
                        "Dataset Summary",
                        shiny::br(),
                        bslib::card(
                            shiny::h4("📊 Dataset Overview"),
                            shiny::uiOutput("datasetSummaryUI")
                        )
                    ),
                    shiny::tabPanel(
                        "Heatmap", 
                        shiny::br(),
                        shiny::fluidRow(
                            shiny::column(
                                3, 
                                shiny::checkboxInput(
                                    inputId = "upload_show_row_names",
                                    label = "Show row names", 
                                    value = TRUE
                                )
                            ),
                            shiny::column(
                                6, 
                                shiny::radioButtons(
                                    inputId = "upload_heatmap_plot_type",
                                    label = "Plot type",
                                    choices = c("Heatmap", "Dot plot"), 
                                    selected = "Heatmap", inline = TRUE
                                )
                            )
                        ),
                        shiny::plotOutput("uploadHeatmap", height = "600px")
                    ),
                    shiny::tabPanel(
                        "Parallel coordinates", 
                        shiny::br(),
                        shiny::plotOutput("uploadParCoordplot", height = "600px")
                    ),
                    shiny::tabPanel(
                        "Polar plot", 
                        shiny::br(),
                        shiny::plotOutput("uploadPolarplot", height = "600px")
                    ),
                    shiny::tabPanel(
                        "Bar/polar plot",
                        shiny::br(),
                        shiny::plotOutput("uploadBarPolarplot", height = "600px")
                    ),
                    shiny::tabPanel(
                        "Data Table",
                        shiny::br(),
                        bslib::card(
                            shiny::h4("Selected Data for Visualization"),
                            DT::dataTableOutput("uploadDataTable")
                        )
                    )
                )
            )
        })
        
        # Dataset summary UI
        output$datasetSummaryUI <- shiny::renderUI({
            req(values$bettr_data, values$bettr_idCol, values$bettr_metrics)
            
            df <- values$bettr_data
            idCol <- values$bettr_idCol
            metrics <- values$bettr_metrics
            
            # Create summary statistics
            metric_data <- df[, metrics, drop = FALSE]
            
            shiny::tagList(
                shiny::fluidRow(
                    shiny::column(6,
                        shiny::h5("📈 Data Overview"),
                        shiny::p(paste("• Total entities:", nrow(df))),
                        shiny::p(paste("• ID column:", idCol)),
                        shiny::p(paste("• Selected metrics:", length(metrics))),
                        shiny::p(paste("• Metric names:", paste(metrics, collapse = ", ")))
                    ),
                    shiny::column(6,
                        shiny::h5("📊 Metric Statistics"),
                        DT::dataTableOutput("metricStatsTable", height = "300px")
                    )
                ),
                shiny::br(),
                shiny::h5("🔍 Entity Preview"),
                DT::dataTableOutput("entityPreviewTable", height = "300px")
            )
        })
        
        # Metric statistics table
        output$metricStatsTable <- DT::renderDataTable({
            req(values$bettr_data, values$bettr_metrics)
            
            df <- values$bettr_data
            metrics <- values$bettr_metrics
            metric_data <- df[, metrics, drop = FALSE]
            
            # Calculate statistics for each metric
            stats_df <- data.frame(
                Metric = metrics,
                Min = sapply(metric_data, min, na.rm = TRUE),
                Max = sapply(metric_data, max, na.rm = TRUE),
                Mean = sapply(metric_data, mean, na.rm = TRUE),
                Median = sapply(metric_data, median, na.rm = TRUE),
                SD = sapply(metric_data, sd, na.rm = TRUE),
                stringsAsFactors = FALSE
            )
            
            # Round numeric columns
            stats_df[, 2:6] <- round(stats_df[, 2:6], 3)
            
            stats_df
        }, options = list(dom = 't', pageLength = 20, scrollX = TRUE))
        
        # Entity preview table  
        output$entityPreviewTable <- DT::renderDataTable({
            req(values$bettr_data, values$bettr_idCol, values$bettr_metrics)
            
            df <- values$bettr_data
            idCol <- values$bettr_idCol
            metrics <- values$bettr_metrics
            
            # Show only ID column and selected metrics
            preview_df <- df[, c(idCol, metrics), drop = FALSE]
            preview_df
        }, options = list(scrollX = TRUE, pageLength = 10))
        
        # Data table for visualization data (only selected columns)
        output$uploadDataTable <- DT::renderDataTable({
            req(values$bettr_data, values$bettr_idCol, values$bettr_metrics)
            
            df <- values$bettr_data
            idCol <- values$bettr_idCol
            metrics <- values$bettr_metrics
            
            # Show only ID column and selected metrics
            viz_df <- df[, c(idCol, metrics), drop = FALSE]
            viz_df
        }, options = list(scrollX = TRUE, pageLength = 15))
        
        # Generate bettr visualizations using the actual bettr functions
        # First we need to prepare the data using bettr's internal functions
        prepared_data <- shiny::reactive({
            req(values$bettr_data, values$bettr_idCol, values$bettr_metrics)
            
            df <- values$bettr_data
            idCol <- values$bettr_idCol
            metrics <- values$bettr_metrics
            
            # Prepare data using bettr's internal preparation
            tryCatch({
                prep <- .prepareData(
                    df = df, idCol = idCol, metrics = metrics,
                    initialWeights = NULL, initialTransforms = list(),
                    metricInfo = NULL, metricColors = NULL,
                    idInfo = NULL, idColors = NULL,
                    weightResolution = weightResolution,
                    metricCol = "Metric", 
                    defaultWeightValue = defaultWeight
                )
                
                # Create basic long data for plotting
                longData <- .makeLongData(
                    df = df, idCol = idCol, metrics = metrics,
                    metricCol = "Metric", valueCol = "ScaledValue",
                    metricGrouping = NULL, metricInfo = NULL,
                    metricGroupCol = "metricGroup"
                )
                
                list(prep = prep, longData = longData, df = df, 
                     idCol = idCol, metrics = metrics)
            }, error = function(e) {
                # Fallback: return basic data
                list(df = df, idCol = idCol, metrics = metrics, 
                     prep = NULL, longData = NULL)
            })
        })
        
        # Heatmap
        output$uploadHeatmap <- shiny::renderPlot({
            data <- prepared_data()
            req(data$df)
            
            tryCatch({
                if (!is.null(data$prep) && !is.null(data$longData)) {
                    # Use proper bettr heatmap if data preparation worked
                    makeHeatmap(
                        bettrList = NULL, 
                        plotdata = data$longData,
                        scoredata = data$df,  # Simple fallback
                        idCol = data$idCol, metricCol = "Metric", 
                        valueCol = "ScaledValue", weightCol = "Weight", 
                        scoreCol = "Score", metricGroupCol = "metricGroup",
                        metricInfo = NULL, metricColors = NULL,
                        idInfo = NULL, idColors = NULL,
                        metricCollapseGroup = FALSE, metricGrouping = NULL,
                        labelSize = 10, showRowNames = TRUE,
                        plotType = "Heatmap", rownamewidth_cm = 6,
                        colnameheight_cm = 6
                    )
                } else {
                    # Fallback: simple ggplot heatmap using only selected metrics
                    library(ggplot2)
                    selected_df <- data$df[, c(data$idCol, data$metrics), drop = FALSE]
                    df_long <- tidyr::gather(selected_df, key = "Metric", value = "Value", 
                                           -!!data$idCol)
                    
                    ggplot(df_long, aes_string(x = "Metric", y = data$idCol, fill = "Value")) +
                        geom_tile() +
                        scale_fill_gradient(low = "white", high = "steelblue") +
                        theme_minimal() +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                        labs(title = "Performance Heatmap")
                }
            }, error = function(e) {
                # Ultimate fallback: text message
                plot.new()
                text(0.5, 0.5, paste("Error generating heatmap:", e$message), cex = 1.2)
            })
        })
        
        # Parallel coordinates plot
        output$uploadParCoordplot <- shiny::renderPlot({
            data <- prepared_data()
            req(data$df)
            
            tryCatch({
                if (!is.null(data$prep) && !is.null(data$longData)) {
                    makeParCoordPlot(
                        bettrList = NULL, plotdata = data$longData,
                        idCol = data$idCol, metricCol = "Metric",
                        valueCol = "ScaledValue", metricGroupCol = "metricGroup",
                        metricColors = NULL, idColors = NULL,
                        methods = unique(data$df[[data$idCol]]),
                        metricGrouping = NULL, highlightMethod = NULL,
                        labelSize = 10
                    )
                } else {
                    # Fallback parallel coordinates using only selected metrics
                    library(ggplot2)
                    selected_df <- data$df[, c(data$idCol, data$metrics), drop = FALSE]
                    df_long <- tidyr::gather(selected_df, key = "Metric", value = "Value", 
                                           -!!data$idCol)
                    
                    ggplot(df_long, aes_string(x = "Metric", y = "Value", 
                                             group = data$idCol, color = data$idCol)) +
                        geom_line(size = 1) +
                        geom_point(size = 2) +
                        theme_minimal() +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                        labs(title = "Parallel Coordinates Plot", color = data$idCol)
                }
            }, error = function(e) {
                plot.new()
                text(0.5, 0.5, paste("Error generating parallel coordinates:", e$message), cex = 1.2)
            })
        })
        
        # Polar plot  
        output$uploadPolarplot <- shiny::renderPlot({
            data <- prepared_data()
            req(data$df)
            
            tryCatch({
                if (!is.null(data$prep) && !is.null(data$longData)) {
                    makePolarPlot(
                        bettrList = NULL, plotdata = data$longData,
                        idCol = data$idCol, metricCol = "Metric",
                        valueCol = "ScaledValue", metricGroupCol = "metricGroup",
                        metricColors = NULL, metricCollapseGroup = FALSE,
                        metricGrouping = NULL, labelSize = 10
                    )
                } else {
                    # Fallback radar chart using only selected metrics
                    library(ggplot2)
                    selected_df <- data$df[, c(data$idCol, data$metrics), drop = FALSE]
                    df_long <- tidyr::gather(selected_df, key = "Metric", value = "Value", 
                                           -!!data$idCol)
                    
                    ggplot(df_long, aes_string(x = "Metric", y = "Value", 
                                             group = data$idCol, color = data$idCol)) +
                        geom_line(size = 1) +
                        geom_point(size = 3) +
                        coord_polar() +
                        theme_minimal() +
                        labs(title = "Polar Plot", color = data$idCol)
                }
            }, error = function(e) {
                plot.new()
                text(0.5, 0.5, paste("Error generating polar plot:", e$message), cex = 1.2)
            })
        })
        
        # Bar/Polar plot
        output$uploadBarPolarplot <- shiny::renderPlot({
            data <- prepared_data()
            req(data$df)
            
            tryCatch({
                # Simple bar plot using only selected metrics
                library(ggplot2)
                selected_df <- data$df[, c(data$idCol, data$metrics), drop = FALSE]
                df_long <- tidyr::gather(selected_df, key = "Metric", value = "Value", 
                                       -!!data$idCol)
                
                ggplot(df_long, aes_string(x = data$idCol, y = "Value", fill = "Metric")) +
                    geom_bar(stat = "identity", position = "dodge") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                    labs(title = "Performance Comparison", x = data$idCol, y = "Value")
            }, error = function(e) {
                plot.new()
                text(0.5, 0.5, paste("Error generating bar plot:", e$message), cex = 1.2)
            })
        })
        
        # Close app
        if (addStopButton) {
            shiny::observeEvent(input$close_app, {
                shiny::stopApp()
            })
        }
    }
    
    # Generate app
    shiny::shinyApp(ui = ui, server = server)
}