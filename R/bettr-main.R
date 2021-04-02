#' Launch bettr app to explore and aggregate performance metrics
#' 
#' @param df A \code{data.frame} in wide format. 
#' @param idCol Character scalar, indicating which column of \code{df} 
#'   that contains IDs of the entities to be compared (e.g., methods).
#' @param metrics_num,metrics_cat Character vectors, indicating which of the 
#'   columns of \code{df} that correspond to, numeric and categorical, 
#'   respectively, metrics of interest.
#' @param initialWeights Named numeric vector providing initial weights for 
#'   aggregating the metric scores. Must have one entry per metric included 
#'   in \code{df}.
#' @param initialFlips,initialOffsets,initialTransforms Named vectors 
#'   giving the initial value of the flip, offset and transforms for each 
#'   metric. 
#' @param metricGroups Named list of named character vectors. Each list entry 
#'   corresponds to one grouping of metrics. The grouping much be a named 
#'   vector indicating the respective group for each metric. 
#'  
#' @export
#' 
#' @author Charlotte Soneson
#' 
#' @importFrom shiny tagList sliderInput shinyApp renderUI renderPlot 
#'   updateNumericInput observeEvent validate need observe outputOptions tags 
#'   HTML tagList renderUI radioButtons numericInput checkboxInput column 
#'   plotOutput uiOutput column fluidRow tabPanelBody tabsetPanel 
#'   updateTabsetPanel selectizeInput reactive reactiveValues uiOutput hr 
#'   actionButton br tabPanel titlePanel navbarPage
#' @importFrom sortable rank_list
#' @importFrom ggplot2 theme_minimal geom_bar aes ggplot geom_boxplot 
#'   coord_flip geom_jitter 
#' @importFrom dplyr select %>% contains
#' @importFrom cowplot plot_grid
#' @importFrom tidyr gather
#' @importFrom bslib bs_theme
#' @importFrom rlang sym
#' 
bettr <- function(df, idCol = "Method", 
                  metrics_num = setdiff(colnames(df), idCol),
                  metrics_cat = c(), initialWeights = NULL,
                  initialFlips = NULL, initialOffsets = NULL,
                  initialTransforms = NULL, metricGroups = list()) {
    ## All metrics (numeric and categorical) ----------------------------------
    metrics <- c(metrics_num, metrics_cat)
    
    .checkInputArguments(df = df, idCol = idCol, metrics_num = metrics_num,
                         metrics_cat = metrics_cat, 
                         initialWeights = initialWeights,
                         initialFlips = initialFlips,
                         initialOffsets = initialOffsets,
                         initialTransforms = initialTransforms,
                         metricGroups = metricGroups)
    
    ## Define column names assigned by the function ---------------------------
    scoreCol <- "Score"
    weightCol <- "Weight"
    metricCol <- "Metric"
    valueCol <- "ScaledValue"
    groupCol <- "Group"
    
    ## Assign initial values of flips, offsets and transforms -----------------
    if (is.null(initialFlips)) {
        initialFlips <- rep(FALSE, length(metrics_num))
        names(initialFlips) <- metrics_num
    }
    if (is.null(initialOffsets)) {
        initialOffsets <- rep(0, length(metrics_num))
        names(initialOffsets) <- metrics_num
    }
    if (is.null(initialTransforms)) {
        initialTransforms <- rep("None", length(metrics_num))
        names(initialTransforms) <- metrics_num
    }
    
    ## Assign initial weights -------------------------------------------------
    if (is.null(initialWeights)) {
        initialWeights <- rep(0.2, length(metrics))
        names(initialWeights) <- metrics
    } else {
        ## Round initial weights to closest 0.05 to fit with the sliders
        initialWeights <- round(initialWeights * 20) / 20
    }

    ## UI definition ----------------------------------------------------------
    p_layout <- 
        shiny::navbarPage(
            shiny::titlePanel("bettr"),
            theme = bslib::bs_theme(bootswatch = "darkly"),
            
            shiny::br(),
            
            shiny::fluidRow(
                ## Plots ------------------------------------------------------
                shiny::column(
                    9, 
                    shiny::tabsetPanel(
                        type = "tabs",
                        shiny::tabPanel("Heatmap", 
                                        shiny::numericInput(inputId = "heatmap_height",
                                                            label = "Plot height",
                                                            value = 400, min = 200,
                                                            max = 1000, step = 10),
                                        shiny::uiOutput("bettrHeatmapUI")),
                        shiny::tabPanel("Parallel coordinates", 
                                        shiny::plotOutput("bettrParCoordplot")),
                        shiny::tabPanel("Polar plot", 
                                        shiny::plotOutput("bettrPolarplot")),
                        shiny::tabPanel("Bar/polar plot",
                                        shiny::plotOutput("bettrBarPolarplot"))
                    ),
                    
                    ## Some white space ---------------------------------------
                    shiny::br(),
                    shiny::br(),
                    
                    ## Variable transformations -------------------------------
                    shiny::fluidRow(
                        shiny::column(
                            3, 
                            shiny::uiOutput(outputId = "metricToManipulateUI")
                        ), 
                        shiny::column(
                            9, 
                            shiny::uiOutput(outputId = 
                                                "metricManipulationSummaryUI")
                        )
                    )
                ),
                
                ## Weight controls --------------------------------------------
                shiny::column(
                    3, 
                    shiny::uiOutput(outputId = "metricGroupingUI"),
                    shiny::hr(color = "white"),
                    shiny::uiOutput(outputId = "highlightMethodUI"),
                    shiny::hr(color = "white"), 
                    shiny::uiOutput(outputId = "weights"),
                    shiny::hr(color = "white"), 
                    shiny::actionButton(inputId = "resetWeights", 
                                        label = "Reset to uniform weights")
                )
            )
        )
    
    ## Server definition ------------------------------------------------------
    server_function <- function(input, output, session) {
        
        ## Initialize data storage --------------------------------------------
        values <- shiny::reactiveValues(
            df = df,
            metrics = metrics,
            nMetrics = length(metrics),
            metricGroups = metricGroups,
            methods = unique(df[[idCol]])
        )
        
        ## Processed data -----------------------------------------------------
        procdata <- shiny::reactive({
            tmp <- values$df
            for (m in values$metrics) {
                if (is.numeric(values$df[[m]])) {
                    tmp[[m]] <- .transformNumericVariable(
                        x = values$df[[m]],
                        flip = input[[paste0(m, "_flip")]], 
                        offset = input[[paste0(m, "_offset")]], 
                        transf = .getTransf(input[[paste0(m, "_transform")]]), 
                        bincuts = NULL
                    )
                } else {
                    tmp[[m]] <- .transformCategoricalVariable(
                        x = values$df[[m]],
                        levels = input[[paste0(m, "_levels")]]
                    )
                }
            }
            tmp
        })
        
        ## Long-form data for plotting ----------------------------------------
        ## Needs to use the processed data, since we must make sure that the 
        ## value that goes in the 'value' column is numeric
        longdata <- shiny::reactive({
            pd <- procdata() %>%
                dplyr::select(!!rlang::sym(idCol), 
                              dplyr::contains(values$metrics)) %>%
                tidyr::gather(key = "Metric", value = "ScaledValue", 
                              -!!rlang::sym(idCol))
            ## Add weight column for later score calculations
            for (m in values$metrics) {
                pd[[weightCol]][pd$Metric == m] <- 
                    input[[paste0(m, "_weight")]]
            }
            ## Add grouping of metrics
            if (input$metricGrouping != "---") {
                pd[[groupCol]] <- 
                    values$metricGroups[[input$metricGrouping]][pd$Metric]
            }
            pd
        })
        
        ## UI element to select grouping of metrics ---------------------------
        output$metricGroupingUI <- shiny::renderUI({
            shiny::selectizeInput(inputId = "metricGrouping",
                                  label = "Grouping of metrics",
                                  choices = c("---", names(values$metricGroups)),
                                  selected = "---")
        })
        ## UI element to select method to highlight ---------------------------
        output$highlightMethodUI <- shiny::renderUI({
            shiny::selectInput(inputId = "highlightMethod",
                               label = "Highlight ID",
                               choices = values$methods, 
                               selected = NULL, 
                               multiple = TRUE)
        })
        
        ## UI element to select metric to transform ---------------------------
        output$metricToManipulateUI <- shiny::renderUI({
            shiny::selectizeInput(inputId = "metricToManipulate",
                                  label = "Select metric",
                                  choices = c("---", values$metrics),
                                  selected = "---")
        })
        
        ## Display transformation options for selected metric -----------------
        shiny::observeEvent(input$metricToManipulate, {
            shiny::updateTabsetPanel(inputId = "metricManipulationSummary", 
                                     selected = input$metricToManipulate)
        })
        
        ## UI element to transform metric values ------------------------------
        shiny::observe({
            output$metricManipulationSummaryUI <- shiny::renderUI({
                do.call(
                    shiny::tabsetPanel, 
                    c(list(type = "hidden", 
                           id = "metricManipulationSummary", 
                           ## Empty body when "---" is selected
                           shiny::tabPanelBody(
                               value = "---",
                               NULL
                           )),
                      ## One tab panel per metric. The actual panel content is 
                      ## created below (it's different for numeric and 
                      ## categorical variables)
                      lapply(values$metrics, function(i) {
                          shiny::tabPanelBody(
                              value = i, 
                              shiny::fluidRow(
                                  ## Input controls
                                  shiny::column(
                                      4,
                                      shiny::uiOutput(
                                          outputId = paste0(i, "_transformUI")
                                      )
                                  ),
                                  ## Summary plots
                                  shiny::column(
                                      8, 
                                      shiny::plotOutput(
                                          outputId = paste0(i, "_plotsummary")
                                      )
                                  ) 
                              )
                          )
                      })
                    )
                )
            })
        })

        ## Create transformation interface for numeric metrics ----------------
        lapply(metrics_num, function(m) {
            output[[paste0(m, "_transformUI")]] <- shiny::renderUI({
                shiny::tagList(
                    shiny::checkboxInput(
                        inputId = paste0(m, "_flip"),
                        label = "Flip",
                        value = initialFlips[m]
                    ),
                    shiny::numericInput(
                        inputId = paste0(m, "_offset"),
                        label = "Offset",
                        value = initialOffsets[m]
                    ),
                    shiny::radioButtons(
                        inputId = paste0(m, "_transform"),
                        label = "Transform",
                        choices = c("None", "z-score",
                                    "[0,1]", "[-1,1]",
                                    "Rank"),
                        selected = initialTransforms[m]
                    )
                )
            })
        })
        ## Create transformation interface for categorical metrics ------------
        lapply(metrics_cat, function(m) {
            output[[paste0(m, "_transformUI")]] <- shiny::renderUI({
                shiny::tagList(
                    sortable::rank_list(
                        text = "Levels",
                        labels = levels(factor(values$df[[m]])),
                        input_id = paste0(m, "_levels"),
                        class = c("default-sortable", "custom-sortable")
                    ),
                    ## Set the colors for the levels ranked list box
                    ## First color is surrounding, second is levels
                    shiny::tags$style(
                        shiny::HTML(".rank-list-container.custom-sortable {
                                    background-color: #3c453c;
                                    }
                                    .custom-sortable .rank-list-item {
                                    background-color: #02075d;
                                    }
                                    ")
                    )
                )
            })
        })
        ## Make sure that hidden tabs (metrics that are currently not being
        ## transformed) are not suspended 
        lapply(metrics, function(m) {
            shiny::outputOptions(output, paste0(m, "_transformUI"),
                                 suspendWhenHidden = FALSE)
        })
        
        ## Create summary plots for transformed metric ------------------------
        shiny::observe({
            lapply(values$metrics, function(m) {
                output[[paste0(m, "_plotsummary")]] <- shiny::renderPlot({
                    shiny::validate(
                        shiny::need(procdata(), "No processed data")
                    )
                    cowplot::plot_grid(
                        ggplot2::ggplot(data.frame(metric = procdata()[[m]]),
                                        ggplot2::aes(x = metric)) + 
                            ggplot2::geom_bar() + 
                            ggplot2::theme_minimal(),
                        ggplot2::ggplot(data.frame(metric = procdata()[[m]]),
                                        ggplot2::aes(x = 1, y = metric)) + 
                            ggplot2::geom_boxplot(outlier.size = -1) + 
                            ggplot2::geom_jitter(width = 0.2, size = 2, 
                                                 pch = 1) + 
                            ggplot2::theme_minimal() +
                            ggplot2::coord_flip(),
                        ncol = 1
                    )
                })
            })
            
        })
        
        
        ## Reset all weights upon action button click -------------------------
        shiny::observeEvent(input$resetWeights, {
            for (j in values$metrics) {
                shiny::updateNumericInput(
                    session, inputId = paste0(j, "_weight"), 
                    value = 0.2
                )
            }
        })
        
        ## Parallel coordinates plot ------------------------------------------
        output$bettrParCoordplot <- shiny::renderPlot({
            if (is.null(longdata())) {
                NULL
            } else {
                .makeParCoordPlot(df = longdata(), idCol = idCol, 
                                  metricCol = metricCol, valueCol = valueCol, 
                                  groupCol = groupCol, methods = values$methods,
                                  highlightMethod = input$highlightMethod, 
                                  metricGrouping = input$metricGrouping)
            }
        })
        
        ## Polar plot ---------------------------------------------------------
        output$bettrPolarplot <- shiny::renderPlot({
            if (is.null(longdata())) {
                NULL
            } else {
                .makePolarPlot(df = longdata(), idCol = idCol, 
                               metricCol = metricCol, valueCol = valueCol,
                               weightCol = weightCol, scoreCol = scoreCol)
            }
        })
        
        ## Bar + polar plot ---------------------------------------------------
        output$bettrBarPolarplot <- shiny::renderPlot({
            if (is.null(longdata())) {
                NULL
            } else {
                .makeBarPolarPlot(df = longdata(), idCol = idCol, 
                                  metricCol = metricCol, valueCol = valueCol, 
                                  weightCol = weightCol, scoreCol = scoreCol, 
                                  methods = values$methods)
            }
        })
        
        ## Heatmap ------------------------------------------------------------
        output$bettrHeatmapUI <- shiny::renderUI({
            shiny::plotOutput(
                "bettrHeatmap",
                height = paste0(input$heatmap_height,
                                "px"))
        })
        output$bettrHeatmap <- shiny::renderPlot({
            if (is.null(longdata())) {
                NULL
            } else {
                .makeHeatmap(df = longdata(), idCol = idCol, 
                             metricCol = metricCol, valueCol = valueCol, 
                             weightCol = weightCol, scoreCol = scoreCol, 
                             groupCol = groupCol)
            }
        })
        
        
        ## Define weight controls ---------------------------------------------
        output$weights <- shiny::renderUI({
            if (is.null(values$metrics)) {
                NULL
            } else {
                do.call(shiny::tagList,
                        lapply(values$metrics, function(i) {
                            shiny::sliderInput(
                                inputId = paste0(i, "_weight"),
                                label = i,
                                value = initialWeights[i],
                                min = 0,
                                max = 1,
                                step = 0.05
                            )
                        }))
            }
        })
        
    }
    
    # Generate app ------------------------------------------------------------
    shiny::shinyApp(ui = p_layout, server = server_function)
}