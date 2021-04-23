#' Launch bettr app to explore and aggregate performance metrics
#' 
#' @param df A `data.frame` in wide format. Should contain one column 
#'   with the IDs of the entities to be compared, and one column for each 
#'   metric to use for the comparison.
#' @param idCol Character scalar, indicating the name of the column of `df` 
#'   that contains IDs of the entities to be compared (e.g., methods).
#' @param metrics Character vector, indicating which of the 
#'   columns of `df` that correspond to metrics of interest. Only metrics
#'   included here will be displayed.
#' @param initialWeights Named numeric vector providing initial weights for 
#'   each metric to use for aggregating them into a final score. Must contain 
#'   one entry per metric included in `metrics_num` or `metrics_cat`.
#' @param initialTransforms Named list with initial values of transformation 
#'   parameters for each metric. Each list entry should correspond to one 
#'   metric, and take the form of a list with up to four elements, named:
#'   
#'   * **flip**: Logical scalar; whether or not to flip the sign of the 
#'     metric values. Defaults to `FALSE`.
#'   * **offset**: Numeric scalar; offset to add to the (flipped) 
#'     metric values. Defaults to `0`.
#'   * **transform**: Character scalar; one of 'None', 'z-score', 
#'     '\[0,1\]', '\[-1,1\]' or 'Rank', indicating which transform to apply to 
#'     the metric values (after any flipping and/or adding the offset). 
#'     Defaults to 'None'.
#'   * **cuts**: Numeric vector or `NULL`; the cut points that will 
#'     be used to bin the metric values (after the other transformations). 
#'     Defaults to `NULL`. 
#'
#'   Only values deviating from the defaults need to be explicitly specified, 
#'   the others will be initialized to their default values.
#' @param metricInfo `data.frame` with annotations for metrics. Must have 
#'   a column named 'Metric' identifying the respective metrics.
#' @param metricColors Named list with colors used for columns of 
#'   `metricInfo`. Should follow the format required for ComplexHeatmap 
#'   heatmap annotations. The list can include an entry named 'Metric', which 
#'   contains a named vector with colors to use for metrics. 
#' @param idInfo `data.frame` with annotations for entities. Must have a 
#'   column named according to `idCol` identifying the respective entities. 
#' @param idColors Named list with colors used for columns of `idInfo`. 
#'   Should follow the format required for ComplexHeatmap heatmap annotations. 
#'   The list can include an entry named according to `idCol`, which 
#'   contains a named vector with colors to use for entities. 
#' @param weightResolution Numeric scalar in (0,1), giving the resolution at 
#'   which weights can be specified using the sliders in the interface.
#' @param bstheme Character scalar giving the bootswatch theme for the app 
#'   (see https://bootswatch.com/). Default 'darkly'.
#' @param appTitle Character scalar giving the title that will be used for 
#'   the app. Defaults to 'bettr'.
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
#' @importFrom shinyjqui jqui_resizable
#' @importFrom dplyr select %>% contains
#' @importFrom tidyr gather
#' @importFrom bslib bs_theme
#' @importFrom rlang .data
#' @importFrom stats setNames
#' 
#' @examples 
#' df <- data.frame(Method = c("M1", "M2", "M3"), metric1 = c(1, 2, 3),
#'                  metric2 = c(3, 1, 2), metric3 = factor(c("a", "a", "b")))
#' initialTransforms <- list(metric1 = list(flip = TRUE, offset = 4))
#' metricInfo <- data.frame(Metric = c("metric1", "metric2", "metric3"),
#'                          Group = c("G1", "G2", "G2"))
#' idInfo <- data.frame(Method = c("M1", "M2", "M3"), 
#'                      Type = c("T1", "T1", "T2"))
#' metricColors = list(Group = c(G1 = "red", G2 = "blue"))
#' if (interactive()) {
#'     bettr(df = df, idCol = "Method", 
#'     metrics = c("metric1", "metric2", "metric3"),
#'     initialTransforms = initialTransforms,
#'     metricInfo = metricInfo, metricColors = metricColors,
#'     idInfo = idInfo)
#' }
#' 
bettr <- function(df, idCol = "Method", 
                  metrics = setdiff(colnames(df), idCol),
                  initialWeights = NULL,
                  initialTransforms = list(),
                  metricInfo = NULL, metricColors = NULL,
                  idInfo = NULL, idColors = NULL,
                  weightResolution = 0.05, bstheme = "darkly",
                  appTitle = "bettr") {
    
    ## Define column names assigned internally --------------------------------
    scoreCol <- "Score"
    weightCol <- "Weight"
    metricCol <- "Metric"
    valueCol <- "ScaledValue"
    groupCol <- "Group"
    initialWeightValue <- 0.2
    
    ## Check validity of input arguments --------------------------------------
    .checkInputArguments(df = df, idCol = idCol, metrics = metrics,
                         metricCol = metricCol,
                         initialWeights = initialWeights,
                         initialTransforms = initialTransforms,
                         metricInfo = metricInfo, idInfo = idInfo, 
                         weightResolution = weightResolution, 
                         bstheme = bstheme, appTitle = appTitle)
    
    ## Split metrics into numeric and categorical -----------------------------
    metrics_classes <- vapply(df[, metrics], class, NA_character_)
    metrics_num <- intersect(
        metrics, names(metrics_classes[metrics_classes %in% c("numeric", 
                                                              "integer")])
    )
    metrics_cat <- intersect(
        metrics, names(metrics_classes[metrics_classes %in% 
                                           c("factor", "character", 
                                             "logical")])
    )
    
    ## Define annotation colors -----------------------------------------------
    if (is.null(idInfo)) {
        idColors <- .generateColors(
            data.frame(id = unique(df[[idCol]])) %>% stats::setNames(idCol),
            idColors, ggplot2Columns = idCol
        )
    } else {
        idColors <- .generateColors(idInfo, idColors, ggplot2Columns = idCol)
    }
    
    if (is.null(metricInfo)) {
        metricColors <- .generateColors(
            data.frame(metric = metrics) %>% stats::setNames(metricCol),
            metricColors, ggplot2Columns = metricCol
        )
    } else {
        metricColors <- .generateColors(metricInfo, metricColors, 
                                        ggplot2Columns = metricCol)
    }
    
    ## Add non-specified initializations and check validity -------------------
    initialTransforms <- .completeInitialization(initialTransforms, 
                                                 metrics_num)
    
    ## Assign initial weights -------------------------------------------------
    initialWeights <- .assignInitialWeights(
        weights = initialWeights, metrics = metrics,
        initialWeightValue = initialWeightValue,
        weightResolution = weightResolution)

    ## UI definition ----------------------------------------------------------
    p_layout <- 
        shiny::navbarPage(
            shiny::titlePanel(appTitle),
            theme = bslib::bs_theme(bootswatch = bstheme),
            
            shiny::br(),
            
            shiny::fluidRow(
                ## Plots ------------------------------------------------------
                shiny::column(
                    9, 
                    shiny::tabsetPanel(
                        type = "tabs",
                        shiny::tabPanel(
                            "Heatmap", 
                            shiny::fluidRow(
                                shiny::column(
                                    2,
                                    shiny::numericInput(
                                        inputId = "heatmap_labelsize",
                                        label = "Label size", 
                                        value = 10, min = 2, max = 20, step = 1
                                    )
                                ),
                                shiny::column(1),
                                shiny::column(
                                    6,
                                    shiny::radioButtons(
                                        inputId = "heatmap_id_ordering",
                                        label = "ID ordering by score",
                                        choices = c("high-to-low", 
                                                    "low-to-high"),
                                        selected = "high-to-low",
                                        inline = TRUE
                                    )
                                )
                            ),
                            shiny::uiOutput("bettrHeatmapUI")
                        ),
                        shiny::tabPanel(
                            "Parallel coordinates", 
                            shiny::fluidRow(
                                shiny::column(
                                    2,
                                    shiny::numericInput(
                                        inputId = "parcoord_labelsize",
                                        label = "Label size", 
                                        value = 10, min = 2, max = 20, step = 1
                                    )
                                )
                            ),
                            shiny::uiOutput("bettrParCoordplotUI")
                        ),
                        shiny::tabPanel(
                            "Polar plot", 
                            shiny::fluidRow(
                                shiny::column(
                                    2,
                                    shiny::numericInput(
                                        inputId = "polar_labelsize",
                                        label = "Label size", 
                                        value = 10, min = 2, max = 20, step = 1
                                    )
                                ),
                                shiny::column(1),
                                shiny::column(
                                    6,
                                    shiny::radioButtons(
                                        inputId = "polar_id_ordering",
                                        label = "ID ordering by score",
                                        choices = c("high-to-low", 
                                                    "low-to-high"),
                                        selected = "high-to-low",
                                        inline = TRUE
                                    )
                                )
                            ),
                            shiny::uiOutput("bettrPolarplotUI")
                        ),
                        shiny::tabPanel(
                            "Bar/polar plot",
                            shiny::fluidRow(
                                shiny::column(
                                    2,
                                    shiny::numericInput(
                                        inputId = "barpolar_labelsize",
                                        label = "Label size", 
                                        value = 10, min = 2, max = 20, step = 1
                                    )
                                ),
                                shiny::column(
                                    2,
                                    shiny::checkboxInput(
                                        inputId = "barpolar_showcomp",
                                        label = "Show\nscore\ncomposition",
                                        value = FALSE
                                    )
                                ),
                                shiny::column(
                                    2,
                                    shiny::numericInput(
                                        inputId = "barpolar_scalefactor",
                                        label = "Scale\npolar plots",
                                        value = 1.5, step = 0.05,
                                        min = 0.1, max = 3.1
                                    )
                                ),
                                shiny::column(1),
                                shiny::column(
                                    5,
                                    shiny::radioButtons(
                                        inputId = "barpolar_id_ordering",
                                        label = "ID ordering by score",
                                        choices = c("high-to-low", 
                                                    "low-to-high"),
                                        selected = "high-to-low",
                                        inline = TRUE
                                    )
                                )
                            ),
                            shiny::uiOutput("bettrBarPolarplotUI")
                        )
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
            metricInfo = metricInfo,
            idInfo = idInfo,
            methods = unique(df[[idCol]])
        )
        
        ## Processed data -----------------------------------------------------
        procdata <- shiny::reactive({
            tmp <- values$df
            for (m in values$metrics) {
                if (m %in% metrics_num) {
                    tmp[[m]] <- .transformNumericVariable(
                        x = values$df[[m]],
                        flip = input[[paste0(m, "_flip")]], 
                        offset = input[[paste0(m, "_offset")]], 
                        transf = .getTransf(input[[paste0(m, "_transform")]]), 
                        bincuts = sort(as.numeric(input[[paste0(m, "_bincuts")]]))
                    )
                } else if (m %in% metrics_cat) {
                    tmp[[m]] <- .transformCategoricalVariable(
                        x = values$df[[m]],
                        levels = input[[paste0(m, "_levels")]]
                    )
                } else {
                    stop("Encountered metric that could not be identified ",
                         "as numeric or categorical: ", m)
                }
            }
            tmp
        })
        
        ## Long-form data for plotting ----------------------------------------
        ## Needs to use the processed data, since we must make sure that the 
        ## value that goes in the 'value' column is numeric
        longdata <- shiny::reactive({
            pd <- procdata() %>%
                dplyr::select(.data[[idCol]], 
                              dplyr::contains(values$metrics)) %>%
                tidyr::gather(key = "Metric", value = "ScaledValue", 
                              -.data[[idCol]])
            ## Add weight column for later score calculations
            for (m in values$metrics) {
                pd[[weightCol]][pd$Metric == m] <- 
                    input[[paste0(m, "_weight")]]
            }
            ## Add grouping of metrics
            if (input$metricGrouping != "---") {
                pd[[groupCol]] <- 
                    values$metricInfo[[input$metricGrouping]][
                        match(pd$Metric, values$metricInfo[[metricCol]])]
            }
            pd
        })
        
        ## UI element to select grouping of metrics ---------------------------
        output$metricGroupingUI <- shiny::renderUI({
            shiny::selectizeInput(
                inputId = "metricGrouping",
                label = "Grouping of metrics",
                choices = c("---", setdiff(colnames(values$metricInfo), 
                                           metricCol)),
                selected = "---"
            )
        })
        
        ## UI element to select method to highlight ---------------------------
        output$highlightMethodUI <- shiny::renderUI({
            shiny::selectInput(
                inputId = "highlightMethod",
                label = "Highlight ID",
                choices = values$methods, 
                selected = NULL, 
                multiple = TRUE
            )
        })
        
        ## UI element to select metric to transform ---------------------------
        output$metricToManipulateUI <- shiny::renderUI({
            shiny::selectizeInput(
                inputId = "metricToManipulate",
                label = "Select metric to transform",
                choices = c("---", values$metrics),
                selected = "---"
            )
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
                        value = initialTransforms[[m]][["flip"]]
                    ),
                    shiny::numericInput(
                        inputId = paste0(m, "_offset"),
                        label = "Offset",
                        value = initialTransforms[[m]][["offset"]]
                    ),
                    shiny::radioButtons(
                        inputId = paste0(m, "_transform"),
                        label = "Transform",
                        choices = c("None", "z-score",
                                    "[0,1]", "[-1,1]",
                                    "Rank"),
                        selected = initialTransforms[[m]][["transform"]]
                    ),
                    shiny::selectizeInput(
                        inputId = paste0(m, "_bincuts"),
                        label = "Cut points for\ncategorization",
                        choices = initialTransforms[[m]][["cuts"]],
                        selected = initialTransforms[[m]][["cuts"]],
                        multiple = TRUE,
                        options = list(create = TRUE)
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
                    .makeMetricSummaryPlot(x = procdata()[[m]])
                })
            })
            
        })
        
        
        ## Reset all weights upon action button click -------------------------
        shiny::observeEvent(input$resetWeights, {
            for (j in values$metrics) {
                shiny::updateNumericInput(
                    session, inputId = paste0(j, "_weight"), 
                    value = initialWeightValue
                )
            }
        })
        
        ## Parallel coordinates plot ------------------------------------------
        output$bettrParCoordplotUI <- shiny::renderUI({
            shinyjqui::jqui_resizable(shiny::plotOutput(
                "bettrParCoordplot"))
        })
        output$bettrParCoordplot <- shiny::renderPlot({
            if (is.null(longdata())) {
                NULL
            } else {
                .makeParCoordPlot(df = longdata(), idCol = idCol, 
                                  metricCol = metricCol, valueCol = valueCol, 
                                  groupCol = groupCol, methods = values$methods,
                                  highlightMethod = input$highlightMethod, 
                                  metricGrouping = input$metricGrouping,
                                  labelSize = input$parcoord_labelsize, 
                                  metricColors = metricColors,
                                  idColors = idColors)
            }
        })
        
        ## Polar plot ---------------------------------------------------------
        output$bettrPolarplotUI <- shiny::renderUI({
            shinyjqui::jqui_resizable(shiny::plotOutput(
                "bettrPolarplot"))
        })
        output$bettrPolarplot <- shiny::renderPlot({
            if (is.null(longdata())) {
                NULL
            } else {
                .makePolarPlot(df = longdata(), idCol = idCol, 
                               metricCol = metricCol, valueCol = valueCol,
                               weightCol = weightCol, scoreCol = scoreCol,
                               labelSize = input$polar_labelsize,
                               ordering = input$polar_id_ordering,
                               metricColors = metricColors)
            }
        })
        
        ## Bar + polar plot ---------------------------------------------------
        output$bettrBarPolarplotUI <- shiny::renderUI({
            shinyjqui::jqui_resizable(shiny::plotOutput(
                "bettrBarPolarplot"))
        })
        output$bettrBarPolarplot <- shiny::renderPlot({
            if (is.null(longdata())) {
                NULL
            } else {
                .makeBarPolarPlot(df = longdata(), idCol = idCol, 
                                  metricCol = metricCol, valueCol = valueCol, 
                                  weightCol = weightCol, scoreCol = scoreCol, 
                                  methods = values$methods, 
                                  labelSize = input$barpolar_labelsize,
                                  ordering = input$barpolar_id_ordering,
                                  showComposition = input$barpolar_showcomp,
                                  scaleFactorPolars = input$barpolar_scalefactor, 
                                  metricColors = metricColors)
            }
        })
        
        ## Heatmap ------------------------------------------------------------
        output$bettrHeatmapUI <- shiny::renderUI({
            shinyjqui::jqui_resizable(shiny::plotOutput(
                "bettrHeatmap"))
        })
        output$bettrHeatmap <- shiny::renderPlot({
            if (is.null(longdata())) {
                NULL
            } else {
                .makeHeatmap(df = longdata(), idCol = idCol, 
                             metricCol = metricCol, valueCol = valueCol, 
                             weightCol = weightCol, scoreCol = scoreCol, 
                             groupCol = groupCol, 
                             metricInfo = values$metricInfo,
                             idInfo = values$idInfo,
                             labelSize = input$heatmap_labelsize,
                             ordering = input$heatmap_id_ordering, 
                             idColors = idColors, metricColors = metricColors)
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
                                step = weightResolution
                            )
                        }))
            }
        })
        
    }
    
    # Generate app ------------------------------------------------------------
    shiny::shinyApp(ui = p_layout, server = server_function)
}