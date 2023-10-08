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
#'     '\[0,1\]', '\[-1,1\]', 'Rank', 'Rank+\[0,1\]' or 'z-score+\[0,1\]', 
#'     indicating which transform to apply to 
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
#' @importFrom bslib bs_theme sidebar accordion accordion_panel page_sidebar
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
    metricGroupCol <- "metricGroup"
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
    metrics_classes <- vapply(df[, metrics, drop = FALSE], class, NA_character_)
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
    metricsWithWeights <- c(
        metrics, unlist(lapply(colnames(metricInfo), function(cn) {
            unique(paste0(cn, "_", metricInfo[[cn]]))
        })))
    initialWeights <- .assignInitialWeights(
        weights = initialWeights, 
        metrics = metricsWithWeights,
        initialWeightValue = initialWeightValue,
        weightResolution = weightResolution)
    
    ## UI definition ----------------------------------------------------------
    p_layout <- 
        bslib::page_sidebar(
            title = appTitle,
            theme = bslib::bs_theme(bootswatch = bstheme, version = 5),
            
            sidebar = bslib::sidebar(
                bslib::accordion(
                    open = TRUE, 
                    multiple = TRUE,
                    bslib::accordion_panel(
                        "Methods/IDs",
                        shiny::uiOutput(outputId = "highlightMethodUI"),
                        shiny::radioButtons(
                            inputId = "id_ordering",
                            label = "ID ordering by score",
                            choices = c("high-to-low", 
                                        "low-to-high"),
                            selected = "high-to-low",
                            inline = TRUE
                        ),
                        shiny::checkboxInput(
                            inputId = "showOnlyTopIds",
                            label = "Show only top IDs",
                            value = FALSE
                        ),
                        shiny::conditionalPanel(
                            condition = "input.showOnlyTopIds == true",
                            shiny::numericInput(
                                inputId = "nbrTopIds",
                                label = "Number of IDs",
                                value = 10
                            ),
                            shiny::uiOutput(outputId = "idTopNGroupingUI")
                        )
                    ),
                    bslib::accordion_panel(
                        "Metrics",
                        shiny::uiOutput(outputId = "metricGroupingUI"),
                        shiny::checkboxInput(inputId = "metricCollapseGroup",
                                             label = "Collapse by group",
                                             value = FALSE),
                    ),
                    bslib::accordion_panel(
                        "Plot settings",
                        shiny::numericInput(
                            inputId = "labelsize",
                            label = "Label size", 
                            value = 10, min = 2, max = 20, step = 1
                        )
                    ),
                    bslib::accordion_panel(
                        "Weights",
                        shiny::uiOutput(outputId = "weights"),
                        shiny::actionButton(inputId = "resetWeights", 
                                            label = "Reset to uniform weights")
                    )
                )
            ),
            
            
            ## Plots --------------------------------------------------
            shiny::tabsetPanel(
                type = "tabs",
                shiny::tabPanel(
                    "Heatmap", 
                    shiny::br(),
                    shiny::fluidRow(
                        shiny::column(
                            3, 
                            shiny::checkboxInput(
                                inputId = "show_row_names",
                                label = "Show row names", 
                                value = TRUE
                            )
                        )
                    ),
                    shiny::uiOutput("bettrHeatmapUI")
                ),
                shiny::tabPanel(
                    "Parallel coordinates", 
                    shiny::br(),
                    shiny::uiOutput("bettrParCoordplotUI")
                ),
                shiny::tabPanel(
                    "Polar plot", 
                    shiny::br(),
                    shiny::uiOutput("bettrPolarplotUI")
                ),
                shiny::tabPanel(
                    "Bar/polar plot",
                    shiny::br(),
                    shiny::fluidRow(
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
                        )
                    ),
                    shiny::uiOutput("bettrBarPolarplotUI")
                ),
                shiny::tabPanel(
                    "Filter methods/metrics",
                    shiny::br(),
                    shiny::selectInput(inputId = "keepIds", 
                                       label = "IDs to keep",
                                       choices = unique(df[[idCol]]),
                                       selected = unique(df[[idCol]]),
                                       multiple = TRUE),
                    shiny::uiOutput("idFilterByInfoUI"),
                    shiny::hr(), 
                    shiny::selectInput(inputId = "keepMetrics",
                                       label = "Metrics to keep",
                                       choices = metrics,
                                       selected = metrics,
                                       multiple = TRUE),
                    shiny::uiOutput("metricFilterByInfoUI")
                ),
                shiny::tabPanel(
                    "Transform metrics",
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
                )
            )
        )
    
    ## Server definition ------------------------------------------------------
    #nocov start
    server_function <- function(input, output, session) {
        
        ## Initialize data storage --------------------------------------------
        values <- shiny::reactiveValues(
            df = df,
            metrics = metrics,
            nMetrics = length(metrics),
            metricInfo = metricInfo,
            idInfo = idInfo,
            methods = unique(df[[idCol]]),
            currentWeights = initialWeights
        )
        
        ## Filtered data ------------------------------------------------------
        ## Only keep metrics and methods selected in the filter tab
        filtdata <- shiny::reactive({
            if (!is.null(values$idInfo)) {
                idFilt <- values$idInfo
                for (nm in setdiff(colnames(values$idInfo), idCol)) {
                    idFilt <- idFilt %>%
                        dplyr::filter(.data[[nm]] %in% 
                                          input[[paste0("keepIdBy_", nm)]])
                }
                tmp <- values$df %>%
                    dplyr::filter(.data[[idCol]] %in% intersect(input$keepIds, 
                                                                idFilt[[idCol]]))
            } else {
                tmp <- values$df %>%
                    dplyr::filter(.data[[idCol]] %in% input$keepIds)
            }
            
            if (!is.null(values$metricInfo)) {
                metricFilt <- values$metricInfo
                for (nm in setdiff(colnames(values$metricInfo), metricCol)) {
                    metricFilt <- metricFilt %>%
                        dplyr::filter(.data[[nm]] %in% 
                                          input[[paste0("keepMetricBy_", nm)]])
                }
                tmp <- tmp %>%
                    dplyr::select(-dplyr::any_of(
                        setdiff(values$metrics, intersect(input$keepMetrics,
                                                          metricFilt[[metricCol]]))))
            } else {
                tmp <- tmp %>%
                    dplyr::select(-dplyr::any_of(setdiff(values$metrics, input$keepMetrics)))
            }
            tmp
        })
        
        ## Record retained metrics and methods
        metricsInUse <- shiny::reactive({
            intersect(values$metrics, input$keepMetrics)
        })
        methodsInUse <- shiny::reactive({
            intersect(values$df[[idCol]], input$keepIds)
        })
        
        ## Processed data -----------------------------------------------------
        procdata <- shiny::reactive({
            tmp <- filtdata()
            for (m in intersect(colnames(filtdata()), metricsInUse())) {
                if (m %in% metrics_num) {
                    tmp[[m]] <- .transformNumericVariable(
                        x = filtdata()[[m]],
                        flip = input[[paste0(m, "_flip")]], 
                        offset = input[[paste0(m, "_offset")]], 
                        transf = .getTransf(input[[paste0(m, "_transform")]]), 
                        bincuts = sort(as.numeric(input[[paste0(m, "_bincuts")]]))
                    )
                } else if (m %in% metrics_cat) {
                    tmp[[m]] <- .transformCategoricalVariable(
                        x = filtdata()[[m]],
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
                              dplyr::contains(metricsInUse())) %>%
                tidyr::gather(key = "Metric", value = "ScaledValue", 
                              -.data[[idCol]])
            ## Add grouping of metrics
            if (input$metricGrouping != "---") {
                pd[[metricGroupCol]] <- 
                    values$metricInfo[[input$metricGrouping]][
                        match(pd$Metric, values$metricInfo[[metricCol]])]
            }
            pd
        })
        
        ## Long-form data with weights
        longdataweights <- shiny::reactive({
            pd <- longdata()
            ## Add weight column for later score calculations
            if (input$metricCollapseGroup && input$metricGrouping != "---") {
                for (m in unique(pd[[metricGroupCol]])) {
                    if (is.null(input[[paste0(input$metricGrouping, "_", m, "_weight")]])) {
                        return(NULL)
                    }
                    pd[[weightCol]][pd[[metricGroupCol]] == m] <- 
                        input[[paste0(input$metricGrouping, "_", m, "_weight")]]
                }
            } else {
                for (m in metricsInUse()) {
                    pd[[weightCol]][pd[[metricCol]] == m] <- 
                        input[[paste0(m, "_weight")]]
                }
            }
            pd
        })
        
        ## Collapsed data (average metrics)
        collapseddata <- shiny::reactive({
            if (input$metricCollapseGroup && input$metricGrouping != "---") {
                longdataweights() %>%
                    dplyr::group_by(.data[[idCol]], .data[[metricGroupCol]]) %>%
                    dplyr::summarize("{ valueCol }" := mean(.data[[valueCol]], na.rm = TRUE),
                                     "{ weightCol }" := mean(.data[[weightCol]], na.rm = TRUE)) %>%
                    dplyr::mutate("{ metricCol }" := .data[[metricGroupCol]]) %>%
                    dplyr::ungroup() %>%
                    as.data.frame()
            } else {
                longdataweights()
            }
        })
        
        ## Calculate scores ---------------------------------------------------
        scoredata <- shiny::reactive({
            scoreDf <- collapseddata() %>%
                dplyr::group_by(.data[[idCol]]) %>%
                dplyr::summarize(
                    "{scoreCol}" := sum(.data[[weightCol]] *
                                            .data[[valueCol]],
                                        na.rm = TRUE)
                ) 
            if (!is.null(values$idInfo)) {
                scoreDf <- scoreDf %>%
                    dplyr::left_join(idInfo, 
                                     by = idCol)
                if (input$idTopNGrouping != "---") {
                    scoreDf <- scoreDf %>%
                        dplyr::group_by(.data[[input$idTopNGrouping]])
                }
            }
            if (input$id_ordering == "high-to-low") {
                if (input$showOnlyTopIds) {
                    scoreDf <- scoreDf %>%
                        dplyr::slice_max(order_by = .data[[scoreCol]],
                                         n = input$nbrTopIds)
                }
                scoreDf <- scoreDf %>%
                    dplyr::ungroup() %>% 
                    dplyr::arrange(dplyr::desc(.data[[scoreCol]]))
            } else {
                if (input$showOnlyTopIds) {
                    scoreDf <- scoreDf %>%
                        dplyr::slice_min(order_by = .data[[scoreCol]],
                                         n = input$nbrTopIds)
                }
                scoreDf <- scoreDf %>%
                    dplyr::ungroup() %>%
                    dplyr::arrange(.data[[scoreCol]])
            }
            scoreDf
        })
        
        ## Final filtered data ------------------------------------------------
        plotdata <- shiny::reactive({
            tmp <- collapseddata() %>%
                dplyr::filter(.data[[idCol]] %in% scoredata()[[idCol]])
            tmp[[idCol]] <- factor(tmp[[idCol]], 
                                   levels = scoredata()[[idCol]])
            tmp
        })
        
        ## UI element to filter methods by grouping columns -------------------
        output$idFilterByInfoUI <- shiny::renderUI({
            if (is.null(values$idInfo)) {
                NULL
            } else {
                lapply(setdiff(colnames(values$idInfo), idCol), 
                       function(nm) {
                           shiny::selectInput(inputId = paste0("keepIdBy_", nm),
                                              label = nm, 
                                              choices = unique(values$idInfo[[nm]]),
                                              selected = unique(values$idInfo[[nm]]),
                                              multiple = TRUE)
                       }) 
            }
        })
        outputOptions(output, "idFilterByInfoUI", suspendWhenHidden = FALSE)
        
        ## UI element to filter metrics by grouping columns -------------------
        output$metricFilterByInfoUI <- shiny::renderUI({
            if (is.null(values$metricInfo)) {
                NULL
            } else {
                lapply(setdiff(colnames(values$metricInfo), metricCol), 
                       function(nm) {
                           shiny::selectInput(inputId = paste0("keepMetricBy_", nm),
                                              label = nm, 
                                              choices = unique(values$metricInfo[[nm]]),
                                              selected = unique(values$metricInfo[[nm]]),
                                              multiple = TRUE)
                       }) 
            }
        })
        outputOptions(output, "metricFilterByInfoUI", suspendWhenHidden = FALSE)
        
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
        
        ## UI element to select grouping of methods before selecting top N ----
        output$idTopNGroupingUI <- shiny::renderUI({
            shiny::selectizeInput(
                inputId = "idTopNGrouping",
                label = "Grouping of IDs",
                choices = c("---", setdiff(colnames(values$idInfo), 
                                           idCol)),
                selected = "---"
            )
        })
        
        ## UI element to select method to highlight ---------------------------
        output$highlightMethodUI <- shiny::renderUI({
            shiny::selectInput(
                inputId = "highlightMethod",
                label = "Highlight ID",
                choices = methodsInUse(), 
                selected = NULL, 
                multiple = TRUE
            )
        })
        
        ## UI element to select metric to transform ---------------------------
        output$metricToManipulateUI <- shiny::renderUI({
            shiny::selectizeInput(
                inputId = "metricToManipulate",
                label = "Select metric to transform",
                choices = c("---", metricsInUse()),
                selected = "---"
            )
        })
        outputOptions(output, "metricToManipulateUI", suspendWhenHidden = FALSE)
        
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
                      lapply(metricsInUse(), function(i) {
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
            outputOptions(output, "metricManipulationSummaryUI", suspendWhenHidden = FALSE)
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
                                    "Rank", "Rank+[0,1]", "z-score+[0,1]"),
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
            lapply(metricsInUse(), function(m) {
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
            for (j in metrics) {
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
            if (is.null(longdataweights())) {
                NULL
            } else {
                .makeParCoordPlot(df = plotdata(), idCol = idCol, 
                                  metricCol = metricCol, valueCol = valueCol, 
                                  metricGroupCol = metricGroupCol, methods = methodsInUse(),
                                  highlightMethod = input$highlightMethod, 
                                  metricGrouping = input$metricGrouping,
                                  labelSize = input$labelsize, 
                                  metricColors = metricColors,
                                  idColors = idColors,
                                  metricCollapseGroup = input$metricCollapseGroup)
            }
        })
        
        ## Polar plot ---------------------------------------------------------
        output$bettrPolarplotUI <- shiny::renderUI({
            shinyjqui::jqui_resizable(shiny::plotOutput(
                "bettrPolarplot"))
        })
        output$bettrPolarplot <- shiny::renderPlot({
            if (is.null(longdataweights())) {
                NULL
            } else {
                .makePolarPlot(df = plotdata(), scores = scoredata(), idCol = idCol, 
                               metricCol = metricCol, valueCol = valueCol,
                               weightCol = weightCol, scoreCol = scoreCol,
                               metricGroupCol = metricGroupCol, 
                               labelSize = input$labelsize,
                               metricColors = metricColors,
                               metricCollapseGroup = input$metricCollapseGroup,
                               metricGrouping = input$metricGrouping,
                               showOnlyTopIds = input$showOnlyTopIds,
                               nbrTopIds = input$nbrTopIds)
            }
        })
        
        ## Bar + polar plot ---------------------------------------------------
        output$bettrBarPolarplotUI <- shiny::renderUI({
            shinyjqui::jqui_resizable(shiny::plotOutput(
                "bettrBarPolarplot"))
        })
        output$bettrBarPolarplot <- shiny::renderPlot({
            if (is.null(longdataweights())) {
                NULL
            } else {
                .makeBarPolarPlot(df = plotdata(), scores = scoredata(), idCol = idCol, 
                                  metricCol = metricCol, valueCol = valueCol, 
                                  weightCol = weightCol, scoreCol = scoreCol, 
                                  metricGroupCol = metricGroupCol, 
                                  methods = methodsInUse(), 
                                  labelSize = input$labelsize,
                                  showComposition = input$barpolar_showcomp,
                                  scaleFactorPolars = input$barpolar_scalefactor, 
                                  metricColors = metricColors,
                                  metricCollapseGroup = input$metricCollapseGroup,
                                  metricGrouping = input$metricGrouping,
                                  showOnlyTopIds = input$showOnlyTopIds,
                                  nbrTopIds = input$nbrTopIds)
            }
        })
        
        ## Heatmap ------------------------------------------------------------
        output$bettrHeatmapUI <- shiny::renderUI({
            shinyjqui::jqui_resizable(shiny::plotOutput(
                "bettrHeatmap"))
        })
        output$bettrHeatmap <- shiny::renderPlot({
            if (is.null(longdataweights())) {
                NULL
            } else {
                .makeHeatmap(df = plotdata(), scores = scoredata(), idCol = idCol, 
                             metricCol = metricCol, valueCol = valueCol, 
                             weightCol = weightCol, scoreCol = scoreCol, 
                             metricGroupCol = metricGroupCol, 
                             metricInfo = values$metricInfo,
                             idInfo = values$idInfo,
                             labelSize = input$labelsize,
                             idColors = idColors, metricColors = metricColors,
                             metricCollapseGroup = input$metricCollapseGroup,
                             metricGrouping = input$metricGrouping, 
                             showRowNames = input$show_row_names,
                             showOnlyTopIds = input$showOnlyTopIds,
                             nbrTopIds = input$nbrTopIds)
            }
        })
        
        
        ## Define weight controls ---------------------------------------------
        ## Make sure that weights are retained even when the collapsing by
        ## group status (and thus the displayed weight sliders) changes
        shiny::observe({
            lapply(metricsWithWeights, function(mww) {
                if (!is.null(input[[paste0(mww, "_weight")]])) {
                    values$currentWeights[mww] <- input[[paste0(mww, "_weight")]]
                }
            })
        })
        
        output$weights <- shiny::renderUI({
            if (is.null(values$metrics) || is.null(values$currentWeights)) {
                NULL
            } else {
                if (input$metricCollapseGroup && input$metricGrouping != "---") {
                    if (is.null(longdata()[[metricGroupCol]])) {
                        NULL
                    } else {
                        do.call(shiny::tagList,
                                lapply(unique(longdata()[[metricGroupCol]]), function(i) {
                                    shiny::sliderInput(
                                        inputId = paste0(input$metricGrouping,
                                                         "_", i, "_weight"),
                                        label = i,
                                        value = values$currentWeights[
                                            paste0(input$metricGrouping,
                                                   "_", i)],
                                        min = 0,
                                        max = 1,
                                        step = weightResolution
                                    )
                                }))
                    }
                } else {
                    do.call(shiny::tagList,
                            lapply(metricsInUse(), function(i) {
                                shiny::sliderInput(
                                    inputId = paste0(i, "_weight"),
                                    label = i,
                                    value = values$currentWeights[i],
                                    min = 0,
                                    max = 1,
                                    step = weightResolution
                                )
                            }))
                }
            }
        })
        
    }
    #nocov end
    
    # Generate app ------------------------------------------------------------
    shiny::shinyApp(ui = p_layout, server = server_function)
}
