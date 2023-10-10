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
#' @importFrom shiny uiOutput radioButtons checkboxInput conditionalPanel 
#'   numericInput actionButton tabsetPanel tabPanel br fluidRow column 
#'   selectInput hr reactiveValues reactive outputOptions renderUI 
#'   selectizeInput updateTabsetPanel observe observeEvent tabPanelBody 
#'   plotOutput tagList tags HTML validate need renderPlot updateNumericInput 
#'   sliderInput shinyApp
#' @importFrom sortable rank_list
#' @importFrom shinyjqui jqui_resizable
#' @importFrom dplyr filter select any_of group_by summarize mutate ungroup 
#'   left_join slice_max arrange slice_min %>%
#' @importFrom tidyr gather
#' @importFrom bslib bs_theme sidebar accordion accordion_panel page_sidebar
#' @importFrom rlang .data
#' @importFrom stats setNames
#' @importFrom Hmisc wtd.quantile
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
bettr <- function(df, idCol = "Method", metrics = setdiff(colnames(df), idCol),
                  initialWeights = NULL, initialTransforms = list(),
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
    
    ## Prepare data -----------------------------------------------------------
    prep <- .prepareData(df = df, idCol = idCol, metrics = metrics, 
                         initialWeights = initialWeights,
                         initialTransforms = initialTransforms, 
                         metricInfo = metricInfo, 
                         metricColors = metricColors, 
                         idInfo = idInfo,
                         idColors = idColors,
                         weightResolution = weightResolution,
                         metricCol = metricCol, 
                         initialWeightValue = initialWeightValue)
    
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
                            inputId = "scoreMethod",
                            label = "Score aggregation method",
                            choices = c("weighted mean", 
                                        "weighted median",
                                        "weighted fraction highest",
                                        "weighted fraction lowest"),
                            selected = "weighted mean",
                            inline = TRUE
                        ),
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
                        ),
                        shiny::numericInput(
                            inputId = "hmheight",
                            label = "Heatmap height (manual)",
                            value = 600, min = 100, max = 1000
                        ),
                        shiny::numericInput(
                            inputId = "hm_rownamewidth",
                            label = "Heatmap row name max width (cm)",
                            value = 6, min = 1, max = 15
                        ),
                        shiny::numericInput(
                            inputId = "hm_colnameheight",
                            label = "Heatmap column name max height (cm)",
                            value = 6, min = 1, max = 15
                        )
                        # shiny::actionButton(
                        #     inputId = "update_size",
                        #     label = "Get current height"
                        # )
                    ),
                    bslib::accordion_panel(
                        "Weights",
                        shiny::uiOutput(outputId = "weights"),
                        shiny::actionButton(inputId = "resetWeights", 
                                            label = "Reset to uniform weights")
                    )
                )
            ),
            
            
            ## Plots ----------------------------------------------------------
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
                    bslib::card(
                        "You can exclude methods and/or metrics from the summaries by removing them from the selection boxes below. The selection can be done either by the name of the method/metric, or via any other provided attribute (if any). The intersection of methods satisfying the selection criteria will be retained. To add back a (set of) methods/metrics, click in the corresponding selection box to see a dropdown menu with available values."
                    ),
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
                    bslib::card(
                        "The interface below can be used to apply transformations to the provided metric values, to make them more comparable to each other. For example, it is important for the interpretability of the cross-metric aggregation that a high value always corresponds to either 'good' or 'bad' performance."
                    ),
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
                shiny::tabPanel(
                    "Data table",
                    shiny::br(),
                    bslib::card(
                        "This data table contains the transformed values of all metrics, as well as the aggregated scores."
                    ),
                    shiny::br(),
                    DT::dataTableOutput(outputId = "scoreTable")
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
            currentWeights = prep$initialWeights
        )
        
        ## Filtered data ------------------------------------------------------
        ## Only keep metrics and methods selected in the filter tab
        filtdata <- shiny::reactive({
            shiny::validate(
                shiny::need(input$keepIds, "No keepIds"),
                shiny::need(input$keepMetrics, "No keepMetrics")
            )
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
            intersect(values$metrics, colnames(filtdata()))
        })
        methodsInUse <- shiny::reactive({
            unique(filtdata()[[idCol]])
        })
        
        ## Processed data -----------------------------------------------------
        procdata <- shiny::reactive({
            shiny::validate(
                shiny::need(filtdata(), ""),
                shiny::need(metricsInUse(), ""),
                shiny::need(prep, "")
            )
            temp_need1 = lapply(intersect(prep$metrics_num, metricsInUse()), function(m) {
                cond <- paste0("shiny::need(is.logical(input$", m, "_flip) && !is.null(input$", m, "_offset) && !is.null(input$", m, "_transform), '')")
                eval(parse(text = cond))
            })
            do.call(shiny::validate, temp_need1)
            temp_need2 = lapply(intersect(prep$metrics_cat, metricsInUse()), function(m) {
                cond <- paste0("shiny::need(!is.null(input$", m, "_levels), '')")
                eval(parse(text = cond))
            })
            do.call(shiny::validate, temp_need2)
            
            tmp <- filtdata()
            for (m in intersect(colnames(filtdata()), metricsInUse())) {
                if (m %in% prep$metrics_num) {
                    tmp[[m]] <- .transformNumericVariable(
                        x = filtdata()[[m]],
                        flip = input[[paste0(m, "_flip")]], 
                        offset = input[[paste0(m, "_offset")]], 
                        transf = .getTransf(input[[paste0(m, "_transform")]]), 
                        bincuts = sort(as.numeric(input[[paste0(m, "_bincuts")]]))
                    )
                } else if (m %in% prep$metrics_cat) {
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
            if (input$scoreMethod == "weighted mean") {
                scoreDf <- collapseddata() %>%
                    dplyr::group_by(.data[[idCol]]) %>%
                    dplyr::summarize(
                        "{scoreCol}" := sum(.data[[weightCol]] *
                                                .data[[valueCol]],
                                            na.rm = TRUE) / 
                            sum(.data[[weightCol]] * !is.na(.data[[valueCol]]), 
                                na.rm = TRUE)
                    ) 
            } else if (input$scoreMethod == "weighted median") {
                scoreDf <- collapseddata() %>%
                    dplyr::group_by(.data[[idCol]]) %>%
                    dplyr::summarize(
                        "{scoreCol}" := as.numeric(Hmisc::wtd.quantile(
                            x = .data[[valueCol]], 
                            w = .data[[weightCol]],
                            probs = 0.5,
                            na.rm = TRUE))
                    ) 
            } else if (input$scoreMethod == "weighted fraction highest") {
                scoreDf <- collapseddata() %>%
                    dplyr::group_by(.data[[metricCol]]) %>%
                    dplyr::mutate(
                        tempScore = (.data[[valueCol]] == 
                                         max(.data[[valueCol]], na.rm = TRUE))
                    ) %>%
                    dplyr::ungroup() %>%
                    dplyr::group_by(.data[[idCol]]) %>%
                    dplyr::summarize(
                        "{scoreCol}" := sum(
                            .data[[weightCol]] * .data$tempScore,
                            na.rm = TRUE) / 
                            sum(.data[[weightCol]] * !is.na(.data[[valueCol]]), 
                                na.rm = TRUE)
                    ) 
            } else if (input$scoreMethod == "weighted fraction lowest") {
                scoreDf <- collapseddata() %>%
                    dplyr::group_by(.data[[metricCol]]) %>%
                    dplyr::mutate(
                        tempScore = (.data[[valueCol]] == 
                                         min(.data[[valueCol]], na.rm = TRUE))
                    ) %>%
                    dplyr::ungroup() %>%
                    dplyr::group_by(.data[[idCol]]) %>%
                    dplyr::summarize(
                        "{scoreCol}" := sum(
                            .data[[weightCol]] * .data$tempScore,
                            na.rm = TRUE) / 
                            sum(.data[[weightCol]] * !is.na(.data[[valueCol]]), 
                                na.rm = TRUE)
                    ) 
            }
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
            shiny::updateNumericInput(session, "hmheight",
                                      value = 200 + 35 * length(unique(tmp[[idCol]])))
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
        shiny::outputOptions(output, "idFilterByInfoUI", suspendWhenHidden = FALSE)
        
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
        shiny::outputOptions(output, "metricFilterByInfoUI", suspendWhenHidden = FALSE)
        
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
        shiny::outputOptions(output, "metricToManipulateUI", suspendWhenHidden = FALSE)
        
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
            shiny::outputOptions(output, "metricManipulationSummaryUI", suspendWhenHidden = FALSE)
        })
        
        ## Create transformation interface for numeric metrics ----------------
        lapply(prep$metrics_num, function(m) {
            output[[paste0(m, "_transformUI")]] <- shiny::renderUI({
                shiny::tagList(
                    shiny::checkboxInput(
                        inputId = paste0(m, "_flip"),
                        label = "Flip",
                        value = prep$initialTransforms[[m]][["flip"]]
                    ),
                    shiny::numericInput(
                        inputId = paste0(m, "_offset"),
                        label = "Offset",
                        value = prep$initialTransforms[[m]][["offset"]]
                    ),
                    shiny::radioButtons(
                        inputId = paste0(m, "_transform"),
                        label = "Transform",
                        choices = c("None", "z-score",
                                    "[0,1]", "[-1,1]",
                                    "Rank", "Rank+[0,1]", "z-score+[0,1]"),
                        selected = prep$initialTransforms[[m]][["transform"]]
                    ),
                    shiny::selectizeInput(
                        inputId = paste0(m, "_bincuts"),
                        label = "Cut points for\ncategorization",
                        choices = prep$initialTransforms[[m]][["cuts"]],
                        selected = prep$initialTransforms[[m]][["cuts"]],
                        multiple = TRUE,
                        options = list(create = TRUE)
                    )
                )
            })
        })
        
        ## Create transformation interface for categorical metrics ------------
        lapply(prep$metrics_cat, function(m) {
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
        
        ## Score table --------------------------------------------------------
        output$scoreTable <- DT::renderDataTable({
            tmpdf <- plotdata() %>%
                dplyr::select(-.data[[weightCol]]) %>%
                tidyr::pivot_wider(names_from = .data[[metricCol]], 
                                   values_from = .data[[valueCol]]) %>%
                dplyr::left_join(scoredata(), by = idCol) %>%
                dplyr::mutate("{scoreCol}" := signif(.data[[scoreCol]], 
                                                     digits = 4))
            if (input$id_ordering == "high-to-low") {
                tmpdf %>%
                    dplyr::arrange(dplyr::desc(.data[[scoreCol]]))
            } else {
                tmpdf %>%
                    dplyr::arrange(.data[[scoreCol]])
            }
        })
        
        ## Parallel coordinates plot ------------------------------------------
        output$bettrParCoordplotUI <- shiny::renderUI({
            shinyjqui::jqui_resizable(shiny::plotOutput(
                "bettrParCoordplot"))
        })
        output$bettrParCoordplot <- shiny::renderPlot({
            if (is.null(plotdata()) || is.null(scoredata())) {
                NULL
            } else {
                .makeParCoordPlot(df = plotdata(), idCol = idCol, 
                                  metricCol = metricCol, valueCol = valueCol, 
                                  metricGroupCol = metricGroupCol, methods = methodsInUse(),
                                  highlightMethod = input$highlightMethod, 
                                  metricGrouping = input$metricGrouping,
                                  labelSize = input$labelsize, 
                                  metricColors = prep$metricColors,
                                  idColors = prep$idColors,
                                  metricCollapseGroup = input$metricCollapseGroup)
            }
        })
        
        ## Polar plot ---------------------------------------------------------
        output$bettrPolarplotUI <- shiny::renderUI({
            shinyjqui::jqui_resizable(shiny::plotOutput(
                "bettrPolarplot"))
        })
        output$bettrPolarplot <- shiny::renderPlot({
            if (is.null(plotdata()) || is.null(scoredata())) {
                NULL
            } else {
                .makePolarPlot(df = plotdata(), scores = scoredata(), idCol = idCol, 
                               metricCol = metricCol, valueCol = valueCol,
                               weightCol = weightCol, scoreCol = scoreCol,
                               metricGroupCol = metricGroupCol, 
                               labelSize = input$labelsize,
                               metricColors = prep$metricColors,
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
            if (is.null(plotdata()) || is.null(scoredata())) {
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
                                  metricColors = prep$metricColors,
                                  metricCollapseGroup = input$metricCollapseGroup,
                                  metricGrouping = input$metricGrouping,
                                  showOnlyTopIds = input$showOnlyTopIds,
                                  nbrTopIds = input$nbrTopIds)
            }
        })
        
        ## Heatmap ------------------------------------------------------------
        output$bettrHeatmapUI <- shiny::renderUI({
            shinyjqui::jqui_resizable(shiny::plotOutput("bettrHeatmap",
                                                        height = paste0(input$hmheight, "px")))
        })
        # observeEvent(input$update_size, {
        #     shiny::updateNumericInput(session, "hmheight", value = input$bettrHeatmap_size[[2]])
        # })
        output$bettrHeatmap <- shiny::renderPlot({
            if (is.null(plotdata()) || is.null(scoredata())) {
                NULL
            } else {
                .makeHeatmap(df = plotdata(), scores = scoredata(), idCol = idCol, 
                             metricCol = metricCol, valueCol = valueCol, 
                             weightCol = weightCol, scoreCol = scoreCol, 
                             metricGroupCol = metricGroupCol, 
                             metricInfo = values$metricInfo,
                             idInfo = values$idInfo,
                             labelSize = input$labelsize,
                             idColors = prep$idColors, 
                             metricColors = prep$metricColors,
                             metricCollapseGroup = input$metricCollapseGroup,
                             metricGrouping = input$metricGrouping, 
                             showRowNames = input$show_row_names,
                             showOnlyTopIds = input$showOnlyTopIds,
                             nbrTopIds = input$nbrTopIds,
                             rownamewidth_cm = input$hm_rownamewidth,
                             colnameheight_cm = input$hm_colnameheight)
            }
        })
        
        
        ## Define weight controls ---------------------------------------------
        ## Make sure that weights are retained even when the collapsing by
        ## group status (and thus the displayed weight sliders) changes
        shiny::observe({
            lapply(prep$metricsWithWeights, function(mww) {
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
