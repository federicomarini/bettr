#' Launch bettr app to explore and aggregate performance metrics
#' 
#' @param df A \code{data.frame} in wide format. 
#' @param methodCol Character scalar, indicating which column of \code{df} 
#'   that contains method IDs.
#' @param metrics_num,metrics_cat Character vectors, indicating which of the 
#'   columns of \code{df} that correspond to, numeric and categorical, 
#'   respectively, metrics of interest.
#' @param initialWeights Named numeric vector providing initial weights for 
#'   aggregating the metric scores. Must have one entry per metric included 
#'   in \code{df}.
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
#' @importFrom ggplot2 expand_limits theme element_text theme_minimal geom_bar 
#'   aes ggplot labs element_blank coord_polar ylim geom_col facet_wrap 
#'   scale_size_manual geom_point geom_line geom_boxplot coord_flip geom_jitter 
#' @importFrom dplyr group_by summarize mutate ungroup arrange select filter 
#'   %>% pull desc contains
#' @importFrom cowplot draw_plot plot_grid
#' @importFrom tidyr spread gather
#' @importFrom ComplexHeatmap Heatmap columnAnnotation rowAnnotation
#'   anno_barplot
#' @importFrom tibble tibble column_to_rownames
#' @importFrom bslib bs_theme
#' @importFrom rlang sym :=
#' @importFrom grid gpar unit
#' @importFrom circlize colorRamp2
#' @importFrom methods is
#' 
bettr <- function(df, methodCol = "Method", 
                  metrics_num = setdiff(colnames(df), methodCol),
                  metrics_cat = c(),
                  initialWeights = NULL,
                  metricGroups = list()) {
    ## All metrics (numeric and categorical) ----------------------------------
    metrics <- c(metrics_num, metrics_cat)
    
    ## Check input arguments --------------------------------------------------
    stopifnot(exprs = {
        methods::is(df, "data.frame")
        methods::is(methodCol, "character")
        length(methodCol) == 1
        methodCol %in% colnames(df)
        all(metrics %in% colnames(df))
        is.null(initialWeights) || (is.numeric(initialWeights) &&
                                        !is.null(names(initialWeights)) &&
                                        all(metrics %in%
                                                names(initialWeights)) &&
                                        !all(initialWeights == 0) && 
                                        all(initialWeights >= 0) && 
                                        all(initialWeights <= 1))
        all(sapply(metrics_num, function(m) is.numeric(df[[m]])))
        all(sapply(metrics_cat, function(m) is.factor(df[[m]]) || 
                       is.character(df[[m]])))
        is.list(metricGroups)
        length(metricGroups) == 0 || (!is.null(names(metricGroups)) &&
                                          all(sapply(metricGroups, function(mg) {
                                              all(metrics %in% names(mg))
                                          })))
    })

    ## Define column names assigned by the function ---------------------------
    scoreCol <- "Score"
    weightCol <- "Weight"
    metricCol <- "Metric"
    valueCol <- "ScaledValue"
    groupCol <- "Group"
    
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
                                        shiny::plotOutput("bettrHeatmap")),
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
            methods = unique(df[[methodCol]])
        )
        
        ## Processed data -----------------------------------------------------
        procdata <- shiny::reactive({
            tmp <- values$df
            for (m in values$metrics) {
                if (is.numeric(values$df[[m]])) {
                    tmp[[m]] <- transformNumericVariable(
                        x = values$df[[m]],
                        flip = input[[paste0(m, "_flip")]], 
                        offset = input[[paste0(m, "_offset")]], 
                        transf = getTransf(input[[paste0(m, "_transform")]]), 
                        bincuts = NULL
                    )
                } else {
                    tmp[[m]] <- transformCategoricalVariable(
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
                dplyr::select(!!rlang::sym(methodCol), 
                              dplyr::contains(values$metrics)) %>%
                tidyr::gather(key = "Metric", value = "ScaledValue", 
                              -!!rlang::sym(methodCol))
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
            shiny::selectizeInput(inputId = "highlightMethod",
                                  label = "Highlight method",
                                  choices = c("---", values$methods), 
                                  selected = "---")
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
                        label = paste("Flip", m),
                        value = FALSE
                    ),
                    shiny::numericInput(
                        inputId = paste0(m, "_offset"),
                        label = paste("Offset", m),
                        value = 0
                    ),
                    shiny::radioButtons(
                        inputId = paste0(m, "_transform"),
                        label = paste("Transform", m),
                        choices = c("None", "z-score",
                                    "[0,1]", "[-1,1]",
                                    "Rank"),
                        selected = "None"
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
                lwidths <- rep(0.75, length(values$methods))
                names(lwidths) <- values$methods
                if (input$highlightMethod != "---") {
                    lwidths[input$highlightMethod] <- 2.5
                }
                if (input$metricGrouping != "---") {
                    tmp <- longdata() %>% 
                        dplyr::arrange(!!rlang::sym(groupCol)) %>%
                        dplyr::mutate("{metricCol}" := factor(
                            !!rlang::sym(metricCol),
                            levels = unique(!!rlang::sym(metricCol))))
                    gp <- ggplot2::ggplot(tmp,
                                          aes(x = !!rlang::sym(metricCol), 
                                              y = !!rlang::sym(valueCol))) + 
                        ggplot2::geom_boxplot(outlier.size = -1,
                                              aes(fill = !!rlang::sym(groupCol)),
                                              alpha = 0.4)
                } else {
                    tmp <- longdata()
                    gp <- ggplot2::ggplot(tmp,
                                          aes(x = !!rlang::sym(metricCol), 
                                              y = !!rlang::sym(valueCol))) + 
                        ggplot2::geom_boxplot(outlier.size = -1)
                }
                gp + 
                    ggplot2::geom_line(aes(group = !!rlang::sym(methodCol),
                                           color = !!rlang::sym(methodCol),
                                           size = !!rlang::sym(methodCol)),
                                       alpha = 0.75) +
                    ggplot2::geom_point(aes(group = !!rlang::sym(methodCol),
                                            color = !!rlang::sym(methodCol))) +
                    ggplot2::scale_size_manual(values = lwidths) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(axis.text.x = ggplot2::element_text(
                        angle = 90, hjust = 1, vjust = 0.5))
            }
        })
        
        ## Polar plot ---------------------------------------------------------
        output$bettrPolarplot <- shiny::renderPlot({
            if (is.null(longdata())) {
                NULL
            } else {
                levs <- longdata() %>%
                    dplyr::group_by(!!rlang::sym(methodCol)) %>%
                    dplyr::summarize(
                        "{scoreCol}" := sum(!!rlang::sym(weightCol) *
                                                !!rlang::sym(valueCol),
                                            na.rm = TRUE)
                    ) %>%
                    dplyr::arrange(dplyr::desc(!!rlang::sym(scoreCol))) %>%
                    dplyr::pull(!!rlang::sym(methodCol))
                ggplot2::ggplot(longdata() %>% 
                                    dplyr::mutate("{methodCol}" := 
                                                      factor(!!rlang::sym(methodCol),
                                                             levels = levs)),
                                ggplot2::aes(x = !!rlang::sym(metricCol), 
                                             y = !!rlang::sym(valueCol),
                                             fill = !!rlang::sym(metricCol))) + 
                    ggplot2::geom_col(width = 1, color = "white") +
                    ggplot2::coord_polar() + 
                    ggplot2::facet_wrap(facets = methodCol) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(axis.text = ggplot2::element_blank())
            }
        })
        
        ## Bar + polar plot ---------------------------------------------------
        output$bettrBarPolarplot <- shiny::renderPlot({
            if (is.null(longdata())) {
                NULL
            } else {
                ## Define polar plots
                rplots <- lapply(values$methods, function(m) {
                    ggplot2::ggplot(longdata() %>% 
                                        dplyr::filter(!!rlang::sym(methodCol) == m),
                                    ggplot2::aes(x = !!rlang::sym(metricCol), 
                                                 y = !!rlang::sym(valueCol),
                                                 fill = !!rlang::sym(metricCol))) + 
                        ggplot2::geom_col(width = 1, color = "white") +
                        ggplot2::ylim(min(0, min(longdata()[[valueCol]])),
                                      max(longdata()[[valueCol]])) + 
                        ggplot2::coord_polar() + 
                        ggplot2::theme_minimal() +
                        ggplot2::theme(axis.text = ggplot2::element_blank(),
                                       legend.position = "none",
                                       plot.background = ggplot2::element_blank(),
                                       plot.margin = grid::unit(c(0, 0, 0, 0), "cm"),
                                       panel.spacing = grid::unit(0, "cm")) + 
                        ggplot2::labs(x = "", y = "")
                })
                names(rplots) <- values$methods
                
                scores <- longdata() %>%
                    dplyr::group_by(!!rlang::sym(methodCol)) %>%
                    dplyr::summarize(
                        "{scoreCol}" := sum(!!rlang::sym(weightCol) *
                                                !!rlang::sym(valueCol),
                                            na.rm = TRUE)
                    ) %>%
                    dplyr::arrange(dplyr::desc(!!rlang::sym(scoreCol)))
                levs <- scores %>%
                    dplyr::pull(!!rlang::sym(methodCol))
                rx <- length(levs)
                ry <- max(0, max(scores[[scoreCol]])) - min(0, min(scores[[scoreCol]]))
                sx <- 2.5
                sy <- ry/rx * sx
                
                bplot <- ggplot2::ggplot(longdata() %>% 
                                    dplyr::mutate("{methodCol}" := 
                                                      factor(!!rlang::sym(methodCol),
                                                             levels = levs)),
                                ggplot2::aes(x = !!rlang::sym(methodCol), 
                                             y = !!rlang::sym(weightCol) * 
                                                 !!rlang::sym(valueCol),
                                             fill = !!rlang::sym(metricCol))) +
                    ggplot2::geom_bar(stat = "identity", width = 0.1) + 
                    ggplot2::theme_minimal() +
                    ggplot2::theme(axis.text.x = ggplot2::element_text(
                        angle = 90, hjust = 1, vjust = 0.5)) +
                    ggplot2::expand_limits(y = max(scores[[scoreCol]]) + sy)
                bplot <- bplot + 
                    ggplot2::theme(legend.position = "none")
                for (i in seq_along(levs)) {
                    l <- levs[i]
                    bplot <- bplot +
                        cowplot::draw_plot(
                            rplots[[l]], x = (i - sx/2 - 0.1), 
                            y = scores[[scoreCol]][scores[[methodCol]] == l],
                            width = sx, height = sy, scale = 1.5, 
                            hjust = 0, vjust = 0,
                            halign = 0.5, valign = 0.5)
                }
                bplot
            }
        })
        
        ## Heatmap ------------------------------------------------------------
        output$bettrHeatmap <- shiny::renderPlot({
            if (is.null(longdata())) {
                NULL
            } else {
                rowAnnot <- longdata() %>%
                    dplyr::group_by(!!rlang::sym(methodCol)) %>%
                    dplyr::summarize(
                        "{scoreCol}" := sum(!!rlang::sym(weightCol) *
                                                !!rlang::sym(valueCol),
                                            na.rm = TRUE)
                    ) %>%
                    dplyr::arrange(dplyr::desc(!!rlang::sym(scoreCol)))
                tmp <- longdata()
                tmp[[methodCol]] <- factor(tmp[[methodCol]], 
                                           levels = rowAnnot[[methodCol]])
                mat <- tmp %>%
                    dplyr::select(c(!!rlang::sym(methodCol),
                                    !!rlang::sym(metricCol),
                                    !!rlang::sym(valueCol))) %>%
                    tidyr::spread(key = !!rlang::sym(metricCol),
                                  value = !!rlang::sym(valueCol), fill = NA) %>%
                    as.data.frame() %>%
                    tibble::column_to_rownames(var = methodCol) %>%
                    as.matrix()
                rowAnnot <- rowAnnot[match(rownames(mat), 
                                           rowAnnot[[methodCol]]), ,
                                     drop = FALSE] %>% as.data.frame() %>%
                    tibble::column_to_rownames(var = methodCol)
                
                colAnnot <- longdata() %>%
                    dplyr::filter(!duplicated(!!rlang::sym(metricCol))) %>%
                    dplyr::select(c(metricCol, weightCol,
                                    dplyr::contains(groupCol))) 
                if (groupCol %in% colnames(colAnnot)) {
                    colAnnot <- colAnnot %>%
                        dplyr::arrange(!!rlang::sym(groupCol))
                    mat <- mat[, match(colAnnot[[metricCol]], 
                                       colnames(mat)), drop = FALSE]
                }
                colAnnot <- tibble::tibble(
                    colAnnot[match(colnames(mat), 
                                   colAnnot[[metricCol]]), , 
                             drop = FALSE]) %>% as.data.frame() %>%
                    tibble::column_to_rownames(var = metricCol)
                
                rowAnnot <- ComplexHeatmap::rowAnnotation(
                    Score = ComplexHeatmap::anno_barplot(
                        rowAnnot[[scoreCol]],
                        width = grid::unit(2, "cm"), 
                        baseline = 0)
                )
                if (groupCol %in% colnames(colAnnot)) {
                    colAnnot <- ComplexHeatmap::columnAnnotation(
                        Weight = ComplexHeatmap::anno_barplot(
                            colAnnot[[weightCol]],
                            height = grid::unit(2, "cm"), 
                            baseline = 0),
                        Group = colAnnot[[groupCol]]
                    )
                } else {
                    colAnnot <- ComplexHeatmap::columnAnnotation(
                        Weight = ComplexHeatmap::anno_barplot(
                            colAnnot[[weightCol]],
                            height = grid::unit(2, "cm"), 
                            baseline = 0)
                    )
                }
                
                heatmapCols = circlize::colorRamp2(
                    seq(min(mat), max(mat), length = 3), 
                    c("blue", "#EEEEEE", "red")
                )
                
                ComplexHeatmap::Heatmap(
                    matrix = mat, name = "Relative\nvalue",
                    col = heatmapCols,
                    na_col = "lightgrey",
                    rect_gp = grid::gpar(col = "white", lwd = 1),
                    cluster_rows = FALSE, 
                    cluster_columns = FALSE, 
                    row_names_side = "left",
                    top_annotation = colAnnot, 
                    right_annotation = rowAnnot
                )
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