#' Launch bettr app to explore and aggregate performance metrics
#' 
#' @param df A \code{data.frame} in wide format. 
#' @param methodCol Character scalar, indicating which column of \code{df} 
#'   that contains method IDs.
#' @param metrics Character vector, indicating which of the columns of 
#'   \code{df} that correspond to metrics of interest.
#' @param initialWeights Named numeric vector providing initial weights for 
#'   aggregating the metric scores. Must have one entry per metric included 
#'   in \code{df}.
#'  
#' @export
#' 
#' @author Charlotte Soneson
#' 
#' @importFrom shiny navbarPage plotOutput numericInput br fluidRow titlePanel
#'   fluidRow column tabsetPanel radioButtons actionButton uiOutput 
#'   reactiveValues observeEvent updateNumericInput isolate renderPlot
#'   tagList shinyApp tabPanel
#' @importFrom stats sd
#' @importFrom ggplot2 ggplot aes geom_bar theme_minimal geom_boxplot geom_line 
#'   geom_point scale_size_manual theme geom_col coord_polar facet_wrap 
#'   element_blank labs ylim expand_limits
#' @importFrom dplyr group_by summarize mutate ungroup arrange select filter 
#'   %>% pull
#' @importFrom cowplot draw_plot
#' @importFrom tidyr spread
#' @importFrom ComplexHeatmap Heatmap columnAnnotation rowAnnotation
#'   anno_barplot
#' @importFrom tibble tibble column_to_rownames
#' @importFrom bslib bs_theme
#' @importFrom rlang sym :=
#' @importFrom grid gpar unit
#' @importFrom circlize colorRamp2
#' @importFrom methods is
#' 
bettr <- function(df, methodCol = "Method", metrics = setdiff(colnames(df), methodCol),
                  initialWeights = NULL) {
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
                                        !all(initialWeights == 0))
    })

    ## Define column names assigned by the function ---------------------------
    scoreCol <- "Score"
    weightCol <- "Weight"
    metricCol <- "Metric"
    valueCol <- "ScaledValue"
    
    ## Assign initial weights -------------------------------------------------
    if (is.null(initialWeights)) {
        nMetrics <- length(metrics)
        initialWeights <- rep(0.2, nMetrics)
        names(initialWeights) <- metrics
    } else {
        ## TODO: Scale initial weights to [0,1]
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
                        tabPanel("Heatmap", 
                                 shiny::plotOutput("bettrHeatmap")),
                        tabPanel("Parallel coordinates", 
                                 shiny::plotOutput("bettrParCoordplot")),
                        tabPanel("Polar plot", 
                                 shiny::plotOutput("bettrPolarplot")),
                        tabPanel("Bar/polar plot",
                                 shiny::plotOutput("bettrBarPolarplot"))
                    ),
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
                                                "metricManipulationSummaryID")
                        )
                    )
                ),
                
                ## Controls ---------------------------------------------------
                shiny::column(
                    3, 
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
            methods = unique(df[[methodCol]])
        )
        
        ## Processed data
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
                        levels = unique(values$df[[m]])
                    )
                }
            }
            tmp
        })
        
        longdata <- shiny::reactive({
            pd <- procdata() %>%
                dplyr::select(!!rlang::sym(methodCol), 
                              dplyr::contains(values$metrics)) %>%
                tidyr::gather(key = "Metric", value = "ScaledValue", 
                              -!!rlang::sym(methodCol))
            for (m in metrics) {
                pd[[weightCol]][pd$Metric == m] <- input[[paste0(m, "_weight")]]
            }
            pd
        })
        
        output$highlightMethodUI <- shiny::renderUI({
            shiny::selectizeInput(inputId = "highlightMethod",
                                  label = "Highlight method",
                                  choices = c("---", values$methods), 
                                  selected = "---")
        })
        
        output$metricToManipulateUI <- shiny::renderUI({
            shiny::selectizeInput(inputId = "metricToManipulate",
                                  label = "Select metric",
                                  choices = c("---", values$metrics),
                                  selected = "---")
        })
        
        ## Data transformations
        shiny::observeEvent(input$metricToManipulate, {
            shiny::updateTabsetPanel(inputId = "metricManipulationSummary", 
                                     selected = input$metricToManipulate)
        })
        ## Make one tab panel per metric
        shiny::observe({
            output$metricManipulationSummaryID <- shiny::renderUI({
                do.call(shiny::tabsetPanel, 
                        c(list(type = "hidden", 
                               id = "metricManipulationSummary"), 
                          lapply(values$metrics, function(i) {
                              shiny::tabPanelBody(
                                  value = i, 
                                  shiny::fluidRow(
                                      shiny::column(4, 
                                                    shiny::checkboxInput(
                                                        inputId = paste0(i, "_flip"), 
                                                        label = paste("Flip", i), 
                                                        value = FALSE
                                                    ),
                                                    shiny::numericInput(
                                                        inputId = paste0(i, "_offset"),
                                                        label = paste("Offset", i), 
                                                        value = 0
                                                    ),
                                                    shiny::radioButtons(
                                                        inputId = paste0(i, "_transform"),
                                                        label = paste("Transform", i),
                                                        choices = c("None", "z-score",
                                                                    "[0,1]", "[-1,1]",
                                                                    "Rank"),
                                                        selected = "None"
                                                    )
                                      ), 
                                      shiny::column(8, 
                                                    shiny::plotOutput(
                                                        outputId = paste0(i, "_plotsummary")
                                                    )) 
                                  )
                              )
                          })
                        )
                )
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
        
        ## Define plots -------------------------------------------------------
        output$bettrParCoordplot <- shiny::renderPlot({
            if (is.null(longdata())) {
                NULL
            } else {
                lwidths <- rep(0.75, length(values$methods))
                names(lwidths) <- values$methods
                if (input$highlightMethod != "---") {
                    lwidths[input$highlightMethod] <- 2.5
                }
                ggplot2::ggplot(longdata(),
                                aes(x = !!rlang::sym(metricCol), 
                                    y = !!rlang::sym(valueCol))) + 
                    ggplot2::geom_boxplot(outlier.size = -1) + 
                    ggplot2::geom_line(aes(group = !!rlang::sym(methodCol),
                                           color = !!rlang::sym(methodCol),
                                           size = !!rlang::sym(methodCol)),
                                       alpha = 0.75) +
                    ggplot2::geom_point(aes(group = !!rlang::sym(methodCol),
                                            color = !!rlang::sym(methodCol))) +
                    ggplot2::scale_size_manual(values = lwidths) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                                       hjust = 1,
                                                                       vjust = 0.5))
            }
        })
        
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
                                       plot.margin = unit(c(0, 0, 0, 0), "cm"),
                                       panel.spacing = unit(0, "cm")) + 
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
                    theme(legend.position = "none")
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
        
        ## This gets kind of strange with negative values
        output$bettrBarplot <- shiny::renderPlot({
            if (is.null(longdata())) {
                NULL
            } else {
                ggplot2::ggplot(longdata(), 
                                ggplot2::aes(x = !!rlang::sym(methodCol),
                                             y = !!rlang::sym(weightCol) * 
                                                 !!rlang::sym(valueCol),
                                             fill = !!rlang::sym(metricCol))) + 
                    ggplot2::geom_bar(stat = "identity", position = "stack") + 
                    ggplot2::theme_minimal() + 
                    ggplot2::theme(axis.text.x = ggplot2::element_text(
                        angle = 90, hjust = 1, vjust = 0.5))
            }
        })
        
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
                    dplyr::select(c(!!rlang::sym(metricCol), weightCol)) 
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
                colAnnot <- ComplexHeatmap::columnAnnotation(
                    Weight = ComplexHeatmap::anno_barplot(
                        colAnnot[[weightCol]],
                        height = grid::unit(2, "cm"), 
                        baseline = 0)
                )
                
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
        
        shiny::observe({
            lapply(values$metrics, function(m) {
                output[[paste0(m, "_plotsummary")]] <- renderPlot({
                    hist(procdata()[[m]])
                })
            })
            
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