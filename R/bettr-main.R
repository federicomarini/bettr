#' Launch bettr app to explore and aggregate performance metrics
#' 
#' @param df A \code{data.frame} in long format. Must have at least 
#'   three columns: one providing the method ID (for the methods 
#'   that we want to compare), one providing the metric ID, and
#'   one providing the value of each metric for each method.
#' @param methodCol,metricCol,valueCol Character scalars, 
#'   indicating which columns of \code{df} that contain method IDs, metric IDs
#'   and metric values, respectively.
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
bettr <- function(df, methodCol = "Method", metricCol = "Metric",
                  valueCol = "Value", initialWeights = NULL) {
    ## Check input arguments --------------------------------------------------
    stopifnot(exprs = {
        methods::is(df, "data.frame")
        methods::is(methodCol, "character")
        length(methodCol) == 1
        methods::is(metricCol, "character")
        length(metricCol) == 1
        methods::is(valueCol, "character")
        length(valueCol) == 1
        methodCol %in% colnames(df)
        metricCol %in% colnames(df)
        valueCol %in% colnames(df)
        methods::is(df[[valueCol]], "numeric")
        !("Score" %in% colnames(df))
        !("Weight" %in% colnames(df))
        !("ScaledValue" %in% colnames(df))
        is.null(initialWeights) || (is.numeric(initialWeights) && 
                                        !is.null(names(initialWeights)) && 
                                        all(unique(df[[metricCol]]) %in% 
                                                names(initialWeights)) && 
                                        !all(initialWeights == 0))
    })
    
    df[[methodCol]] <- as.character(df[[methodCol]])
    df[[metricCol]] <- as.character(df[[metricCol]])
    
    ## Define column names assigned by the function ---------------------------
    scoreCol <- "Score"
    weightCol <- "Weight"
    
    ## Assign initial weights -------------------------------------------------
    if (is.null(initialWeights)) {
        nMetrics <- length(unique(df[[metricCol]]))
        initialWeights <- rep(0.5, nMetrics)
        # initialWeights <- rep(1/nMetrics, nMetrics)
        names(initialWeights) <- unique(df[[metricCol]])
    } else {
        ## Scale to [0, 1]
        if (length(unique(abs(initialWeights))) == 1) {
            initialWeights <- rep(0.5, nMetrics)
        } else {
            initialWeights <- 
                (abs(initialWeights) - min(abs(initialWeights))) /
                (max(abs(initialWeights)) - min(abs(initialWeights)))
        }
    }
    df[[weightCol]] <- initialWeights[df[[metricCol]]]
    
    initValueCol <- valueCol
    df <- df %>% 
        dplyr::mutate(ScaledValue = !!rlang::sym(valueCol))
    valueCol <- "ScaledValue"
    
    ## UI definition ----------------------------------------------------------
    p_layout <- 
        shiny::navbarPage(
            shiny::titlePanel("bettr"),
            theme = bslib::bs_theme(bootswatch = "darkly"),
            
            shiny::br(),
            
            shiny::fluidRow(
                ## Plot
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
                    shiny::fluidRow(
                        shiny::column(4, 
                                      shiny::radioButtons(
                                          inputId = "scaleType",
                                          label = "Scaling of \n metrics",
                                          choices = c("None", "z-score",
                                                      "[0,1]", "[-1,1]",
                                                      "Rank"),
                                          selected = "None"
                                      )
                        ), 
                        shiny::column(4, 
                                      shiny::uiOutput(
                                          outputId = "highlightMethodUI"
                                      )
                        )
                    )
                ),
                
                ## Controls
                shiny::column(
                    3, 
                    shiny::actionButton(inputId = "resetWeights", 
                                        label = "Reset to uniform weights"),
                    shiny::hr(color = "white"), 
                    shiny::actionButton(inputId = "scaleWeights",
                                        label = "Scale weights to sum to 1"),
                    shiny::hr(color = "white"), 
                    shiny::uiOutput(outputId = "weights")
                )
            )
        )
    
    ## Server definition ------------------------------------------------------
    server_function <- function(input, output, session) {
        
        ## Initialize data storage --------------------------------------------
        values <- shiny::reactiveValues(
            df = df,
            nMetrics = length(unique(df[[metricCol]])),
            metrics = unique(df[[metricCol]]),
            methods = unique(df[[methodCol]])
        )
        
        output$highlightMethodUI <- shiny::renderUI({
            shiny::selectizeInput(inputId = "highlightMethod",
                                  label = "Highlight method",
                                  choices = c("---", values$methods), 
                                  selected = "---")
        })
        
        ## Limit the possible weights (have to be in (0, 1)) ------------------
        ## Limit the values to [0.001, 1 - (nMetrics * 0.001)]
        ## Must be executed _before_ the other inputs are updated
        ## TODO: This doesn't work as intended
        # lapply(unique(df[[metricCol]]), function(i) {
        #   qnum <- paste0(i, "_weight")
        #   shiny::observeEvent(input[[qnum]], {
        #     if (input[[qnum]] > 0.99) {
        #       shiny::updateNumericInput(session, inputId = qnum, value = 0.99)
        #     } else if (input[[qnum]] < 0.001) {
        #       shiny::updateNumericInput(session, inputId = qnum, value = 0.001)
        #     }
        #   }, priority = 10)
        # })
        
        ## Reset all weights upon action button click -------------------------
        shiny::observeEvent(input$resetWeights, {
            for (j in values$metrics) {
                shiny::updateNumericInput(
                    session, inputId = paste0(j, "_weight"), 
                    value = 0.5
                )
            }
        })
        
        ## Scale weights upon action button click -----------------------------
        shiny::observeEvent(input$scaleWeights, {
            totWeight <- 
                sum(abs(values$df[[weightCol]][!duplicated(values$df[[metricCol]])]))
            if (totWeight == 0) {
                totWeight <- 1
            }
            for (j in values$metrics) {
                shiny::updateNumericInput(
                    session, inputId = paste0(j, "_weight"), 
                    value = input[[paste0(j, "_weight")]]/totWeight
                )
            }
        })
        
        ## Scale data ---------------------------------------------------------
        shiny::observeEvent(input$scaleType, {
            if (input$scaleType == "None") {
                values$df <- values$df %>%
                    dplyr::mutate(ScaledValue = !!rlang::sym(initValueCol))
            } else if (input$scaleType == "[0,1]") {
                values$df <- values$df %>%
                    dplyr::group_by(!!rlang::sym(metricCol)) %>%
                    dplyr::mutate(
                        ScaledValue = (!!rlang::sym(initValueCol) - 
                                           min(!!rlang::sym(initValueCol)))/
                            (max(!!rlang::sym(initValueCol)) - 
                                 min(!!rlang::sym(initValueCol)))
                    ) %>%
                    dplyr::ungroup() %>%
                    as.data.frame()
            } else if (input$scaleType == "[-1,1]") {
                values$df <- values$df %>%
                    dplyr::group_by(!!rlang::sym(metricCol)) %>%
                    dplyr::mutate(
                        ScaledValue = (!!rlang::sym(initValueCol) - 
                                           min(!!rlang::sym(initValueCol)) + 
                                           !!rlang::sym(initValueCol) - 
                                           max(!!rlang::sym(initValueCol)))/
                            (max(!!rlang::sym(initValueCol)) - 
                                 min(!!rlang::sym(initValueCol)))
                    ) %>%
                    dplyr::ungroup() %>%
                    as.data.frame()
            } else if (input$scaleType == "z-score") {
                values$df <- values$df %>%
                    dplyr::group_by(!!rlang::sym(metricCol)) %>%
                    dplyr::mutate(
                        ScaledValue = (!!rlang::sym(initValueCol) - 
                                           mean(!!rlang::sym(initValueCol)))/
                            stats::sd(!!rlang::sym(initValueCol))
                    ) %>%
                    dplyr::ungroup() %>%
                    as.data.frame()
            } else if (input$scaleType == "Rank") {
                values$df <- values$df %>%
                    dplyr::group_by(!!rlang::sym(metricCol)) %>%
                    dplyr::mutate(
                        ScaledValue = order(order(!!rlang::sym(initValueCol)))
                    ) %>%
                    dplyr::ungroup() %>%
                    as.data.frame()
            }
            
        })
        
        ## Modify the 'Weight' column of values$df when any slider changes ----
        lapply(unique(df[[metricCol]]), function(i) {
            qnum <- paste0(i, "_weight")
            shiny::observeEvent(input[[qnum]], {
                ## Use isolate() to avoid infinite update loop (changes to other 
                ## sliders don't trigger new changes)
                shiny::isolate({
                    ## Calculate "remaining" weight to distribute
                    # remweight <- 1 - input[[qnum]]
                    # tmp <- unlist(lapply(setdiff(values$metrics, i), function(j) {
                    #   input[[paste0(j, "_weight")]]
                    # }))
                    # tottmp <- sum(tmp)
                    # for (j in setdiff(values$metrics, i)) {
                    #   if (abs(input[[paste0(j, "_weight")]] - 
                    #           remweight * input[[paste0(j, "_weight")]]/tottmp) > 0.0001) {
                    #     shiny::updateNumericInput(
                    #       session, inputId = paste0(j, "_weight"), 
                    #       value = remweight * input[[paste0(j, "_weight")]]/tottmp
                    #     )
                    #   }
                    # }
                    for (j in values$metrics) {
                        if (abs(values$df[values$df[[metricCol]] == j, 
                                          weightCol][1] - 
                                input[[paste0(j, "_weight")]]) > 0.0001) {
                            values$df[values$df[[metricCol]] == j, weightCol] <- 
                                input[[paste0(j, "_weight")]]
                        }
                    }
                })
            })
        })
        
        ## Define plots -------------------------------------------------------
        output$bettrParCoordplot <- shiny::renderPlot({
            if (is.null(values$df)) {
                NULL
            } else {
                lwidths <- rep(0.75, length(values$methods))
                names(lwidths) <- values$methods
                if (input$highlightMethod != "---") {
                    lwidths[input$highlightMethod] <- 2.5
                }
                ggplot2::ggplot(values$df,
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
            if (is.null(values$df)) {
                NULL
            } else {
                levs <- values$df %>%
                    dplyr::group_by(!!rlang::sym(methodCol)) %>%
                    dplyr::summarize(
                        "{scoreCol}" := sum(!!rlang::sym(weightCol) *
                                                !!rlang::sym(valueCol),
                                            na.rm = TRUE)
                    ) %>%
                    dplyr::arrange(dplyr::desc(!!rlang::sym(scoreCol))) %>%
                    dplyr::pull(!!rlang::sym(methodCol))
                ggplot2::ggplot(values$df %>% 
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
            if (is.null(values$df)) {
                NULL
            } else {
                ## Define polar plots
                rplots <- lapply(values$methods, function(m) {
                    ggplot2::ggplot(values$df %>% 
                                        dplyr::filter(!!rlang::sym(methodCol) == m),
                                    ggplot2::aes(x = !!rlang::sym(metricCol), 
                                                 y = !!rlang::sym(valueCol),
                                                 fill = !!rlang::sym(metricCol))) + 
                        ggplot2::geom_col(width = 1, color = "white") +
                        ggplot2::ylim(min(0, min(values$df[[valueCol]])),
                                      max(values$df[[valueCol]])) + 
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
                
                scores <- values$df %>%
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
                
                bplot <- ggplot2::ggplot(values$df %>% 
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
            if (is.null(values$df)) {
                NULL
            } else {
                ggplot2::ggplot(values$df, 
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
            if (is.null(values$df)) {
                NULL
            } else {
                rowAnnot <- values$df %>%
                    dplyr::group_by(!!rlang::sym(methodCol)) %>%
                    dplyr::summarize(
                        "{scoreCol}" := sum(!!rlang::sym(weightCol) *
                                                !!rlang::sym(valueCol),
                                            na.rm = TRUE)
                    ) %>%
                    dplyr::arrange(dplyr::desc(!!rlang::sym(scoreCol)))
                tmp <- values$df
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
                colAnnot <- values$df %>%
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
        
        ## Define weight controls ---------------------------------------------
        output$weights <- shiny::renderUI({
            if (is.null(values$df)) {
                NULL
            } else {
                do.call(shiny::tagList,
                        lapply(values$metrics, function(i) {
                            shiny::numericInput(
                                inputId = paste0(i, "_weight"),
                                label = i,
                                value = values$df[values$df[[metricCol]] == i, 
                                                  weightCol][1],
                                min = -5,
                                max = 5,
                                step = 0.0001
                            )
                        }))
            }
        })
        
    }
    
    # Generate app ------------------------------------------------------------
    shiny::shinyApp(ui = p_layout, server = server_function)
}