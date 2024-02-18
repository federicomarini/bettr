#' Create a bar/polar plot
#' 
#' Create a bar/polar plot. The input arguments for this functions are 
#' typically generated using \code{\link{bettrPrepare}}, which ensures that 
#' all required columns are available. 
#' 
#' @inheritParams makeHeatmap
#' @param showComposition Logical scalar indicating whether to show the 
#'     composition of the score in the bar plots. This is only interpretable
#'     if the scores are obtained via a weighted mean approach.
#' @param scaleFactorPolars Numeric scalar giving the scale factor determining 
#'     the size of the polar plots. 
#' @param methods Character vector containing the methods for which to make 
#'     polar plots. If \code{NULL} (default), all methods will be used. 
#' 
#' @author Charlotte Soneson
#' @export
#' 
#' @returns 
#' A \code{ggplot} object.
#' 
#' @importFrom dplyr filter pull mutate
#' @importFrom rlang .data :=
#' @importFrom ggplot2 ggplot aes geom_col ylim coord_polar theme_minimal 
#'     theme element_blank labs geom_bar expand_limits element_text
#' @importFrom cowplot draw_plot get_legend plot_grid
#' @importFrom grid unit
#' 
#' @examples
#' ## Generate example data
#' df <- data.frame(Method = c("M1", "M2", "M3"), 
#'                  metric1 = c(1, 2, 3),
#'                  metric2 = c(3, 1, 2))
#' metricInfo <- data.frame(Metric = c("metric1", "metric2", "metric3"),
#'                          Group = c("G1", "G2", "G2"))
#' idInfo <- data.frame(Method = c("M1", "M2", "M3"), 
#'                      Type = c("T1", "T1", "T2"))
#' prepData <- bettrPrepare(df = df, idCol = "Method", 
#'                          metricInfo = metricInfo, idInfo = idInfo)
#' makeBarPolarPlot(bettrList = prepData, showComposition = TRUE)
#'                  
makeBarPolarPlot <- function(bettrList = NULL, 
                             plotdata, scoredata, idCol, metricCol = "Metric", 
                             valueCol = "ScaledValue", weightCol = "Weight", 
                             scoreCol = "Score", 
                             metricGroupCol = "metricGroup", metricColors, 
                             metricCollapseGroup = FALSE, 
                             metricGrouping = "---",
                             methods = NULL, labelSize = 10,
                             showComposition = FALSE, scaleFactorPolars = 1) {
    
    ## If bettrList is provided, extract arguments from there
    if (!is.null(bettrList)) {
        stopifnot(all(c("plotdata", "scoredata", "idCol", "metricCol", 
                        "valueCol", "weightCol", "scoreCol", "metricGroupCol", 
                        "metricColors", "metricCollapseGroup", 
                        "metricGrouping", "methods") %in% names(bettrList)))
        plotdata <- bettrList$plotdata
        scoredata <- bettrList$scoredata
        idCol <- bettrList$idCol
        metricCol <- bettrList$metricCol
        valueCol <- bettrList$valueCol
        weightCol <- bettrList$weightCol
        scoreCol <- bettrList$scoreCol
        metricGroupCol <- bettrList$metricGroupCol
        metricColors <- bettrList$metricColors
        metricCollapseGroup <- bettrList$metricCollapseGroup
        metricGrouping <- bettrList$metricGrouping
        methods <- bettrList$methods
    }
    
    if (is.null(methods)) {
        methods <- unique(plotdata[[idCol]])
    }
    if (metricCollapseGroup && !is.null(plotdata[[metricGroupCol]])) {
        metricColors[[metricCol]] <- metricColors[[metricGrouping]]
    }
    
    ## Define polar plots -----------------------------------------------------
    rplots <- lapply(methods, function(m) {
        ggplot2::ggplot(plotdata |> 
                            dplyr::filter(.data[[idCol]] == m),
                        ggplot2::aes(x = .data[[metricCol]], 
                                     y = .data[[valueCol]],
                                     fill = .data[[metricCol]])) + 
            ggplot2::geom_col(width = 1, color = "white") +
            ggplot2::ylim(min(0, min(plotdata[[valueCol]], na.rm = TRUE)),
                          max(plotdata[[valueCol]], na.rm = TRUE)) + 
            ggplot2::coord_polar() + 
            ggplot2::scale_fill_manual(values = metricColors[[metricCol]]) + 
            ggplot2::theme_minimal() +
            ggplot2::theme(
                axis.text = ggplot2::element_blank(),
                legend.text = ggplot2::element_text(size = labelSize),
                legend.title = ggplot2::element_text(size = labelSize),
                plot.background = ggplot2::element_blank(),
                plot.margin = grid::unit(c(0, 0, 0, 0), "cm"),
                panel.spacing = grid::unit(0, "cm")
            ) + 
            ggplot2::labs(x = "", y = "")
    })
    names(rplots) <- methods
    
    ## Get legend from one polar plot, remove it from all
    legnd <- cowplot::get_legend(rplots[[1]])
    
    rplots <- lapply(rplots, function(rp) {
        rp + ggplot2::theme(legend.position = "none")
    })
    
    ## Define data for barplot ------------------------------------------------
    levs <- scoredata |>
        dplyr::pull(.data[[idCol]])
    rx <- length(levs)
    ry <- max(0, max(scoredata[[scoreCol]])) - 
        min(0, min(scoredata[[scoreCol]]))
    sx <- 2.5
    sy <- ry/rx * sx
    
    ## Plot -------------------------------------------------------------------
    if (showComposition) {
        plotdata <- plotdata |>
            dplyr::group_by(.data[[idCol]]) |>
            dplyr::mutate("{weightCol}" := .data[[weightCol]] / 
                              sum(.data[[weightCol]] * 
                                      !is.na(.data[[valueCol]]), 
                                  na.rm = TRUE)) |>
            dplyr::ungroup()
        ## Split bars by metric contribution to score
        bplot <- ggplot2::ggplot(plotdata |> 
                                     dplyr::mutate("{idCol}" := 
                                                       factor(.data[[idCol]],
                                                              levels = levs)),
                                 ggplot2::aes(x = .data[[idCol]], 
                                              y = .data[[weightCol]] * 
                                                  .data[[valueCol]])) + 
            ggplot2::geom_bar(stat = "identity", width = 0.2,
                              aes(fill = .data[[metricCol]])) + 
            ggplot2::scale_fill_manual(values = metricColors[[metricCol]])
    } else {
        ## Show only final score in bars
        bplot <- ggplot2::ggplot(scoredata |> 
                                     dplyr::mutate("{idCol}" := 
                                                       factor(.data[[idCol]],
                                                              levels = levs)),
                                 ggplot2::aes(x = .data[[idCol]], 
                                              y = .data[[scoreCol]])) + 
            ggplot2::geom_bar(stat = "identity", width = 0.2, fill = "grey")
    }
    bplot <- bplot + 
        ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(
                angle = 90, hjust = 1, vjust = 0.5, size = labelSize),
            axis.text.y = ggplot2::element_text(size = labelSize),
            axis.title = ggplot2::element_text(size = labelSize)) +
        ggplot2::expand_limits(y = max(scoredata[[scoreCol]]) + sy) + 
        ggplot2::theme(legend.position = "none")
    
    ## Add polar plots
    for (i in seq_along(levs)) {
        l <- levs[i]
        bplot <- bplot +
            cowplot::draw_plot(
                rplots[[l]], x = (i - sx/2 - 0.1), 
                y = scoredata[[scoreCol]][scoredata[[idCol]] == l],
                width = sx, height = sy, scale = scaleFactorPolars, 
                hjust = 0, vjust = 0,
                halign = 0.5, valign = 0.5)
    }
    
    ## Add legend for metrics
    cowplot::plot_grid(bplot, legnd, rel_widths = c(1, 0.2), nrow = 1)
}
