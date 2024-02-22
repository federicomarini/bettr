#' @keywords internal
#' @noRd
.assignInitialWeights <- function(weights, metrics, defaultWeightValue,
                                  weightResolution = 0.05) {
    if (is.null(weights)) {
        weights <- rep(defaultWeightValue, length(metrics))
        names(weights) <- metrics
    } else {
        ## Round initial weights to right resolution to fit with the sliders
        weights <- round(weights * (1/weightResolution)) / (1/weightResolution)
        missing_metrics <- setdiff(metrics, names(weights))
        if (length(missing_metrics) > 0) {
            weights_add <- rep(defaultWeightValue, length(missing_metrics))
            names(weights_add) <- missing_metrics
            weights <- c(weights, weights_add)
        }
    }
    weights
}

#' @keywords internal
#' @noRd
#' @importFrom cowplot plot_grid
#' @importFrom ggplot2 ggplot aes geom_bar theme_minimal geom_boxplot 
#'     geom_jitter coord_flip
.makeMetricSummaryPlot <- function(x) {
    cowplot::plot_grid(
        ggplot2::ggplot(data.frame(metric = x),
                        ggplot2::aes(x = .data[["metric"]])) + 
            ggplot2::geom_bar() + 
            ggplot2::theme_minimal(),
        ggplot2::ggplot(data.frame(metric = x),
                        ggplot2::aes(x = 1, 
                                     y = .data[["metric"]])) + 
            ggplot2::geom_boxplot(outlier.size = -1) + 
            ggplot2::geom_jitter(width = 0.2, height = 0,
                                 size = 4, pch = 1) + 
            ggplot2::theme_minimal() +
            ggplot2::coord_flip(),
        ncol = 1
    )
}