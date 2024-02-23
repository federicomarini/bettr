#' @keywords internal
#' @noRd
.checkArgs_makeParCoordPlot <- function(
        plotdata, idCol, metricCol, valueCol, metricGroupCol, metricColors, 
        idColors, methods, metricGrouping, labelSize, highlightMethod) {
    .assertVector(x = plotdata, type = "data.frame")
    .assertScalar(x = idCol, type = "character")
    .assertScalar(x = metricCol, type = "character")
    .assertScalar(x = valueCol, type = "character")
    .assertScalar(x = metricGroupCol, type = "character")
    .assertVector(x = metricColors, type = "list", allowNULL = TRUE)
    .assertVector(x = idColors, type = "list", allowNULL = TRUE)
    .assertVector(x = methods, type = "character")
    .assertScalar(x = metricGrouping, type = "character", allowNULL = TRUE)
    .assertScalar(x = labelSize, type = "numeric")
    .assertScalar(x = highlightMethod, type = "character", allowNULL = TRUE)
}

#' Create a parallel coordinates plot
#' 
#' Create a parallel coordinates plot. The input arguments for this functions 
#' are typically generated using \code{\link{bettrGetReady}}, which ensures  
#' that all required columns are available. 
#' 
#' @inheritParams makeHeatmap
#' @param highlightMethod Character scalar indicating a method that should be 
#'     highlighted in the plot. 
#' @param methods Character vector containing the methods to include. 
#'     If \code{NULL} (default), all methods will be used. 
#' 
#' @author Charlotte Soneson
#' @export 
#' 
#' @returns 
#' A \code{ggplot} object. 
#' 
#' @importFrom dplyr arrange mutate
#' @importFrom rlang .data :=
#' @importFrom scales rescale
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_line geom_point 
#'     scale_linewidth_manual theme_minimal theme element_text 
#'     scale_fill_manual
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
#' prepData <- bettrGetReady(df = df, idCol = "Method", 
#'                           metricInfo = metricInfo, idInfo = idInfo)
#' makeParCoordPlot(bettrList = prepData, highlightMethod = "M2")
#'                  
makeParCoordPlot <- function(bettrList = NULL,
                             plotdata, idCol, metricCol = "Metric", 
                             valueCol = "ScaledValue", 
                             metricGroupCol = "metricGroup",
                             metricColors, idColors,
                             methods = NULL, metricGrouping = "---", 
                             highlightMethod = NULL, labelSize = 10) {
    
    ## If bettrList is provided, extract arguments from there
    if (!is.null(bettrList)) {
        .assertVector(x = bettrList, type = "list")
        stopifnot(all(c("plotdata", "idCol", "metricCol", "valueCol", 
                        "metricGroupCol", "metricColors", "idColors", 
                        "metricGrouping", "methods") %in% names(bettrList)))
        plotdata <- bettrList$plotdata
        idCol <- bettrList$idCol
        metricCol <- bettrList$metricCol
        valueCol <- bettrList$valueCol
        metricGroupCol <- bettrList$metricGroupCol
        metricColors <- bettrList$metricColors
        idColors <- bettrList$idColors
        methods <- bettrList$methods
        metricGrouping <- bettrList$metricGrouping
    }
    
    if (is.null(methods)) {
        methods <- unique(plotdata[[idCol]])
    }
    
    .checkArgs_makeParCoordPlot(
        plotdata = plotdata, idCol = idCol, metricCol = metricCol, 
        valueCol = valueCol, metricGroupCol = metricGroupCol, 
        metricColors = metricColors, idColors = idColors, methods = methods, 
        metricGrouping = metricGrouping, labelSize = labelSize, 
        highlightMethod = highlightMethod)
        
    ## Define line widths -----------------------------------------------------
    lwidths <- rep(0.75, length(methods))
    names(lwidths) <- methods
    lwidths[highlightMethod] <- 2.5
    
    ## Define line transparencies ---------------------------------------------
    alphas <- rep(1, length(methods))
    names(alphas) <- methods
    if (any(highlightMethod %in% methods)) {
        alphas[setdiff(methods, highlightMethod)] <- 0.3
    }
    
    ## Construct plot ---------------------------------------------------------
    if (metricGrouping != "---") {
        ## Reorder metrics according to the chosen grouping
        tmp <- plotdata |> 
            dplyr::arrange(.data[[metricGroupCol]]) |>
            dplyr::mutate("{metricCol}" := factor(
                .data[[metricCol]],
                levels = unique(.data[[metricCol]])))
        gp <- ggplot2::ggplot(tmp,
                              ggplot2::aes(x = .data[[metricCol]], 
                                           y = .data[[valueCol]])) + 
            ggplot2::geom_boxplot(outlier.size = -1,
                                  ggplot2::aes(fill = .data[[metricGroupCol]]),
                                  alpha = 0.4)
        if (methods::is(metricColors[[metricGrouping]], "function")) {
            gp <- gp + 
                ggplot2::scale_fill_gradientn(
                    colors = do.call(
                        metricColors[[metricGrouping]], 
                        list(sort(unique(tmp[[metricGroupCol]])))), 
                    values = scales::rescale(sort(
                        unique(tmp[[metricGroupCol]])))
                )
        } else {
            gp <- gp + 
                ggplot2::scale_fill_manual(
                    values = metricColors[[metricGrouping]]
                )
        }
    } else {
        tmp <- plotdata
        gp <- ggplot2::ggplot(tmp,
                              ggplot2::aes(x = .data[[metricCol]], 
                                           y = .data[[valueCol]])) + 
            ggplot2::geom_boxplot(outlier.size = -1)
    }
    
    ## Plot
    gp + 
        ggplot2::geom_line(ggplot2::aes(group = .data[[idCol]],
                                        color = .data[[idCol]],
                                        linewidth = .data[[idCol]],
                                        alpha = .data[[idCol]])) +
        ggplot2::geom_point(ggplot2::aes(group = .data[[idCol]],
                                         color = .data[[idCol]])) +
        ggplot2::scale_linewidth_manual(values = lwidths) +
        ggplot2::scale_alpha_manual(values = alphas) + 
        ggplot2::scale_color_manual(values = idColors[[idCol]]) + 
        ggplot2::labs(y = "Relative value") + 
        ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(
                angle = 90, hjust = 1, vjust = 0.5, size = labelSize),
            axis.text.y = ggplot2::element_text(size = labelSize),
            axis.title = ggplot2::element_text(size = labelSize),
            legend.text = ggplot2::element_text(size = labelSize),
            legend.title = ggplot2::element_text(size = labelSize))
}