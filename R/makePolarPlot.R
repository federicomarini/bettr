#' Create a polar plot
#' 
#' Create a polar plot. The input arguments for this functions are 
#' typically generated using \code{\link{bettrPrepare}}, which ensures that 
#' all required columns are available. 
#' 
#' @inheritParams makeHeatmap
#' 
#' @author Charlotte Soneson
#' @export
#' 
#' @returns
#' A \code{ggplot} object.  
#' 
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_col coord_polar facet_wrap 
#'   theme_minimal theme element_blank
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
#' makePolarPlot(bettrList = prepData)
#'                  
makePolarPlot <- function(bettrList = NULL,
                          plotdata, idCol, metricCol = "Metric", 
                          valueCol = "ScaledValue", 
                          metricGroupCol = "metricGroup", 
                          metricColors, metricCollapseGroup = FALSE, 
                          metricGrouping = "---", labelSize = 10) {
   
    ## If bettrList is provided, extract arguments from there
    if (!is.null(bettrList)) {
        stopifnot(all(c("plotdata", "idCol", "metricCol", "valueCol", 
                        "metricGroupCol", "metricColors", 
                        "metricCollapseGroup", "metricGrouping") %in% 
                          names(bettrList)))
        plotdata <- bettrList$plotdata
        idCol <- bettrList$idCol
        metricCol <- bettrList$metricCol
        valueCol <- bettrList$valueCol
        metricGroupCol <- bettrList$metricGroupCol
        metricColors <- bettrList$metricColors
        metricCollapseGroup <- bettrList$metricCollapseGroup
        metricGrouping <- bettrList$metricGrouping
    }
    
    if (metricCollapseGroup && !is.null(plotdata[[metricGroupCol]])) {
        metricColors[[metricCol]] <- metricColors[[metricGrouping]]
    }
    
    ## Plot -------------------------------------------------------------------
    ggplot2::ggplot(plotdata,
                    ggplot2::aes(x = .data[[metricCol]], 
                                 y = .data[[valueCol]],
                                 fill = .data[[metricCol]])) + 
        ggplot2::geom_col(width = 1, color = "white") +
        ggplot2::coord_polar() + 
        ggplot2::scale_fill_manual(values = metricColors[[metricCol]]) + 
        ggplot2::facet_wrap(facets = idCol) +
        ggplot2::labs(x = "", y = "") + 
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text = ggplot2::element_blank(),
                       strip.text = ggplot2::element_text(size = labelSize),
                       legend.text = ggplot2::element_text(size = labelSize),
                       legend.title = ggplot2::element_text(size = labelSize))
}
