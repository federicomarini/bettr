#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_col coord_polar facet_wrap 
#'   theme_minimal theme element_blank
.makePolarPlot <- function(df, scores, idCol, metricCol, valueCol, weightCol, 
                           scoreCol, metricGroupCol, labelSize, 
                           metricColors, metricCollapseGroup, metricGrouping,
                           showOnlyTopIds = FALSE, nbrTopIds = Inf) {
   
    if (metricCollapseGroup && !is.null(df[[metricGroupCol]])) {
        metricColors[[metricCol]] <- metricColors[[metricGrouping]]
    }
    
    ## Plot -------------------------------------------------------------------
    ggplot2::ggplot(df,
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
