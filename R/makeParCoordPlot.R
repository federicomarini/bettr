#' @noRd
#' 
#' @importFrom dplyr arrange mutate
#' @importFrom rlang .data :=
#' @importFrom scales rescale
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_line geom_point 
#'   scale_size_manual theme_minimal theme element_text scale_fill_manual
#'   
.makeParCoordPlot <- function(df, idCol, metricCol, valueCol, metricGroupCol,
                              methods, highlightMethod, 
                              metricGrouping, labelSize, 
                              metricColors, idColors, metricCollapseGroup) {
    
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
        tmp <- df %>% 
            dplyr::arrange(.data[[metricGroupCol]]) %>%
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
                    colors = do.call(metricColors[[metricGrouping]], 
                                     list(sort(unique(tmp[[metricGroupCol]])))), 
                    values = scales::rescale(sort(unique(tmp[[metricGroupCol]])))
                )
        } else {
            gp <- gp + 
                ggplot2::scale_fill_manual(values = metricColors[[metricGrouping]])
        }
    } else {
        tmp <- df
        gp <- ggplot2::ggplot(tmp,
                              ggplot2::aes(x = .data[[metricCol]], 
                                           y = .data[[valueCol]])) + 
            ggplot2::geom_boxplot(outlier.size = -1)
    }
    
    ## Plot
    gp + 
        ggplot2::geom_line(ggplot2::aes(group = .data[[idCol]],
                                        color = .data[[idCol]],
                                        size = .data[[idCol]],
                                        alpha = .data[[idCol]])) +
        ggplot2::geom_point(ggplot2::aes(group = .data[[idCol]],
                                         color = .data[[idCol]])) +
        ggplot2::scale_size_manual(values = lwidths) +
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