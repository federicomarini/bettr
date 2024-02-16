#' @importFrom dplyr filter pull mutate
#' @importFrom rlang .data :=
#' @importFrom ggplot2 ggplot aes geom_col ylim coord_polar theme_minimal 
#'   theme element_blank labs geom_bar expand_limits element_text
#' @importFrom cowplot draw_plot get_legend plot_grid
#' @importFrom grid unit
.makeBarPolarPlot <- function(df, scores, idCol, metricCol, valueCol, 
                              weightCol, scoreCol, metricGroupCol, 
                              methods, labelSize,
                              showComposition = FALSE, 
                              scaleFactorPolars = 1.5, metricColors,
                              metricCollapseGroup, metricGrouping) {
    
    if (metricCollapseGroup && !is.null(df[[metricGroupCol]])) {
        metricColors[[metricCol]] <- metricColors[[metricGrouping]]
    }
    
    ## Define polar plots -----------------------------------------------------
    rplots <- lapply(methods, function(m) {
        ggplot2::ggplot(df %>% 
                            dplyr::filter(.data[[idCol]] == m),
                        ggplot2::aes(x = .data[[metricCol]], 
                                     y = .data[[valueCol]],
                                     fill = .data[[metricCol]])) + 
            ggplot2::geom_col(width = 1, color = "white") +
            ggplot2::ylim(min(0, min(df[[valueCol]], na.rm = TRUE)),
                          max(df[[valueCol]], na.rm = TRUE)) + 
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
    levs <- scores %>%
        dplyr::pull(.data[[idCol]])
    rx <- length(levs)
    ry <- max(0, max(scores[[scoreCol]])) - min(0, min(scores[[scoreCol]]))
    sx <- 2.5
    sy <- ry/rx * sx
    
    ## Plot -------------------------------------------------------------------
    if (showComposition) {
        df <- df %>%
            dplyr::group_by(.data[[idCol]]) %>%
            dplyr::mutate("{weightCol}" := .data[[weightCol]] / 
                              sum(.data[[weightCol]] * 
                                      !is.na(.data[[valueCol]]), 
                                  na.rm = TRUE)) %>%
            dplyr::ungroup()
        ## Split bars by metric contribution to score
        bplot <- ggplot2::ggplot(df %>% 
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
        bplot <- ggplot2::ggplot(scores %>% 
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
        ggplot2::expand_limits(y = max(scores[[scoreCol]]) + sy) + 
        ggplot2::theme(legend.position = "none")
    
    ## Add polar plots
    for (i in seq_along(levs)) {
        l <- levs[i]
        bplot <- bplot +
            cowplot::draw_plot(
                rplots[[l]], x = (i - sx/2 - 0.1), 
                y = scores[[scoreCol]][scores[[idCol]] == l],
                width = sx, height = sy, scale = scaleFactorPolars, 
                hjust = 0, vjust = 0,
                halign = 0.5, valign = 0.5)
    }
    
    ## Add legend for metrics
    cowplot::plot_grid(bplot, legnd, rel_widths = c(1, 0.2), nrow = 1)
}
