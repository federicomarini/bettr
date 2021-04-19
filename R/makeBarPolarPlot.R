#' @keywords internal
#' 
#' @importFrom dplyr filter group_by summarize arrange desc pull mutate
#' @importFrom rlang .data :=
#' @importFrom ggplot2 ggplot aes geom_col ylim coord_polar theme_minimal 
#'   theme element_blank labs geom_bar expand_limits element_text
#' @importFrom cowplot draw_plot
#' @importFrom grid unit
#' 
.makeBarPolarPlot <- function(df, idCol, metricCol, valueCol, 
                              weightCol, scoreCol, methods, 
                              ordering = "high-to-low") {
    if (!(ordering %in% c("high-to-low", "low-to-high"))) {
        stop("ordering must be 'high-to-low' or 'low-to-high'")
    }
    ## Define polar plots
    rplots <- lapply(methods, function(m) {
        ggplot2::ggplot(df %>% 
                            dplyr::filter(.data[[idCol]] == m),
                        ggplot2::aes(x = .data[[metricCol]], 
                                     y = .data[[valueCol]],
                                     fill = .data[[metricCol]])) + 
            ggplot2::geom_col(width = 1, color = "white") +
            ggplot2::ylim(min(0, min(df[[valueCol]])),
                          max(df[[valueCol]])) + 
            ggplot2::coord_polar() + 
            ggplot2::theme_minimal() +
            ggplot2::theme(axis.text = ggplot2::element_blank(),
                           legend.position = "none",
                           plot.background = ggplot2::element_blank(),
                           plot.margin = grid::unit(c(0, 0, 0, 0), "cm"),
                           panel.spacing = grid::unit(0, "cm")) + 
            ggplot2::labs(x = "", y = "")
    })
    names(rplots) <- methods
    
    scores <- df %>%
        dplyr::group_by(.data[[idCol]]) %>%
        dplyr::summarize(
            "{scoreCol}" := sum(.data[[weightCol]] *
                                    .data[[valueCol]],
                                na.rm = TRUE)
        ) 
    if (ordering == "high-to-low") {
        scores <- scores %>%
            dplyr::arrange(dplyr::desc(.data[[scoreCol]]))
    } else {
        scores <- scores %>%
            dplyr::arrange(.data[[scoreCol]])
    }
    levs <- scores %>%
        dplyr::pull(.data[[idCol]])
    rx <- length(levs)
    ry <- max(0, max(scores[[scoreCol]])) - min(0, min(scores[[scoreCol]]))
    sx <- 2.5
    sy <- ry/rx * sx
    
    bplot <- ggplot2::ggplot(scores %>% 
                                 dplyr::mutate("{idCol}" := 
                                                   factor(.data[[idCol]],
                                                          levels = levs)),
                             ggplot2::aes(x = .data[[idCol]], 
                                          y = .data[[scoreCol]])) +
        ggplot2::geom_bar(stat = "identity", width = 0.2, fill = "grey") + 
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
                y = scores[[scoreCol]][scores[[idCol]] == l],
                width = sx, height = sy, scale = 1.5, 
                hjust = 0, vjust = 0,
                halign = 0.5, valign = 0.5)
    }
    bplot
}