#' @keywords internal
#' 
#' @importFrom dplyr group_by summarize arrange desc pull mutate
#' @importFrom rlang .data :=
#' @importFrom ggplot2 ggplot aes geom_col coord_polar facet_wrap 
#'   theme_minimal theme element_blank
#' 
.makePolarPlot <- function(df, idCol, metricCol, valueCol, weightCol, 
                           scoreCol, labelSize, ordering = "high-to-low",
                           metricColors) {
    if (!(ordering %in% c("high-to-low", "low-to-high"))) {
        stop("ordering must be 'high-to-low' or 'low-to-high'")
    }
    
    ## Get ordering of methods by score ---------------------------------------
    levs <- df %>%
        dplyr::group_by(.data[[idCol]]) %>%
        dplyr::summarize(
            "{scoreCol}" := sum(.data[[weightCol]] * .data[[valueCol]],
                                na.rm = TRUE)
        )
    if (ordering == "high-to-low") {
        levs <- levs %>%
            dplyr::arrange(dplyr::desc(.data[[scoreCol]]))
    } else {
        levs <- levs %>%
            dplyr::arrange(.data[[scoreCol]])
    }
    levs <- levs[[idCol]]
    
    ## Plot -------------------------------------------------------------------
    ggplot2::ggplot(df %>% 
                        dplyr::mutate("{idCol}" := 
                                          factor(.data[[idCol]],
                                                 levels = levs)),
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
