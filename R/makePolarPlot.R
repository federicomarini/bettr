#' @keywords internal
#' 
#' @importFrom dplyr group_by summarize arrange desc pull mutate
#' @importFrom rlang sym
#' @importFrom ggplot2 ggplot aes geom_col coord_polar facet_wrap 
#'   theme_minimal theme element_blank
#' 
.makePolarPlot <- function(df, idCol, weightCol, valueCol, scoreCol, 
                           metricCol) {
    levs <- df %>%
        dplyr::group_by(!!rlang::sym(idCol)) %>%
        dplyr::summarize(
            "{scoreCol}" := sum(!!rlang::sym(weightCol) *
                                    !!rlang::sym(valueCol),
                                na.rm = TRUE)
        ) %>%
        dplyr::arrange(dplyr::desc(!!rlang::sym(scoreCol))) %>%
        dplyr::pull(!!rlang::sym(idCol))
    ggplot2::ggplot(df %>% 
                        dplyr::mutate("{idCol}" := 
                                          factor(!!rlang::sym(idCol),
                                                 levels = levs)),
                    ggplot2::aes(x = !!rlang::sym(metricCol), 
                                 y = !!rlang::sym(valueCol),
                                 fill = !!rlang::sym(metricCol))) + 
        ggplot2::geom_col(width = 1, color = "white") +
        ggplot2::coord_polar() + 
        ggplot2::facet_wrap(facets = idCol) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text = ggplot2::element_blank())
}