#' @keywords internal
#' 
#' @importFrom dplyr arrange mutate
#' @importFrom rlang sym :=
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_line geom_point 
#'   scale_size_manual theme_minimal theme element_text
#'   
.makeParCoordPlot <- function(df, idCol, metricCol, valueCol, groupCol,
                              methods, highlightMethod, 
                              metricGrouping) {
    
    lwidths <- rep(0.75, length(methods))
    names(lwidths) <- methods
    lwidths[highlightMethod] <- 2.5
    
    alphas <- rep(1, length(methods))
    names(alphas) <- methods
    if (any(highlightMethod %in% methods)) {
        alphas[setdiff(methods, highlightMethod)] <- 0.3
    }
    
    if (metricGrouping != "---") {
        tmp <- df %>% 
            dplyr::arrange(!!rlang::sym(groupCol)) %>%
            dplyr::mutate("{metricCol}" := factor(
                !!rlang::sym(metricCol),
                levels = unique(!!rlang::sym(metricCol))))
        gp <- ggplot2::ggplot(tmp,
                              ggplot2::aes(x = !!rlang::sym(metricCol), 
                                           y = !!rlang::sym(valueCol))) + 
            ggplot2::geom_boxplot(outlier.size = -1,
                                  ggplot2::aes(fill = !!rlang::sym(groupCol)),
                                  alpha = 0.4)
    } else {
        tmp <- df
        gp <- ggplot2::ggplot(tmp,
                              ggplot2::aes(x = !!rlang::sym(metricCol), 
                                           y = !!rlang::sym(valueCol))) + 
            ggplot2::geom_boxplot(outlier.size = -1)
    }
    gp + 
        ggplot2::geom_line(ggplot2::aes(group = !!rlang::sym(idCol),
                                        color = !!rlang::sym(idCol),
                                        size = !!rlang::sym(idCol),
                                        alpha = !!rlang::sym(idCol))) +
        ggplot2::geom_point(ggplot2::aes(group = !!rlang::sym(idCol),
                                         color = !!rlang::sym(idCol))) +
        ggplot2::scale_size_manual(values = lwidths) +
        ggplot2::scale_alpha_manual(values = alphas) + 
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(
            angle = 90, hjust = 1, vjust = 0.5))
}