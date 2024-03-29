#' Generate random colors for annotations
#' 
#' @param df A `data.frame` with annotations in columns.
#' @param inputColors A list with predefined colors for some of the 
#'     annotations, or `NULL`.
#' @param ggplot2Columns Names of columns for which we should use the standard 
#'     ggplot2 colors if nothing else is specified
#' 
#' @returns A list with color vectors for all annotations.
#' 
#' @keywords internal
#' @noRd
#' 
#' @importFrom circlize colorRamp2
#' @importFrom grDevices colors
#' 
.generateColors <- function(df, inputColors, ggplot2Columns = c()) {
    if (is.null(df)) {
        return(NULL)
    } 
    
    if (is.null(inputColors)) {
        inputColors <- list()
    }
    
    c(inputColors, 
      lapply(df[, setdiff(colnames(df), c(names(inputColors), ggplot2Columns)), 
                drop = FALSE],
             function(x) {
                 if (is.numeric(x)) {
                     circlize::colorRamp2(range(x), 
                                          c("white", 
                                            sample(grDevices::colors(), 1)))
                 } else {
                     structure(sample(grDevices::colors(), length(unique(x))),
                               names = as.character(unique(x)))
                 }
             }),
      lapply(df[, setdiff(ggplot2Columns, names(inputColors)), drop = FALSE],
             function(x) {
                 if (is.numeric(x)) {
                     stop("Can't use ggplot2 colors for continuous columns")
                 } else {
                     structure(.gg_color_hue(length(unique(x))),
                               names = as.character(unique(x)))
                 }
             })
    )
}

#' Emulate ggplot2 colors
#' stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
#' 
#' @param n Numeric scalar, the number of colors to generate.
#' 
#' @returns A length-`n` vector of colors.
#' 
#' @keywords internal
#' @noRd
#' 
#' @importFrom grDevices hcl
.gg_color_hue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    grDevices::hcl(h = hues, l = 65, c = 100)[seq_len(n)]
}

