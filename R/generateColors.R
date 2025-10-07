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
#' @importFrom stats setNames
#'
.generateColors <- function(df, inputColors, ggplot2Columns = character(0L)) {
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
                                            sample(grDevices::colors(), 1L)))
                 } else {
                     setNames(sample(grDevices::colors(), length(unique(x))),
                              nm = as.character(unique(x)))
                 }
             }),
      lapply(df[, setdiff(ggplot2Columns, names(inputColors)), drop = FALSE],
             function(x) {
                 if (is.numeric(x)) {
                     stop("Can't use ggplot2 colors for continuous columns")
                 } else {
                     setNames(.ggColorHue(length(unique(x))),
                              nm = as.character(unique(x)))
                 }
             }))
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
.ggColorHue <- function(n) {
    hues <- seq(15L, 375L, length = n + 1L)
    grDevices::hcl(h = hues, l = 65L, c = 100L)[seq_len(n)]
}
