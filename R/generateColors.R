#' @noRd
#' 
#' @importFrom circlize colorRamp2
#' @importFrom grDevices colors
#' 
.generateColors <- function(df, inputColors) {
    if (is.null(df)) {
        return(NULL)
    } 
    
    if (is.null(inputColors)) {
        inputColors <- list()
    }
    
    c(inputColors, 
      lapply(df[, setdiff(colnames(df), names(inputColors))], function(x) {
          if (is.numeric(x)) {
              circlize::colorRamp2(range(x), 
                                   c("white", sample(grDevices::colors(), 1)))
          } else {
              structure(sample(grDevices::colors(), length(unique(x))),
                        names = unique(x))
          }
      })
    )
    
}