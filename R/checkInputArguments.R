#' @noRd
#' 
#' @importFrom methods is
#' 
.checkInputArguments <- function(df, idCol, metrics,
                                 metricCol, initialWeights, initialTransforms, 
                                 metricInfo, idInfo, weightResolution, bstheme) {
    ## Check input arguments --------------------------------------------------
    if (!methods::is(df, "data.frame")) {
        stop("df must be a data.frame")
    }
    if (!methods::is(idCol, "character") || length(idCol) != 1) {
        stop("idCol must be a character scalar")
    }
    if (!(idCol %in% colnames(df))) {
        stop("idCol must point to a column of df")
    }
    if (!is.null(idInfo) && !(idCol %in% colnames(idInfo))) {
        stop("idInfo must have a column named ", idCol)
    }
    
    if (!all(metrics %in% colnames(df))) {
        stop("All elements of metrics must point to columns of df")
    }
    
    if (!is.null(initialWeights)) {
        if (!(is.numeric(initialWeights) && 
              !is.null(names(initialWeights)) &&
               all(metrics %in% names(initialWeights)) &&
               !all(initialWeights == 0) && 
               all(initialWeights >= 0) && all(initialWeights <= 1))) {
            stop("initialWeights must be a named numeric vector with ",
                 "values between 0 and 1, and with one value for each metric")
        }
    }
    
    if (!is.null(metricInfo)) {
        if (!methods::is(metricInfo, "data.frame")) {
            stop("metricInfo must be a data.frame")
        }
        if (!(metricCol %in% colnames(metricInfo))) {
            stop("metricInfo must have a column named ", metricCol)
        }
        if (!all(metrics %in% metricInfo[[metricCol]])) {
            stop("metricInfo must contain information about all metrics")
        }
    }
    
    if (!is.null(idInfo)) {
        if (!methods::is(idInfo, "data.frame")) {
            stop("idInfo must be a data.frame")
        }
        if (!(idCol %in% colnames(idInfo))) {
            stop("idInfo must have a column named ", idCol)
        }
        if (!all(df[[idCol]] %in% idInfo[[idCol]])) {
            stop("idInfo must contain information about all entities")
        }
    }
    
    if (!(is.numeric(weightResolution) && length(weightResolution) == 1 && 
          weightResolution > 0 && weightResolution < 1)) {
        stop("weightResolution must be a numeric scalar in (0,1)")
    }
}

