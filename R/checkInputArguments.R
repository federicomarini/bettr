#' @keywords internal
#' 
#' @importFrom methods is
#' 
.checkInputArguments <- function(df, idCol, metrics_num, metrics_cat,
                                 metricCol, initialWeights, initialTransforms, 
                                 metricInfo, idInfo, bstheme) {
    metrics <- c(metrics_num, metrics_cat)

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
            stop("initialWeights must be a named numeric vector with",
                 "values between 0 and 1, and with one value for each metric")
        }
    }
    
    if (!all(sapply(metrics_num, function(m) is.numeric(df[[m]])))) {
        stop("All metrics in metrics_num must correspond to numeric",
             "columns in df")
    }
    if (!all(sapply(metrics_cat, function(m) is.factor(df[[m]]) || 
                    is.character(df[m])))) {
        stop("All metrics in metrics_cat must correspond to factor",
             "or character columns in df")
    }
    
    if (!is.null(metricInfo) && !methods::is(metricInfo, "data.frame")) {
        stop("metricGroups must be a list")
    }
    if (!is.null(metricInfo) && !(metricCol %in% colnames(metricInfo))) {
        stop("metricInfo must have a column named ", metricCol)
    }
}