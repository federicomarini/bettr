#' Complete the initialization by specifying transforms for all metrics
#' 
#' @param transformList A list with one element per metric. Each list element 
#'     is another list, with up to four elements, named 'offset', 'flip', 
#'     'transform' and 'cuts'. If any of the elements is missing, it will be 
#'     set to a default value. 
#' @param metrics The full list of metrics. For entries that don't have a list 
#'     element in `transformList`, one will be generated, with default 
#'     values. 
#' 
#' @returns A list containing the complete transform specification for each 
#' metric. 
#' 
#' @keywords internal
#' @noRd
#' 
.completeInitialization <- function(transformList, metrics) {
    .assertVector(x = transformList, type = "list")

    ## Define default values
    defaultValues <- list(offset = 0,
                          flip = FALSE,
                          cuts = NULL,
                          transform = "None")
    
    for (m in metrics) {
        if (m %in% names(transformList)) {
            ## metric already present, just add missing entries and check 
            ## present ones
            for (nm in names(defaultValues)) {
                value <- .checkSpecifiedValue(transformList[[m]][[nm]],
                                              defaultValues[[nm]],
                                              nm)
                if (!is.null(value)) {
                    transformList[[m]][[nm]] <- value
                } else {
                    transformList[[m]][nm] <- list(NULL)
                }
            }
        } else {
            ## metric not present, add default values for all entries
            transformList[[m]] <- defaultValues
        }
    }
    
    transformList
}

#' Check validity of a value specified for a transform
#' 
#' @param value The specified value.
#' @param defaultValue The default value for the transform aspect.
#' @param The entry type, either "offset", "flip", "transform" or "cuts".
#' 
#' @returns A valid value for the aspect (either the provided one or the 
#' default value). Raises an error if an invalid value is provided. 
#' 
#' @keywords internal
#' @noRd
#' 
.checkSpecifiedValue <- function(value, defaultValue, entry) {
    allowedTransforms <- c("None", "z-score", "[0,1]", "[-1,1]", "Rank")
    
    ## If there's no value specified, return the default value
    if (is.null(value)) {
        return(defaultValue)
    }
    
    ## If the specified value is invalid, raise an error
    if (entry == "offset" && (!is.numeric(value) || length(value) > 1)) {
        stop("Specified offsets must be numeric scalars")
    }
    if (entry == "flip" && (!is.logical(value) || length(value) > 1)) {
        stop("Specified flips must be logical scalars")
    }
    if (entry == "transform" && (!is.character(value) || length(value) > 1 ||
                                 !(value %in% allowedTransforms))) {
        stop("Specified transforms must be character scalars, and one of ",
             paste(allowedTransforms, collapse = ", "))
    }
    if (entry == "cuts" && (!is.numeric(value))) {
        stop("Specified cuts must be numeric vectors")
    }
    
    ## Otherwise, return the specified value
    return(value)
}
