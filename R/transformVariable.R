getTransf <- function(v) {
    if (v == "None") {
        return(function(a) a)
    } else if (v == "z-score") {
        return(function(a) scale(a, center = TRUE, scale = TRUE))
    } else if (v == "[0,1]") {
        return(function(a) (a - min(a))/(max(a) - min(a)))
    } else if (v == "[-1,1]") {
        return(function(a) (a - min(a) + a - max(a))/(max(a) - min(a)))
    } else if (v == "Rank") {
        return(function(a) order(order(a)))
    }
}

#' @importFrom Hmisc cut2
transformNumericVariable <- function(x, flip = FALSE, offset = 0, 
                                     transf = function(a) a, 
                                     bincuts = NULL) {
    stopifnot(exprs = {
        is.logical(flip)
        is.numeric(offset)
        length(offset) == 1L
        is.function(transf)
        is.null(bincuts) || is.numeric(bincuts)
    })
    
    ## Step 1: flip sign
    if (flip) {
        x <- -x
    }
    
    ## Step 2: add offset
    x <- x + offset
    
    ## Step 3: transform
    x <- get("transf")(x)
    
    ## Step 4: binarize
    if (!is.null(bincuts)) {
        x <- Hmisc::cut2(x, cuts = bincuts)
        x <- as.numeric(x)
    }
    
    x
}

transformCategoricalVariable <- function(x, levels = NULL) {
    ## If no levels are specified, use the default ones. 
    ## If some levels are specified but they don't cover all the values in x, 
    ## add missing values from x in the beginning.
    if (is.null(levels)) {
        x <- factor(x)
    } else {
        levels <- c(unique(x[!(x %in% levels)]), levels)
        x <- factor(x, levels = levels)
    }
    
    ## Convert to numeric vector
    x <- as.numeric(x)
    
    x
}

