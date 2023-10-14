.prepareData <- function(df, idCol, metrics, initialWeights,
                         initialTransforms, metricInfo, metricColors, 
                         idInfo, idColors, weightResolution, metricCol, 
                         initialWeightValue) {
    
    ## Split metrics into numeric and categorical -----------------------------
    metrics_classes <- vapply(df[, metrics, drop = FALSE], class, NA_character_)
    metrics_num <- intersect(
        metrics, names(metrics_classes[metrics_classes %in% c("numeric", 
                                                              "integer")])
    )
    metrics_cat <- intersect(
        metrics, names(metrics_classes[metrics_classes %in% 
                                           c("factor", "character", 
                                             "logical")])
    )
    
    ## Define annotation colors -----------------------------------------------
    if (!is.null(idInfo) && length(setdiff(colnames(idInfo), idCol)) == 0) {
        idInfo <- NULL
    }
    if (is.null(idInfo)) {
        idColors <- .generateColors(
            data.frame(id = unique(df[[idCol]])) %>% stats::setNames(idCol),
            idColors, ggplot2Columns = idCol
        )
    } else {
        idColors <- .generateColors(idInfo, idColors, ggplot2Columns = idCol)
    }
    
    if (!is.null(metricInfo) && length(setdiff(colnames(metricInfo), metricCol)) == 0) {
        metricInfo <- NULL
    }
    if (is.null(metricInfo)) {
        metricColors <- .generateColors(
            data.frame(metric = metrics) %>% stats::setNames(metricCol),
            metricColors, ggplot2Columns = metricCol
        )
    } else {
        metricColors <- .generateColors(metricInfo, metricColors, 
                                        ggplot2Columns = metricCol)
    }
    
    ## Add non-specified initializations and check validity -------------------
    initialTransforms <- .completeInitialization(initialTransforms, 
                                                 metrics_num)
    
    ## Assign initial weights -------------------------------------------------
    metricsWithWeights <- c(
        metrics, unlist(lapply(colnames(metricInfo), function(cn) {
            unique(paste0(cn, "_", metricInfo[[cn]]))
        })))
    initialWeights <- .assignInitialWeights(
        weights = initialWeights, 
        metrics = metricsWithWeights,
        initialWeightValue = initialWeightValue,
        weightResolution = weightResolution)
    
    ## Return -----------------------------------------------------------------
    list(metrics_num = metrics_num, 
         metrics_cat = metrics_cat, idColors = idColors, 
         metricColors = metricColors, initialTransforms = initialTransforms,
         metricsWithWeights = metricsWithWeights, 
         initialWeights = initialWeights, 
         idInfo = idInfo, metricInfo = metricInfo)
}