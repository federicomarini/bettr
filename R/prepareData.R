#' Prepare data for further analysis
#' 
#' @param df A `data.frame` in wide format. Should contain one column 
#'     with the IDs of the entities to be compared, and one column for each 
#'     metric to use for the comparison.
#' @param idCol Character scalar, indicating the name of the column of `df` 
#'     and/or `idInfo` that contains IDs of the entities to be compared 
#'     (e.g., methods).
#' @param metrics Character vector, indicating which of the 
#'     columns of `df` that correspond to metrics of interest. Only metrics
#'     included here will be displayed.
#' @param initialWeights Named numeric vector providing initial weights for 
#'     each metric to use for aggregating them into a final score. Must contain 
#'     one entry per metric included in `metrics`.
#' @param initialTransforms Named list with initial values of transformation 
#'     parameters for each metric. Each list entry should correspond to one 
#'     metric, and take the form of a list with up to four elements ("flip",
#'     "offset", "transform", "cuts").
#' @param metricInfo `data.frame` with annotations for metrics. Must have 
#'     a column named 'Metric' identifying the respective metrics.
#' @param metricColors Named list with colors used for columns of 
#'     `metricInfo`. Should follow the format required for ComplexHeatmap 
#'     heatmap annotations. The list can include an entry named 'Metric', which 
#'     contains a named vector with colors to use for metrics. 
#' @param idInfo `data.frame` with annotations for entities. Must have a 
#'     column named according to `idCol` identifying the respective entities. 
#' @param idColors Named list with colors used for columns of `idInfo`. 
#'     Should follow the format required for ComplexHeatmap heatmap 
#'     annotations. The list can include an entry named according to `idCol`, 
#'     which contains a named vector with colors to use for entities. 
#' @param weightResolution Numeric scalar in (0,1), giving the resolution at 
#'     which weights can be specified using the sliders in the interface.
#' @param metricCol Character scalar giving the name of the column in 
#'     `metricInfo` containing metric names. 
#' @param defaultWeightValue Numeric scalar, the default value that will be 
#'     used for the weights if nothing else is specified.
#' 
#' @returns A list with processed data.
#' 
#' @keywords internal
#' @noRd
#' 
#' @importFrom stats setNames
.prepareData <- function(df, idCol, metrics, initialWeights,
                         initialTransforms, metricInfo, metricColors, 
                         idInfo, idColors, weightResolution, metricCol, 
                         defaultWeightValue) {
    
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
            data.frame(id = unique(df[[idCol]])) |> stats::setNames(idCol),
            idColors, ggplot2Columns = idCol
        )
    } else {
        idColors <- .generateColors(idInfo, idColors, ggplot2Columns = idCol)
    }
    
    if (!is.null(metricInfo) && length(setdiff(colnames(metricInfo), 
                                               metricCol)) == 0) {
        metricInfo <- NULL
    }
    if (is.null(metricInfo)) {
        metricColors <- .generateColors(
            data.frame(metric = metrics) |> stats::setNames(metricCol),
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
        defaultWeightValue = defaultWeightValue,
        weightResolution = weightResolution)
    
    ## Return -----------------------------------------------------------------
    list(metrics_num = metrics_num, 
         metrics_cat = metrics_cat, idColors = idColors, 
         metricColors = metricColors, initialTransforms = initialTransforms,
         metricsWithWeights = metricsWithWeights, 
         initialWeights = initialWeights, 
         idInfo = idInfo, metricInfo = metricInfo)
}
