#' Filter data by method or metric ID or annotation
#' 
#' @param df A `data.frame` with methods in rows and metrics in columns.
#' @param idInfo A `data.frame` with annotations for methods, or `NULL`.
#' @param idCol Character scalar indicating the name of the column of `df` 
#'     and `idInfo` that contains method names.
#' @param keepIds Character vector with names of methods to retain.
#' @param keepIdsBy A named list with one entry for each column in `idInfo`.  
#'     The entry should be a vector of values for that column that define
#'     methods to keep. 
#' @param metricInfo A `data.frame` with annotations for metrics, or `NULL`.
#' @param metricCol Character scalar indicating the name of the column of 
#'     `metricInfo` that contains method names.
#' @param keepMetrics Character vector with names of metrics to retain.
#' @param keepMetricsBy A named list with one entry for each column in 
#'     `metricInfo`. The entry should be a vector of values for that column 
#'     that define metrics to keep. 
#' @param metrics A vector of all the metrics considered in the analysis. 
#' 
#' @returns A filtered `data.frame` (a subset of `df`).
#' 
#' @keywords internal
#' @noRd
#' 
#' @importFrom dplyr filter select any_of
#' 
.filterData <- function(df, idInfo, idCol, keepIds, keepIdsBy, metricInfo,
                        metricCol, keepMetrics, keepMetricsBy, metrics) {
    if (!is.null(idInfo)) {
        idFilt <- idInfo
        for (nm in setdiff(colnames(idInfo), idCol)) {
            idFilt <- idFilt |>
                dplyr::filter(.data[[nm]] %in% keepIdsBy[[nm]])
        }
        tmp <- df |>
            dplyr::filter(.data[[idCol]] %in% intersect(keepIds, 
                                                        idFilt[[idCol]]))
    } else {
        tmp <- df |>
            dplyr::filter(.data[[idCol]] %in% keepIds)
    }

    if (!is.null(metricInfo)) {
        metricFilt <- metricInfo
        for (nm in setdiff(colnames(metricInfo), metricCol)) {
            metricFilt <- metricFilt |>
                dplyr::filter(.data[[nm]] %in% keepMetricsBy[[nm]])
        }
        tmp <- tmp |>
            dplyr::select(-dplyr::any_of(
                setdiff(metrics, intersect(keepMetrics,
                                           metricFilt[[metricCol]]))))
    } else {
        tmp <- tmp |>
            dplyr::select(-dplyr::any_of(setdiff(metrics, keepMetrics)))
    }
    tmp
}
