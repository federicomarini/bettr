.filterData <- function(df, idInfo, idCol, keepIds, keepIdsBy, metricInfo,
                        metricCol, keepMetrics, keepMetricsBy, metrics) {
    if (!is.null(idInfo)) {
        idFilt <- idInfo
        for (nm in setdiff(colnames(idInfo), idCol)) {
            idFilt <- idFilt %>%
                dplyr::filter(.data[[nm]] %in% keepIdsBy[[nm]])
        }
        tmp <- df %>%
            dplyr::filter(.data[[idCol]] %in% intersect(keepIds, 
                                                        idFilt[[idCol]]))
    } else {
        tmp <- df %>%
            dplyr::filter(.data[[idCol]] %in% keepIds)
    }

    if (!is.null(metricInfo)) {
        metricFilt <- metricInfo
        for (nm in setdiff(colnames(metricInfo), metricCol)) {
            metricFilt <- metricFilt %>%
                dplyr::filter(.data[[nm]] %in% keepMetricsBy[[nm]])
        }
        tmp <- tmp %>%
            dplyr::select(-dplyr::any_of(
                setdiff(metrics, intersect(keepMetrics,
                                           metricFilt[[metricCol]]))))
    } else {
        tmp <- tmp %>%
            dplyr::select(-dplyr::any_of(setdiff(metrics, keepMetrics)))
    }
    tmp
}