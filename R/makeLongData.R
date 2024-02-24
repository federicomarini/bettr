#' Convert metric data to long format
#' 
#' @param df A `data.frame` with methods in rows and metrics in columns.
#' @param idCol Character scalars indicating the name of the column of `df` 
#'     containing method names. 
#' @param metrics Character vector with metric names to retain.
#' @param metricCol,valueCol Character scalars providing the names of the 
#'     columns containing metric names and values in the output `data.frame`.
#' @param metricGrouping Either "---" or the name of a column of `metricInfo`
#'     to group metrics by.
#' @param metricInfo A `data.frame` containing annotations for metrics. 
#' @param metricGroupCol Character scalar providing the name of the column 
#'     in the output `data.frame` that will contain the value used to group 
#'     the metrics. 
#' 
#' @returns A long-format `data.frame`.
#' 
#' @keywords internal
#' @noRd
#' 
#' @importFrom tidyr pivot_longer 
#' @importFrom dplyr select contains all_of
.makeLongData <- function(df, idCol, metrics, metricCol, valueCol,
                          metricGrouping, metricInfo, metricGroupCol) {
    pd <- df |>
        dplyr::select(dplyr::all_of(idCol), 
                      dplyr::contains(metrics)) |>
        tidyr::pivot_longer(names_to = metricCol, values_to = valueCol,
                            -dplyr::all_of(idCol))
    ## Add grouping of metrics
    if (metricGrouping != "---") {
        pd[[metricGroupCol]] <- 
            metricInfo[[metricGrouping]][
                match(pd[[metricCol]], metricInfo[[metricCol]])]
    }
    pd
}

#' Add a weight column to a long-format data.frame
#' 
#' @param df A long-format `data.frame`.
#' @param metricCollapseGroup Logical scalar indicating whether to collapse 
#'     metrics by the grouping variable or not.
#' @param metricGrouping Either "---" (no grouping) or the name of the 
#'     annotation column used to group the metrics. 
#' @param metricGroupCol Character scalar indicating the name of the column 
#'     in `df` that contains the metric grouping.
#' @param weights A vector or list with weights.
#' @param weightCol Character scalar providing the name of the column in the 
#'     output `data.frame` that will contain the weights. 
#' @param metrics Character vector containing the metrics to keep (if no 
#'     grouping is done).
#' @param metricCol Character scalar giving the name of the column of `df`
#'     that contains the metric name. 
#' 
#' @returns A `data.frame` with a weight column added.
#' 
#' @keywords internal
#' @noRd
.addWeightsToLongData <- function(df, metricCollapseGroup, metricGrouping,
                                  metricGroupCol, weights, weightCol,
                                  metrics, metricCol) {
    pd <- df
    ## Add weight column for later score calculations
    if (metricCollapseGroup && metricGrouping != "---") {
        for (m in unique(pd[[metricGroupCol]])) {
            if (is.null(weights[[paste0(metricGrouping, "_", m, "_weight")]])) {
                return(NULL)
            }
            pd[[weightCol]][pd[[metricGroupCol]] == m] <- 
                weights[[paste0(metricGrouping, "_", m, "_weight")]]
        }
    } else {
        for (m in metrics) {
            pd[[weightCol]][pd[[metricCol]] == m] <- 
                weights[[paste0(m, "_weight")]]
        }
    }
    pd
}

#' Collapse a `data.frame` by method and metric group
#' 
#' @param df A long-format `data.frame`.
#' @param metricCollapseGroup Logical scalar indicating whether to collapse 
#'     metrics by the grouping variable or not.
#' @param metricGrouping Either "---" (no grouping) or the name of the 
#'     annotation column used to group the metrics. 
#' @param metricGroupCol Character scalar indicating the name of the column 
#'     in `df` that contains the metric grouping.
#' @param idCol Character scalar indicating the name of the column in `df`
#'     that contains the method names.
#' @param valueCol,weightCol,metricCol Character scalar providing the names 
#'     of the columns in the output `data.frame` that will contain the 
#'     collapsed values, weights and metric names.
#' @param collapseMethod Function used to perform the collapsing (e.g. mean).
#' 
#' @returns A collapsed `data.frame`.
#' 
#' @keywords internal
#' @noRd
#' 
#' @importFrom dplyr group_by summarize mutate ungroup
.collapseLongData <- function(df, metricCollapseGroup, metricGrouping,
                              idCol, metricGroupCol, valueCol, weightCol, 
                              metricCol, collapseMethod) {
    if (metricCollapseGroup && metricGrouping != "---") {
        df |>
            dplyr::group_by(.data[[idCol]], .data[[metricGroupCol]]) |>
            dplyr::summarize(
                "{ valueCol }" := get(collapseMethod)(.data[[valueCol]], 
                                                      na.rm = TRUE),
                "{ weightCol }" := mean(.data[[weightCol]], na.rm = TRUE)
            ) |>
            dplyr::mutate(
                "{ valueCol }" := replace(.data[[valueCol]], 
                                          !is.finite(.data[[valueCol]]), NA)
            ) |>
            dplyr::mutate("{ metricCol }" := .data[[metricGroupCol]]) |>
            dplyr::ungroup() |>
            as.data.frame()
    } else {
        df
    }
}
