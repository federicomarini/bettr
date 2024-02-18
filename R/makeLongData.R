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
