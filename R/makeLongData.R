#' @importFrom tidyr pivot_longer 
#' @importFrom dplyr select contains
.makeLongData <- function(df, idCol, metrics, metricCol, valueCol,
                          metricGrouping, metricInfo, metricGroupCol) {
    pd <- df %>%
        dplyr::select(.data[[idCol]], 
                      dplyr::contains(metrics)) %>%
        tidyr::pivot_longer(names_to = metricCol, values_to = valueCol,
                            -.data[[idCol]])
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

#' @importFrom dplyr group_by sumarize mutate ungroup
.collapseLongData <- function(df, metricCollapseGroup, metricGrouping,
                              idCol, metricGroupCol, valueCol, weightCol, 
                              metricCol) {
    if (metricCollapseGroup && metricGrouping != "---") {
        df %>%
            dplyr::group_by(.data[[idCol]], .data[[metricGroupCol]]) %>%
            dplyr::summarize("{ valueCol }" := mean(.data[[valueCol]], na.rm = TRUE),
                             "{ weightCol }" := mean(.data[[weightCol]], na.rm = TRUE)) %>%
            dplyr::mutate("{ metricCol }" := .data[[metricGroupCol]]) %>%
            dplyr::ungroup() %>%
            as.data.frame()
    } else {
        df
    }
}


