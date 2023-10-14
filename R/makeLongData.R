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