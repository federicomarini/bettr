#' @importFrom dplyr group_by summarize mutate ungroup
#' @importFrom Hmisc wtd.quantile
#' @param df A `data.frame` with columns `idCol`, `weightCol` and `valueCol`.
#'     The `metricCol` column is required if `scoreMethod` is 
#'     "weighted fraction highest" or "weighted fraction lowest".
#' @keywords internal
#' @noRd
.calculateScores <- function(df, scoreMethod, idCol, scoreCol, weightCol, 
                             valueCol, metricCol) {
    if (scoreMethod == "weighted mean") {
        scoreDf <- df |>
            dplyr::group_by(.data[[idCol]]) |>
            dplyr::summarize(
                "{scoreCol}" := sum(.data[[weightCol]] *
                                        .data[[valueCol]],
                                    na.rm = TRUE) / 
                    sum(.data[[weightCol]] * !is.na(.data[[valueCol]]), 
                        na.rm = TRUE)
            ) 
    } else if (scoreMethod == "weighted median") {
        scoreDf <- df |>
            dplyr::group_by(.data[[idCol]]) |>
            dplyr::summarize(
                "{scoreCol}" := as.numeric(Hmisc::wtd.quantile(
                    x = .data[[valueCol]], 
                    w = .data[[weightCol]],
                    probs = 0.5,
                    na.rm = TRUE))
            ) 
    } else if (scoreMethod == "weighted fraction highest") {
        scoreDf <- df |>
            dplyr::group_by(.data[[metricCol]]) |>
            dplyr::mutate(
                tempScore = (.data[[valueCol]] == 
                                 max(.data[[valueCol]], na.rm = TRUE))
            ) |>
            dplyr::ungroup() |>
            dplyr::group_by(.data[[idCol]]) |>
            dplyr::summarize(
                "{scoreCol}" := sum(
                    .data[[weightCol]] * .data$tempScore,
                    na.rm = TRUE) / 
                    sum(.data[[weightCol]] * !is.na(.data[[valueCol]]), 
                        na.rm = TRUE)
            ) 
    } else if (scoreMethod == "weighted fraction lowest") {
        scoreDf <- df |>
            dplyr::group_by(.data[[metricCol]]) |>
            dplyr::mutate(
                tempScore = (.data[[valueCol]] == 
                                 min(.data[[valueCol]], na.rm = TRUE))
            ) |>
            dplyr::ungroup() |>
            dplyr::group_by(.data[[idCol]]) |>
            dplyr::summarize(
                "{scoreCol}" := sum(
                    .data[[weightCol]] * .data$tempScore,
                    na.rm = TRUE) / 
                    sum(.data[[weightCol]] * !is.na(.data[[valueCol]]), 
                        na.rm = TRUE)
            ) 
    }
    scoreDf |> dplyr::ungroup()
}

#' @importFrom dplyr left_join group_by slice_max ungroup arrange desc 
#'     slice_min
#' @keywords internal
#' @noRd
.sortAndFilterScoreData <- function(scoreDf, idInfo, idCol, scoreCol,
                                    idTopNGrouping, idOrdering, showOnlyTopIds, 
                                    nbrTopIds) {
    if (!is.null(idInfo)) {
        scoreDf <- scoreDf |>
            dplyr::left_join(idInfo, by = idCol)
        if (idTopNGrouping != "---") {
            scoreDf <- scoreDf |>
                dplyr::group_by(.data[[idTopNGrouping]])
        }
    }
    if (idOrdering == "high-to-low") {
        if (showOnlyTopIds) {
            scoreDf <- scoreDf |>
                dplyr::slice_max(order_by = .data[[scoreCol]],
                                 n = nbrTopIds)
        }
        scoreDf <- scoreDf |>
            dplyr::ungroup() |> 
            dplyr::arrange(dplyr::desc(.data[[scoreCol]]))
    } else {
        if (showOnlyTopIds) {
            scoreDf <- scoreDf |>
                dplyr::slice_min(order_by = .data[[scoreCol]],
                                 n = nbrTopIds)
        }
        scoreDf <- scoreDf |>
            dplyr::ungroup() |>
            dplyr::arrange(.data[[scoreCol]])
    }
    scoreDf
}
