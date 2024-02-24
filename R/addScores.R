#' Calculate a weighted score for each method
#'  
#' @param df A `data.frame` with columns `idCol`, `weightCol` and `valueCol`.
#'     The `metricCol` column is required if `scoreMethod` is 
#'     "weighted fraction highest" or "weighted fraction lowest".
#' @param scoreMethod A character scalar providing the score method to use. 
#'     One of "weighted mean", "weighted median", "weighted fraction highest" 
#'     or "weighted fraction lowest".
#' @param idCol,weightCol,valueCol,metricCol Character scalars
#'     indicating which column in `df` corresponds to the method, weight,
#'     value and metric name, respectively.
#' @param scoreCol Character scalar giving the name of the score column that 
#'     will be generated in the output data.
#' 
#' @returns A `data.frame` with scores for each method.  
#' 
#' @keywords internal
#' @noRd
#' 
#' @importFrom dplyr group_by summarize mutate ungroup
#' @importFrom Hmisc wtd.quantile
#' 
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

#' Sort and filter score data frame
#' 
#' The score data frame can be filtered to only keep the top N methods
#' (either overall or within each group), and will be sorted by the score.
#' 
#' @param scoreDf A `data.frame` with scores for all methods. 
#' @param idInfo A `data.frame` with annotations for methods.
#' @param idCol,scoreCol Character scalars indicating the names of the 
#'     columns containing method names and scores.
#' @param idTopNGrouping Either "---" (no grouping) or the name of a column 
#'     in `idInfo` to group the methods by before filtering.
#' @param idOrdering Character scalar indicating how to sort the scores, 
#'     either "high-to-low" or "low-to-high".
#' @param showOnlyTopIds Logical scalar indicating whether to filter the 
#'     data frame to only show top methods.
#' @param nbrTopIds Numeric scalar giving the number of top-ranked methods to 
#'     retain. 
#' 
#' @returns A filtered and sorted `data.frame`.
#' 
#' @keywords internal
#' @noRd
#' 
#' @importFrom dplyr left_join group_by slice_max ungroup arrange desc 
#'     slice_min
#'     
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
