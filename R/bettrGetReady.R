#' @keywords internal
#' @noRd
.checkArgs_bettrGetReady <- function(
        df, idCol, metrics, initialWeights, initialTransforms, metricInfo, 
        metricColors, idInfo, idColors, scoreMethod, idOrdering, 
        showOnlyTopIds, nbrTopIds, idTopNGrouping, keepIds, metricGrouping,
        metricCollapseGroup, metricCollapseMethod) {
    .assertVector(x = df, type = "data.frame")
    .assertScalar(x = idCol, type = "character", validValues = colnames(df))
    .assertVector(x = metrics, type = "character", validValues = colnames(df))
    .assertVector(x = initialWeights, type = "numeric", rngIncl = c(0, 1), 
                  allowNULL = TRUE)
    .assertVector(x = initialTransforms, type = "list", allowNULL = TRUE)
    .assertVector(x = metricInfo, type = "data.frame", allowNULL = TRUE)
    .assertVector(x = metricColors, type = "list", allowNULL = TRUE)
    .assertVector(x = idInfo, type = "data.frame", allowNULL = TRUE)
    .assertVector(x = idColors, type = "list", allowNULL = TRUE)
    .assertScalar(x = scoreMethod, type = "character", 
                  validValues = c("weighted mean", "weighted median",
                                  "weighted fraction highest",
                                  "weighted fraction lowest"))
    .assertScalar(x = idOrdering, type = "character", 
                  validValues = c("high-to-low", "low-to-high"))
    .assertScalar(x = showOnlyTopIds, type = "logical")
    .assertScalar(x = nbrTopIds, type = "numeric")
    .assertScalar(x = idTopNGrouping, type = "character", allowNULL = TRUE)
    .assertVector(x = keepIds, type = "character", allowNULL = TRUE, 
                  validValues = df[[idCol]])
    .assertScalar(x = metricGrouping, type = "character", allowNULL = TRUE)
    .assertScalar(x = metricCollapseGroup, type = "logical")
    .assertScalar(x = metricCollapseMethod, type = "character", 
                  validValues = c("mean", "max", "min"))
}

#' Prepare data for plotting with bettr
#' 
#' Prepare input data for plotting with bettr. This function replicates the 
#' steps that are performed in the shiny app.
#' 
#' @inheritParams bettr
#' @param scoreMethod Character scalar specifying the scoring method, that is, 
#'     how to aggregate scores across metrics. Should be one of 
#'     \code{"weighted mean"}, \code{"weighted median"}, 
#'     \code{"weighted fraction highest"} or \code{"weighted fraction lowest"}.
#' @param idOrdering Character scalar indicating whether methods should be  
#'     ranked with highest aggregated scores on top (\code{"high-to-low"}) or 
#'     opposite (\code{"low-to-high"}).
#' @param showOnlyTopIds Logical scalar indicating whether to only retain the 
#'     top N methods (ranked by the aggregated score). 
#' @param nbrTopIds If \code{showOnlyTopIds} is \code{TRUE}, the number of 
#'     top-ranked methods to retain.
#' @param idTopNGrouping If \code{showOnlyTopIds} is \code{TRUE}, a 
#'     character scalar providing the name of a column in \code{idInfo} that 
#'     groups the methods. If specified, he top \code{nbrTopIds} within each 
#'     group will be retained. 
#' @param keepIds Character vector indicating which methods (a subset of the 
#'     values in \code{df[[idCol]]}) that should be considered. If \code{NULL},
#'     all methods are considered. 
#' @param metricGrouping A character scalar providing the name of a column in 
#'     \code{metricInfo} by which metrics should be grouped. If \code{NULL},
#'     no grouping is performed. 
#' @param metricCollapseGroup A logical scalar indicating whether metric 
#'     values should be collapsed within each group defined by 
#'     \code{metricGrouping}.
#' @param metricCollapseMethod If \code{metricCollapseGroup} is \code{TRUE}, 
#'     the way in which metric values are collapsed within a group. Should be 
#'     one of \code{"mean"}, \code{"max"} or \code{"min"}.
#' 
#' @returns 
#' A list of objects, which can be directly used as inputs for the bettr 
#' plotting functions. See the man page for the respective plotting function 
#' for more details. 
#' 
#' @author Charlotte Soneson
#' @export
#' 
#' @importFrom dplyr filter
#' 
#' @examples 
#' ## Generate example data
#' df <- data.frame(Method = c("M1", "M2", "M3"), 
#'                  metric1 = c(1, 2, 3),
#'                  metric2 = c(3, 1, 2))
#' metricInfo <- data.frame(Metric = c("metric1", "metric2", "metric3"),
#'                          Group = c("G1", "G2", "G2"))
#' idInfo <- data.frame(Method = c("M1", "M2", "M3"), 
#'                      Type = c("T1", "T1", "T2"))
#' prepData <- bettrGetReady(df = df, idCol = "Method", 
#'                           metricInfo = metricInfo, idInfo = idInfo)
#' prepData <- bettrGetReady(df = df, idCol = "Method", 
#'                           metricInfo = metricInfo, idInfo = idInfo, 
#'                           metricGrouping = "Group", 
#'                           metricCollapseGroup = TRUE)
#' 
bettrGetReady <- function(df, idCol = "Method", 
                          metrics = setdiff(colnames(df), idCol), 
                          initialWeights = NULL, 
                          initialTransforms = list(),
                          metricInfo = NULL, metricColors = NULL,
                          idInfo = NULL, idColors = NULL, 
                          scoreMethod = "weighted mean", 
                          idOrdering = "high-to-low", 
                          showOnlyTopIds = FALSE, nbrTopIds = 10, 
                          idTopNGrouping = NULL, 
                          keepIds = NULL, 
                          metricGrouping = NULL, metricCollapseGroup = FALSE, 
                          metricCollapseMethod = "mean", bettrSE = NULL) {

    ## Get arguments from bettrSE if provided ---------------------------------
    if (!is.null(bettrSE)) {
        .assertVector(x = bettrSE, type = "SummarizedExperiment")
        df <- as.data.frame(SummarizedExperiment::assay(bettrSE, "values"))
        df[[idCol]] <- rownames(df)
        metrics <- S4Vectors::metadata(bettrSE)$bettrInfo$metrics
        initialWeights <- S4Vectors::metadata(bettrSE)$bettrInfo$initialWeights
        initialTransforms <- 
            S4Vectors::metadata(bettrSE)$bettrInfo$initialTransforms
        metricColors <- S4Vectors::metadata(bettrSE)$bettrInfo$metricColors
        idColors <- S4Vectors::metadata(bettrSE)$bettrInfo$idColors
        metricInfo <- as.data.frame(SummarizedExperiment::colData(bettrSE))
        if (ncol(metricInfo) == 0) {
            metricInfo <- NULL
        }
        idInfo <- as.data.frame(SummarizedExperiment::rowData(bettrSE))
        if (ncol(idInfo) == 0) {
            idInfo <- NULL
        }
    }
    
    ## Check arguments
    ## -------------------------------------------------------------------------
    .checkArgs_bettrGetReady(
        df = df, idCol = idCol, metrics = metrics,
        initialWeights = initialWeights, 
        initialTransforms = initialTransforms, 
        metricInfo = metricInfo, metricColors = metricColors, 
        idInfo = idInfo, idColors = idColors, 
        scoreMethod = scoreMethod, idOrdering = idOrdering,
        showOnlyTopIds = showOnlyTopIds, nbrTopIds = nbrTopIds,
        idTopNGrouping = idTopNGrouping, keepIds = keepIds,
        metricGrouping = metricGrouping, 
        metricCollapseGroup = metricCollapseGroup,
        metricCollapseMethod = metricCollapseMethod
    )

    
    ## Define variables
    ## -------------------------------------------------------------------------
    scoreCol <- "Score"
    weightCol <- "Weight"
    metricCol <- "Metric"
    valueCol <- "ScaledValue"
    metricGroupCol <- "metricGroup"
    defaultWeightValue <- 0.2
    
    if (is.null(idTopNGrouping)) {
        idTopNGrouping <- "---"
    }
    if (is.null(keepIds)) {
        keepIds <- unique(df[[idCol]])
    }
    if (is.null(metricGrouping)) {
        metricGrouping <- "---"
    }

    ## Initial preparation
    ## -------------------------------------------------------------------------
    prep <- .prepareData(
        df = df, idCol = idCol, metrics = metrics, 
        initialWeights = initialWeights,
        initialTransforms = initialTransforms, 
        metricInfo = metricInfo, 
        metricColors = metricColors, 
        idInfo = idInfo,
        idColors = idColors,
        weightResolution = 0.05,
        metricCol = metricCol, 
        defaultWeightValue = defaultWeightValue
    )
    
    values <- list(
        df = df,
        metrics = metrics,
        nMetrics = length(metrics),
        metricInfo = prep$metricInfo,
        idInfo = prep$idInfo,
        methods = unique(df[[idCol]]),
        currentWeights = prep$initialWeights
    )
    
    idFilters <- setdiff(colnames(values$idInfo), idCol)
    metricFilters <- setdiff(colnames(values$metricInfo), metricCol)
    filtdata <- .filterData(
        df = values$df, idInfo = values$idInfo, idCol = idCol,
        keepIds = keepIds,
        keepIdsBy = lapply(
            structure(idFilters, names = idFilters),
            function(nm) idInfo[[nm]]),
        metricInfo = values$metricInfo,
        metricCol = metricCol, keepMetrics = values$metrics,
        keepMetricsBy = lapply(
            structure(metricFilters, names = metricFilters),
            function(nm) metricInfo[[nm]]),
        metrics = values$metrics
    )
    
    metricsInUse <- intersect(values$metrics, colnames(filtdata))
    methodsInUse <- unique(filtdata[[idCol]])
    
    procdata <- filtdata
    for (m in intersect(colnames(filtdata), metricsInUse)) {
        if (m %in% prep$metrics_num) {
            procdata[[m]] <- .transformNumericVariable(
                x = filtdata[[m]],
                flip = prep$initialTransforms[[m]]$flip, 
                offset = prep$initialTransforms[[m]]$offset, 
                transf = .getTransf(prep$initialTransforms[[m]]$transform), 
                bincuts = sort(as.numeric(prep$initialTransforms[[m]]$cuts))
            )
        } else if (m %in% prep$metrics_cat) {
            procdata[[m]] <- .transformCategoricalVariable(
                x = filtdata[[m]],
                levels = prep$initialTransforms[[m]]$levels
            )
        } else {
            stop("Encountered metric that could not be identified ",
                 "as numeric or categorical: ", m)
        }
    }
    
    
    longdata <- .makeLongData(
        df = procdata, idCol = idCol, 
        metrics = metricsInUse, metricCol = metricCol,
        valueCol = valueCol, 
        metricGrouping = metricGrouping, 
        metricInfo = metricInfo,
        metricGroupCol = metricGroupCol
    )
    
    names(prep$initialWeights) <- paste0(names(prep$initialWeights), "_weight")
    longdataweights <- .addWeightsToLongData(
        df = longdata, 
        metricCollapseGroup = metricCollapseGroup,
        metricGrouping = metricGrouping,
        metricGroupCol = metricGroupCol,
        weights = prep$initialWeights,
        weightCol = weightCol, 
        metrics = metricsInUse,
        metricCol = metricCol
    )
    
    collapseddata <- .collapseLongData(
        df = longdataweights, 
        metricCollapseGroup = metricCollapseGroup,
        metricGrouping = metricGrouping,
        idCol = idCol, metricGroupCol = metricGroupCol,
        valueCol = valueCol, weightCol = weightCol, 
        metricCol = metricCol, 
        collapseMethod = metricCollapseMethod
    )
    
    scoreDf <- .calculateScores(
        df = collapseddata, 
        scoreMethod = scoreMethod, 
        idCol = idCol, scoreCol = scoreCol, 
        weightCol = weightCol, valueCol = valueCol, 
        metricCol = metricCol
    )
    scoredata <- .sortAndFilterScoreData(
        scoreDf = scoreDf, 
        idInfo = values$idInfo, 
        idCol = idCol, scoreCol = scoreCol,
        idTopNGrouping = idTopNGrouping,
        idOrdering = idOrdering,
        showOnlyTopIds = showOnlyTopIds, 
        nbrTopIds = nbrTopIds
    )
    
    plotdata <- collapseddata |>
        dplyr::filter(.data[[idCol]] %in% scoredata[[idCol]])
    plotdata[[idCol]] <- factor(plotdata[[idCol]], 
                                levels = scoredata[[idCol]])
    
    ## Return processed data
    ## -------------------------------------------------------------------------
    list(plotdata = plotdata, scoredata = scoredata, idColors = prep$idColors, 
         metricColors = prep$metricColors, metricGrouping = metricGrouping,
         metricCollapseGroup = metricCollapseGroup, idInfo = values$idInfo, 
         metricInfo = values$metricInfo, metricGroupCol = metricGroupCol, 
         methods = methodsInUse, idCol = idCol, metricCol = metricCol, 
         valueCol = valueCol, weightCol = weightCol, scoreCol = scoreCol)
}