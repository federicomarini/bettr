#' @keywords internal
#' @noRd
#' 
.checkArgs_assembleSE <- function(
        df, idCol, metrics, initialWeights, initialTransforms, metricInfo, 
        metricColors, idInfo, idColors) {
    ## df is a data.frame with one column containing method IDs, and the others
    ## representing metric values
    .assertVector(x = df, type = "data.frame")
    .assertScalar(x = idCol, type = "character", validValues = colnames(df))
    .assertVector(x = metrics, type = "character", 
                  validValues = setdiff(colnames(df), idCol))
    if (!all(metrics == make.names(metrics))) {
        stop("All metrics must be valid names (i.e., no spaces or special ",
             "characters). Please modify the metric names accordingly.")
    }
    .assertVector(x = initialWeights, type = "numeric", rngIncl = c(0, 1), 
                  allowNULL = TRUE)
    .assertVector(x = initialTransforms, type = "list", allowNULL = TRUE)
    .assertVector(x = metricInfo, type = "data.frame", allowNULL = TRUE)
    .assertVector(x = metricColors, type = "list", allowNULL = TRUE)
    .assertVector(x = idInfo, type = "data.frame", allowNULL = TRUE)
    .assertVector(x = idColors, type = "list", allowNULL = TRUE)
    
    if (!is.null(initialWeights)) {
        .assertVector(x = names(initialWeights), type = "character")
    }
    
    if (length(initialTransforms) != 0) {
        .assertVector(x = names(initialTransforms), type = "character")
    }
    
    if (!is.null(metricInfo)) {
        if (!("Metric" %in% colnames(metricInfo))) {
            stop("metricInfo must have a column named 'Metric'")
        }
        if (any(c("input", "output") %in% colnames(metricInfo))) {
            stop("metricInfo can not have columns named 'input' or 'output'")
        }
        if (!all(metrics %in% metricInfo$Metric)) {
            warning("metricInfo does not provide annotations for all metrics")
        }
    }
    
    if (!is.null(idInfo)) {
        if (!(idCol %in% colnames(idInfo))) {
            stop("idInfo must have a column named '", idCol, "'")
        }
        if (any(c("input", "output") %in% colnames(idInfo))) {
            stop("idInfo can not have columns named 'input' or 'output'")
        }
        if (!all(df[[idCol]] %in% idInfo[[idCol]])) {
            warning("idInfo does not provide annotations for all methods")
        }
    }
}

#' Assemble all bettr input into a SummarizedExperiment object
#' 
#' Assemble all bettr input into a \code{SummarizedExperiment} object. This 
#' has the advantage of keeping all data together in a single object, and can 
#' be used as input to \code{bettr} or \code{bettrGetReady}, instead of
#' providing the individual components. 
#' 
#' @author Charlotte Soneson
#' @export
#' 
#' @inheritParams bettr
#' 
#' @importFrom SummarizedExperiment SummarizedExperiment assay rowData colData
#' @importFrom S4Vectors DataFrame
#' @importFrom tibble column_to_rownames
#' 
#' @returns 
#' A SummarizedExperiment object with rows corresponding to methods and columns 
#' corresponding to metrics. 
#' 
#' @examples
#' df <- data.frame(Method = c("M1", "M2", "M3"), 
#'                  metric1 = c(1, 2, 3),
#'                  metric2 = c(3, 1, 2))
#' metricInfo <- data.frame(Metric = c("metric1", "metric2", "metric3"),
#'                          Group = c("G1", "G2", "G2"))
#' idInfo <- data.frame(Method = c("M1", "M2", "M3"), 
#'                      Type = c("T1", "T1", "T2"))
#' bettrSE <- assembleSE(df = df, metricInfo = metricInfo, idInfo = idInfo)
#'                      
assembleSE <- function(df, idCol = "Method", 
                       metrics = setdiff(colnames(df), idCol),
                       initialWeights = NULL, initialTransforms = list(),
                       metricInfo = NULL, metricColors = NULL,
                       idInfo = NULL, idColors = NULL) {
    
    ## Check arguments
    ## -------------------------------------------------------------------------
    .checkArgs_assembleSE(df = df, idCol = idCol, metrics = metrics,
                          initialWeights = initialWeights,
                          initialTransforms = initialTransforms,
                          metricInfo = metricInfo, metricColors = metricColors,
                          idInfo = idInfo, idColors = idColors)

    ## Assemble SE
    ## -------------------------------------------------------------------------
    a <- df |> as.data.frame() |>
        tibble::column_to_rownames(idCol)
    
    se <- SummarizedExperiment::SummarizedExperiment(
        assays = list(values = a),
        metadata = list(bettrInfo = list(idCol = idCol, metrics = metrics, 
                                         initialWeights = initialWeights, 
                                         initialTransforms = initialTransforms,
                                         metricColors = metricColors,
                                         idColors = idColors))
    )

    if (!is.null(idInfo)) {
        idInfo <- S4Vectors::DataFrame(idInfo)
        rownames(idInfo) <- idInfo[[idCol]]
        SummarizedExperiment::rowData(se) <- 
            idInfo[rownames(a), , drop = FALSE]
    }
    if (!is.null(metricInfo)) {
        metricInfo <- S4Vectors::DataFrame(metricInfo)
        rownames(metricInfo) <- metricInfo$Metric
        SummarizedExperiment::colData(se) <- 
            metricInfo[colnames(a), , drop = FALSE]
    }
    
    se
}
