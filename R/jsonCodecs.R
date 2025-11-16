#' Export SummarizedExperiment to bettr JSON format
#'
#' Export a bettr SummarizedExperiment object to a standardized JSON format
#' that can be re-imported using \code{bettrSEFromJSON}.
#'
#' @param bettrSE A \code{SummarizedExperiment} object created by
#'     \code{assembleSE}.
#' @param file Character scalar, path to the output JSON file. If NULL,
#'     returns the JSON string without writing to file.
#' @param pretty Logical scalar, whether to format the JSON output with
#'     indentation for readability (default: TRUE).
#'
#' @return If \code{file} is NULL, returns a JSON string. Otherwise, writes
#'     to file and returns the file path invisibly.
#'
#' @export
#' 
#' @author Daniel Incicau
#'
#' @importFrom jsonlite toJSON
#' @importFrom SummarizedExperiment assay colData rowData
#' @importFrom S4Vectors metadata
#'
#' @examples
#' df <- data.frame(Method = c("M1", "M2", "M3"),
#'                  metric1 = c(1, 2, 3),
#'                  metric2 = c(3, 1, 2))
#' bettrSE <- assembleSE(df = df)
#' json_str <- bettrToJSON(bettrSE)
#'
bettrToJSON <- function(bettrSE, file = NULL, pretty = TRUE) {
    .assertVector(x = bettrSE, type = "SummarizedExperiment")
    if (!is.null(file)) {
        .assertScalar(x = file, type = "character")
    }
    .assertScalar(x = pretty, type = "logical")

    # Extract metadata
    meta <- S4Vectors::metadata(bettrSE)
    if (is.null(meta$bettrInfo)) {
        stop("bettrSE must contain bettrInfo in metadata")
    }

    bettrInfo <- meta$bettrInfo
    idCol <- bettrInfo$idCol

    # Extract assay data
    values <- SummarizedExperiment::assay(bettrSE, "values")

    # Build the data frame in wide format
    df <- as.data.frame(values)
    df[[idCol]] <- rownames(values)

    # Reorder columns to put idCol first
    df <- df[, c(idCol, setdiff(colnames(df), idCol)), drop = FALSE]

    # Extract metricInfo if present
    metricInfo <- NULL
    if (ncol(SummarizedExperiment::colData(bettrSE)) > 0) {
        metricInfo <- as.data.frame(SummarizedExperiment::colData(bettrSE))
        # Remove rownames from export (they're just the Metric column)
        rownames(metricInfo) <- NULL
    }

    # Extract idInfo if present
    idInfo <- NULL
    if (ncol(SummarizedExperiment::rowData(bettrSE)) > 0) {
        idInfo <- as.data.frame(SummarizedExperiment::rowData(bettrSE))
        # Remove rownames from export
        rownames(idInfo) <- NULL
    }

    # Convert named vectors to lists to preserve names in JSON
    # This is necessary because JSON arrays don't preserve R named vector names
    initialWeights_export <- bettrInfo$initialWeights
    if (!is.null(initialWeights_export) && !is.null(names(initialWeights_export))) {
        initialWeights_export <- as.list(initialWeights_export)
    }

    metricColors_export <- bettrInfo$metricColors
    if (!is.null(metricColors_export)) {
        metricColors_export <- lapply(metricColors_export, function(x) {
            if (!is.null(names(x))) {
                as.list(x)
            } else {
                x
            }
        })
    }

    idColors_export <- bettrInfo$idColors
    if (!is.null(idColors_export)) {
        idColors_export <- lapply(idColors_export, function(x) {
            if (!is.null(names(x))) {
                as.list(x)
            } else {
                x
            }
        })
    }

    # Build the JSON structure
    json_list <- list(
        idCol = idCol,
        data = df,
        metricInfo = metricInfo,
        idInfo = idInfo,
        initialWeights = initialWeights_export,
        initialTransforms = bettrInfo$initialTransforms,
        metricColors = metricColors_export,
        idColors = idColors_export
    )

    # Convert to JSON
    json_str <- jsonlite::toJSON(json_list, pretty = pretty,
                                 auto_unbox = TRUE, na = "null")

    # Convert to plain character (removes "json" class from jsonlite)
    json_str <- as.character(json_str)

    # Write to file or return string
    if (!is.null(file)) {
        write(json_str, file = file)
        return(invisible(file))
    } else {
        return(json_str)
    }
}

#' @keywords internal
#' @noRd
.validateBettrJSON <- function(json_list) {
    # Check required fields
    required <- c("idCol", "data")
    missing <- setdiff(required, names(json_list))
    if (length(missing) > 0) {
        stop("JSON is missing required fields: ", paste(missing, collapse = ", "))
    }

    # Validate idCol
    .assertScalar(x = json_list$idCol, type = "character")

    # Validate data
    if (!is.data.frame(json_list$data) && !is.list(json_list$data)) {
        stop("data must be a data frame or list")
    }

    df <- as.data.frame(json_list$data)
    if (!(json_list$idCol %in% colnames(df))) {
        stop("idCol '", json_list$idCol, "' not found in data columns")
    }

    # Validate metricInfo if present
    if (!is.null(json_list$metricInfo) && length(json_list$metricInfo) > 0) {
        metricInfo <- as.data.frame(json_list$metricInfo)
        if (ncol(metricInfo) > 0 && !("Metric" %in% colnames(metricInfo))) {
            stop("metricInfo must contain a 'Metric' column")
        }
    }

    # Validate idInfo if present
    if (!is.null(json_list$idInfo) && length(json_list$idInfo) > 0) {
        idInfo <- as.data.frame(json_list$idInfo)
        if (ncol(idInfo) > 0 && !(json_list$idCol %in% colnames(idInfo))) {
            stop("idInfo must contain the idCol '", json_list$idCol, "'")
        }
    }

    # Validate initialTransforms if present
    if (!is.null(json_list$initialTransforms)) {
        if (!is.list(json_list$initialTransforms)) {
            stop("initialTransforms must be a list")
        }
        # Check that each transform has valid fields
        for (metric_name in names(json_list$initialTransforms)) {
            trans <- json_list$initialTransforms[[metric_name]]
            if (!is.list(trans)) {
                stop("Each initialTransform entry must be a list")
            }
            valid_fields <- c("flip", "offset", "transform", "cuts")
            invalid <- setdiff(names(trans), valid_fields)
            if (length(invalid) > 0) {
                warning("Transform for '", metric_name,
                        "' has unknown fields: ", paste(invalid, collapse = ", "))
            }
        }
    }

    # Validate initialWeights if present
    if (!is.null(json_list$initialWeights)) {
        if (length(json_list$initialWeights) > 0) {
            if (!is.list(json_list$initialWeights) &&
                !is.numeric(json_list$initialWeights)) {
                stop("initialWeights must be a named numeric vector or list")
            }
        }
    }

    invisible(TRUE)
}

#' Import bettr data from JSON format
#'
#' Import bettr data from a standardized JSON format and create a
#' SummarizedExperiment object using \code{assembleSE}.
#'
#' @param file Character scalar, path to the JSON file to import.
#' @param json Character scalar, JSON string to parse. If provided, \code{file}
#'     is ignored.
#'
#' @return A \code{SummarizedExperiment} object that can be passed to
#'     \code{bettr}.
#'
#' @export
#' 
#' @author Daniel Incicau
#'
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' df <- data.frame(Method = c("M1", "M2", "M3"),
#'                  metric1 = c(1, 2, 3),
#'                  metric2 = c(3, 1, 2))
#' bettrSE <- assembleSE(df = df)
#' json_file <- tempfile(fileext = ".json")
#' bettrToJSON(bettrSE, file = json_file)
#' bettrSE_reload <- bettrFromJSON(file = json_file)
#'
bettrFromJSON <- function(file = NULL, json = NULL) {
    # Input validation
    if (is.null(file) && is.null(json)) {
        stop("Either 'file' or 'json' must be provided")
    }

    if (!is.null(file) && !is.null(json)) {
        warning("Both 'file' and 'json' provided; using 'json'")
    }

    # Read JSON
    if (!is.null(json)) {
        .assertScalar(x = json, type = "character")
        json_list <- jsonlite::fromJSON(json, simplifyVector = TRUE)
    } else {
        .assertScalar(x = file, type = "character")
        if (!file.exists(file)) {
            stop("File not found: ", file)
        }
        json_list <- jsonlite::fromJSON(file, simplifyVector = TRUE)
    }

    # Validate JSON structure
    .validateBettrJSON(json_list)

    # Extract components
    idCol <- json_list$idCol
    df <- as.data.frame(json_list$data)

    # Remove row names if present
    rownames(df) <- NULL

    # Determine metrics (all columns except idCol)
    metrics <- setdiff(colnames(df), idCol)

    # Extract optional components
    metricInfo <- if (!is.null(json_list$metricInfo) && length(json_list$metricInfo) > 0) {
        mi <- as.data.frame(json_list$metricInfo)
        if (ncol(mi) > 0) mi else NULL
    } else {
        NULL
    }

    idInfo <- if (!is.null(json_list$idInfo) && length(json_list$idInfo) > 0) {
        ii <- as.data.frame(json_list$idInfo)
        if (ncol(ii) > 0) ii else NULL
    } else {
        NULL
    }

    # Handle initialWeights - convert empty list to NULL
    initialWeights <- json_list$initialWeights
    if (!is.null(initialWeights) && length(initialWeights) == 0) {
        initialWeights <- NULL
    }
    # Ensure initialWeights is a named numeric vector if present
    if (!is.null(initialWeights) && length(initialWeights) > 0) {
        if (is.list(initialWeights)) {
            initialWeights <- unlist(initialWeights)
        }
        if (is.null(names(initialWeights))) {
            warning("initialWeights has no names, setting to NULL")
            initialWeights <- NULL
        }
    }

    # Handle initialTransforms - convert from list to proper format
    initialTransforms <- json_list$initialTransforms
    if (!is.null(initialTransforms) && length(initialTransforms) == 0) {
        initialTransforms <- list()
    }

    # Handle metricColors - ensure named vectors within the list
    metricColors <- json_list$metricColors
    if (!is.null(metricColors) && is.list(metricColors)) {
        metricColors <- lapply(metricColors, function(x) {
            if (is.list(x) && !is.null(names(x))) {
                unlist(x)
            } else {
                x
            }
        })
    }

    # Handle idColors - ensure named vectors within the list
    idColors <- json_list$idColors
    if (!is.null(idColors) && is.list(idColors)) {
        idColors <- lapply(idColors, function(x) {
            if (is.list(x) && !is.null(names(x))) {
                unlist(x)
            } else {
                x
            }
        })
    }

    # Call assembleSE to create the SummarizedExperiment
    bettrSE <- assembleSE(
        df = df,
        idCol = idCol,
        metrics = metrics,
        initialWeights = initialWeights,
        initialTransforms = initialTransforms,
        metricInfo = metricInfo,
        metricColors = metricColors,
        idInfo = idInfo,
        idColors = idColors
    )

    return(bettrSE)
}
