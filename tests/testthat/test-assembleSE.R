test_that("assembleSE works", {
    df <- data.frame(Method = c("M1", "M2", "M3"), 
                     metric1 = c(1, 2, 3),
                     metric2 = c(3, 1, 2))
    metricInfo <- data.frame(Metric = c("metric1", "metric2", "metric3"),
                             Group = c("G1", "G2", "G2"))
    idInfo <- data.frame(Method = c("M1", "M2", "M3"), 
                         Type = c("T1", "T1", "T2"))
    
    ## Test that function returns error with invalid input
    ## -------------------------------------------------------------------------
    .args <- list(df = df, idCol = "Method", 
                  metrics = c("metric1", "metric2"),
                  initialWeights = NULL, initialTransforms = list(),
                  metricInfo = NULL, metricColors = NULL,
                  idInfo = NULL, idColors = NULL)
    
    args <- .args
    args$df <- 1
    expect_error(do.call(assembleSE, args), 
                 "'df' must be of class 'data.frame'")
    
    args <- .args
    args$idCol <- 1
    expect_error(do.call(assembleSE, args), 
                 "'idCol' must be of class 'character'")
    args$idCol <- c("metric1", "Method")
    expect_error(do.call(assembleSE, args), 
                 "'idCol' must have length 1")
    args$idCol <- "missing"
    expect_error(do.call(assembleSE, args), 
                 "All values in 'idCol' must be one of")
    
    args <- .args
    args$metrics <- 1
    expect_error(do.call(assembleSE, args), 
                 "'metrics' must be of class 'character'")
    args$metrics <- c("metric1", "Method")
    expect_error(do.call(assembleSE, args), 
                 "All values in 'metrics' must be one of")
    args$metrics <- c("metric1", "metric2", "metric3")
    expect_error(do.call(assembleSE, args), 
                 "All values in 'metrics' must be one of")
    
    args <- .args
    args$initialWeights <- "1"
    expect_error(do.call(assembleSE, args), 
                 "'initialWeights' must be of class 'numeric'")
    args$initialWeights <- 1
    expect_error(do.call(assembleSE, args), 
                 "'namesinitialWeights' must not be NULL")
    
    args <- .args
    args$initialTransforms <- "1"
    expect_error(do.call(assembleSE, args), 
                 "'initialTransforms' must be of class 'list'")
    args$initialTransforms <- list(1)
    expect_error(do.call(assembleSE, args), 
                 "'namesinitialTransforms' must not be NULL")
    
    args <- .args
    args$metricInfo <- 1
    expect_error(do.call(assembleSE, args), 
                 "'metricInfo' must be of class 'data.frame'")
    args$metricInfo <- metricInfo[, "Group", drop = FALSE]
    expect_error(do.call(assembleSE, args), 
                 "metricInfo must have a column named 'Metric'")
    args$metricInfo <- metricInfo |> dplyr::rename(input = Group)
    expect_error(do.call(assembleSE, args), 
                 "metricInfo can not have columns named")
    
    args <- .args
    args$idInfo <- 1
    expect_error(do.call(assembleSE, args), 
                 "'idInfo' must be of class 'data.frame'")
    args$idInfo <- idInfo[, "Type", drop = FALSE]
    expect_error(do.call(assembleSE, args), 
                 "idInfo must have a column named 'Method'")
    args$idInfo <- idInfo |> dplyr::rename(input = Type)
    expect_error(do.call(assembleSE, args), 
                 "idInfo can not have columns named")
    
    args <- .args
    args$metricColors <- 1
    expect_error(do.call(assembleSE, args),
                 "'metricColors' must be of class 'list'")

    args <- .args
    args$idColors <- 1
    expect_error(do.call(assembleSE, args),
                 "'idColors' must be of class 'list'")
    
    ## Test that function works as expected with valid input
    ## -------------------------------------------------------------------------
    ## Without annotations
    se <- assembleSE(df = df, idCol = "Method", 
                     metrics = c("metric1", "metric2"),
                     initialWeights = NULL, initialTransforms = list(),
                     metricInfo = NULL, metricColors = NULL,
                     idInfo = NULL, idColors = NULL)
    expect_s4_class(se, "SummarizedExperiment")
    expect_equal(dim(se), c(3, 2))
    expect_equal(colnames(se), c("metric1", "metric2"))
    expect_equal(rownames(se), c("M1", "M2", "M3"))
    expect_equal(SummarizedExperiment::assayNames(se), "values")
    expect_equal(SummarizedExperiment::assay(se, "values"), df[, -1], 
                 ignore_attr = TRUE)
    expect_s4_class(SummarizedExperiment::rowData(se), "DFrame")
    expect_equal(ncol(SummarizedExperiment::rowData(se)), 0)
    expect_s4_class(SummarizedExperiment::colData(se), "DFrame")
    expect_equal(ncol(SummarizedExperiment::colData(se)), 0)
    expect_type(S4Vectors::metadata(se), "list")
    expect_type(S4Vectors::metadata(se)$bettrInfo, "list")
    expect_named(S4Vectors::metadata(se)$bettrInfo, 
                 c("idCol", "metrics", "initialWeights", "initialTransforms",
                   "metricColors", "idColors"))
    expect_null(S4Vectors::metadata(se)$bettrInfo$metricColors)
    expect_null(S4Vectors::metadata(se)$bettrInfo$idColors)
    expect_null(S4Vectors::metadata(se)$bettrInfo$initialWeights)
    expect_equal(S4Vectors::metadata(se)$bettrInfo$idCol, "Method")
    expect_equal(S4Vectors::metadata(se)$bettrInfo$metrics, c("metric1", "metric2"))
    expect_equal(S4Vectors::metadata(se)$bettrInfo$initialTransforms, list())
    
    ## With annotations
    metricColors <- list(metric1 = "blue", metric2 = "green", metric3 = "red")
    idColors <- list(M1 = "purple", M2 = "yellow", M3 = "orange")
    initialTransforms <- list(metric1 = list(flip = TRUE), 
                              metric2 = list(offset = 1, flip = FALSE))
    initialWeights <- c(metric1 = 0.3, metric2 = 0.7)
    se <- assembleSE(df = df, idCol = "Method", 
                     metrics = c("metric1", "metric2"),
                     initialWeights = initialWeights, 
                     initialTransforms = initialTransforms,
                     metricInfo = metricInfo, metricColors = metricColors,
                     idInfo = idInfo, idColors = idColors)
    expect_s4_class(se, "SummarizedExperiment")
    expect_equal(dim(se), c(3, 2))
    expect_equal(colnames(se), c("metric1", "metric2"))
    expect_equal(rownames(se), c("M1", "M2", "M3"))
    expect_equal(SummarizedExperiment::assayNames(se), "values")
    expect_equal(SummarizedExperiment::assay(se, "values"), df[, -1], 
                 ignore_attr = TRUE)
    expect_s4_class(SummarizedExperiment::rowData(se), "DFrame")
    expect_equal(ncol(SummarizedExperiment::rowData(se)), 2)
    expect_equal(as.data.frame(SummarizedExperiment::rowData(se)), idInfo, 
                 ignore_attr = TRUE)
    expect_s4_class(SummarizedExperiment::colData(se), "DFrame")
    expect_equal(ncol(SummarizedExperiment::colData(se)), 2)
    expect_equal(as.data.frame(SummarizedExperiment::colData(se)), 
                 metricInfo[-3, ], ignore_attr = TRUE)
    expect_type(S4Vectors::metadata(se), "list")
    expect_type(S4Vectors::metadata(se)$bettrInfo, "list")
    expect_named(S4Vectors::metadata(se)$bettrInfo, 
                 c("idCol", "metrics", "initialWeights", "initialTransforms",
                   "metricColors", "idColors"))
    expect_equal(S4Vectors::metadata(se)$bettrInfo$metricColors, metricColors)
    expect_equal(S4Vectors::metadata(se)$bettrInfo$idColors, idColors)
    expect_equal(S4Vectors::metadata(se)$bettrInfo$initialWeights, initialWeights)
    expect_equal(S4Vectors::metadata(se)$bettrInfo$idCol, "Method")
    expect_equal(S4Vectors::metadata(se)$bettrInfo$metrics, c("metric1", "metric2"))
    expect_equal(S4Vectors::metadata(se)$bettrInfo$initialTransforms, 
                 initialTransforms)
})
