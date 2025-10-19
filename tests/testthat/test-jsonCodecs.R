test_that("bettrToJSON works", {
    df <- data.frame(Method = c("M1", "M2", "M3"),
                     metric1 = c(1.0, 2.0, 3.0),
                     metric2 = c(3.0, 1.0, 2.0))
    metricInfo <- data.frame(Metric = c("metric1", "metric2"),
                             Group = c("G1", "G2"))
    idInfo <- data.frame(Method = c("M1", "M2", "M3"),
                         Type = c("T1", "T1", "T2"))
    metricColors <- list(Group = c(G1 = "red", G2 = "blue"))
    idColors <- list(Method = c(M1 = "purple", M2 = "yellow", M3 = "orange"))
    initialTransforms <- list(metric1 = list(flip = TRUE, transform = "[0,1]"))
    initialWeights <- c(metric1 = 0.5, metric2 = 0.5)

    ## Test with minimal SE (no annotations)
    ## -------------------------------------------------------------------------
    se <- assembleSE(df = df, idCol = "Method",
                     metrics = c("metric1", "metric2"))

    # Export to JSON string
    json_str <- bettrToJSON(se, pretty = FALSE)
    expect_type(json_str, "character")
    expect_true(nchar(json_str) > 0)

    # Parse and check structure
    json_list <- jsonlite::fromJSON(json_str)
    expect_type(json_list, "list")
    expect_true("idCol" %in% names(json_list))
    expect_true("data" %in% names(json_list))
    expect_false("version" %in% names(json_list))  # No version field
    expect_identical(json_list$idCol, "Method")
    expect_identical(nrow(json_list$data), 3L)
    expect_true("Method" %in% colnames(json_list$data))
    expect_true("metric1" %in% colnames(json_list$data))
    expect_true("metric2" %in% colnames(json_list$data))

    ## Test with full SE (with annotations)
    ## -------------------------------------------------------------------------
    se_full <- assembleSE(df = df, idCol = "Method",
                          metrics = c("metric1", "metric2"),
                          initialWeights = initialWeights,
                          initialTransforms = initialTransforms,
                          metricInfo = metricInfo,
                          metricColors = metricColors,
                          idInfo = idInfo,
                          idColors = idColors)

    json_str_full <- bettrToJSON(se_full, pretty = TRUE)
    json_list_full <- jsonlite::fromJSON(json_str_full)

    expect_true("metricInfo" %in% names(json_list_full))
    expect_true("idInfo" %in% names(json_list_full))
    expect_true("initialWeights" %in% names(json_list_full))
    expect_true("initialTransforms" %in% names(json_list_full))
    expect_true("metricColors" %in% names(json_list_full))
    expect_true("idColors" %in% names(json_list_full))

    expect_identical(nrow(json_list_full$metricInfo), 2L)
    expect_identical(nrow(json_list_full$idInfo), 3L)
    expect_true("Metric" %in% colnames(json_list_full$metricInfo))
    expect_true("Method" %in% colnames(json_list_full$idInfo))

    ## Test file export
    ## -------------------------------------------------------------------------
    tmp_file <- tempfile(fileext = ".json")
    result <- bettrToJSON(se_full, file = tmp_file)
    expect_identical(result, tmp_file)
    expect_true(file.exists(tmp_file))

    # Read back and verify
    file_content <- readLines(tmp_file)
    expect_true(length(file_content) > 0)

    unlink(tmp_file)

    ## Test error conditions
    ## -------------------------------------------------------------------------
    expect_error(bettrToJSON("not_a_se"),
                 "'bettrSE' must be of class 'SummarizedExperiment'")

    expect_error(bettrToJSON(se, file = 123),
                 "'file' must be of class 'character'")

    expect_error(bettrToJSON(se, pretty = "yes"),
                 "'pretty' must be of class 'logical'")

    # Test SE without bettrInfo metadata
    se_invalid <- se
    S4Vectors::metadata(se_invalid) <- list()
    expect_error(bettrToJSON(se_invalid),
                 "bettrSE must contain bettrInfo in metadata")
})

test_that("bettrFromJSON works", {
    df <- data.frame(Method = c("M1", "M2", "M3"),
                     metric1 = c(1.0, 2.0, 3.0),
                     metric2 = c(3.0, 1.0, 2.0))
    metricInfo <- data.frame(Metric = c("metric1", "metric2"),
                             Group = c("G1", "G2"))
    idInfo <- data.frame(Method = c("M1", "M2", "M3"),
                         Type = c("T1", "T1", "T2"))

    ## Test basic import from JSON string
    ## -------------------------------------------------------------------------
    se <- assembleSE(df = df, idCol = "Method",
                     metrics = c("metric1", "metric2"))
    json_str <- bettrToJSON(se)

    se_reload <- bettrFromJSON(json = json_str)
    expect_s4_class(se_reload, "SummarizedExperiment")
    expect_identical(dim(se_reload), dim(se))
    expect_identical(rownames(se_reload), rownames(se))
    expect_identical(colnames(se_reload), colnames(se))
    expect_equal(SummarizedExperiment::assay(se_reload, "values"),
                 SummarizedExperiment::assay(se, "values"),
                 ignore_attr = TRUE)

    ## Test import from file
    ## -------------------------------------------------------------------------
    tmp_file <- tempfile(fileext = ".json")
    bettrToJSON(se, file = tmp_file)

    se_reload_file <- bettrFromJSON(file = tmp_file)
    expect_s4_class(se_reload_file, "SummarizedExperiment")
    expect_identical(dim(se_reload_file), dim(se))

    unlink(tmp_file)

    ## Test with full annotations
    ## -------------------------------------------------------------------------
    se_full <- assembleSE(df = df, idCol = "Method",
                          metrics = c("metric1", "metric2"),
                          initialWeights = c(metric1 = 0.5, metric2 = 0.5),
                          initialTransforms = list(metric1 = list(flip = TRUE)),
                          metricInfo = metricInfo,
                          metricColors = list(Group = c(G1 = "red", G2 = "blue")),
                          idInfo = idInfo,
                          idColors = list(Method = c(M1 = "purple")))

    json_str_full <- bettrToJSON(se_full)
    se_reload_full <- bettrFromJSON(json = json_str_full)

    expect_s4_class(se_reload_full, "SummarizedExperiment")
    expect_identical(dim(se_reload_full), dim(se_full))

    # Check metadata
    meta_orig <- S4Vectors::metadata(se_full)$bettrInfo
    meta_reload <- S4Vectors::metadata(se_reload_full)$bettrInfo
    expect_identical(meta_orig$idCol, meta_reload$idCol)
    expect_identical(meta_orig$metrics, meta_reload$metrics)
    expect_identical(meta_orig$initialWeights, meta_reload$initialWeights)
    # Use expect_equal for transforms as JSON may convert integer to double
    expect_equal(meta_orig$initialTransforms, meta_reload$initialTransforms)
    expect_identical(meta_orig$metricColors, meta_reload$metricColors)
    expect_identical(meta_orig$idColors, meta_reload$idColors)

    # Check colData (metricInfo)
    expect_equal(as.data.frame(SummarizedExperiment::colData(se_reload_full)),
                 as.data.frame(SummarizedExperiment::colData(se_full)),
                 ignore_attr = TRUE)

    # Check rowData (idInfo)
    expect_equal(as.data.frame(SummarizedExperiment::rowData(se_reload_full)),
                 as.data.frame(SummarizedExperiment::rowData(se_full)),
                 ignore_attr = TRUE)

    ## Test error conditions
    ## -------------------------------------------------------------------------
    expect_error(bettrFromJSON(),
                 "Either 'file' or 'json' must be provided")

    expect_error(bettrFromJSON(file = "nonexistent.json"),
                 "File not found")

    expect_error(bettrFromJSON(json = 123),
                 "'json' must be of class 'character'")

    expect_error(bettrFromJSON(file = 123),
                 "'file' must be of class 'character'")

    # Invalid JSON
    expect_error(bettrFromJSON(json = "not valid json"),
                 "lexical error")

    # Missing required fields
    json_missing_idCol <- '{"data": [{"Method": "M1", "metric1": 1}]}'
    expect_error(bettrFromJSON(json = json_missing_idCol),
                 "JSON is missing required fields")

    json_missing_data <- '{"idCol": "Method"}'
    expect_error(bettrFromJSON(json = json_missing_data),
                 "JSON is missing required fields")

    # Invalid idCol
    json_bad_idcol <- '{"idCol": 123, "data": [{"Method": "M1"}]}'
    expect_error(bettrFromJSON(json = json_bad_idcol),
                 "must be of class 'character'")

    # idCol not in data
    json_idcol_missing <- '{"idCol": "missing", "data": [{"Method": "M1"}]}'
    expect_error(bettrFromJSON(json = json_idcol_missing),
                 "idCol 'missing' not found in data columns")
})

test_that("round-trip conversion preserves data", {
    df <- data.frame(Method = c("M1", "M2", "M3"),
                     metric1 = c(1.5, 2.3, 3.7),
                     metric2 = c(3.1, 1.9, 2.4),
                     metric3 = c(0.5, 0.8, 0.3))
    metricInfo <- data.frame(Metric = c("metric1", "metric2", "metric3"),
                             Group = c("G1", "G2", "G1"),
                             Type = c("A", "B", "A"))
    idInfo <- data.frame(Method = c("M1", "M2", "M3"),
                         Category = c("Cat1", "Cat2", "Cat1"),
                         Year = c(2020, 2021, 2022))
    metricColors <- list(Group = c(G1 = "red", G2 = "blue"),
                         Type = c(A = "green", B = "yellow"))
    idColors <- list(Category = c(Cat1 = "purple", Cat2 = "orange"))
    initialTransforms <- list(
        metric1 = list(flip = TRUE, offset = 0, transform = "[0,1]"),
        metric2 = list(flip = FALSE, offset = 1.0, transform = "z-score")
    )
    initialWeights <- c(metric1 = 0.3, metric2 = 0.5, metric3 = 0.2)

    ## Create SE with all features
    ## -------------------------------------------------------------------------
    se_orig <- assembleSE(df = df, idCol = "Method",
                          metrics = c("metric1", "metric2", "metric3"),
                          initialWeights = initialWeights,
                          initialTransforms = initialTransforms,
                          metricInfo = metricInfo,
                          metricColors = metricColors,
                          idInfo = idInfo,
                          idColors = idColors)

    ## Export and reimport
    ## -------------------------------------------------------------------------
    json_str <- bettrToJSON(se_orig)
    se_reload <- bettrFromJSON(json = json_str)

    ## Verify structure
    ## -------------------------------------------------------------------------
    expect_s4_class(se_reload, "SummarizedExperiment")
    expect_identical(dim(se_reload), dim(se_orig))
    expect_identical(rownames(se_reload), rownames(se_orig))
    expect_identical(colnames(se_reload), colnames(se_orig))

    ## Verify assay data (within JSON precision)
    ## -------------------------------------------------------------------------
    assay_orig <- SummarizedExperiment::assay(se_orig, "values")
    assay_reload <- SummarizedExperiment::assay(se_reload, "values")
    expect_equal(assay_orig, assay_reload, tolerance = 1e-10)

    ## Verify metadata
    ## -------------------------------------------------------------------------
    meta_orig <- S4Vectors::metadata(se_orig)$bettrInfo
    meta_reload <- S4Vectors::metadata(se_reload)$bettrInfo

    expect_identical(meta_orig$idCol, meta_reload$idCol)
    expect_identical(meta_orig$metrics, meta_reload$metrics)
    expect_identical(meta_orig$initialWeights, meta_reload$initialWeights)
    # Use expect_equal for transforms as JSON may convert integer to double
    expect_equal(meta_orig$initialTransforms, meta_reload$initialTransforms)
    expect_identical(meta_orig$metricColors, meta_reload$metricColors)
    expect_identical(meta_orig$idColors, meta_reload$idColors)

    ## Verify colData
    ## -------------------------------------------------------------------------
    coldata_orig <- as.data.frame(SummarizedExperiment::colData(se_orig))
    coldata_reload <- as.data.frame(SummarizedExperiment::colData(se_reload))
    expect_equal(coldata_orig, coldata_reload, ignore_attr = TRUE)

    ## Verify rowData
    ## -------------------------------------------------------------------------
    rowdata_orig <- as.data.frame(SummarizedExperiment::rowData(se_orig))
    rowdata_reload <- as.data.frame(SummarizedExperiment::rowData(se_reload))
    expect_equal(rowdata_orig, rowdata_reload, ignore_attr = TRUE)
})

test_that("JSON validation works correctly", {
    ## Valid minimal JSON
    ## -------------------------------------------------------------------------
    json_valid <- '{
        "idCol": "Method",
        "data": [
            {"Method": "M1", "metric1": 1.0},
            {"Method": "M2", "metric1": 2.0}
        ]
    }'
    expect_s4_class(bettrFromJSON(json = json_valid), "SummarizedExperiment")

    ## Test metricInfo validation
    ## -------------------------------------------------------------------------
    # metricInfo without Metric column
    json_bad_metricinfo <- '{
        "idCol": "Method",
        "data": [{"Method": "M1", "metric1": 1.0}],
        "metricInfo": [{"Group": "G1"}]
    }'
    expect_error(bettrFromJSON(json = json_bad_metricinfo),
                 "metricInfo must contain a 'Metric' column")

    ## Test idInfo validation
    ## -------------------------------------------------------------------------
    # idInfo without idCol
    json_bad_idinfo <- '{
        "idCol": "Method",
        "data": [{"Method": "M1", "metric1": 1.0}],
        "idInfo": [{"Type": "T1"}]
    }'
    expect_error(bettrFromJSON(json = json_bad_idinfo),
                 "idInfo must contain the idCol")

    ## Test with empty metadata objects
    ## -------------------------------------------------------------------------
    json_empty_meta <- '{
        "idCol": "Method",
        "data": [{"Method": "M1", "metric1": 1.0}],
        "metricInfo": {},
        "idInfo": {},
        "initialWeights": {},
        "initialTransforms": {}
    }'
    se_empty <- bettrFromJSON(json = json_empty_meta)
    expect_s4_class(se_empty, "SummarizedExperiment")
    expect_identical(ncol(SummarizedExperiment::colData(se_empty)), 0L)
    expect_identical(ncol(SummarizedExperiment::rowData(se_empty)), 0L)
})

test_that("special cases are handled correctly", {
    ## Test with single row
    ## -------------------------------------------------------------------------
    df_single <- data.frame(Method = "M1", metric1 = 1.0, metric2 = 2.0)
    se_single <- assembleSE(df = df_single, idCol = "Method")
    json_single <- bettrToJSON(se_single)
    se_single_reload <- bettrFromJSON(json = json_single)
    expect_identical(nrow(se_single_reload), 1L)

    ## Test with many metrics
    ## -------------------------------------------------------------------------
    df_many <- data.frame(Method = c("M1", "M2"))
    for (i in 1:20) {
        df_many[[paste0("metric", i)]] <- runif(2)
    }
    se_many <- assembleSE(df = df_many, idCol = "Method")
    json_many <- bettrToJSON(se_many)
    se_many_reload <- bettrFromJSON(json = json_many)
    expect_identical(ncol(se_many_reload), 20L)

    ## Test with NA values
    ## -------------------------------------------------------------------------
    df_na <- data.frame(Method = c("M1", "M2", "M3"),
                        metric1 = c(1.0, NA, 3.0),
                        metric2 = c(NA, 2.0, NA))
    se_na <- assembleSE(df = df_na, idCol = "Method")
    json_na <- bettrToJSON(se_na)
    se_na_reload <- bettrFromJSON(json = json_na)

    assay_na <- SummarizedExperiment::assay(se_na_reload, "values")
    expect_true(is.na(assay_na["M2", "metric1"]))
    expect_true(is.na(assay_na["M1", "metric2"]))
    expect_true(is.na(assay_na["M3", "metric2"]))

    ## Test with special characters in names
    ## -------------------------------------------------------------------------
    df_special <- data.frame(
        Method = c("M-1", "M.2", "M_3"),
        metric.one = c(1, 2, 3),
        metric_two = c(4, 5, 6)
    )
    se_special <- assembleSE(df = df_special, idCol = "Method")
    json_special <- bettrToJSON(se_special)
    se_special_reload <- bettrFromJSON(json = json_special)
    expect_identical(rownames(se_special_reload), c("M-1", "M.2", "M_3"))
})

test_that("file I/O works with both parameters", {
    df <- data.frame(Method = c("M1", "M2"), metric1 = c(1, 2))
    se <- assembleSE(df = df, idCol = "Method")

    tmp_file1 <- tempfile(fileext = ".json")
    tmp_file2 <- tempfile(fileext = ".json")

    # Export to file
    bettrToJSON(se, file = tmp_file1)

    # Import from file
    se_reload1 <- bettrFromJSON(file = tmp_file1)
    expect_s4_class(se_reload1, "SummarizedExperiment")

    # Export to string, then save manually
    json_str <- bettrToJSON(se)
    writeLines(json_str, tmp_file2)

    # Import from that file
    se_reload2 <- bettrFromJSON(file = tmp_file2)
    expect_s4_class(se_reload2, "SummarizedExperiment")

    # Both should be equivalent
    expect_equal(
        SummarizedExperiment::assay(se_reload1, "values"),
        SummarizedExperiment::assay(se_reload2, "values")
    )

    # Test warning when both file and json provided
    expect_warning(
        bettrFromJSON(file = tmp_file1, json = json_str),
        "Both 'file' and 'json' provided"
    )

    unlink(c(tmp_file1, tmp_file2))
})
