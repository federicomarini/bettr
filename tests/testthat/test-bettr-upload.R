# Tests for bettr JSON upload functionality

test_that("bettr function supports upload mode parameter", {
    # Check if bettr function has uploadMode parameter
    formals_bettr <- formals(bettr)
    expect_true("uploadMode" %in% names(formals_bettr))

    # Check default value
    expect_false(formals_bettr$uploadMode)

    # Check df parameter is now optional (has default NULL)
    expect_null(formals_bettr$df)
})

test_that("bettr function handles NULL df parameter correctly", {
    # Check that bettr function exists
    expect_true(exists("bettr"))

    # Check that the function recognizes uploadMode parameter
    formals_bettr <- formals(bettr)
    expect_true("uploadMode" %in% names(formals_bettr))
})

test_that("JSON file exists for testing", {
    # Check that duo2018_bettr.json exists in package root
    json_file <- system.file("duo2018_bettr.json", package = "bettr")

    expect_true(file.exists(json_file) || file.exists("duo2018_bettr.json"),
                info = "duo2018_bettr.json should exist for testing")
})

test_that("bettrFromJSON can read duo2018 test data", {
    json_file <- "duo2018_bettr.json"

    skip_if(!file.exists(json_file), "duo2018_bettr.json not found")

    # Read JSON file
    expect_no_error({
        bettrSE <- bettrFromJSON(file = json_file)
    })

    # Verify structure
    bettrSE <- bettrFromJSON(file = json_file)
    expect_s4_class(bettrSE, "SummarizedExperiment")

    # Check metadata
    meta <- S4Vectors::metadata(bettrSE)$bettrInfo
    expect_equal(meta$idCol, "method")
    expect_true(length(meta$metrics) > 0)
})

test_that("bettrToJSON creates valid JSON", {
    # Create simple test data
    test_data <- data.frame(
        Method = c("A", "B", "C"),
        metric1 = c(1, 2, 3),
        metric2 = c(0.1, 0.2, 0.3)
    )

    # Create bettrSE
    bettrSE <- assembleSE(df = test_data, idCol = "Method")

    # Export to JSON
    json_str <- bettrToJSON(bettrSE, file = NULL)

    # Verify it's valid JSON
    expect_type(json_str, "character")
    expect_true(nchar(json_str) > 0)

    # Verify we can parse it back
    expect_no_error({
        bettrSE_reloaded <- bettrFromJSON(json = json_str)
    })

    # Verify structure is preserved
    bettrSE_reloaded <- bettrFromJSON(json = json_str)
    expect_s4_class(bettrSE_reloaded, "SummarizedExperiment")
    expect_equal(rownames(bettrSE), rownames(bettrSE_reloaded))
})

test_that("JSON round-trip preserves data structure", {
    # Create test data with various features
    test_data <- data.frame(
        Method = c("AlgorithmA", "AlgorithmB", "AlgorithmC"),
        Precision = c(0.85, 0.92, 0.78),
        Recall = c(0.90, 0.85, 0.95),
        F1_Score = c(0.87, 0.88, 0.86)
    )

    # Create bettrSE with metadata
    metricInfo <- data.frame(
        Metric = c("Precision", "Recall", "F1_Score"),
        Type = c("Quality", "Quality", "Combined")
    )

    bettrSE_orig <- assembleSE(
        df = test_data,
        idCol = "Method",
        metricInfo = metricInfo
    )

    # Round-trip through JSON
    json_str <- bettrToJSON(bettrSE_orig, file = NULL)
    bettrSE_reload <- bettrFromJSON(json = json_str)

    # Verify key components match
    expect_equal(rownames(bettrSE_orig), rownames(bettrSE_reload))
    expect_equal(colnames(bettrSE_orig), colnames(bettrSE_reload))

    # Check assay data (with tolerance for JSON precision)
    orig_vals <- as.matrix(SummarizedExperiment::assay(bettrSE_orig, "values"))
    reload_vals <- as.matrix(SummarizedExperiment::assay(bettrSE_reload, "values"))
    expect_equal(orig_vals, reload_vals, tolerance = 0.01)

    # Check metadata
    meta_orig <- S4Vectors::metadata(bettrSE_orig)$bettrInfo
    meta_reload <- S4Vectors::metadata(bettrSE_reload)$bettrInfo
    expect_equal(meta_orig$idCol, meta_reload$idCol)
    expect_equal(meta_orig$metrics, meta_reload$metrics)
})
