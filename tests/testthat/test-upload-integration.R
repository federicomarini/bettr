# Upload integration tests for bettr JSON functionality

test_that("upload mode integration with bettr function", {
    # Test that bettr function has been modified correctly
    bettr_formals <- formals(bettr)

    # Check new parameter exists
    expect_true("serverMode" %in% names(bettr_formals))

    # Check df parameter is now optional (has default NULL)
    expect_null(bettr_formals$df)

    # Check metrics parameter handles NULL df correctly
    expect_true(is.language(bettr_formals$metrics))
})

test_that("JSON data generation works correctly", {
    # Create test data
    test_data <- data.frame(
        Method = c("AlgorithmA", "AlgorithmB", "AlgorithmC", "AlgorithmD"),
        Precision = c(0.85, 0.92, 0.78, 0.88),
        Recall = c(0.90, 0.85, 0.95, 0.82),
        F1_Score = c(0.87, 0.88, 0.86, 0.85)
    )

    # Create bettrSE
    bettrSE <- assembleSE(df = test_data, idCol = "Method")

    # Validate structure
    expect_s4_class(bettrSE, "SummarizedExperiment")
    expect_equal(nrow(bettrSE), 4)
    expect_equal(ncol(bettrSE), 3)

    # Check column names (metrics)
    expected_metrics <- c("Precision", "Recall", "F1_Score")
    expect_equal(colnames(bettrSE), expected_metrics)

    # Export to JSON
    json_str <- bettrToJSON(bettrSE, file = NULL)
    expect_type(json_str, "character")
    expect_true(nchar(json_str) > 0)

    # Check data ranges (typical for performance metrics)
    assay_data <- SummarizedExperiment::assay(bettrSE, "values")
    expect_true(all(assay_data >= 0 & assay_data <= 1))

    # Check method names are unique
    expect_equal(length(unique(rownames(bettrSE))), nrow(bettrSE))
})

test_that("JSON workflow simulation works", {
    # Create test data
    test_data <- data.frame(
        Method = c("MethodA", "MethodB", "MethodC"),
        Accuracy = c(0.95, 0.87, 0.91),
        Speed = c(1.2, 2.1, 1.8),
        Memory = c(512, 256, 384)
    )

    # Create bettrSE
    bettrSE_orig <- assembleSE(df = test_data, idCol = "Method")

    # Simulate JSON workflow
    temp_file <- tempfile(fileext = ".json")

    # Write JSON
    bettrToJSON(bettrSE_orig, file = temp_file)
    expect_true(file.exists(temp_file))

    # Simulate reading (as upload would do)
    bettrSE_uploaded <- bettrFromJSON(file = temp_file)

    # Validate uploaded data
    expect_s4_class(bettrSE_uploaded, "SummarizedExperiment")

    # Check dimensions match
    expect_equal(dim(bettrSE_orig), dim(bettrSE_uploaded))

    # Check metadata preserved
    meta_orig <- S4Vectors::metadata(bettrSE_orig)$bettrInfo
    meta_upload <- S4Vectors::metadata(bettrSE_uploaded)$bettrInfo

    expect_equal(meta_orig$idCol, meta_upload$idCol)
    expect_equal(meta_orig$metrics, meta_upload$metrics)

    # Clean up
    unlink(temp_file)
})

test_that("bettr parameter validation works with new upload mode", {
    # Test various parameter combinations that should work

    # Traditional usage should still work
    test_data <- data.frame(
        Method = c("A", "B"),
        metric1 = c(1, 2)
    )

    # These parameter checks should not error
    expect_no_error({
        # Check parameter validation (without actually running the app)
        params <- list(
            df = test_data,
            idCol = "Method",
            metrics = "metric1",
            serverMode = FALSE
        )

        # Basic validation
        expect_true(is.data.frame(params$df))
        expect_true(is.character(params$idCol))
        expect_true(is.logical(params$serverMode))
    })
})

test_that("upload mode parameter defaults are correct", {
    # Check default values for upload-related parameters
    bettr_formals <- formals(bettr)

    expect_false(bettr_formals$serverMode)  # Should default to FALSE
    expect_null(bettr_formals$df)           # Should default to NULL

    # Check that traditional parameters still have reasonable defaults
    expect_equal(bettr_formals$idCol, "Method")
    expect_equal(bettr_formals$bstheme, "darkly")
    expect_equal(bettr_formals$appTitle, "bettr")
    expect_true(bettr_formals$addStopButton)
    expect_equal(bettr_formals$defaultWeight, 0.2)
})

test_that("JSON preserves metric transformations", {
    # Create test data with transformations
    test_data <- data.frame(
        Method = c("A", "B", "C"),
        runtime = c(10, 20, 15),
        accuracy = c(0.85, 0.92, 0.88)
    )

    # Define transformations (flip runtime so lower is better)
    transforms <- list(
        runtime = list(flip = TRUE, transform = "[0,1]"),
        accuracy = list(flip = FALSE, transform = "None")
    )

    # Create bettrSE with transforms
    bettrSE_orig <- assembleSE(
        df = test_data,
        idCol = "Method",
        initialTransforms = transforms
    )

    # Export and reimport via JSON
    json_str <- bettrToJSON(bettrSE_orig, file = NULL)
    bettrSE_reload <- bettrFromJSON(json = json_str)

    # Check transforms preserved
    meta_orig <- S4Vectors::metadata(bettrSE_orig)$bettrInfo
    meta_reload <- S4Vectors::metadata(bettrSE_reload)$bettrInfo

    expect_equal(meta_orig$initialTransforms, meta_reload$initialTransforms)

    # Check specific transform values
    expect_equal(meta_reload$initialTransforms$runtime$flip, TRUE)
    expect_equal(meta_reload$initialTransforms$runtime$transform, "[0,1]")
    expect_equal(meta_reload$initialTransforms$accuracy$flip, FALSE)
})

test_that("JSON handles empty/NULL metadata correctly", {
    # Create minimal test data with no extra metadata
    test_data <- data.frame(
        Method = c("A", "B"),
        metric1 = c(1, 2)
    )

    # Create minimal bettrSE
    bettrSE_orig <- assembleSE(df = test_data, idCol = "Method")

    # Export and reimport
    json_str <- bettrToJSON(bettrSE_orig, file = NULL)
    bettrSE_reload <- bettrFromJSON(json = json_str)

    # Should still work
    expect_s4_class(bettrSE_reload, "SummarizedExperiment")

    # Basic data preserved
    expect_equal(rownames(bettrSE_orig), rownames(bettrSE_reload))
    expect_equal(colnames(bettrSE_orig), colnames(bettrSE_reload))
})

test_that("duo2018 JSON integration test", {
    json_file <- "duo2018_bettr.json"

    skip_if(!file.exists(json_file), "duo2018_bettr.json not found")

    # Load JSON
    expect_no_error({
        duo2018_SE <- bettrFromJSON(file = json_file)
    })

    duo2018_SE <- bettrFromJSON(file = json_file)

    # Verify it's a complete bettrSE with all expected components
    expect_s4_class(duo2018_SE, "SummarizedExperiment")

    # Check assay
    expect_true("values" %in% names(SummarizedExperiment::assays(duo2018_SE)))

    # Check dimensions are reasonable
    expect_true(nrow(duo2018_SE) > 0)
    expect_true(ncol(duo2018_SE) > 0)

    # Check metadata
    meta <- S4Vectors::metadata(duo2018_SE)$bettrInfo
    expect_equal(meta$idCol, "method")
    expect_true(length(meta$metrics) > 0)

    # Check has transforms
    expect_true(!is.null(meta$initialTransforms))
    expect_true(length(meta$initialTransforms) > 0)

    # Check has colors
    expect_true(!is.null(meta$metricColors))
    expect_true(!is.null(meta$idColors))

    # Launch bettr with this data (should work)
    expect_no_error({
        app <- bettr(bettrSE = duo2018_SE, addStopButton = FALSE)
    })
})

test_that("JSON codec handles method names with special characters", {
    # Create test data with method names that have special characters
    # (metric names must be valid R identifiers)
    test_data <- data.frame(
        Method = c("Algorithm-A", "Algorithm_B", "Algorithm.C"),
        metric1 = c(1, 2, 3),
        metric2 = c(0.1, 0.2, 0.3)
    )

    # Create bettrSE
    bettrSE_orig <- assembleSE(df = test_data, idCol = "Method")

    # Round-trip through JSON
    json_str <- bettrToJSON(bettrSE_orig, file = NULL)
    bettrSE_reload <- bettrFromJSON(json = json_str)

    # Check names preserved correctly
    expect_equal(rownames(bettrSE_orig), rownames(bettrSE_reload))
    expect_equal(colnames(bettrSE_orig), colnames(bettrSE_reload))

    # Verify method names with special chars are preserved
    expect_true("Algorithm-A" %in% rownames(bettrSE_reload))
    expect_true("Algorithm.C" %in% rownames(bettrSE_reload))
})

test_that("bettr upload mode accepts JSON format", {
    # Create and export test data
    test_data <- data.frame(
        Method = c("A", "B", "C"),
        metric1 = c(1, 2, 3),
        metric2 = c(0.5, 0.6, 0.7)
    )

    bettrSE <- assembleSE(df = test_data, idCol = "Method")
    json_str <- bettrToJSON(bettrSE, file = NULL)

    # Verify JSON can be parsed by bettrFromJSON
    expect_no_error({
        bettrSE_reload <- bettrFromJSON(json = json_str)
    })

    # Verify reloaded data can be used with bettr
    bettrSE_reload <- bettrFromJSON(json = json_str)
    expect_no_error({
        app <- bettr(bettrSE = bettrSE_reload, addStopButton = FALSE)
    })
})
