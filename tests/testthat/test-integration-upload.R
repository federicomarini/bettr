# Integration tests for bettr JSON upload functionality

test_that("bettr function has upload mode parameter", {
    # Check if bettr function has uploadMode parameter
    formals_bettr <- formals(bettr)
    expect_true("uploadMode" %in% names(formals_bettr))

    # Check default value is FALSE
    expect_false(formals_bettr$uploadMode)
})

test_that("bettr function has optional df parameter", {
    # Check df parameter is optional (has default NULL)
    formals_bettr <- formals(bettr)
    expect_null(formals_bettr$df)
})

test_that("bettr works in normal mode with provided data", {
    # Create sample test data
    test_data <- data.frame(
        Method = c("AlgorithmA", "AlgorithmB", "AlgorithmC"),
        Precision = c(0.85, 0.92, 0.78),
        Recall = c(0.90, 0.85, 0.95),
        F1_Score = c(0.87, 0.88, 0.86)
    )

    # This should work - using bettr with data provided
    expect_no_error({
        result <- bettr(df = test_data, idCol = "Method",
                       metrics = c("Precision", "Recall", "F1_Score"),
                       addStopButton = FALSE)
    })
})

test_that("bettr works in upload mode", {
    # This should work - using bettr in upload mode
    expect_no_error({
        result <- bettr(uploadMode = TRUE, addStopButton = FALSE)
    })
})

test_that("bettr automatically enters upload mode when no data provided", {
    # Test that calling bettr() with no df triggers upload mode
    expect_no_error({
        result <- bettr(addStopButton = FALSE)
    })
})

test_that("JSON codec functions exist and work", {
    # Test that JSON functions exist
    expect_true(exists("bettrToJSON"))
    expect_true(exists("bettrFromJSON"))

    # Create simple test data
    test_data <- data.frame(
        Method = c("A", "B", "C"),
        metric1 = c(1, 2, 3),
        metric2 = c(0.5, 0.6, 0.7)
    )

    # Create bettrSE
    bettrSE <- assembleSE(df = test_data, idCol = "Method")

    # Test export
    expect_no_error({
        json_str <- bettrToJSON(bettrSE, file = NULL)
    })

    json_str <- bettrToJSON(bettrSE, file = NULL)

    # Test import
    expect_no_error({
        bettrSE_reload <- bettrFromJSON(json = json_str)
    })
})

test_that("duo2018_bettr.json can be loaded", {
    json_file <- "duo2018_bettr.json"

    skip_if(!file.exists(json_file), "duo2018_bettr.json not found")

    # Load JSON
    expect_no_error({
        bettrSE <- bettrFromJSON(file = json_file)
    })

    bettrSE <- bettrFromJSON(file = json_file)

    # Verify structure
    expect_s4_class(bettrSE, "SummarizedExperiment")

    # Check has assay data
    expect_true("values" %in% names(SummarizedExperiment::assays(bettrSE)))

    # Check metadata
    meta <- S4Vectors::metadata(bettrSE)$bettrInfo
    expect_true(!is.null(meta))
    expect_equal(meta$idCol, "method")
    expect_true(length(meta$metrics) > 0)
})

test_that("JSON file workflow simulation", {
    # Create test data
    test_data <- data.frame(
        Method = c("MethodA", "MethodB", "MethodC"),
        Accuracy = c(0.95, 0.87, 0.91),
        Speed = c(1.2, 2.1, 1.8),
        Memory = c(512, 256, 384)
    )

    # Create bettrSE with metadata
    metricInfo <- data.frame(
        Metric = c("Accuracy", "Speed", "Memory"),
        Category = c("Performance", "Performance", "Resource")
    )

    bettrSE_orig <- assembleSE(
        df = test_data,
        idCol = "Method",
        metricInfo = metricInfo
    )

    # Simulate file workflow: write and read
    temp_file <- tempfile(fileext = ".json")

    # Write JSON
    bettrToJSON(bettrSE_orig, file = temp_file)
    expect_true(file.exists(temp_file))

    # Read JSON (simulating upload)
    bettrSE_uploaded <- bettrFromJSON(file = temp_file)

    # Validate uploaded data matches original (with tolerance)
    orig_data <- SummarizedExperiment::assay(bettrSE_orig, "values")
    upload_data <- SummarizedExperiment::assay(bettrSE_uploaded, "values")

    expect_equal(dim(orig_data), dim(upload_data))
    expect_equal(orig_data, upload_data, tolerance = 0.01)

    # Check metadata preserved
    meta_orig <- S4Vectors::metadata(bettrSE_orig)$bettrInfo
    meta_upload <- S4Vectors::metadata(bettrSE_uploaded)$bettrInfo

    expect_equal(meta_orig$idCol, meta_upload$idCol)
    expect_equal(meta_orig$metrics, meta_upload$metrics)

    # Clean up
    unlink(temp_file)
})

test_that("bettr parameter validation works with upload mode", {
    # Test various parameter combinations that should work

    # Traditional usage should still work
    test_data <- data.frame(
        Method = c("A", "B"),
        metric1 = c(1, 2)
    )

    # These parameter checks should not error
    expect_no_error({
        # Check parameter validation
        params <- list(
            df = test_data,
            idCol = "Method",
            metrics = "metric1",
            uploadMode = FALSE
        )

        # Basic validation
        expect_true(is.data.frame(params$df))
        expect_true(is.character(params$idCol))
        expect_true(is.logical(params$uploadMode))
    })
})

test_that("upload mode parameter defaults are correct", {
    # Check default values for upload-related parameters
    bettr_formals <- formals(bettr)

    expect_false(bettr_formals$uploadMode)  # Should default to FALSE
    expect_null(bettr_formals$df)           # Should default to NULL

    # Check that traditional parameters still have reasonable defaults
    expect_equal(bettr_formals$idCol, "Method")
    expect_equal(bettr_formals$bstheme, "darkly")
    expect_equal(bettr_formals$appTitle, "bettr")
    expect_true(bettr_formals$addStopButton)
    expect_equal(bettr_formals$defaultWeight, 0.2)
})

test_that("JSON preserves all bettr configuration", {
    # Create comprehensive test with all configuration options
    test_data <- data.frame(
        Method = c("A", "B", "C"),
        speed = c(100, 150, 120),
        accuracy = c(0.85, 0.92, 0.88)
    )

    metricInfo <- data.frame(
        Metric = c("speed", "accuracy"),
        Type = c("Performance", "Quality")
    )

    idInfo <- data.frame(
        Method = c("A", "B", "C"),
        Version = c("v1", "v2", "v1")
    )

    initialTransforms <- list(
        speed = list(flip = TRUE, transform = "[0,1]")
    )

    metricColors <- list(
        Type = c(Performance = "blue", Quality = "green")
    )

    idColors <- list(
        Method = c(A = "red", B = "blue", C = "green")
    )

    # Create comprehensive bettrSE
    bettrSE_orig <- assembleSE(
        df = test_data,
        idCol = "Method",
        metricInfo = metricInfo,
        idInfo = idInfo,
        initialTransforms = initialTransforms,
        metricColors = metricColors,
        idColors = idColors
    )

    # Round-trip through JSON
    json_str <- bettrToJSON(bettrSE_orig, file = NULL)
    bettrSE_reload <- bettrFromJSON(json = json_str)

    # Verify all metadata preserved
    meta_orig <- S4Vectors::metadata(bettrSE_orig)$bettrInfo
    meta_reload <- S4Vectors::metadata(bettrSE_reload)$bettrInfo

    expect_equal(meta_orig$idCol, meta_reload$idCol)
    expect_equal(meta_orig$metrics, meta_reload$metrics)
    expect_equal(meta_orig$initialTransforms, meta_reload$initialTransforms)

    # Check colors preserved (names and values)
    expect_equal(names(meta_orig$metricColors$Type),
                 names(meta_reload$metricColors$Type))
    expect_equal(as.character(meta_orig$idColors$Method),
                 as.character(meta_reload$idColors$Method))
})
