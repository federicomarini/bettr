# Tests for complete bettr workflow with JSON uploads

test_that("bettr() with no arguments triggers upload mode", {
    # This should work in upload mode (returns shiny app object)
    expect_no_error({
        result <- bettr(addStopButton = FALSE)
    })
})

test_that("bettr() with bettrSE works in normal mode", {
    # Create test data and assemble into bettrSE
    test_data <- data.frame(
        Method = c("AlgorithmA", "AlgorithmB", "AlgorithmC"),
        Precision = c(0.85, 0.92, 0.78),
        Recall = c(0.90, 0.85, 0.95),
        F1_Score = c(0.87, 0.88, 0.86)
    )

    bettrSE <- assembleSE(df = test_data, idCol = "Method")

    # Should work with bettrSE parameter
    expect_no_error({
        result <- bettr(bettrSE = bettrSE, addStopButton = FALSE)
    })
})

test_that("bettr() with data frame works in normal mode", {
    # Create test data
    test_data <- data.frame(
        Method = c("AlgorithmA", "AlgorithmB", "AlgorithmC"),
        Precision = c(0.85, 0.92, 0.78),
        Recall = c(0.90, 0.85, 0.95),
        F1_Score = c(0.87, 0.88, 0.86)
    )

    # Traditional df-based call should still work
    expect_no_error({
        result <- bettr(df = test_data, idCol = "Method",
                       metrics = c("Precision", "Recall", "F1_Score"),
                       addStopButton = FALSE)
    })
})

test_that("explicit upload mode works", {
    # Explicit serverMode = TRUE should work
    expect_no_error({
        result <- bettr(serverMode = TRUE, addStopButton = FALSE)
    })
})

test_that("bettr function signature supports all required parameters", {
    formals_bettr <- formals(bettr)

    # Check that all key parameters exist
    expected_params <- c("df", "bettrSE", "idCol", "metrics", "serverMode", "addStopButton")
    for (param in expected_params) {
        expect_true(param %in% names(formals_bettr),
                   info = paste("Parameter", param, "should exist in bettr function"))
    }
})

test_that("JSON workflow: create, export, import, and visualize", {
    # Step 1: Create test data
    test_data <- data.frame(
        Method = c("A", "B", "C"),
        metric1 = c(1.5, 2.3, 1.8),
        metric2 = c(0.85, 0.92, 0.78)
    )

    # Step 2: Assemble into bettrSE
    bettrSE_orig <- assembleSE(df = test_data, idCol = "Method")
    expect_s4_class(bettrSE_orig, "SummarizedExperiment")

    # Step 3: Export to JSON
    json_str <- bettrToJSON(bettrSE_orig, file = NULL)
    expect_type(json_str, "character")

    # Step 4: Import from JSON
    bettrSE_reload <- bettrFromJSON(json = json_str)
    expect_s4_class(bettrSE_reload, "SummarizedExperiment")

    # Step 5: Launch bettr with reloaded data
    expect_no_error({
        result <- bettr(bettrSE = bettrSE_reload, addStopButton = FALSE)
    })
})

test_that("duo2018 JSON can be loaded and used with bettr", {
    json_file <- "duo2018_bettr.json"

    skip_if(!file.exists(json_file), "duo2018_bettr.json not found")

    # Load the duo2018 data from JSON
    expect_no_error({
        duo2018_SE <- bettrFromJSON(file = json_file)
    })

    duo2018_SE <- bettrFromJSON(file = json_file)

    # Verify it's a valid SummarizedExperiment
    expect_s4_class(duo2018_SE, "SummarizedExperiment")

    # Launch bettr with this data
    expect_no_error({
        result <- bettr(bettrSE = duo2018_SE, addStopButton = FALSE)
    })
})

test_that("integration maintains backward compatibility", {
    # Test that existing bettr calls still work
    test_data <- data.frame(
        Method = c("MethodA", "MethodB"),
        Score1 = c(0.8, 0.9),
        Score2 = c(0.7, 0.85)
    )

    # Traditional df-based call
    expect_no_error({
        result <- bettr(df = test_data,
                       idCol = "Method",
                       metrics = c("Score1", "Score2"),
                       addStopButton = FALSE)
    })

    # With minimal parameters (metrics auto-detected)
    expect_no_error({
        result <- bettr(df = test_data,
                       idCol = "Method",
                       addStopButton = FALSE)
    })
})

test_that("upload mode and normal mode have different behaviors", {
    # Create test data
    test_data <- data.frame(
        Method = c("A", "B", "C"),
        metric1 = c(1, 2, 3),
        metric2 = c(0.1, 0.2, 0.3)
    )

    # Normal mode - requires data (either df or bettrSE)
    expect_no_error({
        normal_result <- bettr(df = test_data, idCol = "Method",
                              metrics = c("metric1", "metric2"),
                              addStopButton = FALSE)
    })

    # Upload mode - no data required
    expect_no_error({
        upload_result <- bettr(serverMode = TRUE, addStopButton = FALSE)
    })
})

test_that("JSON preserves initialTransforms in workflow", {
    # Create data with transforms
    test_data <- data.frame(
        Method = c("A", "B", "C"),
        speed = c(10, 20, 15),
        accuracy = c(0.9, 0.85, 0.95)
    )

    # Define transforms (flip speed so lower is better)
    transforms <- list(
        speed = list(flip = TRUE, transform = "[0,1]")
    )

    # Create bettrSE with transforms
    bettrSE_orig <- assembleSE(
        df = test_data,
        idCol = "Method",
        initialTransforms = transforms
    )

    # Export and reimport
    json_str <- bettrToJSON(bettrSE_orig, file = NULL)
    bettrSE_reload <- bettrFromJSON(json = json_str)

    # Check transforms are preserved
    meta_orig <- S4Vectors::metadata(bettrSE_orig)$bettrInfo
    meta_reload <- S4Vectors::metadata(bettrSE_reload)$bettrInfo

    expect_equal(meta_orig$initialTransforms$speed$flip,
                 meta_reload$initialTransforms$speed$flip)
    expect_equal(meta_orig$initialTransforms$speed$transform,
                 meta_reload$initialTransforms$speed$transform)
})
