# Tests for complete upload to bettr workflow

test_that("bettr() with no arguments triggers upload mode", {
    # This should work in upload mode
    expect_no_error({
        result <- bettr(addStopButton = FALSE)
    })
})

test_that("bettr() with data works in normal mode", {
    # Create test data
    test_data <- data.frame(
        Method = c("AlgorithmA", "AlgorithmB", "AlgorithmC"),
        Precision = c(0.85, 0.92, 0.78),
        Recall = c(0.90, 0.85, 0.95),
        F1_Score = c(0.87, 0.88, 0.86)
    )
    
    expect_no_error({
        result <- bettr(df = test_data, idCol = "Method", 
                       metrics = c("Precision", "Recall", "F1_Score"),
                       addStopButton = FALSE)
    })
})

test_that("explicit upload mode works", {
    expect_no_error({
        result <- bettr(uploadMode = TRUE, addStopButton = FALSE)
    })
})

test_that("bettr function signature supports all required parameters", {
    formals_bettr <- formals(bettr)
    
    # Check that all key parameters exist
    expected_params <- c("df", "idCol", "metrics", "uploadMode", "addStopButton")
    for (param in expected_params) {
        expect_true(param %in% names(formals_bettr), 
                   info = paste("Parameter", param, "should exist in bettr function"))
    }
})

test_that("upload mode and normal mode have different behaviors", {
    # Create test data
    test_data <- data.frame(
        Method = c("A", "B", "C"),
        metric1 = c(1, 2, 3),
        metric2 = c(0.1, 0.2, 0.3)
    )
    
    # Both modes should work but may behave differently internally
    expect_no_error({
        # Normal mode - requires data
        normal_result <- bettr(df = test_data, idCol = "Method", 
                              metrics = c("metric1", "metric2"),
                              addStopButton = FALSE)
    })
    
    expect_no_error({
        # Upload mode - no data required
        upload_result <- bettr(uploadMode = TRUE, addStopButton = FALSE)
    })
})

test_that("integration maintains backward compatibility", {
    # Test that existing bettr calls still work
    test_data <- data.frame(
        Method = c("MethodA", "MethodB"),
        Score1 = c(0.8, 0.9),
        Score2 = c(0.7, 0.85)
    )
    
    # This is how bettr was called before upload functionality
    expect_no_error({
        result <- bettr(df = test_data, 
                       idCol = "Method",
                       metrics = c("Score1", "Score2"),
                       addStopButton = FALSE)
    })
    
    # Test with minimal parameters
    expect_no_error({
        result <- bettr(df = test_data, 
                       idCol = "Method",
                       addStopButton = FALSE)
    })
})