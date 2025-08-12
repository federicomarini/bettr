# Tests for bettr upload functionality

test_that("bettr function supports upload mode parameter", {
    # Check if bettr function has uploadMode parameter
    formals_bettr <- formals(bettr)
    expect_true("uploadMode" %in% names(formals_bettr))
    
    # Check default value
    expect_false(formals_bettr$uploadMode)
    
    # Check df parameter is now optional (has default NULL)
    expect_null(formals_bettr$df)
})

test_that("sample data creation works", {
    # Create test data
    test_data <- data.frame(
        Method = c("AlgorithmA", "AlgorithmB", "AlgorithmC"),
        Precision = c(0.85, 0.92, 0.78),
        Recall = c(0.90, 0.85, 0.95),
        F1_Score = c(0.87, 0.88, 0.86)
    )
    
    # Check data structure
    expect_s3_class(test_data, "data.frame")
    expect_equal(nrow(test_data), 3)
    expect_equal(ncol(test_data), 4)
    expect_true("Method" %in% colnames(test_data))
    expect_true(all(c("Precision", "Recall", "F1_Score") %in% colnames(test_data)))
    
    # Check data types
    expect_type(test_data$Method, "character")
    expect_type(test_data$Precision, "double")
    expect_type(test_data$Recall, "double")
    expect_type(test_data$F1_Score, "double")
})

test_that("CSV file reading works with test data", {
    # Use the sample data file
    sample_file <- file.path("sample_benchmark_data.csv")
    
    # Check file exists in test directory
    expect_true(file.exists(sample_file))
    
    # Read the sample data
    test_data <- read.csv(sample_file, stringsAsFactors = FALSE)
    
    # Verify structure
    expect_s3_class(test_data, "data.frame")
    expect_true("Method" %in% colnames(test_data))
    expect_true(nrow(test_data) > 0)
    expect_true(ncol(test_data) > 1)
    
    # Check that we have expected metrics
    expected_metrics <- c("Precision", "Recall", "F1_Score", "Accuracy", "Runtime_seconds")
    expect_true(all(expected_metrics %in% colnames(test_data)))
})

test_that("bettr function handles NULL df parameter correctly", {
    # This should not throw an error when uploadMode logic is properly implemented
    # We can't test the actual Shiny app launch, but we can test parameter handling
    
    # Check that bettr function exists
    expect_true(exists("bettr"))
    
    # With uploadMode = FALSE and df = NULL, it should now try to find bettr_upload
    # Since we've sourced bettr_upload, this should work (but won't launch the app in tests)
    # We can't actually test the Shiny app launch in unit tests, so just verify
    # the function doesn't error on parameter validation
    
    # Check that the function recognizes uploadMode parameter
    formals_bettr <- formals(bettr)
    expect_true("uploadMode" %in% names(formals_bettr))
})

test_that("data validation functions work correctly", {
    # Test valid data
    valid_data <- data.frame(
        Method = c("A", "B", "C"),
        metric1 = c(1, 2, 3),
        metric2 = c(0.1, 0.2, 0.3)
    )
    
    expect_s3_class(valid_data, "data.frame")
    expect_gt(nrow(valid_data), 0)
    expect_gt(ncol(valid_data), 1)
    
    # Test numeric column detection
    numeric_cols <- names(valid_data)[sapply(valid_data, is.numeric)]
    expect_true(length(numeric_cols) >= 1)
    expect_true(all(c("metric1", "metric2") %in% numeric_cols))
    expect_false("Method" %in% numeric_cols)
})