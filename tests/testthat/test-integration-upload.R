# Tests for integration of upload functionality in bettr

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

test_that("upload test data can be created and saved", {
    # Create sample test data for upload testing
    test_data <- data.frame(
        Method = c("AlgorithmA", "AlgorithmB", "AlgorithmC", "AlgorithmD", "AlgorithmE"),
        Precision = c(0.85, 0.92, 0.78, 0.88, 0.95),
        Recall = c(0.90, 0.85, 0.95, 0.82, 0.88),
        F1_Score = c(0.87, 0.88, 0.86, 0.85, 0.91),
        Accuracy = c(0.89, 0.91, 0.83, 0.87, 0.93),
        Runtime_seconds = c(12.5, 8.2, 15.7, 10.1, 9.8)
    )
    
    # Test data structure
    expect_s3_class(test_data, "data.frame")
    expect_equal(nrow(test_data), 5)
    expect_equal(ncol(test_data), 6)
    expect_true("Method" %in% colnames(test_data))
    expect_true(all(c("Precision", "Recall", "F1_Score", "Accuracy") %in% colnames(test_data)))
    
    # Test data types
    expect_type(test_data$Method, "character")
    expect_type(test_data$Precision, "double")
    expect_type(test_data$Recall, "double")
    expect_type(test_data$F1_Score, "double")
    
    # Save test data to CSV for upload testing
    csv_path <- file.path(tempdir(), "integration_test_data.csv")
    write.csv(test_data, csv_path, row.names = FALSE)
    expect_true(file.exists(csv_path))
    
    # Verify we can read it back
    read_data <- read.csv(csv_path, stringsAsFactors = FALSE)
    expect_equal(nrow(read_data), 5)
    expect_true("Method" %in% colnames(read_data))
})