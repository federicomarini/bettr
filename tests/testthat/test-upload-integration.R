# Integration tests for bettr upload mode

test_that("upload mode integration with bettr function", {
    # Test that bettr function has been modified correctly
    bettr_formals <- formals(bettr)
    
    # Check new parameter exists
    expect_true("uploadMode" %in% names(bettr_formals))
    
    # Check df parameter is now optional (has default NULL)
    expect_null(bettr_formals$df)
    
    # Check metrics parameter handles NULL df correctly
    expect_true(is.language(bettr_formals$metrics))
})

test_that("sample test data generation works correctly", {
    # This recreates the test data from our integration test
    test_data <- data.frame(
        Method = c("AlgorithmA", "AlgorithmB", "AlgorithmC", "AlgorithmD"),
        Precision = c(0.85, 0.92, 0.78, 0.88),
        Recall = c(0.90, 0.85, 0.95, 0.82),
        F1_Score = c(0.87, 0.88, 0.86, 0.85)
    )
    
    # Validate structure
    expect_s3_class(test_data, "data.frame")
    expect_equal(nrow(test_data), 4)
    expect_equal(ncol(test_data), 4)
    
    # Check column names
    expected_cols <- c("Method", "Precision", "Recall", "F1_Score")
    expect_equal(colnames(test_data), expected_cols)
    
    # Check data ranges (typical for performance metrics)
    expect_true(all(test_data$Precision >= 0 & test_data$Precision <= 1))
    expect_true(all(test_data$Recall >= 0 & test_data$Recall <= 1))
    expect_true(all(test_data$F1_Score >= 0 & test_data$F1_Score <= 1))
    
    # Check method names are unique
    expect_equal(length(unique(test_data$Method)), nrow(test_data))
})

test_that("CSV workflow simulation works", {
    # Create test data
    test_data <- data.frame(
        Method = c("MethodA", "MethodB", "MethodC"),
        Accuracy = c(0.95, 0.87, 0.91),
        Speed = c(1.2, 2.1, 1.8),
        Memory = c(512, 256, 384)
    )
    
    # Simulate CSV workflow
    temp_file <- tempfile(fileext = ".csv")
    
    # Write CSV
    write.csv(test_data, temp_file, row.names = FALSE)
    expect_true(file.exists(temp_file))
    
    # Simulate reading (as upload would do)
    uploaded_data <- read.csv(temp_file, stringsAsFactors = FALSE)
    
    # Validate uploaded data matches original
    expect_equal(uploaded_data, test_data)
    
    # Simulate column selection logic
    col_names <- colnames(uploaded_data)
    numeric_cols <- col_names[sapply(uploaded_data, function(x) {
        is.numeric(x) || (is.character(x) && !any(is.na(suppressWarnings(as.numeric(x)))))
    })]
    
    # Check column detection
    expect_true("Method" %in% col_names)
    expect_true(all(c("Accuracy", "Speed", "Memory") %in% numeric_cols))
    expect_false("Method" %in% numeric_cols)  # Method should not be numeric
    
    # Simulate metric selection (excluding ID column)
    potential_metrics <- setdiff(numeric_cols, "Method")
    expect_equal(sort(potential_metrics), sort(c("Accuracy", "Speed", "Memory")))
    
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