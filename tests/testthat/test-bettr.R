test_that("bettr stops with invalid inputs", {
    set.seed(123)
    df <- data.frame(Method = rep(c("A", "B", "C"), each = 3),
                     Metric = rep(c("m1", "m2", "m3"), 3),
                     Value = runif(n = 9, min = 0, max = 3))
    
    ## df
    expect_error(bettr(df = as.matrix(df)), 
                 regexp = "is\\(df, \"data.frame\"\\) is not TRUE")
    
    ## methodCol
    expect_error(bettr(df = df, methodCol = 1), 
                 regexp = "is\\(methodCol, \"character\"\\) is not TRUE")
    expect_error(bettr(df = df, methodCol = c("x", "y")), 
                 regexp = "length\\(methodCol\\) == 1 is not TRUE")
    expect_error(bettr(df = df, methodCol = "Missing"), 
                 regexp = "methodCol %in% colnames\\(df\\) is not TRUE")
    
    ## metricCol
    expect_error(bettr(df = df, metricCol = 1), 
                 regexp = "is\\(metricCol, \"character\"\\) is not TRUE")
    expect_error(bettr(df = df, metricCol = c("x", "y")), 
                 regexp = "length\\(metricCol\\) == 1 is not TRUE")
    expect_error(bettr(df = df, metricCol = "Missing"), 
                 regexp = "metricCol %in% colnames\\(df\\) is not TRUE")
    
    ## valueCol
    expect_error(bettr(df = df, valueCol = 1), 
                 regexp = "is\\(valueCol, \"character\"\\) is not TRUE")
    expect_error(bettr(df = df, valueCol = c("x", "y")), 
                 regexp = "length\\(valueCol\\) == 1 is not TRUE")
    expect_error(bettr(df = df, valueCol = "Missing"), 
                 regexp = "valueCol %in% colnames\\(df\\) is not TRUE")
    df$Value2 <- as.character(df$Value)
    expect_error(bettr(df = df, valueCol = "Value2"), 
                 regexp = "is\\(df\\[\\[valueCol\\]\\], \"numeric\"\\) is not TRUE")
    df$Value2 <- NULL
    
    ## Forbidden column names
    df$Score <- seq_len(nrow(df))
    expect_error(bettr(df = df), 
                 regexp = "!\\(\"Score\" %in% colnames\\(df\\)\\) is not TRUE")
    df$Score <- NULL; df$Weight <- seq_len(nrow(df))
    expect_error(bettr(df = df), 
                 regexp = "!\\(\"Weight\" %in% colnames\\(df\\)\\) is not TRUE")
    df$Weight <- NULL; df$ScaledValue <- seq_len(nrow(df))
    expect_error(bettr(df = df), 
                 regexp = "!\\(\"ScaledValue\" %in% colnames\\(df\\)\\) is not TRUE")
    df$ScaledValue <- NULL
    
    ## Initial weights
    expect_error(bettr(df = df, initialWeights = 1),
                 regexp = "is.null\\(initialWeights\\)")
    expect_error(bettr(df = df, initialWeights = c(1, 2, 3)),
                 regexp = "is.null\\(initialWeights\\)")
    expect_error(bettr(df = df, initialWeights = c(A = 1, B = 2, D = 3)),
                 regexp = "is.null\\(initialWeights\\)")
    expect_error(bettr(df = df, initialWeights = c(A = 0, B = 0, C = 0)),
                 regexp = "is.null\\(initialWeights\\)")
})

test_that("bettr runs with valid inputs", {
    ## Without initial weights
    set.seed(123)
    df <- data.frame(Method = rep(c("A", "B", "C"), each = 3),
                     Metric = rep(c("m1", "m2", "m3"), 3),
                     Value = runif(n = 9, min = 0, max = 3))
    app <- bettr(df, methodCol = "Method", metricCol = "Metric",
                 valueCol = "Value", initialWeights = NULL)
    expect_s3_class(app, "shiny.appobj")
    
    ## With initial weights
    app <- bettr(df, methodCol = "Method", metricCol = "Metric",
                 valueCol = "Value", initialWeights = c(m1 = 1, m2 = -1, m3 = 2))
    expect_s3_class(app, "shiny.appobj")
})