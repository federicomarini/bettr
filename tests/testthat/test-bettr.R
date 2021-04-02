test_that("bettr stops with invalid inputs", {
    set.seed(123)
    df <- data.frame(Method = rep(c("A", "B", "C"), each = 3),
                     Metric = rep(c("m1", "m2", "m3"), 3),
                     Value = runif(n = 9, min = 0, max = 3)) %>%
        tidyr::spread(key = Metric, value = Value)
    
    ## df
    expect_error(bettr(df = as.matrix(df)), 
                 regexp = 'is(df, "data.frame") is not TRUE',
                 fixed = TRUE)
    
    ## methodCol
    expect_error(bettr(df = df, idCol = 1), 
                 regexp = 'is(idCol, "character") is not TRUE',
                 fixed = TRUE)
    expect_error(bettr(df = df, idCol = c("x", "y")), 
                 regexp = 'length(idCol) == 1 is not TRUE',
                 fixed = TRUE)
    expect_error(bettr(df = df, idCol = "Missing"), 
                 regexp = 'idCol %in% colnames(df) is not TRUE',
                 fixed = TRUE)
    
    ## metricCol
    expect_error(bettr(df = df, metrics_num = 1), 
                 regexp = 'all(metrics %in% colnames(df)) is not TRUE',
                 fixed = TRUE)
    expect_error(bettr(df = df, metrics_num = c("m1", "m2", "m3", "m4")), 
                 regexp = 'all(metrics %in% colnames(df)) is not TRUE',
                 fixed = TRUE)

    ## Initial weights
    expect_error(bettr(df = df, initialWeights = 1),
                 regexp = 'is.null(initialWeights)',
                 fixed = TRUE)
    expect_error(bettr(df = df, initialWeights = c(1, 2, 3)),
                 regexp = 'is.null(initialWeights)',
                 fixed = TRUE)
})

test_that("bettr runs with valid inputs", {
    ## Without initial weights
    set.seed(123)
    df <- data.frame(Method = rep(c("A", "B", "C"), each = 3),
                     Metric = rep(c("m1", "m2", "m3"), 3),
                     Value = runif(n = 9, min = 0, max = 3)) %>%
        tidyr::spread(key = Metric, value = Value)
    app <- bettr(df, idCol = "Method", metrics_num = c("m1", "m2", "m3"),
                 initialWeights = NULL)
    expect_s3_class(app, "shiny.appobj")
    
    ## With initial weights
    # app <- bettr(df, idCol = "Method", metrics = c("m1", "m2", "m3"),
    #              initialWeights = c(m1 = 1, m2 = 1, m3 = 2))
    # expect_s3_class(app, "shiny.appobj")
})