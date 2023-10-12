test_that("utils work", {
    expect_equal(.assignInitialWeights(weights = NULL, metrics = c("A", "B"), 
                                       initialWeightValue = 0.2,
                                       weightResolution = 0.05),
                 c(A = 0.2, B = 0.2))
    expect_equal(.assignInitialWeights(weights = c(A = 0.12345, B = 0.738189),
                                       metrics = c("A1", "B1"),
                                       initialWeightValue = 1,
                                       weightResolution = 0.05),
                 c(A = 0.10, B = 0.75))
    
    p1 <- .makeMetricSummaryPlot(x = rnorm(10))
    p1
    expect_s3_class(p1, "ggplot")
})
