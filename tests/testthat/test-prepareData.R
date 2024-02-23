test_that("prepareData works", {
    expect_error(.completeInitialization(transformList = 1, metrics = "m1"),
                 "'transformList' must be of class 'list'")
    
    df <- data.frame(Method = rep(c("A", "B", "C"), each = 3),
                     Metric = rep(c("m1", "m2", "m3"), 3),
                     Value = runif(n = 9, min = 0, max = 3)) |>
        tidyr::spread(key = Metric, value = Value)
    df$m3 <- paste0("V", df$m3)
    metrics <- setdiff(colnames(df), "Method")
    metricInfo <- data.frame(Metric = c("m1", "m2", "m3"), num = 1:3)
    idInfo <- data.frame(Method = c("A", "B", "C"), lets = letters[1:3])
    
    set.seed(123)
    pd <- .prepareData(df = df, idCol = "Method", metrics = c("m1", "m2", "m3"),
                       initialWeights = c(m1 = 0.1, m2 = 0.3, m3 = 0.5),
                       initialTransforms = list(), metricInfo = metricInfo, 
                       metricColors = list(), 
                       idInfo = idInfo, idColors = list(), 
                       weightResolution = 0.05, metricCol = "Metric", 
                       defaultWeightValue = 0.2)
    expect_type(pd, "list")
    expect_length(pd, 9L)
    expect_named(pd, c("metrics_num", "metrics_cat", "idColors", 
                       "metricColors", "initialTransforms", 
                       "metricsWithWeights", "initialWeights",
                       "idInfo", "metricInfo"))
    expect_equal(pd$metrics_num, c("m1", "m2"))
    expect_equal(pd$metrics_cat, c("m3"))
    expect_type(pd$idColors, "list")
    expect_equal(pd$idColors$lets, c(a = "lightgoldenrodyellow", 
                                     b = "mediumorchid1", c = "gray26"))
    expect_equal(pd$idColors$Method, c(A = "#F8766D", B = "#00BA38", C = "#619CFF"))
    expect_type(pd$metricColors, "list")
    expect_named(pd$metricColors, c("num", "Metric"))
    expect_type(pd$metricColors$num, "closure")
    expect_equal(pd$metricColors$Metric, c(m1 = "#F8766D", m2 = "#00BA38", m3 = "#619CFF"))
    expect_type(pd$initialTransforms, "list")
    expect_length(pd$initialTransforms, 2)
    expect_named(pd$initialTransforms, c("m1", "m2"))
    expect_type(pd$initialTransforms$m1, "list")
    expect_equal(pd$initialTransforms$m1, pd$initialTransforms$m2)
    expect_type(pd$initialTransforms$m1, "list")
    expect_length(pd$initialTransforms$m1, 4)
    expect_named(pd$initialTransforms$m1, c("offset", "flip", "cuts", "transform"))
    expect_true(is.character(pd$metricsWithWeights))
    expect_equal(pd$metricsWithWeights, c("m1", "m2", "m3", "Metric_m1", 
                                          "Metric_m2", "Metric_m3", 
                                          "num_1", "num_2", "num_3"))
    expect_equal(pd$initialWeights, c(m1 = 0.1, m2 = 0.3, m3 = 0.5,
                                      Metric_m1 = 0.2, Metric_m2 = 0.2,
                                      Metric_m3 = 0.2, num_1 = 0.2, 
                                      num_2 = 0.2, num_3 = 0.2))
    expect_equal(pd$idInfo, idInfo)
    expect_equal(pd$metricInfo, metricInfo)
    
    set.seed(345)
    pd <- .prepareData(df = df, idCol = "Method", metrics = c("m1", "m2", "m3"),
                       initialWeights = NULL,
                       initialTransforms = list(m1 = list(offset = 4),
                                                m2 = list(flip = TRUE)), 
                       metricInfo = metricInfo[, 1, drop = FALSE], 
                       metricColors = list(), 
                       idInfo = idInfo, 
                       idColors = list(Method = c(A = "blue", B = "green", C = "yellow")), 
                       weightResolution = 0.05, metricCol = "Metric", 
                       defaultWeightValue = 0.2)
    expect_type(pd, "list")
    expect_length(pd, 9L)
    expect_named(pd, c("metrics_num", "metrics_cat", "idColors", 
                       "metricColors", "initialTransforms", 
                       "metricsWithWeights", "initialWeights",
                       "idInfo", "metricInfo"))
    expect_equal(pd$metrics_num, c("m1", "m2"))
    expect_equal(pd$metrics_cat, c("m3"))
    expect_type(pd$idColors, "list")
    expect_equal(pd$idColors$lets, c(a = "slategray", b = "lightskyblue4", c = "grey79"))
    expect_equal(pd$idColors$Method, c(A = "blue", B = "green", C = "yellow"))
    expect_type(pd$metricColors, "list")
    expect_named(pd$metricColors, c("Metric"))
    expect_equal(pd$metricColors$Metric, c(m1 = "#F8766D", m2 = "#00BA38", m3 = "#619CFF"))
    expect_type(pd$initialTransforms, "list")
    expect_length(pd$initialTransforms, 2)
    expect_named(pd$initialTransforms, c("m1", "m2"))
    expect_type(pd$initialTransforms$m1, "list")
    expect_equal(pd$initialTransforms$m1$offset, 4)
    expect_equal(pd$initialTransforms$m1$flip, FALSE)
    expect_equal(pd$initialTransforms$m2$offset, 0)
    expect_equal(pd$initialTransforms$m2$flip, TRUE)
    expect_type(pd$initialTransforms$m1, "list")
    expect_length(pd$initialTransforms$m1, 4)
    expect_named(pd$initialTransforms$m1, c("offset", "flip", "cuts", "transform"))
    expect_true(is.character(pd$metricsWithWeights))
    expect_equal(pd$metricsWithWeights, c("m1", "m2", "m3"))
    expect_equal(pd$initialWeights, c(m1 = 0.2, m2 = 0.2, m3 = 0.2))
    expect_equal(pd$idInfo, idInfo)
    expect_null(pd$metricInfo)
    
    set.seed(678)
    pd <- .prepareData(df = df, idCol = "Method", metrics = c("m1", "m2", "m3"),
                       initialWeights = NULL,
                       initialTransforms = list(m1 = list(offset = 4),
                                                m2 = list(flip = TRUE)), 
                       metricInfo = metricInfo[, 1, drop = FALSE], 
                       metricColors = list(), 
                       idInfo = idInfo[, 1, drop = FALSE], 
                       idColors = list(Method = c(A = "blue", B = "green", C = "yellow")), 
                       weightResolution = 0.05, metricCol = "Metric", 
                       defaultWeightValue = 0.2)
    expect_type(pd, "list")
    expect_length(pd, 9L)
    expect_named(pd, c("metrics_num", "metrics_cat", "idColors", 
                       "metricColors", "initialTransforms", 
                       "metricsWithWeights", "initialWeights",
                       "idInfo", "metricInfo"))
    expect_equal(pd$metrics_num, c("m1", "m2"))
    expect_equal(pd$metrics_cat, c("m3"))
    expect_type(pd$idColors, "list")
    expect_equal(pd$idColors$Method, c(A = "blue", B = "green", C = "yellow"))
    expect_type(pd$metricColors, "list")
    expect_named(pd$metricColors, c("Metric"))
    expect_equal(pd$metricColors$Metric, c(m1 = "#F8766D", m2 = "#00BA38", m3 = "#619CFF"))
    expect_type(pd$initialTransforms, "list")
    expect_length(pd$initialTransforms, 2)
    expect_named(pd$initialTransforms, c("m1", "m2"))
    expect_type(pd$initialTransforms$m1, "list")
    expect_equal(pd$initialTransforms$m1$offset, 4)
    expect_equal(pd$initialTransforms$m1$flip, FALSE)
    expect_equal(pd$initialTransforms$m2$offset, 0)
    expect_equal(pd$initialTransforms$m2$flip, TRUE)
    expect_type(pd$initialTransforms$m1, "list")
    expect_length(pd$initialTransforms$m1, 4)
    expect_named(pd$initialTransforms$m1, c("offset", "flip", "cuts", "transform"))
    expect_true(is.character(pd$metricsWithWeights))
    expect_equal(pd$metricsWithWeights, c("m1", "m2", "m3"))
    expect_equal(pd$initialWeights, c(m1 = 0.2, m2 = 0.2, m3 = 0.2))
    expect_null(pd$idInfo)
    expect_null(pd$metricInfo)
})
