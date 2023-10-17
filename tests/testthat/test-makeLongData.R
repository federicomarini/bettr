test_that("long data generation works", {
    df <- data.frame(Method = rep(c("A", "B", "C"), each = 3),
                     Metric = rep(c("m1", "m2", "m3"), 3),
                     Value = runif(n = 9, min = 0, max = 3)) %>%
        tidyr::pivot_wider(names_from = Metric, values_from = Value)
    metrics <- setdiff(colnames(df), "Method")
    metricInfo <- data.frame(Metric = c("m1", "m2", "m3"), num = c(1, 1, 2),
                             lets3 = c("m", "n", "o"))
    idInfo <- data.frame(Method = c("A", "B", "C"), lets = c("a", "b", "b"),
                         lets2 = c("d", "d", "e"))
    
    ## Without grouping
    ld <- .makeLongData(df = df, idCol = "Method", 
                        metrics = c("m1", "m2", "m3"), 
                        metricCol = "Metric", valueCol = "ScaledValue",
                        metricGrouping = "---", metricInfo = metricInfo, 
                        metricGroupCol = "metricGroup")
    expect_s3_class(ld, "data.frame")
    expect_equal(dim(ld), c(9, 3))
    expect_named(ld, c("Method", "Metric", "ScaledValue"))
    expect_equal(ld$Method, rep(c("A", "B", "C"), each = 3))
    expect_equal(ld$Metric, rep(c("m1", "m2", "m3"), 3))
    expect_equal(ld$ScaledValue, unlist(c(df[1, -1], df[2, -1], df[3, -1])),
                 ignore_attr = TRUE)
    
    ## With grouping
    ld <- .makeLongData(df = df, idCol = "Method", 
                        metrics = c("m1", "m2", "m3"), 
                        metricCol = "Metric", valueCol = "ScaledValue",
                        metricGrouping = "num", metricInfo = metricInfo, 
                        metricGroupCol = "metricGroup")
    expect_s3_class(ld, "data.frame")
    expect_equal(dim(ld), c(9, 4))
    expect_named(ld, c("Method", "Metric", "ScaledValue", "metricGroup"))
    expect_equal(ld$Method, rep(c("A", "B", "C"), each = 3))
    expect_equal(ld$Metric, rep(c("m1", "m2", "m3"), 3))
    expect_equal(ld$metricGroup, rep(c(1, 1, 2), 3))
    expect_equal(ld$ScaledValue, unlist(c(df[1, -1], df[2, -1], df[3, -1])),
                 ignore_attr = TRUE)
    
    ## With grouping, don't keep all metrics
    ld <- .makeLongData(df = df, idCol = "Method", 
                        metrics = c("m1", "m3"), 
                        metricCol = "Metric", valueCol = "ScaledValue",
                        metricGrouping = "num", metricInfo = metricInfo, 
                        metricGroupCol = "metricGroup")
    expect_s3_class(ld, "data.frame")
    expect_equal(dim(ld), c(6, 4))
    expect_named(ld, c("Method", "Metric", "ScaledValue", "metricGroup"))
    expect_equal(ld$Method, rep(c("A", "B", "C"), each = 2))
    expect_equal(ld$Metric, rep(c("m1", "m3"), 3))
    expect_equal(ld$metricGroup, rep(c(1, 2), 3))
    expect_equal(ld$ScaledValue, unlist(c(df[1, -c(1, 3)], df[2, -c(1, 3)], df[3, -c(1, 3)])),
                 ignore_attr = TRUE)
})

test_that("adding weights to long data works", {
    df <- data.frame(Method = rep(c("A", "B", "C"), each = 3),
                     Metric = rep(c("m1", "m2", "m3"), 3),
                     Value = runif(n = 9, min = 0, max = 3)) %>%
        tidyr::pivot_wider(names_from = Metric, values_from = Value)
    metrics <- setdiff(colnames(df), "Method")
    metricInfo <- data.frame(Metric = c("m1", "m2", "m3"), num = c(1, 1, 2),
                             lets3 = c("m", "n", "o"))
    idInfo <- data.frame(Method = c("A", "B", "C"), lets = c("a", "b", "b"),
                         lets2 = c("d", "d", "e"))
    
    ## Without grouping
    ld <- .makeLongData(df = df, idCol = "Method", 
                        metrics = c("m1", "m2", "m3"), 
                        metricCol = "Metric", valueCol = "ScaledValue",
                        metricGrouping = "---", metricInfo = metricInfo, 
                        metricGroupCol = "metricGroup")
    ldw <- .addWeightsToLongData(df = ld, metricCollapseGroup = FALSE, 
                                 metricGrouping = "---",
                                 metricGroupCol = "metricGroup", 
                                 weights = list(m1_weight = 0.2, m2_weight = 0.3, m3_weight = 0.1,
                                                Metric_m1_weight = 0.1, Metric_m2_weight = 0.5,
                                                Metric_m3_weight = 0.7, num_1_weight = 0.7, 
                                                num_2_weight = 0.1, lets3_m_weight = 0.4, 
                                                lets3_n_weight = 0.1, lets3_o_weight = 0.9),
                                 weightCol = "Weight", metricCol = "Metric",
                                 metrics = c("m1", "m2", "m3"))
    expect_s3_class(ldw, "data.frame")
    expect_equal(dim(ldw), c(9, 4))
    expect_named(ldw, c("Method", "Metric", "ScaledValue", "Weight"))
    expect_equal(ldw$Method, ld$Method)
    expect_equal(ldw$Metric, ld$Metric)
    expect_equal(ldw$ScaledValue, ld$ScaledValue)
    expect_equal(ldw$Weight, c(0.2, 0.3, 0.1, 0.2, 0.3, 0.1, 0.2, 0.3, 0.1))
    
    ## With grouping
    ld <- .makeLongData(df = df, idCol = "Method", 
                        metrics = c("m1", "m2", "m3"), 
                        metricCol = "Metric", valueCol = "ScaledValue",
                        metricGrouping = "num", metricInfo = metricInfo, 
                        metricGroupCol = "metricGroup")
    ldw <- .addWeightsToLongData(df = ld, metricCollapseGroup = TRUE, 
                                 metricGrouping = "num",
                                 metricGroupCol = "metricGroup", 
                                 weights = list(m1_weight = 0.2, m2_weight = 0.3, m3_weight = 0.1,
                                                Metric_m1_weight = 0.1, Metric_m2_weight = 0.5,
                                                Metric_m3_weight = 0.7, num_1_weight = 0.7, 
                                                num_2_weight = 0.1, lets3_m_weight = 0.4, 
                                                lets3_n_weight = 0.1, lets3_o_weight = 0.9),
                                 weightCol = "Weight", metricCol = "Metric",
                                 metrics = c("m1", "m2", "m3"))
    expect_s3_class(ldw, "data.frame")
    expect_equal(dim(ldw), c(9, 5))
    expect_named(ldw, c("Method", "Metric", "ScaledValue", "metricGroup", "Weight"))
    expect_equal(ldw$Method, ld$Method)
    expect_equal(ldw$Metric, ld$Metric)
    expect_equal(ldw$ScaledValue, ld$ScaledValue)
    expect_equal(ldw$metricGroup, ld$metricGroup)
    expect_equal(ldw$Weight, c(0.7, 0.7, 0.1, 0.7, 0.7, 0.1, 0.7, 0.7, 0.1))
    
    ## Missing weights -> return NULL
    ldw <- .addWeightsToLongData(df = ld, metricCollapseGroup = TRUE, 
                                 metricGrouping = "num",
                                 metricGroupCol = "metricGroup", 
                                 weights = list(m1_weight = 0.2, m2_weight = 0.3, m3_weight = 0.1,
                                                Metric_m1_weight = 0.1, Metric_m2_weight = 0.5,
                                                Metric_m3_weight = 0.7, 
                                                num_2_weight = 0.1, lets3_m_weight = 0.4, 
                                                lets3_n_weight = 0.1, lets3_o_weight = 0.9),
                                 weightCol = "Weight", metricCol = "Metric",
                                 metrics = c("m1", "m2", "m3"))
    expect_null(ldw)
})

test_that("collapsing long data works", {
    df <- data.frame(Method = rep(c("A", "B", "C"), each = 3),
                     Metric = rep(c("m1", "m2", "m3"), 3),
                     Value = runif(n = 9, min = 0, max = 3)) %>%
        tidyr::pivot_wider(names_from = Metric, values_from = Value)
    metrics <- setdiff(colnames(df), "Method")
    metricInfo <- data.frame(Metric = c("m1", "m2", "m3"), num = c(1, 1, 2),
                             lets3 = c("m", "n", "o"))
    idInfo <- data.frame(Method = c("A", "B", "C"), lets = c("a", "b", "b"),
                         lets2 = c("d", "d", "e"))
    
    ## Without grouping
    ld <- .makeLongData(df = df, idCol = "Method", 
                        metrics = c("m1", "m2", "m3"), 
                        metricCol = "Metric", valueCol = "ScaledValue",
                        metricGrouping = "---", metricInfo = metricInfo, 
                        metricGroupCol = "metricGroup")
    ldw <- .addWeightsToLongData(df = ld, metricCollapseGroup = FALSE, 
                                 metricGrouping = "---",
                                 metricGroupCol = "metricGroup", 
                                 weights = list(m1_weight = 0.2, m2_weight = 0.3, m3_weight = 0.1,
                                                Metric_m1_weight = 0.1, Metric_m2_weight = 0.5,
                                                Metric_m3_weight = 0.7, num_1_weight = 0.7, 
                                                num_2_weight = 0.1, lets3_m_weight = 0.4, 
                                                lets3_n_weight = 0.1, lets3_o_weight = 0.9),
                                 weightCol = "Weight", metricCol = "Metric",
                                 metrics = c("m1", "m2", "m3"))
    cld <- .collapseLongData(df = ldw, metricCollapseGroup = FALSE, 
                             metricGrouping = "---", idCol = "Method",
                             metricGroupCol = "metricGroup", 
                             valueCol = "ScaledValue", weightCol = "Weight", 
                             metricCol = "Metric", collapseMethod = "mean")
    expect_equal(ldw, cld)
    
    ## With grouping
    ld <- .makeLongData(df = df, idCol = "Method", 
                        metrics = c("m1", "m2", "m3"), 
                        metricCol = "Metric", valueCol = "ScaledValue",
                        metricGrouping = "num", metricInfo = metricInfo, 
                        metricGroupCol = "metricGroup")
    ldw <- .addWeightsToLongData(df = ld, metricCollapseGroup = TRUE, 
                                 metricGrouping = "num",
                                 metricGroupCol = "metricGroup", 
                                 weights = list(m1_weight = 0.2, m2_weight = 0.3, m3_weight = 0.1,
                                                Metric_m1_weight = 0.1, Metric_m2_weight = 0.5,
                                                Metric_m3_weight = 0.7, num_1_weight = 0.7, 
                                                num_2_weight = 0.1, lets3_m_weight = 0.4, 
                                                lets3_n_weight = 0.1, lets3_o_weight = 0.9),
                                 weightCol = "Weight", metricCol = "Metric",
                                 metrics = c("m1", "m2", "m3"))
    cld <- .collapseLongData(df = ldw, metricCollapseGroup = TRUE, 
                             metricGrouping = "num", idCol = "Method",
                             metricGroupCol = "metricGroup", 
                             valueCol = "ScaledValue", weightCol = "Weight", 
                             metricCol = "Metric", collapseMethod = "mean")
    expect_s3_class(cld, "data.frame")
    expect_equal(dim(cld), c(6, 5))
    expect_named(cld, c("Method", "metricGroup", "ScaledValue", "Weight", "Metric"))
    expect_equal(cld$metricGroup, cld$Metric)
    expect_equal(cld$ScaledValue[cld$Method == "A" & cld$metricGroup == 1], 
                 mean(ldw$ScaledValue[ldw$Method == "A" & ldw$metricGroup == 1]))
    expect_equal(cld$ScaledValue[cld$Method == "B" & cld$metricGroup == 1], 
                 mean(ldw$ScaledValue[ldw$Method == "B" & ldw$metricGroup == 1]))
    expect_equal(cld$ScaledValue[cld$Method == "C" & cld$metricGroup == 1], 
                 mean(ldw$ScaledValue[ldw$Method == "C" & ldw$metricGroup == 1]))
    expect_equal(cld$ScaledValue[cld$Method == "A" & cld$metricGroup == 2], 
                 mean(ldw$ScaledValue[ldw$Method == "A" & ldw$metricGroup == 2]))
    expect_equal(cld$ScaledValue[cld$Method == "B" & cld$metricGroup == 2], 
                 mean(ldw$ScaledValue[ldw$Method == "B" & ldw$metricGroup == 2]))
    expect_equal(cld$ScaledValue[cld$Method == "C" & cld$metricGroup == 2], 
                 mean(ldw$ScaledValue[ldw$Method == "C" & ldw$metricGroup == 2]))
    expect_equal(cld$Weight, c(0.7, 0.1, 0.7, 0.1, 0.7, 0.1))
    
    ## With grouping - max collapse
    ld <- .makeLongData(df = df, idCol = "Method", 
                        metrics = c("m1", "m2", "m3"), 
                        metricCol = "Metric", valueCol = "ScaledValue",
                        metricGrouping = "num", metricInfo = metricInfo, 
                        metricGroupCol = "metricGroup")
    ldw <- .addWeightsToLongData(df = ld, metricCollapseGroup = TRUE, 
                                 metricGrouping = "num",
                                 metricGroupCol = "metricGroup", 
                                 weights = list(m1_weight = 0.2, m2_weight = 0.3, m3_weight = 0.1,
                                                Metric_m1_weight = 0.1, Metric_m2_weight = 0.5,
                                                Metric_m3_weight = 0.7, num_1_weight = 0.7, 
                                                num_2_weight = 0.1, lets3_m_weight = 0.4, 
                                                lets3_n_weight = 0.1, lets3_o_weight = 0.9),
                                 weightCol = "Weight", metricCol = "Metric",
                                 metrics = c("m1", "m2", "m3"))
    cld <- .collapseLongData(df = ldw, metricCollapseGroup = TRUE, 
                             metricGrouping = "num", idCol = "Method",
                             metricGroupCol = "metricGroup", 
                             valueCol = "ScaledValue", weightCol = "Weight", 
                             metricCol = "Metric", collapseMethod = "max")
    expect_s3_class(cld, "data.frame")
    expect_equal(dim(cld), c(6, 5))
    expect_named(cld, c("Method", "metricGroup", "ScaledValue", "Weight", "Metric"))
    expect_equal(cld$metricGroup, cld$Metric)
    expect_equal(cld$ScaledValue[cld$Method == "A" & cld$metricGroup == 1], 
                 max(ldw$ScaledValue[ldw$Method == "A" & ldw$metricGroup == 1]))
    expect_equal(cld$ScaledValue[cld$Method == "B" & cld$metricGroup == 1], 
                 max(ldw$ScaledValue[ldw$Method == "B" & ldw$metricGroup == 1]))
    expect_equal(cld$ScaledValue[cld$Method == "C" & cld$metricGroup == 1], 
                 max(ldw$ScaledValue[ldw$Method == "C" & ldw$metricGroup == 1]))
    expect_equal(cld$ScaledValue[cld$Method == "A" & cld$metricGroup == 2], 
                 max(ldw$ScaledValue[ldw$Method == "A" & ldw$metricGroup == 2]))
    expect_equal(cld$ScaledValue[cld$Method == "B" & cld$metricGroup == 2], 
                 max(ldw$ScaledValue[ldw$Method == "B" & ldw$metricGroup == 2]))
    expect_equal(cld$ScaledValue[cld$Method == "C" & cld$metricGroup == 2], 
                 max(ldw$ScaledValue[ldw$Method == "C" & ldw$metricGroup == 2]))
    expect_equal(cld$Weight, c(0.7, 0.1, 0.7, 0.1, 0.7, 0.1))
    
    ## With grouping - min collapse
    ld <- .makeLongData(df = df, idCol = "Method", 
                        metrics = c("m1", "m2", "m3"), 
                        metricCol = "Metric", valueCol = "ScaledValue",
                        metricGrouping = "num", metricInfo = metricInfo, 
                        metricGroupCol = "metricGroup")
    ldw <- .addWeightsToLongData(df = ld, metricCollapseGroup = TRUE, 
                                 metricGrouping = "num",
                                 metricGroupCol = "metricGroup", 
                                 weights = list(m1_weight = 0.2, m2_weight = 0.3, m3_weight = 0.1,
                                                Metric_m1_weight = 0.1, Metric_m2_weight = 0.5,
                                                Metric_m3_weight = 0.7, num_1_weight = 0.7, 
                                                num_2_weight = 0.1, lets3_m_weight = 0.4, 
                                                lets3_n_weight = 0.1, lets3_o_weight = 0.9),
                                 weightCol = "Weight", metricCol = "Metric",
                                 metrics = c("m1", "m2", "m3"))
    cld <- .collapseLongData(df = ldw, metricCollapseGroup = TRUE, 
                             metricGrouping = "num", idCol = "Method",
                             metricGroupCol = "metricGroup", 
                             valueCol = "ScaledValue", weightCol = "Weight", 
                             metricCol = "Metric", collapseMethod = "min")
    expect_s3_class(cld, "data.frame")
    expect_equal(dim(cld), c(6, 5))
    expect_named(cld, c("Method", "metricGroup", "ScaledValue", "Weight", "Metric"))
    expect_equal(cld$metricGroup, cld$Metric)
    expect_equal(cld$ScaledValue[cld$Method == "A" & cld$metricGroup == 1], 
                 min(ldw$ScaledValue[ldw$Method == "A" & ldw$metricGroup == 1]))
    expect_equal(cld$ScaledValue[cld$Method == "B" & cld$metricGroup == 1], 
                 min(ldw$ScaledValue[ldw$Method == "B" & ldw$metricGroup == 1]))
    expect_equal(cld$ScaledValue[cld$Method == "C" & cld$metricGroup == 1], 
                 min(ldw$ScaledValue[ldw$Method == "C" & ldw$metricGroup == 1]))
    expect_equal(cld$ScaledValue[cld$Method == "A" & cld$metricGroup == 2], 
                 min(ldw$ScaledValue[ldw$Method == "A" & ldw$metricGroup == 2]))
    expect_equal(cld$ScaledValue[cld$Method == "B" & cld$metricGroup == 2], 
                 min(ldw$ScaledValue[ldw$Method == "B" & ldw$metricGroup == 2]))
    expect_equal(cld$ScaledValue[cld$Method == "C" & cld$metricGroup == 2], 
                 min(ldw$ScaledValue[ldw$Method == "C" & ldw$metricGroup == 2]))
    expect_equal(cld$Weight, c(0.7, 0.1, 0.7, 0.1, 0.7, 0.1))
    
    ## With grouping - min collapse (with NAs)
    df0 <- df
    df0$m1[1] <- df0$m2[1] <- NA
    ld <- .makeLongData(df = df0, idCol = "Method", 
                        metrics = c("m1", "m2", "m3"), 
                        metricCol = "Metric", valueCol = "ScaledValue",
                        metricGrouping = "num", metricInfo = metricInfo, 
                        metricGroupCol = "metricGroup")
    ldw <- .addWeightsToLongData(df = ld, metricCollapseGroup = TRUE, 
                                 metricGrouping = "num",
                                 metricGroupCol = "metricGroup", 
                                 weights = list(m1_weight = 0.2, m2_weight = 0.3, m3_weight = 0.1,
                                                Metric_m1_weight = 0.1, Metric_m2_weight = 0.5,
                                                Metric_m3_weight = 0.7, num_1_weight = 0.7, 
                                                num_2_weight = 0.1, lets3_m_weight = 0.4, 
                                                lets3_n_weight = 0.1, lets3_o_weight = 0.9),
                                 weightCol = "Weight", metricCol = "Metric",
                                 metrics = c("m1", "m2", "m3"))
    cld <- .collapseLongData(df = ldw, metricCollapseGroup = TRUE, 
                             metricGrouping = "num", idCol = "Method",
                             metricGroupCol = "metricGroup", 
                             valueCol = "ScaledValue", weightCol = "Weight", 
                             metricCol = "Metric", collapseMethod = "min")
    expect_s3_class(cld, "data.frame")
    expect_equal(dim(cld), c(6, 5))
    expect_named(cld, c("Method", "metricGroup", "ScaledValue", "Weight", "Metric"))
    expect_equal(cld$metricGroup, cld$Metric)
    expect_equal(cld$ScaledValue[cld$Method == "A" & cld$metricGroup == 1], NA_real_)
    expect_equal(cld$ScaledValue[cld$Method == "B" & cld$metricGroup == 1], 
                 min(ldw$ScaledValue[ldw$Method == "B" & ldw$metricGroup == 1]))
    expect_equal(cld$ScaledValue[cld$Method == "C" & cld$metricGroup == 1], 
                 min(ldw$ScaledValue[ldw$Method == "C" & ldw$metricGroup == 1]))
    expect_equal(cld$ScaledValue[cld$Method == "A" & cld$metricGroup == 2], 
                 min(ldw$ScaledValue[ldw$Method == "A" & ldw$metricGroup == 2]))
    expect_equal(cld$ScaledValue[cld$Method == "B" & cld$metricGroup == 2], 
                 min(ldw$ScaledValue[ldw$Method == "B" & ldw$metricGroup == 2]))
    expect_equal(cld$ScaledValue[cld$Method == "C" & cld$metricGroup == 2], 
                 min(ldw$ScaledValue[ldw$Method == "C" & ldw$metricGroup == 2]))
    expect_equal(cld$Weight, c(0.7, 0.1, 0.7, 0.1, 0.7, 0.1))
})
