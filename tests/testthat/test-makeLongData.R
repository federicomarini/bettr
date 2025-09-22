test_that("long data generation works", {
    df <- data.frame(Method = rep(c("A", "B", "C"), each = 3L),
                     Metric = rep(c("m1", "m2", "m3"), 3L),
                     Value = runif(n = 9L, min = 0.0, max = 3.0)) |>
        tidyr::pivot_wider(names_from = Metric, values_from = Value)
    metrics <- setdiff(colnames(df), "Method")
    metricInfo <- data.frame(Metric = c("m1", "m2", "m3"), num = c(1.0, 1.0, 2.0),
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
    expect_identical(dim(ld), c(9L, 3L))
    expect_named(ld, c("Method", "Metric", "ScaledValue"))
    expect_identical(ld$Method, rep(c("A", "B", "C"), each = 3L))
    expect_identical(ld$Metric, rep(c("m1", "m2", "m3"), 3L))
    expect_equal(ld$ScaledValue, unlist(c(df[1L, -1L], df[2L, -1L], df[3L, -1L])),
                 ignore_attr = TRUE)

    ## With grouping
    ld <- .makeLongData(df = df, idCol = "Method",
                        metrics = c("m1", "m2", "m3"),
                        metricCol = "Metric", valueCol = "ScaledValue",
                        metricGrouping = "num", metricInfo = metricInfo,
                        metricGroupCol = "metricGroup")
    expect_s3_class(ld, "data.frame")
    expect_identical(dim(ld), c(9L, 4L))
    expect_named(ld, c("Method", "Metric", "ScaledValue", "metricGroup"))
    expect_identical(ld$Method, rep(c("A", "B", "C"), each = 3L))
    expect_identical(ld$Metric, rep(c("m1", "m2", "m3"), 3L))
    expect_identical(ld$metricGroup, rep(c(1.0, 1.0, 2.0), 3L))
    expect_equal(ld$ScaledValue, unlist(c(df[1L, -1L], df[2L, -1L], df[3L, -1L])),
                 ignore_attr = TRUE)

    ## With grouping, don't keep all metrics
    ld <- .makeLongData(df = df, idCol = "Method",
                        metrics = c("m1", "m3"),
                        metricCol = "Metric", valueCol = "ScaledValue",
                        metricGrouping = "num", metricInfo = metricInfo,
                        metricGroupCol = "metricGroup")
    expect_s3_class(ld, "data.frame")
    expect_identical(dim(ld), c(6L, 4L))
    expect_named(ld, c("Method", "Metric", "ScaledValue", "metricGroup"))
    expect_identical(ld$Method, rep(c("A", "B", "C"), each = 2L))
    expect_identical(ld$Metric, rep(c("m1", "m3"), 3L))
    expect_identical(ld$metricGroup, rep(c(1.0, 2.0), 3L))
    expect_equal(ld$ScaledValue, unlist(c(df[1L, -c(1L, 3L)], df[2L, -c(1L, 3L)],
                                          df[3L, -c(1L, 3L)])),
                 ignore_attr = TRUE)
})

test_that("adding weights to long data works", {
    df <- data.frame(Method = rep(c("A", "B", "C"), each = 3L),
                     Metric = rep(c("m1", "m2", "m3"), 3L),
                     Value = runif(n = 9L, min = 0.0, max = 3.0)) |>
        tidyr::pivot_wider(names_from = Metric, values_from = Value)
    metrics <- setdiff(colnames(df), "Method")
    metricInfo <- data.frame(Metric = c("m1", "m2", "m3"), num = c(1.0, 1.0, 2.0),
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
    expect_identical(dim(ldw), c(9L, 4L))
    expect_named(ldw, c("Method", "Metric", "ScaledValue", "Weight"))
    expect_identical(ldw$Method, ld$Method)
    expect_identical(ldw$Metric, ld$Metric)
    expect_identical(ldw$ScaledValue, ld$ScaledValue)
    expect_identical(ldw$Weight, c(0.2, 0.3, 0.1, 0.2, 0.3, 0.1, 0.2, 0.3, 0.1))

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
    expect_identical(dim(ldw), c(9L, 5L))
    expect_named(ldw, c("Method", "Metric", "ScaledValue", "metricGroup", "Weight"))
    expect_identical(ldw$Method, ld$Method)
    expect_identical(ldw$Metric, ld$Metric)
    expect_identical(ldw$ScaledValue, ld$ScaledValue)
    expect_identical(ldw$metricGroup, ld$metricGroup)
    expect_identical(ldw$Weight, c(0.7, 0.7, 0.1, 0.7, 0.7, 0.1, 0.7, 0.7, 0.1))

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
    df <- data.frame(Method = rep(c("A", "B", "C"), each = 3L),
                     Metric = rep(c("m1", "m2", "m3"), 3L),
                     Value = runif(n = 9L, min = 0.0, max = 3.0)) |>
        tidyr::pivot_wider(names_from = Metric, values_from = Value)
    metrics <- setdiff(colnames(df), "Method")
    metricInfo <- data.frame(Metric = c("m1", "m2", "m3"), num = c(1.0, 1.0, 2.0),
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
    expect_identical(ldw, cld)

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
    expect_identical(dim(cld), c(6L, 5L))
    expect_named(cld, c("Method", "metricGroup", "ScaledValue", "Weight", "Metric"))
    expect_identical(cld$metricGroup, cld$Metric)
    expect_identical(cld$ScaledValue[cld$Method == "A" & cld$metricGroup == 1L],
                     mean(ldw$ScaledValue[ldw$Method == "A" & ldw$metricGroup == 1L]))
    expect_identical(cld$ScaledValue[cld$Method == "B" & cld$metricGroup == 1L],
                     mean(ldw$ScaledValue[ldw$Method == "B" & ldw$metricGroup == 1L]))
    expect_identical(cld$ScaledValue[cld$Method == "C" & cld$metricGroup == 1L],
                     mean(ldw$ScaledValue[ldw$Method == "C" & ldw$metricGroup == 1L]))
    expect_identical(cld$ScaledValue[cld$Method == "A" & cld$metricGroup == 2L],
                     mean(ldw$ScaledValue[ldw$Method == "A" & ldw$metricGroup == 2L]))
    expect_identical(cld$ScaledValue[cld$Method == "B" & cld$metricGroup == 2L],
                     mean(ldw$ScaledValue[ldw$Method == "B" & ldw$metricGroup == 2L]))
    expect_identical(cld$ScaledValue[cld$Method == "C" & cld$metricGroup == 2L],
                     mean(ldw$ScaledValue[ldw$Method == "C" & ldw$metricGroup == 2L]))
    expect_identical(cld$Weight, c(0.7, 0.1, 0.7, 0.1, 0.7, 0.1))

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
    expect_identical(dim(cld), c(6L, 5L))
    expect_named(cld, c("Method", "metricGroup", "ScaledValue", "Weight", "Metric"))
    expect_identical(cld$metricGroup, cld$Metric)
    expect_identical(cld$ScaledValue[cld$Method == "A" & cld$metricGroup == 1L],
                     max(ldw$ScaledValue[ldw$Method == "A" & ldw$metricGroup == 1L]))
    expect_identical(cld$ScaledValue[cld$Method == "B" & cld$metricGroup == 1L],
                     max(ldw$ScaledValue[ldw$Method == "B" & ldw$metricGroup == 1L]))
    expect_identical(cld$ScaledValue[cld$Method == "C" & cld$metricGroup == 1L],
                     max(ldw$ScaledValue[ldw$Method == "C" & ldw$metricGroup == 1L]))
    expect_identical(cld$ScaledValue[cld$Method == "A" & cld$metricGroup == 2L],
                     max(ldw$ScaledValue[ldw$Method == "A" & ldw$metricGroup == 2L]))
    expect_identical(cld$ScaledValue[cld$Method == "B" & cld$metricGroup == 2L],
                     max(ldw$ScaledValue[ldw$Method == "B" & ldw$metricGroup == 2L]))
    expect_identical(cld$ScaledValue[cld$Method == "C" & cld$metricGroup == 2L],
                     max(ldw$ScaledValue[ldw$Method == "C" & ldw$metricGroup == 2L]))
    expect_identical(cld$Weight, c(0.7, 0.1, 0.7, 0.1, 0.7, 0.1))

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
    expect_identical(dim(cld), c(6L, 5L))
    expect_named(cld, c("Method", "metricGroup", "ScaledValue", "Weight", "Metric"))
    expect_identical(cld$metricGroup, cld$Metric)
    expect_identical(cld$ScaledValue[cld$Method == "A" & cld$metricGroup == 1L],
                     min(ldw$ScaledValue[ldw$Method == "A" & ldw$metricGroup == 1L]))
    expect_identical(cld$ScaledValue[cld$Method == "B" & cld$metricGroup == 1L],
                     min(ldw$ScaledValue[ldw$Method == "B" & ldw$metricGroup == 1L]))
    expect_identical(cld$ScaledValue[cld$Method == "C" & cld$metricGroup == 1L],
                     min(ldw$ScaledValue[ldw$Method == "C" & ldw$metricGroup == 1L]))
    expect_identical(cld$ScaledValue[cld$Method == "A" & cld$metricGroup == 2L],
                     min(ldw$ScaledValue[ldw$Method == "A" & ldw$metricGroup == 2L]))
    expect_identical(cld$ScaledValue[cld$Method == "B" & cld$metricGroup == 2L],
                     min(ldw$ScaledValue[ldw$Method == "B" & ldw$metricGroup == 2L]))
    expect_identical(cld$ScaledValue[cld$Method == "C" & cld$metricGroup == 2L],
                     min(ldw$ScaledValue[ldw$Method == "C" & ldw$metricGroup == 2L]))
    expect_identical(cld$Weight, c(0.7, 0.1, 0.7, 0.1, 0.7, 0.1))

    ## With grouping - min collapse (with NAs)
    df0 <- df
    df0$m1[1L] <- df0$m2[1L] <- NA
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
    expect_warning(
        cld <- .collapseLongData(df = ldw, metricCollapseGroup = TRUE,
                                 metricGrouping = "num", idCol = "Method",
                                 metricGroupCol = "metricGroup",
                                 valueCol = "ScaledValue", weightCol = "Weight",
                                 metricCol = "Metric", collapseMethod = "min"),
        "no non-missing arguments to min"
    )
    expect_s3_class(cld, "data.frame")
    expect_identical(dim(cld), c(6L, 5L))
    expect_named(cld, c("Method", "metricGroup", "ScaledValue", "Weight", "Metric"))
    expect_identical(cld$metricGroup, cld$Metric)
    expect_identical(cld$ScaledValue[cld$Method == "A" & cld$metricGroup == 1L], NA_real_)
    expect_identical(cld$ScaledValue[cld$Method == "B" & cld$metricGroup == 1L],
                     min(ldw$ScaledValue[ldw$Method == "B" & ldw$metricGroup == 1L]))
    expect_identical(cld$ScaledValue[cld$Method == "C" & cld$metricGroup == 1L],
                     min(ldw$ScaledValue[ldw$Method == "C" & ldw$metricGroup == 1L]))
    expect_identical(cld$ScaledValue[cld$Method == "A" & cld$metricGroup == 2L],
                     min(ldw$ScaledValue[ldw$Method == "A" & ldw$metricGroup == 2L]))
    expect_identical(cld$ScaledValue[cld$Method == "B" & cld$metricGroup == 2L],
                     min(ldw$ScaledValue[ldw$Method == "B" & ldw$metricGroup == 2L]))
    expect_identical(cld$ScaledValue[cld$Method == "C" & cld$metricGroup == 2L],
                     min(ldw$ScaledValue[ldw$Method == "C" & ldw$metricGroup == 2L]))
    expect_identical(cld$Weight, c(0.7, 0.1, 0.7, 0.1, 0.7, 0.1))
})
