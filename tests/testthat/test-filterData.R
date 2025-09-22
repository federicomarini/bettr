test_that("filterData works", {
    df <- data.frame(Method = rep(c("A", "B", "C"), each = 3L),
                     Metric = rep(c("m1", "m2", "m3"), 3L),
                     Value = runif(n = 9L, min = 0.0, max = 3.0)) |>
        tidyr::spread(key = Metric, value = Value)
    metrics <- setdiff(colnames(df), "Method")
    metricInfo <- data.frame(Metric = c("m1", "m2", "m3"), num = c(1.0, 1.0, 2.0),
                             lets3 = c("m", "n", "o"))
    idInfo <- data.frame(Method = c("A", "B", "C"), lets = c("a", "b", "b"),
                         lets2 = c("d", "d", "e"))

    ## No filtering
    keepIds <- c("A", "B", "C")
    keepIdsBy <- list(lets = c("a", "b"), lets2 = c("d", "e"))
    keepMetrics <- c("m1", "m2", "m3")
    keepMetricsBy <- list(num = c(1.0, 2.0), lets3 = c("m", "n", "o"))
    fd <- .filterData(df = df, idInfo = idInfo, idCol = "Method",
                      keepIds = keepIds, keepIdsBy = keepIdsBy,
                      metricInfo = metricInfo, metricCol = "Metric",
                      keepMetrics = keepMetrics, keepMetricsBy = keepMetricsBy,
                      metrics = metrics)
    expect_s3_class(fd, "data.frame")
    expect_identical(dim(fd), c(3L, 4L))
    expect_named(fd, c("Method", "m1", "m2", "m3"))
    expect_identical(fd$Method, c("A", "B", "C"))
    expect_identical(fd$m1, df$m1)

    ## Method filtering
    keepIds <- c("A", "C")
    keepIdsBy <- list(lets = c("a", "b"), lets2 = c("d", "e"))
    keepMetrics <- c("m1", "m2", "m3")
    keepMetricsBy <- list(num = c(1.0, 2.0), lets3 = c("m", "n", "o"))
    fd <- .filterData(df = df, idInfo = idInfo, idCol = "Method",
                      keepIds = keepIds, keepIdsBy = keepIdsBy,
                      metricInfo = metricInfo, metricCol = "Metric",
                      keepMetrics = keepMetrics, keepMetricsBy = keepMetricsBy,
                      metrics = metrics)
    expect_s3_class(fd, "data.frame")
    expect_identical(dim(fd), c(2L, 4L))
    expect_named(fd, c("Method", "m1", "m2", "m3"))
    expect_identical(fd$Method, c("A", "C"))
    expect_identical(fd$m1, df$m1[c(1L, 3L)])

    ## Method filtering by annotation
    keepIds <- c("A", "C")
    keepIdsBy <- list(lets = c("a", "b"), lets2 = "d")
    keepMetrics <- c("m1", "m2", "m3")
    keepMetricsBy <- list(num = c(1.0, 2.0), lets3 = c("m", "n", "o"))
    fd <- .filterData(df = df, idInfo = idInfo, idCol = "Method",
                      keepIds = keepIds, keepIdsBy = keepIdsBy,
                      metricInfo = metricInfo, metricCol = "Metric",
                      keepMetrics = keepMetrics, keepMetricsBy = keepMetricsBy,
                      metrics = metrics)
    expect_s3_class(fd, "data.frame")
    expect_identical(dim(fd), c(1L, 4L))
    expect_named(fd, c("Method", "m1", "m2", "m3"))
    expect_identical(fd$Method, "A")
    expect_identical(fd$m1, df$m1[1L])

    ## Method filtering
    keepIds <- c("A", "C")
    keepIdsBy <- list(lets = c("a", "b"), lets2 = "e")
    keepMetrics <- c("m1", "m2", "m3")
    keepMetricsBy <- list(num = c(1.0, 2.0), lets3 = c("m", "n", "o"))
    fd <- .filterData(df = df, idInfo = idInfo, idCol = "Method",
                      keepIds = keepIds, keepIdsBy = keepIdsBy,
                      metricInfo = metricInfo, metricCol = "Metric",
                      keepMetrics = keepMetrics, keepMetricsBy = keepMetricsBy,
                      metrics = metrics)
    expect_s3_class(fd, "data.frame")
    expect_identical(dim(fd), c(1L, 4L))
    expect_named(fd, c("Method", "m1", "m2", "m3"))
    expect_identical(fd$Method, "C")
    expect_identical(fd$m1, df$m1[3L])

    ## Method filtering
    keepIds <- c("A", "B", "C")
    keepIdsBy <- list(lets = c("a", "b"), lets2 = "e")
    keepMetrics <- c("m1", "m2", "m3")
    keepMetricsBy <- list(num = c(1.0, 2.0), lets3 = c("m", "n", "o"))
    fd <- .filterData(df = df, idInfo = idInfo, idCol = "Method",
                      keepIds = keepIds, keepIdsBy = keepIdsBy,
                      metricInfo = metricInfo, metricCol = "Metric",
                      keepMetrics = keepMetrics, keepMetricsBy = keepMetricsBy,
                      metrics = metrics)
    expect_s3_class(fd, "data.frame")
    expect_identical(dim(fd), c(1L, 4L))
    expect_named(fd, c("Method", "m1", "m2", "m3"))
    expect_identical(fd$Method, "C")
    expect_identical(fd$m1, df$m1[3L])

    ## Metric filtering
    keepIds <- c("A", "B", "C")
    keepIdsBy <- list(lets = c("a", "b"), lets2 = c("d", "e"))
    keepMetrics <- c("m2", "m3")
    keepMetricsBy <- list(num = c(1.0, 2.0), lets3 = c("m", "n", "o"))
    fd <- .filterData(df = df, idInfo = idInfo, idCol = "Method",
                      keepIds = keepIds, keepIdsBy = keepIdsBy,
                      metricInfo = metricInfo, metricCol = "Metric",
                      keepMetrics = keepMetrics, keepMetricsBy = keepMetricsBy,
                      metrics = metrics)
    expect_s3_class(fd, "data.frame")
    expect_identical(dim(fd), c(3L, 3L))
    expect_named(fd, c("Method", "m2", "m3"))
    expect_identical(fd$Method, c("A", "B", "C"))
    expect_identical(fd$m2, df$m2)

    ## Metric filtering by annotation
    keepIds <- c("A", "B", "C")
    keepIdsBy <- list(lets = c("a", "b"), lets2 = c("d", "e"))
    keepMetrics <- c("m2", "m3")
    keepMetricsBy <- list(num = 2.0, lets3 = c("m", "n", "o"))
    fd <- .filterData(df = df, idInfo = idInfo, idCol = "Method",
                      keepIds = keepIds, keepIdsBy = keepIdsBy,
                      metricInfo = metricInfo, metricCol = "Metric",
                      keepMetrics = keepMetrics, keepMetricsBy = keepMetricsBy,
                      metrics = metrics)
    expect_s3_class(fd, "data.frame")
    expect_identical(dim(fd), c(3L, 2L))
    expect_named(fd, c("Method", "m3"))
    expect_identical(fd$Method, c("A", "B", "C"))
    expect_identical(fd$m3, df$m3)

    ## Method and metric filtering
    keepIds <- c("A", "B")
    keepIdsBy <- list(lets = c("a", "b"), lets2 = c("d", "e"))
    keepMetrics <- c("m2", "m3")
    keepMetricsBy <- list(num = 2.0, lets3 = c("m", "n"))
    fd <- .filterData(df = df, idInfo = idInfo, idCol = "Method",
                      keepIds = keepIds, keepIdsBy = keepIdsBy,
                      metricInfo = metricInfo, metricCol = "Metric",
                      keepMetrics = keepMetrics, keepMetricsBy = keepMetricsBy,
                      metrics = metrics)
    expect_s3_class(fd, "data.frame")
    expect_identical(dim(fd), c(2L, 1L))
    expect_named(fd, "Method")
    expect_identical(fd$Method, c("A", "B"))

    ## Method and metric filtering
    keepIds <- c("A", "B")
    keepIdsBy <- list(lets = c("a", "b"), lets2 = c("d", "e"))
    keepMetrics <- c("m2", "m3")
    keepMetricsBy <- list(num = 1.0, lets3 = c("m", "n"))
    fd <- .filterData(df = df, idInfo = idInfo, idCol = "Method",
                      keepIds = keepIds, keepIdsBy = keepIdsBy,
                      metricInfo = metricInfo, metricCol = "Metric",
                      keepMetrics = keepMetrics, keepMetricsBy = keepMetricsBy,
                      metrics = metrics)
    expect_s3_class(fd, "data.frame")
    expect_identical(dim(fd), c(2L, 2L))
    expect_named(fd, c("Method", "m2"))
    expect_identical(fd$Method, c("A", "B"))

    ## Method and metric filtering, no idInfo
    keepIds <- c("A", "B")
    keepIdsBy <- list(lets = c("a", "b"), lets2 = c("d", "e"))
    keepMetrics <- c("m2", "m3")
    keepMetricsBy <- list(num = 1.0, lets3 = c("m", "n"))
    fd <- .filterData(df = df, idInfo = NULL, idCol = "Method",
                      keepIds = keepIds, keepIdsBy = keepIdsBy,
                      metricInfo = metricInfo, metricCol = "Metric",
                      keepMetrics = keepMetrics, keepMetricsBy = keepMetricsBy,
                      metrics = metrics)
    expect_s3_class(fd, "data.frame")
    expect_identical(dim(fd), c(2L, 2L))
    expect_named(fd, c("Method", "m2"))
    expect_identical(fd$Method, c("A", "B"))

    ## Method and metric filtering, no metricInfo
    keepIds <- c("A", "B")
    keepIdsBy <- list(lets = c("a", "b"), lets2 = c("d", "e"))
    keepMetrics <- c("m2", "m3")
    keepMetricsBy <- list(num = 1.0, lets3 = c("m", "n"))
    fd <- .filterData(df = df, idInfo = NULL, idCol = "Method",
                      keepIds = keepIds, keepIdsBy = keepIdsBy,
                      metricInfo = NULL, metricCol = "Metric",
                      keepMetrics = keepMetrics, keepMetricsBy = keepMetricsBy,
                      metrics = metrics)
    expect_s3_class(fd, "data.frame")
    expect_identical(dim(fd), c(2L, 3L))
    expect_named(fd, c("Method", "m2", "m3"))
    expect_identical(fd$Method, c("A", "B"))
})
