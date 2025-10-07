test_that("bettr stops with invalid inputs", {
    set.seed(123L)
    df <- data.frame(Method = rep(c("A", "B", "C"), each = 3L),
                     Metric = rep(c("m1", "m2", "m3"), 3L),
                     Value = runif(n = 9L, min = 0.0, max = 3.0)) |>
        tidyr::spread(key = Metric, value = Value)
    metrics <- setdiff(colnames(df), "Method")
    metricInfo <- data.frame(Metric = c("m1", "m2", "m3"), num = 1L:3L)
    idInfo <- data.frame(Method = c("A", "B", "C"), lets = letters[1L:3L])

    ## df
    expect_error(bettr(df = as.matrix(df)),
                 "'df' must be of class 'data.frame'")
    df0 <- df
    colnames(df0)[2L] <- "m 1"
    expect_error(bettr(df = df0),
                 "All metrics must be valid names")

    ## idCol
    expect_error(bettr(df = df, idCol = 1L),
                 "'idCol' must be of class 'character'")
    expect_error(bettr(df = df, idCol = c("m1", "m2")),
                 "'idCol' must have length 1")
    expect_error(bettr(df = df, idCol = "Missing"),
                 "All values in 'idCol' must be one of")

    ## metrics
    expect_error(bettr(df = df, metrics = 1L),
                 "'metrics' must be of class 'character'")
    expect_error(bettr(df = df, metrics = "missing"),
                 "All values in 'metrics' must be one of")

    ## initialWeights
    expect_error(bettr(df = df, initialWeights = "x"),
                 "'initialWeights' must be of class 'numeric'")
    expect_error(bettr(df = df, initialWeights = 0.5),
                 "'namesinitialWeights' must not be NULL")
    expect_error(bettr(df = df, initialWeights = rep(0.5, ncol(df) - 1L)),
                 "'namesinitialWeights' must not be NULL")
    expect_error(bettr(df = df, initialWeights = setNames(
        rep(-1.0, ncol(df) - 1L), nm = metrics
    )), "'initialWeights' must be within [0,1]", fixed = TRUE)

    ## metricInfo
    expect_warning({
        out <- bettr(df = df, metricInfo = metricInfo[1L:2L, ])
    }, "metricInfo does not provide annotations for all metrics")
    expect_error(bettr(df = df, metricInfo = as.matrix(metricInfo)),
                 "'metricInfo' must be of class 'data.frame'")
    expect_error(bettr(df = df, metricInfo = metricInfo[, -1L, drop = FALSE]),
                 "metricInfo must have a column named 'Metric'")
    mi2 <- metricInfo
    mi2$input <- 1L
    expect_error(bettr(df = df, metricInfo = mi2),
                 "metricInfo can not have columns named")

    ## idInfo
    expect_warning({
        out <- bettr(df = df, idInfo = idInfo[1L:2L, ])
    }, "idInfo does not provide annotations for all methods")
    expect_error(bettr(df = df, idInfo = as.matrix(idInfo)),
                 "'idInfo' must be of class 'data.frame'")
    expect_error(bettr(df = df, idInfo = idInfo[, -1L, drop = FALSE]),
                 "idInfo must have a column named 'Method'")
    id2 <- idInfo
    id2$input <- 1L
    expect_error(bettr(df = df, idInfo = id2),
                 "idInfo can not have columns named")

    ## initialTransforms
    expect_error(bettr(df = df, initialTransforms = 1L),
                 "'initialTransforms' must be of class 'list'")
    expect_error(bettr(df = df,
                       initialTransforms = list(m1 = list(offset = "x"))),
                 "Specified offsets must be numeric scalars")
    expect_error(bettr(df = df,
                       initialTransforms = list(m1 = list(offset = c(1.0, 2.0)))),
                 "Specified offsets must be numeric scalars")
    expect_error(bettr(df = df,
                       initialTransforms = list(m1 = list(flip = "x"))),
                 "Specified flips must be logical scalars")
    expect_error(bettr(df = df,
                       initialTransforms = list(m1 = list(flip = c(TRUE, FALSE)))),
                 "Specified flips must be logical scalars")
    expect_error(bettr(df = df,
                       initialTransforms = list(m1 = list(transform = 1.0))),
                 "Specified transforms must be character scalars")
    expect_error(bettr(df = df,
                       initialTransforms = list(m1 = list(transform = "x"))),
                 "Specified transforms must be character scalars")
    expect_error(bettr(df = df,
                       initialTransforms = list(m1 = list(transform = c("Rank", "None")))),
                 "Specified transforms must be character scalars")
    expect_error(bettr(df = df,
                       initialTransforms = list(m1 = list(cuts = "x"))),
                 "Specified cuts must be numeric vectors")
    expect_error(bettr(df = df,
                       initialTransforms = list(m1 = list(cuts = FALSE))),
                 "Specified cuts must be numeric vectors")

    ## weightResolution
    expect_error(bettr(df = df, weightResolution = 2.0),
                 "'weightResolution' must be within [0,1]",
                 fixed = TRUE)
    expect_error(bettr(df = df, weightResolution = c(0.1, 0.2)),
                 "'weightResolution' must have length 1")
    expect_error(bettr(df = df, weightResolution = "0.5"),
                 "'weightResolution' must be of class 'numeric'")

    ## bstheme
    expect_error(bettr(df = df, bstheme = "missing"),
                 "is not a known preset theme")

    ## appTitle
    expect_error(bettr(df = df, appTitle = 2L),
                 "'appTitle' must be of class 'character'")
    expect_error(bettr(df = df, appTitle = c("t1", "t2")),
                 "'appTitle' must have length 1")
})

test_that("bettr runs with valid inputs", {
    set.seed(123L)
    df <- data.frame(Method = rep(c("A", "B", "C"), each = 3L),
                     Metric = rep(c("m1", "m2", "m3"), 3L),
                     Value = runif(n = 9L, min = 0.0, max = 3.0)) |>
        tidyr::spread(key = Metric, value = Value)
    df$m3 <- paste0(df$m3, "x")
    metrics <- setdiff(colnames(df), "Method")
    metricInfo <- data.frame(Metric = c("m1", "m2", "m3"), num = 1L:3L)
    idInfo <- data.frame(Method = c("A", "B", "C"), lets = letters[1L:3L])

    ## Default parameters
    app <- bettr(df, idCol = "Method")
    expect_s3_class(app, "shiny.appobj")

    ## Specify subset of metrics
    app <- bettr(df, idCol = "Method", metrics = c("m1", "m3"))
    expect_s3_class(app, "shiny.appobj")

    ## With SE as input
    se <- assembleSE(df = df, idCol = "Method",
                     metrics = setdiff(colnames(df), "Method"),
                     initialWeights = NULL, initialTransforms = list(),
                     metricInfo = metricInfo, idInfo = idInfo)
    app <- bettr(bettrSE = se)
    expect_s3_class(app, "shiny.appobj")

    se <- assembleSE(df = df, idCol = "Method",
                     metrics = setdiff(colnames(df), "Method"),
                     initialWeights = NULL, initialTransforms = list(),
                     metricInfo = NULL, idInfo = NULL)
    app <- bettr(bettrSE = se)
    expect_s3_class(app, "shiny.appobj")

    ## Specify initial weights
    app <- bettr(df, idCol = "Method",
                 initialWeights = c(m1 = 0.1, m2 = 0.5, m3 = 0.7))
    expect_s3_class(app, "shiny.appobj")

    ## Specify initial transforms
    app <- bettr(df, idCol = "Method", metrics = c("m1", "m3"),
                 initialTransforms = list(m1 = list(offset = 2.0,
                                                    cuts = 1.5),
                                          m2 = list(flip = TRUE,
                                                    transform = "Rank")))
    expect_s3_class(app, "shiny.appobj")

    ## Specify metricInfo
    app <- bettr(df, idCol = "Method", metricInfo = metricInfo)
    expect_s3_class(app, "shiny.appobj")

    ## Specify metricColors
    app <- bettr(df, idCol = "Method", metricInfo = metricInfo,
                 metricColors = list(num = circlize::colorRamp2(c(1.0, 3.0), c("white", "red"))))
    expect_s3_class(app, "shiny.appobj")

    ## Specify idInfo
    app <- bettr(df, idCol = "Method", idInfo = idInfo)
    expect_s3_class(app, "shiny.appobj")

    ## Specify idColors
    app <- bettr(df, idCol = "Method", idInfo = idInfo,
                 idColors = list(lets = c(a = "blue", b = "red", c = "green")))
    expect_s3_class(app, "shiny.appobj")

    ## Change theme
    app <- bettr(df, idCol = "Method", bstheme = "sketchy")
    expect_s3_class(app, "shiny.appobj")
})
