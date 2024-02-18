test_that("bettr stops with invalid inputs", {
    set.seed(123)
    df <- data.frame(Method = rep(c("A", "B", "C"), each = 3),
                     Metric = rep(c("m1", "m2", "m3"), 3),
                     Value = runif(n = 9, min = 0, max = 3)) |>
        tidyr::spread(key = Metric, value = Value)
    metrics <- setdiff(colnames(df), "Method")
    metricInfo <- data.frame(Metric = c("m1", "m2", "m3"), num = 1:3)
    idInfo <- data.frame(Method = c("A", "B", "C"), lets = letters[1:3])

    ## df
    expect_error(bettr(df = as.matrix(df)),
                 regexp = 'df must be a data.frame',
                 fixed = TRUE)
    df0 <- df
    colnames(df0)[2] <- "m 1"
    expect_error(bettr(df = df0), 
                 regexp = 'All metrics must be valid names',
                 fixed = TRUE)

    ## idCol
    expect_error(bettr(df = df, idCol = 1),
                 regexp = 'idCol must be a character scalar',
                 fixed = TRUE)
    expect_error(bettr(df = df, idCol = c("x", "y")),
                 regexp = 'idCol must be a character scalar',
                 fixed = TRUE)
    expect_error(bettr(df = df, idCol = "Missing"),
                 regexp = 'idCol must point to a column of df',
                 fixed = TRUE)

    ## metrics
    expect_error(bettr(df = df, metrics = 1),
                 regexp = 'All elements of metrics must point to columns of df',
                 fixed = TRUE)
    expect_error(bettr(df = df, metrics = "missing"),
                 regexp = 'All elements of metrics must point to columns of df',
                 fixed = TRUE)
    expect_error(bettr(df = df, metrics = c(metrics, "missing")),
                 regexp = 'All elements of metrics must point to columns of df',
                 fixed = TRUE)

    ## initialWeights
    expect_error(bettr(df = df, initialWeights = "x"),
                 regexp = 'initialWeights must be a named numeric vector',
                 fixed = FALSE)
    expect_error(bettr(df = df, initialWeights = 0.5),
                 regexp = 'initialWeights must be a named numeric vector',
                 fixed = FALSE)
    expect_error(bettr(df = df, initialWeights = rep(0.5, ncol(df) - 1)),
                 regexp = 'initialWeights must be a named numeric vector',
                 fixed = FALSE)
    expect_error(bettr(df = df, initialWeights = structure(
        rep(0.5, ncol(df) - 1), names = paste0(metrics, "x"))),
        regexp = 'initialWeights must be a named numeric vector',
        fixed = FALSE)
    expect_error(bettr(df = df, initialWeights = structure(
        rep(0, ncol(df) - 1), names = metrics)),
        regexp = 'initialWeights must be a named numeric vector',
        fixed = FALSE)
    expect_error(bettr(df = df, initialWeights = structure(
        rep(-1, ncol(df) - 1), names = metrics)),
        regexp = 'initialWeights must be a named numeric vector',
        fixed = FALSE)
    expect_error(bettr(df = df, initialWeights = structure(
        rep(1.5, ncol(df) - 1), names = metrics)),
        regexp = 'initialWeights must be a named numeric vector',
        fixed = FALSE)
    
    ## metricInfo
    expect_error(bettr(df = df, metricInfo = metricInfo[1:2, ]),
                 regexp = 'metricInfo must contain information about all metrics',
                 fixed = TRUE)
    expect_error(bettr(df = df, metricInfo = as.matrix(metricInfo)),
                 regexp = 'metricInfo must be a data.frame',
                 fixed = TRUE)
    expect_error(bettr(df = df, metricInfo = metricInfo[, -1, drop = FALSE]),
                 regexp = 'metricInfo must have a column named Metric',
                 fixed = TRUE)
    mi2 <- metricInfo
    mi2$input <- 1
    expect_error(bettr(df = df, metricInfo = mi2), 
                 regexp = 'metricInfo can not have columns named', 
                 fixed = TRUE)
    
    ## idInfo
    expect_error(bettr(df = df, idInfo = idInfo[1:2, ]),
                 regexp = 'idInfo must contain information about all entities',
                 fixed = TRUE)
    expect_error(bettr(df = df, idInfo = as.matrix(idInfo)),
                 regexp = 'idInfo must be a data.frame',
                 fixed = TRUE)
    expect_error(bettr(df = df, idInfo = idInfo[, -1, drop = FALSE]),
                 regexp = 'idInfo must have a column named Method',
                 fixed = TRUE)
    id2 <- idInfo
    id2$input <- 1
    expect_error(bettr(df = df, idInfo = id2), 
                 regexp = 'idInfo can not have columns named', 
                 fixed = TRUE)
    
    ## initialTransforms
    expect_error(bettr(df = df, initialTransforms = 1),
                 regexp = 'transformList must be a list',
                 fixed = TRUE)
    expect_error(bettr(df = df, 
                       initialTransforms = list(m1 = list(offset = "x"))),
                 regexp = 'Specified offsets must be numeric scalars',
                 fixed = TRUE)
    expect_error(bettr(df = df, 
                       initialTransforms = list(m1 = list(offset = c(1, 2)))),
                 regexp = 'Specified offsets must be numeric scalars',
                 fixed = TRUE)
    expect_error(bettr(df = df, 
                       initialTransforms = list(m1 = list(flip = "x"))),
                 regexp = 'Specified flips must be logical scalars',
                 fixed = TRUE)
    expect_error(bettr(df = df, 
                       initialTransforms = list(m1 = list(flip = c(TRUE, FALSE)))),
                 regexp = 'Specified flips must be logical scalars',
                 fixed = TRUE)
    expect_error(bettr(df = df, 
                       initialTransforms = list(m1 = list(transform = 1))),
                 regexp = 'Specified transforms must be character scalars',
                 fixed = FALSE)
    expect_error(bettr(df = df, 
                       initialTransforms = list(m1 = list(transform = "x"))),
                 regexp = 'Specified transforms must be character scalars',
                 fixed = FALSE)
    expect_error(bettr(df = df, 
                       initialTransforms = list(m1 = list(transform = c("Rank", "None")))),
                 regexp = 'Specified transforms must be character scalars',
                 fixed = FALSE)
    expect_error(bettr(df = df, 
                       initialTransforms = list(m1 = list(cuts = "x"))),
                 regexp = 'Specified cuts must be numeric vectors',
                 fixed = TRUE)
    expect_error(bettr(df = df, 
                       initialTransforms = list(m1 = list(cuts = FALSE))),
                 regexp = 'Specified cuts must be numeric vectors',
                 fixed = TRUE)
    
    ## weightResolution
    expect_error(bettr(df = df, weightResolution = 2), 
                 regexp = 'weightResolution must be a numeric scalar in', 
                 fixed = TRUE)
    expect_error(bettr(df = df, weightResolution = c(0.1, 0.2)), 
                 regexp = 'weightResolution must be a numeric scalar in', 
                 fixed = TRUE)
    expect_error(bettr(df = df, weightResolution = -1), 
                 regexp = 'weightResolution must be a numeric scalar in', 
                 fixed = TRUE)
    expect_error(bettr(df = df, weightResolution = "0.5"), 
                 regexp = 'weightResolution must be a numeric scalar in', 
                 fixed = TRUE)
    
    ## bstheme
    expect_error(bettr(df = df, bstheme = "missing"),
                 regexp = "is not a known preset theme",
                 fixed = FALSE)
    
    ## appTitle
    expect_error(bettr(df = df, appTitle = 2), 
                 regexp = 'appTitle must be a character scalar', 
                 fixed = TRUE)
    expect_error(bettr(df = df, appTitle = c("t1", "t2")), 
                 regexp = 'appTitle must be a character scalar', 
                 fixed = TRUE)
})

test_that("bettr runs with valid inputs", {
    set.seed(123)
    df <- data.frame(Method = rep(c("A", "B", "C"), each = 3),
                     Metric = rep(c("m1", "m2", "m3"), 3),
                     Value = runif(n = 9, min = 0, max = 3)) |>
        tidyr::spread(key = Metric, value = Value)
    df$m3 <- paste0(df$m3, "x")
    metrics <- setdiff(colnames(df), "Method")
    metricInfo <- data.frame(Metric = c("m1", "m2", "m3"), num = 1:3)
    idInfo <- data.frame(Method = c("A", "B", "C"), lets = letters[1:3])
    
    ## Default parameters
    app <- bettr(df, idCol = "Method")
    expect_s3_class(app, "shiny.appobj")

    ## Specify subset of metrics
    app <- bettr(df, idCol = "Method", metrics = c("m1", "m3"))
    expect_s3_class(app, "shiny.appobj")
    
    ## Specify initial weights
    app <- bettr(df, idCol = "Method", 
                 initialWeights = c(m1 = 0.1, m2 = 0.5, m3 = 0.7))
    expect_s3_class(app, "shiny.appobj")
    
    ## Specify initial transforms
    app <- bettr(df, idCol = "Method", metrics = c("m1", "m3"),
                 initialTransforms = list(m1 = list(offset = 2, 
                                                    cuts = 1.5),
                                          m2 = list(flip = TRUE, 
                                                    transform = "Rank")))
    expect_s3_class(app, "shiny.appobj")
    
    ## Specify metricInfo
    app <- bettr(df, idCol = "Method", metricInfo = metricInfo)
    expect_s3_class(app, "shiny.appobj")
    
    ## Specify metricColors
    app <- bettr(df, idCol = "Method", metricInfo = metricInfo,
                 metricColors = list(num = circlize::colorRamp2(c(1, 3), c("white", "red"))))
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


