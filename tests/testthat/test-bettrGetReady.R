test_that("bettrGetReady works", {
    df <- data.frame(Method = c("M1", "M2", "M3"), 
                     metric1 = c(1, 2, 3),
                     metric2 = c(3, 1, 2),
                     metric3 = c(2, 1, NA))
    metricInfo <- data.frame(Metric = c("metric1", "metric2", "metric3"),
                             Group = c("G1", "G2", "G2"))
    idInfo <- data.frame(Method = c("M1", "M2", "M3"), 
                         Type = c("T1", "T1", "T2"))
    
    ## Check that the function returns an error with incorrect input
    ## -------------------------------------------------------------------------
    .args <- list(df = df, idCol = "Method", 
                  metrics = c("metric1", "metric2"), 
                  initialWeights = NULL, 
                  initialTransforms = list(),
                  metricInfo = NULL, metricColors = NULL,
                  idInfo = NULL, idColors = NULL, 
                  scoreMethod = "weighted mean", 
                  idOrdering = "high-to-low", 
                  showOnlyTopIds = FALSE, nbrTopIds = 10, 
                  idTopNGrouping = NULL, 
                  keepIds = NULL, 
                  metricGrouping = NULL, metricCollapseGroup = FALSE, 
                  metricCollapseMethod = "mean")
    
    args <- .args
    args$df <- 1
    expect_error(do.call(bettrGetReady, args), 
                 "'df' must be of class 'data.frame'")
    
    args <- .args
    args$idCol <- 1
    expect_error(do.call(bettrGetReady, args), 
                 "'idCol' must be of class 'character'")
    args$idCol <- c("Method", "metric1")
    expect_error(do.call(bettrGetReady, args), 
                 "'idCol' must have length 1")
    
    args <- .args
    args$metrics <- 1
    expect_error(do.call(bettrGetReady, args), 
                 "'metrics' must be of class 'character'")
    args$metrics <- "missing"
    expect_error(do.call(bettrGetReady, args), 
                 "All values in 'metrics' must be one of")
    
    args <- .args
    args$initialWeights <- "1"
    expect_error(do.call(bettrGetReady, args), 
                 "'initialWeights' must be of class 'numeric'")
    
    args <- .args
    args$initialTransforms <- 1
    expect_error(do.call(bettrGetReady, args), 
                 "'initialTransforms' must be of class 'list'")
    
    args <- .args
    args$metricInfo <- seq(1, 3)
    expect_error(do.call(bettrGetReady, args), 
                 "'metricInfo' must be of class 'data.frame'")
    
    args <- .args
    args$metricColors <- seq(1, 3)
    expect_error(do.call(bettrGetReady, args), 
                 "'metricColors' must be of class 'list'")
    
    args <- .args
    args$idInfo <- seq(1, 3)
    expect_error(do.call(bettrGetReady, args), 
                 "'idInfo' must be of class 'data.frame'")
    
    args <- .args
    args$idColors <- seq(1, 3)
    expect_error(do.call(bettrGetReady, args), 
                 "'idColors' must be of class 'list'")
    
    args <- .args
    args$scoreMethod <- 1
    expect_error(do.call(bettrGetReady, args), 
                 "'scoreMethod' must be of class 'character'")
    args$scoreMethod <- c("weighted mean", "weighted median")
    expect_error(do.call(bettrGetReady, args), 
                 "'scoreMethod' must have length 1")
    args$scoreMethod <- "missing"
    expect_error(do.call(bettrGetReady, args), 
                 "All values in 'scoreMethod' must be one of")
    
    args <- .args
    args$idOrdering <- 1
    expect_error(do.call(bettrGetReady, args), 
                 "'idOrdering' must be of class 'character'")
    args$idOrdering <- c("high-to-low", "low-to-high")
    expect_error(do.call(bettrGetReady, args), 
                 "'idOrdering' must have length 1")
    args$idOrdering <- "missing"
    expect_error(do.call(bettrGetReady, args), 
                 "All values in 'idOrdering' must be one of")
    
    args <- .args
    args$showOnlyTopIds <- 1
    expect_error(do.call(bettrGetReady, args), 
                 "'showOnlyTopIds' must be of class 'logical'")
    args$showOnlyTopIds <- c(TRUE, FALSE)
    expect_error(do.call(bettrGetReady, args), 
                 "'showOnlyTopIds' must have length 1")
    
    args <- .args
    args$nbrTopIds <- TRUE
    expect_error(do.call(bettrGetReady, args), 
                 "'nbrTopIds' must be of class 'numeric'")
    args$nbrTopIds <- c(1, 2)
    expect_error(do.call(bettrGetReady, args), 
                 "'nbrTopIds' must have length 1")
    
    args <- .args
    args$idTopNGrouping <- TRUE
    expect_error(do.call(bettrGetReady, args), 
                 "'idTopNGrouping' must be of class 'character'")
    args$idTopNGrouping <- c("Group", "Group")
    expect_error(do.call(bettrGetReady, args), 
                 "'idTopNGrouping' must have length 1")
    
    args <- .args
    args$keepIds <- TRUE
    expect_error(do.call(bettrGetReady, args), 
                 "'keepIds' must be of class 'character'")
    
    args <- .args
    args$metricGrouping <- TRUE
    expect_error(do.call(bettrGetReady, args), 
                 "'metricGrouping' must be of class 'character'")
    args$metricGrouping <- c("Group", "Group")
    expect_error(do.call(bettrGetReady, args), 
                 "'metricGrouping' must have length 1")
    
    args <- .args
    args$metricCollapseGroup <- 1
    expect_error(do.call(bettrGetReady, args), 
                 "'metricCollapseGroup' must be of class 'logical'")
    args$metricCollapseGroup <- c(TRUE, FALSE)
    expect_error(do.call(bettrGetReady, args), 
                 "'metricCollapseGroup' must have length 1")
    
    args <- .args
    args$metricCollapseMethod <- 1
    expect_error(do.call(bettrGetReady, args), 
                 "'metricCollapseMethod' must be of class 'character'")
    args$metricCollapseMethod <- c("mean", "max")
    expect_error(do.call(bettrGetReady, args), 
                 "'metricCollapseMethod' must have length 1")
    args$metricCollapseMethod <- "missing"
    expect_error(do.call(bettrGetReady, args), 
                 "All values in 'metricCollapseMethod' must be one of")
    
    args <- .args
    df0 <- df
    df0$metric2 <- list(seq_len(3), seq_len(2), seq_len(5))
    args$df <- df0
    expect_error(do.call(bettrGetReady, args),
                 "Encountered metric that could not be identified")
    
    ## Check that the function behaves as expected with valid input
    ## -------------------------------------------------------------------------
    ## Mostly defaults
    out <- bettrGetReady(df = df, idCol = "Method", 
                         metrics = c("metric1", "metric2", "metric3"), 
                         initialWeights = NULL, 
                         initialTransforms = list(),
                         metricInfo = NULL, metricColors = NULL,
                         idInfo = NULL, idColors = NULL, 
                         scoreMethod = "weighted mean", 
                         idOrdering = "high-to-low", 
                         showOnlyTopIds = FALSE, nbrTopIds = 10, 
                         idTopNGrouping = NULL, 
                         keepIds = NULL, 
                         metricGrouping = NULL, metricCollapseGroup = FALSE, 
                         metricCollapseMethod = "mean")
    expect_type(out, "list")
    expect_named(out, c("plotdata", "scoredata", "idColors", "metricColors", 
                        "metricGrouping", "metricCollapseGroup", "idInfo", 
                        "metricInfo", "metricGroupCol", "methods", "idCol", 
                        "metricCol", "valueCol", "weightCol", "scoreCol"))
    expect_s3_class(out$plotdata, "data.frame")
    expect_equal(dim(out$plotdata), c(9, 4))
    expect_named(out$plotdata, c("Method", "Metric", "ScaledValue", "Weight"))
    expect_equal(out$plotdata$Weight, rep(0.2, 9))
    ## Factor levels of Method indicate performance ranking
    expect_equal(out$plotdata$Method, factor(rep(c("M1", "M2", "M3"), 
                                                 each = 3), 
                                             levels = c("M3", "M1", "M2")))
    expect_equal(out$plotdata$Metric, rep(c("metric1", "metric2", "metric3"), 3))
    expect_equal(out$plotdata$ScaledValue, c(1, 3, 2, 2, 1, 1, 3, 2, NA))
    expect_s3_class(out$scoredata, "data.frame")
    expect_equal(dim(out$scoredata), c(3, 2))
    expect_named(out$scoredata, c("Method", "Score"))
    expect_equal(out$scoredata$Method, c("M3", "M1", "M2"))
    expect_equal(out$scoredata$Score, c(2.5, 2, 4/3))
    expect_type(out$idColors, "list")
    expect_named(out$idColors, "Method")
    expect_length(out$idColors$Method, 3)
    expect_type(out$metricColors, "list")
    expect_named(out$metricColors, "Metric")
    expect_length(out$metricColors$Metric, 3)
    expect_equal(out$metricGrouping, "---")
    expect_false(out$metricCollapseGroup)
    expect_null(out$idInfo)
    expect_null(out$metricInfo)
    expect_equal(out$metricGroupCol, "metricGroup")
    expect_equal(out$methods, c("M1", "M2", "M3"))
    expect_equal(out$idCol, "Method")
    expect_equal(out$metricCol, "Metric")
    expect_equal(out$valueCol, "ScaledValue")
    expect_equal(out$weightCol, "Weight")
    expect_equal(out$scoreCol, "Score")
    
    ## Recode one variable as categorical
    df0 <- df
    df0$metric2 <- c("A3", "C1", "F2")
    out <- bettrGetReady(df = df0, idCol = "Method", 
                         metrics = c("metric1", "metric2", "metric3"), 
                         initialWeights = NULL, 
                         initialTransforms = list(metric2 = list(levels = c("C1", "F2", "A3"))),
                         metricInfo = NULL, metricColors = NULL,
                         idInfo = NULL, idColors = NULL, 
                         scoreMethod = "weighted mean", 
                         idOrdering = "high-to-low", 
                         showOnlyTopIds = FALSE, nbrTopIds = 10, 
                         idTopNGrouping = NULL, 
                         keepIds = NULL, 
                         metricGrouping = NULL, metricCollapseGroup = FALSE, 
                         metricCollapseMethod = "mean")
    expect_type(out, "list")
    expect_named(out, c("plotdata", "scoredata", "idColors", "metricColors", 
                        "metricGrouping", "metricCollapseGroup", "idInfo", 
                        "metricInfo", "metricGroupCol", "methods", "idCol", 
                        "metricCol", "valueCol", "weightCol", "scoreCol"))
    expect_s3_class(out$plotdata, "data.frame")
    expect_equal(dim(out$plotdata), c(9, 4))
    expect_named(out$plotdata, c("Method", "Metric", "ScaledValue", "Weight"))
    expect_equal(out$plotdata$Weight, rep(0.2, 9))
    ## Factor levels of Method indicate performance ranking
    expect_equal(out$plotdata$Method, factor(rep(c("M1", "M2", "M3"), 
                                                 each = 3), 
                                             levels = c("M3", "M1", "M2")))
    expect_equal(out$plotdata$Metric, rep(c("metric1", "metric2", "metric3"), 3))
    expect_equal(out$plotdata$ScaledValue, c(1, 3, 2, 2, 1, 1, 3, 2, NA))
    expect_s3_class(out$scoredata, "data.frame")
    expect_equal(dim(out$scoredata), c(3, 2))
    expect_named(out$scoredata, c("Method", "Score"))
    expect_equal(out$scoredata$Method, c("M3", "M1", "M2"))
    expect_equal(out$scoredata$Score, c(2.5, 2, 4/3))
    expect_type(out$idColors, "list")
    expect_named(out$idColors, "Method")
    expect_length(out$idColors$Method, 3)
    expect_type(out$metricColors, "list")
    expect_named(out$metricColors, "Metric")
    expect_length(out$metricColors$Metric, 3)
    expect_equal(out$metricGrouping, "---")
    expect_false(out$metricCollapseGroup)
    expect_null(out$idInfo)
    expect_null(out$metricInfo)
    expect_equal(out$metricGroupCol, "metricGroup")
    expect_equal(out$methods, c("M1", "M2", "M3"))
    expect_equal(out$idCol, "Method")
    expect_equal(out$metricCol, "Metric")
    expect_equal(out$valueCol, "ScaledValue")
    expect_equal(out$weightCol, "Weight")
    expect_equal(out$scoreCol, "Score")
    
    ## Different weighting -> different levels in out$plotdata$Method
    out <- bettrGetReady(df = df, idCol = "Method", 
                         metrics = c("metric1", "metric2", "metric3"), 
                         initialWeights = c(metric1 = 0, metric2 = 1, metric3 = 0), 
                         initialTransforms = list(),
                         metricInfo = NULL, metricColors = NULL,
                         idInfo = NULL, idColors = NULL, 
                         scoreMethod = "weighted mean", 
                         idOrdering = "high-to-low", 
                         showOnlyTopIds = FALSE, nbrTopIds = 10, 
                         idTopNGrouping = NULL, 
                         keepIds = NULL, 
                         metricGrouping = NULL, metricCollapseGroup = FALSE, 
                         metricCollapseMethod = "mean")
    expect_type(out, "list")
    expect_named(out, c("plotdata", "scoredata", "idColors", "metricColors", 
                        "metricGrouping", "metricCollapseGroup", "idInfo", 
                        "metricInfo", "metricGroupCol", "methods", "idCol", 
                        "metricCol", "valueCol", "weightCol", "scoreCol"))
    expect_s3_class(out$plotdata, "data.frame")
    expect_equal(dim(out$plotdata), c(9, 4))
    expect_named(out$plotdata, c("Method", "Metric", "ScaledValue", "Weight"))
    expect_equal(out$plotdata$Weight, rep(c(0, 1, 0), 3))
    ## Factor levels of Method indicate performance ranking
    expect_equal(out$plotdata$Method, factor(rep(c("M1", "M2", "M3"), 
                                                 each = 3), 
                                             levels = c("M1", "M3", "M2")))
    expect_equal(out$plotdata$Metric, rep(c("metric1", "metric2", "metric3"), 3))
    expect_equal(out$plotdata$ScaledValue, c(1, 3, 2, 2, 1, 1, 3, 2, NA))
    expect_s3_class(out$scoredata, "data.frame")
    expect_equal(dim(out$scoredata), c(3, 2))
    expect_named(out$scoredata, c("Method", "Score"))
    expect_equal(out$scoredata$Method, c("M1", "M3", "M2"))
    expect_equal(out$scoredata$Score, c(3, 2, 1))
    expect_type(out$idColors, "list")
    expect_named(out$idColors, "Method")
    expect_length(out$idColors$Method, 3)
    expect_type(out$metricColors, "list")
    expect_named(out$metricColors, "Metric")
    expect_length(out$metricColors$Metric, 3)
    expect_equal(out$metricGrouping, "---")
    expect_false(out$metricCollapseGroup)
    expect_null(out$idInfo)
    expect_null(out$metricInfo)
    expect_equal(out$metricGroupCol, "metricGroup")
    expect_equal(out$methods, c("M1", "M2", "M3"))
    expect_equal(out$idCol, "Method")
    expect_equal(out$metricCol, "Metric")
    expect_equal(out$valueCol, "ScaledValue")
    expect_equal(out$weightCol, "Weight")
    expect_equal(out$scoreCol, "Score")
    
    ## Transform metrics
    out <- bettrGetReady(df = df, idCol = "Method", 
                         metrics = c("metric1", "metric2", "metric3"), 
                         initialWeights = c(metric1 = 1, metric2 = 1, metric3 = 0), 
                         initialTransforms = list(metric2 = list(flip = TRUE), 
                                                  metric1 = list(transform = "[0,1]")),
                         metricInfo = NULL, 
                         metricColors = list(Metric = c(metric1 = "blue", metric2 = "green")),
                         idInfo = NULL, idColors = NULL, 
                         scoreMethod = "weighted mean", 
                         idOrdering = "high-to-low", 
                         showOnlyTopIds = FALSE, nbrTopIds = 10, 
                         idTopNGrouping = NULL, 
                         keepIds = NULL, 
                         metricGrouping = NULL, metricCollapseGroup = FALSE, 
                         metricCollapseMethod = "mean")
    expect_type(out, "list")
    expect_named(out, c("plotdata", "scoredata", "idColors", "metricColors", 
                        "metricGrouping", "metricCollapseGroup", "idInfo", 
                        "metricInfo", "metricGroupCol", "methods", "idCol", 
                        "metricCol", "valueCol", "weightCol", "scoreCol"))
    expect_s3_class(out$plotdata, "data.frame")
    expect_equal(dim(out$plotdata), c(9, 4))
    expect_named(out$plotdata, c("Method", "Metric", "ScaledValue", "Weight"))
    expect_equal(out$plotdata$Weight, rep(c(1, 1, 0), 3))
    ## Factor levels of Method indicate performance ranking
    expect_equal(out$plotdata$Method, factor(rep(c("M1", "M2", "M3"), 
                                                 each = 3), 
                                             levels = c("M2", "M3", "M1")))
    expect_equal(out$plotdata$Metric, rep(c("metric1", "metric2", "metric3"), 3))
    expect_equal(out$plotdata$ScaledValue, c(0, -3, 2, 0.5, -1, 1, 1, -2, NA))
    expect_s3_class(out$scoredata, "data.frame")
    expect_equal(dim(out$scoredata), c(3, 2))
    expect_named(out$scoredata, c("Method", "Score"))
    expect_equal(out$scoredata$Method, c("M2", "M3", "M1"))
    expect_equal(out$scoredata$Score, c(-0.25, -0.5, -1.5))
    expect_type(out$idColors, "list")
    expect_named(out$idColors, "Method")
    expect_length(out$idColors$Method, 3)
    expect_type(out$metricColors, "list")
    expect_named(out$metricColors, "Metric")
    expect_length(out$metricColors$Metric, 2)
    expect_equal(out$metricColors, list(Metric = c(metric1 = "blue", metric2 = "green")))
    expect_equal(out$metricGrouping, "---")
    expect_false(out$metricCollapseGroup)
    expect_null(out$idInfo)
    expect_null(out$metricInfo)
    expect_equal(out$metricGroupCol, "metricGroup")
    expect_equal(out$methods, c("M1", "M2", "M3"))
    expect_equal(out$idCol, "Method")
    expect_equal(out$metricCol, "Metric")
    expect_equal(out$valueCol, "ScaledValue")
    expect_equal(out$weightCol, "Weight")
    expect_equal(out$scoreCol, "Score")
    
    ## Group metrics, only top 2 methods
    out <- bettrGetReady(df = df, idCol = "Method", 
                         metrics = c("metric1", "metric2", "metric3"), 
                         initialWeights = c(metric1 = 0, metric2 = 1, metric3 = 0, 
                                            Group_G1 = 1, Group_G2 = 3), 
                         initialTransforms = list(),
                         metricInfo = metricInfo, metricColors = NULL,
                         idInfo = idInfo, idColors = NULL, 
                         scoreMethod = "weighted mean", 
                         idOrdering = "high-to-low", 
                         showOnlyTopIds = TRUE, nbrTopIds = 2, 
                         idTopNGrouping = NULL, 
                         keepIds = NULL, 
                         metricGrouping = "Group", metricCollapseGroup = TRUE, 
                         metricCollapseMethod = "mean")
    expect_type(out, "list")
    expect_named(out, c("plotdata", "scoredata", "idColors", "metricColors", 
                        "metricGrouping", "metricCollapseGroup", "idInfo", 
                        "metricInfo", "metricGroupCol", "methods", "idCol", 
                        "metricCol", "valueCol", "weightCol", "scoreCol"))
    expect_s3_class(out$plotdata, "data.frame")
    expect_equal(dim(out$plotdata), c(4, 5))
    expect_named(out$plotdata, c("Method", "metricGroup", "ScaledValue",
                                 "Weight", "Metric"))
    expect_equal(out$plotdata$Weight, rep(c(1, 3), 2))
    ## Factor levels of Method indicate performance ranking
    expect_equal(out$plotdata$Method, factor(rep(c("M1", "M3"), 
                                                 each = 2), 
                                             levels = c("M3", "M1")))
    expect_equal(out$plotdata$Metric, rep(c("G1", "G2"), 2))
    expect_equal(out$plotdata$ScaledValue, c(1, 2.5, 3, 2))
    expect_s3_class(out$scoredata, "data.frame")
    expect_equal(dim(out$scoredata), c(2, 3))
    expect_named(out$scoredata, c("Method", "Score", "Type"))
    expect_equal(out$scoredata$Method, c("M3", "M1"))
    expect_equal(out$scoredata$Score, c(2.25, 2.125))
    expect_type(out$idColors, "list")
    expect_named(out$idColors, c("Type", "Method"))
    expect_length(out$idColors$Method, 3)
    expect_type(out$metricColors, "list")
    expect_named(out$metricColors, c("Group", "Metric"))
    expect_length(out$metricColors$Metric, 3)
    expect_equal(out$metricGrouping, "Group")
    expect_true(out$metricCollapseGroup)
    expect_equal(out$idInfo, idInfo)
    expect_equal(out$metricInfo, metricInfo)
    expect_equal(out$metricGroupCol, "metricGroup")
    expect_equal(out$methods, c("M1", "M2", "M3"))
    expect_equal(out$idCol, "Method")
    expect_equal(out$metricCol, "Metric")
    expect_equal(out$valueCol, "ScaledValue")
    expect_equal(out$weightCol, "Weight")
    expect_equal(out$scoreCol, "Score")
    
    ## Group metrics, top 2 methods within each type
    out <- bettrGetReady(df = df, idCol = "Method", 
                         metrics = c("metric1", "metric2", "metric3"), 
                         initialWeights = c(metric1 = 0, metric2 = 1, metric3 = 0, 
                                            Group_G1 = 1, Group_G2 = 3), 
                         initialTransforms = list(),
                         metricInfo = metricInfo, metricColors = NULL,
                         idInfo = idInfo, idColors = NULL, 
                         scoreMethod = "weighted mean", 
                         idOrdering = "high-to-low", 
                         showOnlyTopIds = TRUE, nbrTopIds = 2, 
                         idTopNGrouping = "Type", 
                         keepIds = NULL, 
                         metricGrouping = "Group", metricCollapseGroup = TRUE, 
                         metricCollapseMethod = "mean")
    expect_type(out, "list")
    expect_named(out, c("plotdata", "scoredata", "idColors", "metricColors", 
                        "metricGrouping", "metricCollapseGroup", "idInfo", 
                        "metricInfo", "metricGroupCol", "methods", "idCol", 
                        "metricCol", "valueCol", "weightCol", "scoreCol"))
    expect_s3_class(out$plotdata, "data.frame")
    expect_equal(dim(out$plotdata), c(6, 5))
    expect_named(out$plotdata, c("Method", "metricGroup", "ScaledValue",
                                 "Weight", "Metric"))
    expect_equal(out$plotdata$Weight, rep(c(1, 3), 3))
    ## Factor levels of Method indicate performance ranking
    expect_equal(out$plotdata$Method, factor(rep(c("M1", "M2", "M3"), 
                                                 each = 2), 
                                             levels = c("M3", "M1", "M2")))
    expect_equal(out$plotdata$Metric, rep(c("G1", "G2"), 3))
    expect_equal(out$plotdata$ScaledValue, c(1, 2.5, 2, 1, 3, 2))
    expect_s3_class(out$scoredata, "data.frame")
    expect_equal(dim(out$scoredata), c(3, 3))
    expect_named(out$scoredata, c("Method", "Score", "Type"))
    expect_equal(out$scoredata$Method, c("M3", "M1", "M2"))
    expect_equal(out$scoredata$Score, c(2.25, 2.125, 1.25))
    expect_type(out$idColors, "list")
    expect_named(out$idColors, c("Type", "Method"))
    expect_length(out$idColors$Method, 3)
    expect_type(out$metricColors, "list")
    expect_named(out$metricColors, c("Group", "Metric"))
    expect_length(out$metricColors$Metric, 3)
    expect_equal(out$metricGrouping, "Group")
    expect_true(out$metricCollapseGroup)
    expect_equal(out$idInfo, idInfo)
    expect_equal(out$metricInfo, metricInfo)
    expect_equal(out$metricGroupCol, "metricGroup")
    expect_equal(out$methods, c("M1", "M2", "M3"))
    expect_equal(out$idCol, "Method")
    expect_equal(out$metricCol, "Metric")
    expect_equal(out$valueCol, "ScaledValue")
    expect_equal(out$weightCol, "Weight")
    expect_equal(out$scoreCol, "Score")
})
