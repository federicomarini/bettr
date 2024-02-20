test_that("adding scores works", {
    set.seed(123)
    df <- data.frame(Method = rep(c("A", "B", "C"), each = 3),
                     Metric = rep(c("m1", "m2", "m3"), 3),
                     Value = c(NA, runif(n = 8, min = 0, max = 3))) |>
        tidyr::pivot_wider(names_from = Metric, values_from = Value)
    metrics <- setdiff(colnames(df), "Method")
    metricInfo <- data.frame(Metric = c("m1", "m2", "m3"), num = c(1, 1, 2),
                             lets3 = c("m", "n", "o"))
    idInfo <- data.frame(Method = c("A", "B", "C"), lets = c("a", "b", "b"),
                         lets2 = c("d", "d", "e"))
    
    ld <- .makeLongData(df = df, idCol = "Method", 
                        metrics = c("m1", "m2", "m3"), 
                        metricCol = "Metric", valueCol = "ScaledValue",
                        metricGrouping = "---", metricInfo = metricInfo, 
                        metricGroupCol = "metricGroup")
    ldw <- .addWeightsToLongData(
        df = ld, metricCollapseGroup = FALSE, 
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
    
    ## Weighted mean
    scd <- .calculateScores(df = cld, scoreMethod = "weighted mean", 
                            idCol = "Method", scoreCol = "Score", 
                            weightCol = "Weight", valueCol = "ScaledValue", 
                            metricCol = "Metric")
    expect_s3_class(scd, "data.frame")
    expect_equal(dim(scd), c(3, 2))
    expect_named(scd, c("Method", "Score"))
    expect_equal(scd$Method, c("A", "B", "C"))
    expect_equal(scd$Score, c(1.238278, 2.203737, 1.283924), tolerance = 0.001)
    
    ## Sort and filter
    ## -- keep all, no grouping
    sfcd <- .sortAndFilterScoreData(scoreDf = scd, idInfo = idInfo, 
                                    idCol = "Method", scoreCol = "Score",
                                    idTopNGrouping = "---", 
                                    idOrdering = "high-to-low", 
                                    showOnlyTopIds = FALSE, nbrTopIds = 1)
    expect_s3_class(sfcd, "data.frame")
    expect_equal(dim(sfcd), c(3, 4))
    expect_named(sfcd, c("Method", "Score", "lets", "lets2"))
    expect_true(all(diff(sfcd$Score) <= 0))
    expect_equal(sfcd$Method, c("B", "C", "A"))
    expect_equal(sfcd$Score, c(2.203737, 1.283924, 1.238278), tolerance = 0.001)
    
    ## -- keep all, no grouping, low-to-high
    sfcd <- .sortAndFilterScoreData(scoreDf = scd, idInfo = idInfo, 
                                    idCol = "Method", scoreCol = "Score",
                                    idTopNGrouping = "---", 
                                    idOrdering = "low-to-high", 
                                    showOnlyTopIds = FALSE, nbrTopIds = 1)
    expect_s3_class(sfcd, "data.frame")
    expect_equal(dim(sfcd), c(3, 4))
    expect_named(sfcd, c("Method", "Score", "lets", "lets2"))
    expect_true(all(diff(sfcd$Score) >= 0))
    expect_equal(sfcd$Method, c("A", "C", "B"))
    expect_equal(sfcd$Score, c(1.238278, 1.283924, 2.203737), tolerance = 0.001)
    
    ## -- top 1, no grouping
    sfcd <- .sortAndFilterScoreData(scoreDf = scd, idInfo = idInfo, 
                                    idCol = "Method", scoreCol = "Score",
                                    idTopNGrouping = "---", 
                                    idOrdering = "high-to-low", 
                                    showOnlyTopIds = TRUE, nbrTopIds = 1)
    expect_s3_class(sfcd, "data.frame")
    expect_equal(dim(sfcd), c(1, 4))
    expect_named(sfcd, c("Method", "Score", "lets", "lets2"))
    expect_equal(sfcd$Method, c("B"))
    expect_equal(sfcd$Score, c(2.203737), tolerance = 0.001)
    
    ## -- top 1, no grouping, no idInfo
    sfcd <- .sortAndFilterScoreData(scoreDf = scd, idInfo = NULL, 
                                    idCol = "Method", scoreCol = "Score",
                                    idTopNGrouping = "---", 
                                    idOrdering = "high-to-low", 
                                    showOnlyTopIds = TRUE, nbrTopIds = 1)
    expect_s3_class(sfcd, "data.frame")
    expect_equal(dim(sfcd), c(1, 2))
    expect_named(sfcd, c("Method", "Score"))
    expect_equal(sfcd$Method, c("B"))
    expect_equal(sfcd$Score, c(2.203737), tolerance = 0.001)
    
    ## -- top 1 low-to-high, no grouping
    sfcd <- .sortAndFilterScoreData(scoreDf = scd, idInfo = idInfo, 
                                    idCol = "Method", scoreCol = "Score",
                                    idTopNGrouping = "---", 
                                    idOrdering = "low-to-high", 
                                    showOnlyTopIds = TRUE, nbrTopIds = 1)
    expect_s3_class(sfcd, "data.frame")
    expect_equal(dim(sfcd), c(1, 4))
    expect_named(sfcd, c("Method", "Score", "lets", "lets2"))
    expect_equal(sfcd$Method, c("A"))
    expect_equal(sfcd$Score, c(1.238278), tolerance = 0.001)
    
    ## -- top 1, grouping
    sfcd <- .sortAndFilterScoreData(scoreDf = scd, idInfo = idInfo, 
                                    idCol = "Method", scoreCol = "Score",
                                    idTopNGrouping = "lets", 
                                    idOrdering = "high-to-low", 
                                    showOnlyTopIds = TRUE, nbrTopIds = 1)
    expect_s3_class(sfcd, "data.frame")
    expect_equal(dim(sfcd), c(2, 4))
    expect_named(sfcd, c("Method", "Score", "lets", "lets2"))
    expect_equal(sfcd$Method, c("B", "A"))
    expect_equal(sfcd$Score, c(2.203737, 1.238278), tolerance = 0.001)
    
    ## -- try to group by non-existing variable
    expect_error(.sortAndFilterScoreData(scoreDf = scd, idInfo = idInfo, 
                                         idCol = "Method", scoreCol = "Score",
                                         idTopNGrouping = "missing", 
                                         idOrdering = "high-to-low", 
                                         showOnlyTopIds = TRUE, nbrTopIds = 1),
                 "Must group by variables found")
    
    ## Weighted median
    scd <- .calculateScores(df = cld, scoreMethod = "weighted median", 
                            idCol = "Method", scoreCol = "Score", 
                            weightCol = "Weight", valueCol = "ScaledValue", 
                            metricCol = "Metric")
    expect_s3_class(scd, "data.frame")
    expect_equal(dim(scd), c(3, 2))
    expect_named(scd, c("Method", "Score"))
    expect_equal(scd$Method, c("A", "B", "C"))
    expect_equal(scd$Score, c(2.364915, 2.821402, 2.677257), tolerance = 0.001)
    
    ## Weighted fraction highest
    scd <- .calculateScores(df = cld, scoreMethod = "weighted fraction highest", 
                            idCol = "Method", scoreCol = "Score", 
                            weightCol = "Weight", valueCol = "ScaledValue", 
                            metricCol = "Metric")
    expect_s3_class(scd, "data.frame")
    expect_equal(dim(scd), c(3, 2))
    expect_named(scd, c("Method", "Score"))
    expect_equal(scd$Method, c("A", "B", "C"))
    ## B highest for m1, m2, m3
    expect_equal(scd$Score, c(0, 1, 0), tolerance = 0.001)
    
    ## Weighted fraction lowest
    scd <- .calculateScores(df = cld, scoreMethod = "weighted fraction lowest", 
                            idCol = "Method", scoreCol = "Score", 
                            weightCol = "Weight", valueCol = "ScaledValue", 
                            metricCol = "Metric")
    expect_s3_class(scd, "data.frame")
    expect_equal(dim(scd), c(3, 2))
    expect_named(scd, c("Method", "Score"))
    expect_equal(scd$Method, c("A", "B", "C"))
    ## C lowest for m1; A lowest for m2 and m3 (and missing for m1)
    expect_equal(scd$Score, c(1, 0, 0.33333), tolerance = 0.001)
    
})
