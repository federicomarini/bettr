test_that("ParCoordPlot works", {
    scoredata <- data.frame(
        Method = paste0("M", seq_len(8L)),
        Score = c(5.86, 5.71, 4.86, 4.71, 4.57, 4.29, 3.57, 2.43),
        Type = c("T1", "T1", "T2", "T1", "T2", "T1", "T2", "T3")
    )
    plotdata <- data.frame(
        Method = rep(paste0("M", seq_len(8L)), 3L),
        Metric = rep(paste0("S", seq_len(3L)), each = 8L),
        Group = rep(c("A", "A", "B"), each = 8L),
        NumCol = rep(c(1.0, 1.0, 2.0), each = 8L),
        ScaledValue = c(7.0, 8.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 8.0, 7.0, 5.0, 4.0, 6.0, 1.0, 3.0, 2.0, 8.0, 7.0, 3.0, 5.0, 2.0, 4.0, 6.0, 1.0),
        Weight = 0.2
    )
    metricInfo <- data.frame(Metric = paste0("S", seq_len(3L)),
                             Group = c("A", "A", "B"),
                             NumCol = 1L:3L)
    idInfo <- scoredata[, c("Method", "Type")]

    bpp <- makeParCoordPlot(
        bettrList = NULL,
        plotdata = plotdata, idCol = "Method",
        metricCol = "Metric", valueCol = "ScaledValue",
        metricGroupCol = "metricGroup",
        metricColors = list(Metric = c("blue", "red", "green")),
        idColors = list(Method = .gg_color_hue(8L)),
        methods = unique(scoredata$Method),
        metricGrouping = "---", highlightMethod = "---", labelSize = 10.0
    )
    bpp
    expect_true(ggplot2::is_ggplot(bpp))

    bpp <- makeParCoordPlot(
        bettrList = NULL,
        plotdata = plotdata, idCol = "Method",
        metricCol = "Metric", valueCol = "ScaledValue",
        metricGroupCol = "metricGroup",
        metricColors = list(Metric = c("blue", "red", "green")),
        idColors = list(Method = .gg_color_hue(8L)),
        methods = NULL,
        metricGrouping = "---", highlightMethod = "---", labelSize = 10.0
    )
    bpp
    expect_true(ggplot2::is_ggplot(bpp))

    ## With bettrList instead
    bpp2 <- makeParCoordPlot(
        bettrList = list(
            plotdata = plotdata, idCol = "Method",
            metricCol = "Metric", valueCol = "ScaledValue",
            metricGroupCol = "metricGroup",
            metricColors = list(Metric = c("blue", "red", "green")),
            idColors = list(Method = .gg_color_hue(8L)),
            methods = unique(scoredata$Method),
            metricGrouping = "---"
        ), highlightMethod = "---", labelSize = 10.0
    )
    bpp2
    expect_true(ggplot2::is_ggplot(bpp2))

    plotdata$metricGroup <- plotdata$Group
    bpp <- makeParCoordPlot(
        bettrList = NULL,
        plotdata = plotdata, idCol = "Method",
        metricCol = "Metric", valueCol = "ScaledValue",
        metricGroupCol = "metricGroup",
        metricColors = list(Group = c("blue", "red")),
        idColors = list(Method = .gg_color_hue(8L)),
        methods = unique(scoredata$Method),
        metricGrouping = "Group", highlightMethod = "---", labelSize = 10.0
    )
    bpp
    expect_true(ggplot2::is_ggplot(bpp))

    plotdata$metricGroup <- plotdata$NumCol
    bpp <- makeParCoordPlot(
        bettrList = NULL,
        plotdata = plotdata, idCol = "Method",
        metricCol = "Metric", valueCol = "ScaledValue",
        metricGroupCol = "metricGroup",
        metricColors = list(NumCol = circlize::colorRamp2(c(1.0, 2.0), c("white", "blue"))),
        idColors = list(Method = .gg_color_hue(8L)),
        methods = unique(scoredata$Method),
        metricGrouping = "NumCol", highlightMethod = "---", labelSize = 10.0
    )
    bpp
    expect_true(ggplot2::is_ggplot(bpp))

    bpp <- makeParCoordPlot(
        bettrList = NULL,
        plotdata = plotdata, idCol = "Method",
        metricCol = "Metric", valueCol = "ScaledValue",
        metricGroupCol = "metricGroup",
        metricColors = list(Metric = c("blue", "red", "green")),
        idColors = list(Method = .gg_color_hue(8L)),
        methods = unique(scoredata$Method),
        metricGrouping = "---", highlightMethod = "M1", labelSize = 10.0
    )
    bpp
    expect_true(ggplot2::is_ggplot(bpp))
})
