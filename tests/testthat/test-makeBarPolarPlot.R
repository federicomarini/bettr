test_that("BarPolarPlot works", {
    scoredata <- data.frame(
        Method = paste0("M", seq_len(8L)),
        Score = c(5.86, 5.71, 4.86, 4.71, 4.57, 4.29, 3.57, 2.43),
        Type = c("T1", "T1", "T2", "T1", "T2", "T1", "T2", "T3")
    )
    plotdata <- data.frame(
        Method = rep(paste0("M", seq_len(8L)), 3L),
        Metric = rep(paste0("S", seq_len(3L)), each = 8L),
        Group = rep(c("A", "A", "B"), each = 8L),
        ScaledValue = c(7.0, 8.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 8.0, 7.0, 5.0, 4.0, 6.0, 1.0, 3.0, 2.0, 8.0, 7.0, 3.0, 5.0, 2.0, 4.0, 6.0, 1.0),
        Weight = 0.2
    )
    metricInfo <- data.frame(Metric = paste0("S", seq_len(3L)),
                             Group = c("A", "A", "B"))
    idInfo <- scoredata[, c("Method", "Type")]

    bpp <- makeBarPolarPlot(
        bettrList = NULL,
        plotdata = plotdata, scoredata = scoredata, idCol = "Method",
        metricCol = "Metric", valueCol = "ScaledValue", weightCol = "Weight",
        scoreCol = "Score", metricGroupCol = "---",
        metricColors = list(Metric = c("blue", "red", "green")),
        metricCollapseGroup = FALSE, metricGrouping = "Group",
        methods = unique(scoredata$Method), labelSize = 10.0,
        showComposition = FALSE, scaleFactorPolars = 1.5
    )
    expect_true(ggplot2::is_ggplot(bpp))

    bpp <- makeBarPolarPlot(
        bettrList = NULL,
        plotdata = plotdata, scoredata = scoredata, idCol = "Method",
        metricCol = "Metric", valueCol = "ScaledValue", weightCol = "Weight",
        scoreCol = "Score", metricGroupCol = "---",
        metricColors = list(Metric = c("blue", "red", "green")),
        metricCollapseGroup = FALSE, metricGrouping = "Group",
        methods = NULL, labelSize = 10.0,
        showComposition = FALSE, scaleFactorPolars = 1.5
    )
    expect_true(ggplot2::is_ggplot(bpp))

    ## With bettrList instead
    bpp2 <- makeBarPolarPlot(
        bettrList = list(plotdata = plotdata, scoredata = scoredata,
                         idCol = "Method", metricCol = "Metric",
                         valueCol = "ScaledValue", weightCol = "Weight",
                         scoreCol = "Score", metricGroupCol = "---",
                         metricColors = list(Metric = c("blue", "red", "green")),
                         metricCollapseGroup = FALSE, metricGrouping = "Group",
                         methods = unique(scoredata$Method)),
        labelSize = 10.0, showComposition = FALSE, scaleFactorPolars = 1.5
    )
    expect_true(ggplot2::is_ggplot(bpp2))

    bpp <- makeBarPolarPlot(
        bettrList = NULL,
        plotdata = plotdata, scoredata = scoredata, idCol = "Method",
        metricCol = "Metric", valueCol = "ScaledValue", weightCol = "Weight",
        scoreCol = "Score", metricGroupCol = "---",
        metricColors = list(Metric = c("blue", "red", "green")),
        metricCollapseGroup = FALSE, metricGrouping = "Group",
        methods = unique(scoredata$Method), labelSize = 10.0,
        showComposition = TRUE, scaleFactorPolars = 1.5
    )
    expect_true(ggplot2::is_ggplot(bpp))

    grpdf <- plotdata |>
        dplyr::group_by(Method, Group) |>
        dplyr::summarize(ScaledValue = mean(ScaledValue),
                         Weight = mean(Weight),
                         .groups = "drop") |>
        dplyr::mutate(Metric = Group) |>
        dplyr::rename(metricGroup = Group)
    bpp <- makeBarPolarPlot(
        bettrList = NULL,
        plotdata = grpdf, scoredata = scoredata, idCol = "Method",
        metricCol = "Metric", valueCol = "ScaledValue", weightCol = "Weight",
        scoreCol = "Score", metricGroupCol = "metricGroup",
        metricColors = list(Group = c("blue", "red", "green")),
        metricCollapseGroup = TRUE, metricGrouping = "Group",
        methods = unique(scoredata$Method), labelSize = 10.0,
        showComposition = TRUE, scaleFactorPolars = 1.5
    )
    expect_true(ggplot2::is_ggplot(bpp))
})
