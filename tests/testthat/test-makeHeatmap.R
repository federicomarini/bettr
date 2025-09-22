test_that("makeHeatmap works", {
    scoredata <- data.frame(
        Method = paste0("M", seq_len(8L)),
        Score = c(5.86, 5.71, 4.86, 4.71, 4.57, 4.29, 3.57, 2.43),
        Type = c("T1", "T1", "T2", "T1", "T2", "T1", "T2", "T3")
    )
    plotdata <- data.frame(
        Method = rep(paste0("M", seq_len(8L)), 3L),
        Metric = rep(paste0("S", seq_len(3L)), each = 8L),
        Group = rep(c("A", "A", "B"), each = 8L),
        ScaledValue = c(7.0, 8.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 8.0,
                        7.0, 5.0, 4.0, 6.0, 1.0, 3.0, 2.0, 8.0, 7.0, 3.0,
                        5.0, 2.0, 4.0, 6.0, 1.0),
        Weight = 0.2
    )
    metricInfo <- data.frame(Metric = paste0("S", seq_len(3L)),
                             Group = c("A", "A", "B"))
    idInfo <- scoredata[, c("Method", "Type")]

    hm <- makeHeatmap(
        bettrList = NULL,
        plotdata = plotdata, scoredata = scoredata, idCol = "Method",
        metricCol = "Metric", valueCol = "ScaledValue", weightCol = "Weight",
        scoreCol = "Score", metricGroupCol = "---", metricInfo = NULL,
        metricColors = NULL, idInfo = NULL, idColors = NULL,
        metricCollapseGroup = FALSE, metricGrouping = NULL,
        labelSize = 10.0, showRowNames = TRUE,
        plotType = "Heatmap", rownamewidth_cm = 6.0, colnameheight_cm = 6.0
    )
    expect_s4_class(hm, "HeatmapList")

    ## With bettrList instead
    hm2 <- makeHeatmap(
        bettrList = list(plotdata = plotdata, scoredata = scoredata,
                         idCol = "Method", metricCol = "Metric",
                         valueCol = "ScaledValue", weightCol = "Weight",
                         scoreCol = "Score", metricGroupCol = "---",
                         metricInfo = NULL,
                         metricColors = NULL, idInfo = NULL, idColors = NULL,
                         metricCollapseGroup = FALSE, metricGrouping = NULL),
        labelSize = 10.0, showRowNames = TRUE,
        plotType = "Heatmap", rownamewidth_cm = 6.0, colnameheight_cm = 6.0
    )
    expect_s4_class(hm2, "HeatmapList")

    expect_error(makeHeatmap(
        bettrList = NULL,
        plotdata = plotdata, scoredata = scoredata, idCol = "Method",
        metricCol = "Metric", valueCol = "ScaledValue", weightCol = "Weight",
        scoreCol = "Score", metricGroupCol = "---", metricInfo = NULL,
        metricColors = NULL, idInfo = NULL, idColors = NULL,
        metricCollapseGroup = FALSE, metricGrouping = NULL,
        labelSize = 10.0, showRowNames = TRUE,
        plotType = "unknown", rownamewidth_cm = 6.0, colnameheight_cm = 6.0
    ), "All values in 'plotType' must be one of")

    hm <- makeHeatmap(
        bettrList = NULL,
        plotdata = plotdata, scoredata = scoredata, idCol = "Method",
        metricCol = "Metric", valueCol = "ScaledValue", weightCol = "Weight",
        scoreCol = "Score", metricGroupCol = "---", metricInfo = NULL,
        metricColors = NULL, idInfo = NULL, idColors = NULL,
        metricCollapseGroup = FALSE, metricGrouping = NULL,
        labelSize = 10.0, showRowNames = TRUE,
        plotType = "Dot plot", rownamewidth_cm = 6.0, colnameheight_cm = 6.0
    )
    expect_s4_class(hm, "HeatmapList")

    grpdf <- plotdata |>
        dplyr::group_by(Method, Group) |>
        dplyr::summarize(ScaledValue = mean(ScaledValue),
                         Weight = mean(Weight),
                         .groups = "drop") |>
        dplyr::mutate(Metric = Group) |>
        dplyr::rename(metricGroup = Group)
    hm <- makeHeatmap(
        bettrList = NULL,
        plotdata = grpdf, scoredata = scoredata, idCol = "Method",
        metricCol = "Metric", valueCol = "ScaledValue", weightCol = "Weight",
        scoreCol = "Score", metricGroupCol = "metricGroup",
        metricInfo = metricInfo, metricColors = NULL,
        idInfo = NULL, idColors = NULL,
        metricCollapseGroup = TRUE, metricGrouping = "Group",
        labelSize = 10.0, showRowNames = TRUE,
        plotType = "Heatmap", rownamewidth_cm = 6.0, colnameheight_cm = 6.0
    )
    expect_s4_class(hm, "HeatmapList")

    hm <- makeHeatmap(
        bettrList = NULL,
        plotdata = plotdata, scoredata = scoredata, idCol = "Method",
        metricCol = "Metric", valueCol = "ScaledValue", weightCol = "Weight",
        scoreCol = "Score", metricGroupCol = "---", metricInfo = NULL,
        metricColors = NULL, idInfo = idInfo, idColors = NULL,
        metricCollapseGroup = FALSE, metricGrouping = NULL,
        labelSize = 10.0, showRowNames = TRUE,
        plotType = "Heatmap", rownamewidth_cm = 6.0, colnameheight_cm = 6.0
    )
    expect_s4_class(hm, "HeatmapList")

    negdf <- plotdata
    negdf$ScaledValue[1L] <- -8.0
    hm <- makeHeatmap(
        bettrList = NULL,
        plotdata = negdf, scoredata = scoredata, idCol = "Method",
        metricCol = "Metric", valueCol = "ScaledValue", weightCol = "Weight",
        scoreCol = "Score", metricGroupCol = "---", metricInfo = NULL,
        metricColors = NULL, idInfo = idInfo, idColors = NULL,
        metricCollapseGroup = FALSE, metricGrouping = NULL,
        labelSize = 10.0, showRowNames = TRUE,
        plotType = "Heatmap", rownamewidth_cm = 6.0, colnameheight_cm = 6.0
    )
    expect_s4_class(hm, "HeatmapList")

    negdf <- plotdata
    negdf$ScaledValue <- -negdf$ScaledValue
    hm <- makeHeatmap(
        bettrList = NULL,
        plotdata = negdf, scoredata = scoredata, idCol = "Method",
        metricCol = "Metric", valueCol = "ScaledValue", weightCol = "Weight",
        scoreCol = "Score", metricGroupCol = "---", metricInfo = NULL,
        metricColors = NULL, idInfo = idInfo, idColors = NULL,
        metricCollapseGroup = FALSE, metricGrouping = NULL,
        labelSize = 10.0, showRowNames = TRUE,
        plotType = "Heatmap", rownamewidth_cm = 6.0, colnameheight_cm = 6.0
    )
    expect_s4_class(hm, "HeatmapList")
})
