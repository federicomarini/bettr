test_that("makeHeatmap works", {
    scoredata <- data.frame(
        Method = paste0("M", seq_len(8)),
        Score = c(5.86, 5.71, 4.86, 4.71, 4.57, 4.29, 3.57, 2.43),
        Type = c("T1", "T1", "T2", "T1", "T2", "T1", "T2", "T3")
    )
    plotdata <- data.frame(
        Method = rep(paste0("M", seq_len(8)), 3),
        Metric = rep(paste0("S", seq_len(3)), each = 8),
        Group = rep(c("A", "A", "B"), each = 8),
        ScaledValue = c(7, 8, 6, 5, 4, 3, 2, 1, 8, 7, 5, 4, 6, 1, 3, 2, 8, 7, 3, 5, 2, 4, 6, 1),
        Weight = 0.2
    )
    metricInfo <- data.frame(Metric = paste0("S", seq_len(3)),
                             Group = c("A", "A", "B"))
    idInfo <- scoredata[, c("Method", "Type")]
    
    hm <- makeHeatmap(
        plotdata = plotdata, scoredata = scoredata, idCol = "Method", 
        metricCol = "Metric", valueCol = "ScaledValue", weightCol = "Weight", 
        scoreCol = "Score", metricGroupCol = "---", metricInfo = NULL, 
        idInfo = NULL, labelSize = 10, idColors = NULL, metricColors = NULL,
        metricCollapseGroup = FALSE, metricGrouping = NULL, showRowNames = TRUE, 
        plotType = "Heatmap", rownamewidth_cm = 6, colnameheight_cm = 6
    )
    expect_s4_class(hm, "HeatmapList")
    
    expect_error(makeHeatmap(
        plotdata = plotdata, scoredata = scoredata, idCol = "Method", 
        metricCol = "Metric", valueCol = "ScaledValue", weightCol = "Weight", 
        scoreCol = "Score", metricGroupCol = "---", metricInfo = NULL, 
        idInfo = NULL, labelSize = 10, idColors = NULL, metricColors = NULL,
        metricCollapseGroup = FALSE, metricGrouping = NULL, showRowNames = TRUE, 
        plotType = "unknown", rownamewidth_cm = 6, colnameheight_cm = 6
    ), "Unknown plot type")
    
    hm <- makeHeatmap(
        plotdata = plotdata, scoredata = scoredata, idCol = "Method", 
        metricCol = "Metric", valueCol = "ScaledValue", weightCol = "Weight", 
        scoreCol = "Score", metricGroupCol = "---", metricInfo = NULL, 
        idInfo = NULL, labelSize = 10, idColors = NULL, metricColors = NULL,
        metricCollapseGroup = FALSE, metricGrouping = NULL, showRowNames = TRUE, 
        plotType = "Dot plot", rownamewidth_cm = 6, colnameheight_cm = 6
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
        plotdata = grpdf, scoredata = scoredata, idCol = "Method", 
        metricCol = "Metric", valueCol = "ScaledValue", weightCol = "Weight", 
        scoreCol = "Score", metricGroupCol = "metricGroup", 
        metricInfo = metricInfo, 
        idInfo = NULL, labelSize = 10, idColors = NULL, metricColors = NULL,
        metricCollapseGroup = TRUE, metricGrouping = "Group", showRowNames = TRUE, 
        plotType = "Heatmap", rownamewidth_cm = 6, colnameheight_cm = 6
    )
    expect_s4_class(hm, "HeatmapList")
    
    hm <- makeHeatmap(
        plotdata = plotdata, scoredata = scoredata, idCol = "Method", 
        metricCol = "Metric", valueCol = "ScaledValue", weightCol = "Weight", 
        scoreCol = "Score", metricGroupCol = "---", metricInfo = NULL, 
        idInfo = idInfo, labelSize = 10, idColors = NULL, metricColors = NULL,
        metricCollapseGroup = FALSE, metricGrouping = NULL, showRowNames = TRUE, 
        plotType = "Heatmap", rownamewidth_cm = 6, colnameheight_cm = 6
    )
    expect_s4_class(hm, "HeatmapList")
    
    negdf <- plotdata
    negdf$ScaledValue[1] <- -8
    hm <- makeHeatmap(
        plotdata = negdf, scoredata = scoredata, idCol = "Method", 
        metricCol = "Metric", valueCol = "ScaledValue", weightCol = "Weight", 
        scoreCol = "Score", metricGroupCol = "---", metricInfo = NULL, 
        idInfo = idInfo, labelSize = 10, idColors = NULL, metricColors = NULL,
        metricCollapseGroup = FALSE, metricGrouping = NULL, showRowNames = TRUE, 
        plotType = "Heatmap", rownamewidth_cm = 6, colnameheight_cm = 6
    )
    expect_s4_class(hm, "HeatmapList")
    
    negdf <- plotdata
    negdf$ScaledValue <- -negdf$ScaledValue
    hm <- makeHeatmap(
        plotdata = negdf, scoredata = scoredata, idCol = "Method", 
        metricCol = "Metric", valueCol = "ScaledValue", weightCol = "Weight", 
        scoreCol = "Score", metricGroupCol = "---", metricInfo = NULL, 
        idInfo = idInfo, labelSize = 10, idColors = NULL, metricColors = NULL,
        metricCollapseGroup = FALSE, metricGrouping = NULL, showRowNames = TRUE, 
        plotType = "Heatmap", rownamewidth_cm = 6, colnameheight_cm = 6
    )
    expect_s4_class(hm, "HeatmapList")
})
