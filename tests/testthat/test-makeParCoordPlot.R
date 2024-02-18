test_that("ParCoordPlot works", {
    scoredata <- data.frame(
        Method = paste0("M", seq_len(8)),
        Score = c(5.86, 5.71, 4.86, 4.71, 4.57, 4.29, 3.57, 2.43),
        Type = c("T1", "T1", "T2", "T1", "T2", "T1", "T2", "T3")
    )
    plotdata <- data.frame(
        Method = rep(paste0("M", seq_len(8)), 3),
        Metric = rep(paste0("S", seq_len(3)), each = 8),
        Group = rep(c("A", "A", "B"), each = 8),
        NumCol = rep(c(1, 1, 2), each = 8),
        ScaledValue = c(7, 8, 6, 5, 4, 3, 2, 1, 8, 7, 5, 4, 6, 1, 3, 2, 8, 7, 3, 5, 2, 4, 6, 1),
        Weight = 0.2
    )
    metricInfo <- data.frame(Metric = paste0("S", seq_len(3)),
                             Group = c("A", "A", "B"),
                             NumCol = 1:3)
    idInfo <- scoredata[, c("Method", "Type")]
    
    bpp <- makeParCoordPlot(
        df = plotdata, idCol = "Method", 
        metricCol = "Metric", valueCol = "ScaledValue",  
        methods = unique(scoredata$Method), metricGroupCol = "metricGroup", 
        highlightMethod = "---", idColors = list(Method = .gg_color_hue(8)),
        labelSize = 10, metricColors = list(Metric = c("blue", "red", "green")),
        metricGrouping = "---"
    )
    bpp
    expect_s3_class(bpp, "ggplot")
    
    plotdata$metricGroup <- plotdata$Group
    bpp <- makeParCoordPlot(
        df = plotdata, idCol = "Method", 
        metricCol = "Metric", valueCol = "ScaledValue", 
        methods = unique(scoredata$Method), metricGroupCol = "metricGroup", 
        highlightMethod = "---", idColors = list(Method = .gg_color_hue(8)),
        labelSize = 10, metricColors = list(Group = c("blue", "red")),
        metricGrouping = "Group"
    )
    bpp
    expect_s3_class(bpp, "ggplot")
    
    plotdata$metricGroup <- plotdata$NumCol
    bpp <- makeParCoordPlot(
        df = plotdata, idCol = "Method", 
        metricCol = "Metric", valueCol = "ScaledValue", 
        methods = unique(scoredata$Method), metricGroupCol = "metricGroup", 
        highlightMethod = "---", idColors = list(Method = .gg_color_hue(8)),
        labelSize = 10, metricColors = list(NumCol = circlize::colorRamp2(c(1, 2), c("white", "blue"))),
        metricGrouping = "NumCol"
    )
    bpp
    expect_s3_class(bpp, "ggplot")
    
    bpp <- makeParCoordPlot(
        df = plotdata, idCol = "Method", 
        metricCol = "Metric", valueCol = "ScaledValue",  
        methods = unique(scoredata$Method), metricGroupCol = "metricGroup", 
        highlightMethod = "M1", idColors = list(Method = .gg_color_hue(8)),
        labelSize = 10, metricColors = list(Metric = c("blue", "red", "green")),
        metricGrouping = "---"
    )
    bpp
    expect_s3_class(bpp, "ggplot")
    
})
