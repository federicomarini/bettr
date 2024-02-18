test_that("PolarPlot works", {
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
    
    bpp <- makePolarPlot(
        plotdata = plotdata, idCol = "Method", 
        metricCol = "Metric", valueCol = "ScaledValue", metricGroupCol = "---", 
        labelSize = 10, metricColors = list(Metric = c("blue", "red", "green")),
        metricCollapseGroup = FALSE, metricGrouping = "Group"
    )
    expect_s3_class(bpp, "ggplot")
    
    bpp <- makePolarPlot(
        plotdata = plotdata, idCol = "Method", 
        metricCol = "Metric", valueCol = "ScaledValue", metricGroupCol = "---", 
        labelSize = 10, metricColors = list(Metric = c("blue", "red", "green")),
        metricCollapseGroup = FALSE, metricGrouping = "Group"
    )
    expect_s3_class(bpp, "ggplot")
    
    grpdf <- plotdata |>
        dplyr::group_by(Method, Group) |>
        dplyr::summarize(ScaledValue = mean(ScaledValue),
                         Weight = mean(Weight), 
                         .groups = "drop") |>
        dplyr::mutate(Metric = Group) |>
        dplyr::rename(metricGroup = Group)
    bpp <- makePolarPlot(
        plotdata = grpdf, idCol = "Method", 
        metricCol = "Metric", valueCol = "ScaledValue", 
        metricGroupCol = "metricGroup", 
        labelSize = 10, metricColors = list(Group = c("blue", "red", "green")),
        metricCollapseGroup = TRUE, metricGrouping = "Group"
    )
    expect_s3_class(bpp, "ggplot")
})
