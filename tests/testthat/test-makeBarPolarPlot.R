test_that("BarPolarPlot works", {
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
    
    bpp <- makeBarPolarPlot(
        bettrList = NULL,
        plotdata = plotdata, scoredata = scoredata, idCol = "Method", 
        metricCol = "Metric", valueCol = "ScaledValue", weightCol = "Weight", 
        scoreCol = "Score", metricGroupCol = "---", 
        metricColors = list(Metric = c("blue", "red", "green")),
        metricCollapseGroup = FALSE, metricGrouping = "Group",
        methods = unique(scoredata$Method), labelSize = 10,
        showComposition = FALSE, scaleFactorPolars = 1.5
    )
    expect_s3_class(bpp, "ggplot")
    
    bpp <- makeBarPolarPlot(
        bettrList = NULL,
        plotdata = plotdata, scoredata = scoredata, idCol = "Method", 
        metricCol = "Metric", valueCol = "ScaledValue", weightCol = "Weight", 
        scoreCol = "Score", metricGroupCol = "---", 
        metricColors = list(Metric = c("blue", "red", "green")),
        metricCollapseGroup = FALSE, metricGrouping = "Group",
        methods = NULL, labelSize = 10,
        showComposition = FALSE, scaleFactorPolars = 1.5
    )
    expect_s3_class(bpp, "ggplot")
    
    ## With bettrList instead
    bpp2 <- makeBarPolarPlot(
        bettrList = list(plotdata = plotdata, scoredata = scoredata, 
                         idCol = "Method", metricCol = "Metric", 
                         valueCol = "ScaledValue", weightCol = "Weight", 
                         scoreCol = "Score", metricGroupCol = "---", 
                         metricColors = list(Metric = c("blue", "red", "green")),
                         metricCollapseGroup = FALSE, metricGrouping = "Group",
                         methods = unique(scoredata$Method)), 
        labelSize = 10, showComposition = FALSE, scaleFactorPolars = 1.5
    )
    expect_s3_class(bpp2, "ggplot")
    
    bpp <- makeBarPolarPlot(
        bettrList = NULL,
        plotdata = plotdata, scoredata = scoredata, idCol = "Method", 
        metricCol = "Metric", valueCol = "ScaledValue", weightCol = "Weight", 
        scoreCol = "Score", metricGroupCol = "---", 
        metricColors = list(Metric = c("blue", "red", "green")),
        metricCollapseGroup = FALSE, metricGrouping = "Group",
        methods = unique(scoredata$Method), labelSize = 10,
        showComposition = TRUE, scaleFactorPolars = 1.5
    )
    expect_s3_class(bpp, "ggplot")
    
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
        methods = unique(scoredata$Method), labelSize = 10,
        showComposition = TRUE, scaleFactorPolars = 1.5
    )
    expect_s3_class(bpp, "ggplot")
})
