test_that("bettr app works", {
    df <- data.frame(Method = c("M1", "M2", "M3"), metric1 = c(1, 2, 3),
                     metric2 = c(3, 1, 2), metric3 = factor(c("a", "a", "b")))
    initialTransforms <- list(metric1 = list(flip = TRUE, offset = 4))
    metricInfo <- data.frame(Metric = c("metric1", "metric2", "metric3"),
                             Group = c("G1", "G2", "G2"))
    idInfo <- data.frame(Method = c("M1", "M2", "M3"), 
                         Type = c("T1", "T1", "T2"))
    metricColors <- list(Group = c(G1 = "red", G2 = "blue"))
    
    ## Don't run these tests on the CRAN build servers
    skip_on_cran()
    
    shiny_app <- bettr(df = df, idCol = "Method", 
                       metrics = c("metric1", "metric2", "metric3"), 
                       initialTransforms = initialTransforms, 
                       metricInfo = metricInfo, metricColors = metricColors, 
                       idInfo = idInfo)
    
    ## To generate the code below, run 
    ## shinytest2::record_test(shiny_app, name = "bettr_app", seed = 42)
    ## and copy the code in the display
    
    app <- shinytest2::AppDriver$new(shiny_app, name = "bettr_app", 
                                     seed = 42, wait = TRUE,
                                     width = 1589, height = 999)
    
    app$set_inputs(metric1_weight = 0.2)
    app$set_inputs(metric2_weight = 0.2)
    app$set_inputs(metric3_weight = 0.2)
    app$set_window_size(width = 1589, height = 999)
    app$set_inputs(metricGrouping = "Group")
    app$set_inputs(metricCollapseGroup = TRUE)
    app$expect_values()
    
    app$set_inputs(scoreMethod = "weighted median")
    app$expect_values()
})