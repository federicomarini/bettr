#' Launch bettr app to explore and aggregate performance metrics
#' 
#' @param df A \code{data.frame} in long format. Must have at least 
#'   three columns: one providing the method ID (for the methods 
#'   that we want to compare), one providing the metric ID, and
#'   one providing the value of each metric for each method.
#' @param methodCol,metricCol,valueCol Character scalars, 
#'   indicating which columns of \code{df} that contain method IDs, metric IDs
#'   and metric values, respectively.
#' @param initialWeights Named numeric vector providing initial weights for 
#'   aggregating the metric scores. Must have one entry per metric included 
#'   in \code{df}.
#'  
#' @export
#' 
#' @author Charlotte Soneson
#' 
#' @importFrom shiny navbarPage plotOutput numericInput br fluidRow titlePanel
#'   fluidRow column tabsetPanel radioButtons actionButton uiOutput 
#'   reactiveValues observeEvent updateNumericInput isolate renderPlot
#'   tagList shinyApp tabPanel
#' @importFrom stats sd
#' @importFrom ggplot2 ggplot aes geom_bar theme_minimal
#' @importFrom dplyr group_by summarize mutate ungroup arrange select filter 
#'   %>%
#' @importFrom tidyr spread
#' @importFrom ComplexHeatmap Heatmap columnAnnotation rowAnnotation
#'   anno_barplot
#' @importFrom tibble tibble column_to_rownames
#' @importFrom bslib bs_theme
#' @importFrom rlang sym :=
#' @importFrom grid gpar unit
#' @importFrom circlize colorRamp2
#' 
bettr <- function(df, methodCol = "Method", metricCol = "Metric",
                  valueCol = "Value", initialWeights = NULL) {
  ## Check input arguments ----------------------------------------------------
  ## TODO
  ## df data.frame
  ## methodCol, metricCol, valueCol character scalars
  ## initialWeights named numeric vector
  ## One initial weight per metric, absolute values can't sum to 0
  ## methodCol, metricCol, valueCol exist
  ## Can't contain a column named ScaledValue
  
  ## Define column names assigned by the function -----------------------------
  scoreCol <- "Score"
  weightCol <- "Weight"
  
  ## Assign initial weights ---------------------------------------------------
  if (is.null(initialWeights)) {
    nMetrics <- length(unique(df[[metricCol]]))
    initialWeights <- rep(1/nMetrics, nMetrics)
    names(initialWeights) <- unique(df[[metricCol]])
  } else {
    initialWeights <- initialWeights/sum(abs(initialWeights))
  }
  df[[weightCol]] <- initialWeights[df[[metricCol]]]
  
  initValueCol <- valueCol
  df <- df %>% 
    dplyr::mutate(ScaledValue = !!rlang::sym(valueCol))
  valueCol <- "ScaledValue"

  ## UI definition ------------------------------------------------------------
  p_layout <- 
    shiny::navbarPage(
      shiny::titlePanel("bettr"),
      theme = bslib::bs_theme(bootswatch = "darkly"),
      
      shiny::br(),
      
      shiny::fluidRow(
        ## Plot
        shiny::column(9, 
                      shiny::tabsetPanel(type = "tabs",
                                         tabPanel("Heatmap", 
                                                  shiny::plotOutput("bettrHeatmap"))),
                      shiny::radioButtons(inputId = "scaleType",
                                          label = "Scaling",
                                          choices = c("None", "z-score",
                                                      "[0,1]", "[-1,1]",
                                                      "Rank"),
                                          selected = "None")
        ),
        
        ## Controls
        shiny::column(3, 
                      shiny::actionButton(inputId = "resetWeights", 
                                          label = "Reset to uniform weights"),
                      shiny::uiOutput(outputId = "weights"))
      )
    )
  
  ## Server definition --------------------------------------------------------
  server_function <- function(input, output, session) {
    
    ## Initialize data storage ------------------------------------------------
    values <- shiny::reactiveValues(df = df,
                                    nMetrics = length(unique(df[[metricCol]])),
                                    metrics = unique(df[[metricCol]]))

    ## Limit the possible weights (have to be in (0, 1)) ----------------------
    ## Limit the values to [0.001, 1 - (nMetrics * 0.001)]
    ## Must be executed _before_ the other inputs are updated
    ## TODO: This doesn't work as intended
    # lapply(unique(df[[metricCol]]), function(i) {
    #   qnum <- paste0(i, "_weight")
    #   shiny::observeEvent(input[[qnum]], {
    #     if (input[[qnum]] > 0.99) {
    #       shiny::updateNumericInput(session, inputId = qnum, value = 0.99)
    #     } else if (input[[qnum]] < 0.001) {
    #       shiny::updateNumericInput(session, inputId = qnum, value = 0.001)
    #     }
    #   }, priority = 10)
    # })
    
    ## Reset all weights upon action button click -----------------------------
    shiny::observeEvent(input$resetWeights, {
      for (j in values$metrics) {
        shiny::updateNumericInput(
          session, inputId = paste0(j, "_weight"), 
          value = 1/values$nMetrics, 
        )
      }
    })
    
    ## Scale data -------------------------------------------------------------
    shiny::observeEvent(input$scaleType, {
      if (input$scaleType == "None") {
        values$df <- values$df %>%
          dplyr::mutate(ScaledValue = !!rlang::sym(initValueCol))
      } else if (input$scaleType == "[0,1]") {
        values$df <- values$df %>%
          dplyr::group_by(!!rlang::sym(metricCol)) %>%
          dplyr::mutate(ScaledValue = (!!rlang::sym(initValueCol) - 
                                         min(!!rlang::sym(initValueCol)))/
                          (max(!!rlang::sym(initValueCol)) - 
                             min(!!rlang::sym(initValueCol)))) %>%
          dplyr::ungroup() %>%
          as.data.frame()
      } else if (input$scaleType == "[-1,1]") {
        values$df <- values$df %>%
          dplyr::group_by(!!rlang::sym(metricCol)) %>%
          dplyr::mutate(ScaledValue = (!!rlang::sym(initValueCol) - 
                                         min(!!rlang::sym(initValueCol)) + 
                                         !!rlang::sym(initValueCol) - 
                                         max(!!rlang::sym(initValueCol)))/
                          (max(!!rlang::sym(initValueCol)) - 
                             min(!!rlang::sym(initValueCol)))) %>%
          dplyr::ungroup() %>%
          as.data.frame()
      } else if (input$scaleType == "z-score") {
        values$df <- values$df %>%
          dplyr::group_by(!!rlang::sym(metricCol)) %>%
          dplyr::mutate(ScaledValue = (!!rlang::sym(initValueCol) - 
                                         mean(!!rlang::sym(initValueCol)))/
                          stats::sd(!!rlang::sym(initValueCol))) %>%
          dplyr::ungroup() %>%
          as.data.frame()
      } else if (input$scaleType == "Rank") {
        values$df <- values$df %>%
          dplyr::group_by(!!rlang::sym(metricCol)) %>%
          dplyr::mutate(ScaledValue = order(order(!!rlang::sym(initValueCol)))) %>%
          dplyr::ungroup() %>%
          as.data.frame()
      }
      
    })
    
    ## Modify the 'Weight' column of values$df when any slider changes --------
    lapply(unique(df[[metricCol]]), function(i) {
      qnum <- paste0(i, "_weight")
      shiny::observeEvent(input[[qnum]], {
        ## Use isolate() to avoid infinite update loop (changes to other 
        ## sliders don't trigger new changes)
        shiny::isolate({
          ## Calculate "remaining" weight to distribute
          remweight <- 1 - input[[qnum]]
          tmp <- unlist(lapply(setdiff(values$metrics, i), function(j) {
            input[[paste0(j, "_weight")]]
          }))
          tottmp <- sum(tmp)
          for (j in setdiff(values$metrics, i)) {
            if (abs(input[[paste0(j, "_weight")]] - 
                    remweight * input[[paste0(j, "_weight")]]/tottmp) > 0.0001) {
              shiny::updateNumericInput(
                session, inputId = paste0(j, "_weight"), 
                value = remweight * input[[paste0(j, "_weight")]]/tottmp
              )
            }
          }
          for (j in values$metrics) {
            if (abs(values$df[values$df[[metricCol]] == j, weightCol][1] - 
                    input[[paste0(j, "_weight")]]) > 0.0001) {
              values$df[values$df[[metricCol]] == j, weightCol] <- 
                input[[paste0(j, "_weight")]]
            }
          }
        })
      })
    })
    
    ## Define plot ------------------------------------------------------------
    output$bettrHeatmap <- shiny::renderPlot({
      if (is.null(values$df)) {
        NULL
      } else {
        rowAnnot <- values$df %>%
          dplyr::group_by(!!rlang::sym(methodCol)) %>%
          dplyr::summarize("{scoreCol}" := sum(!!rlang::sym(weightCol) *
                                                 !!rlang::sym(valueCol),
                                               na.rm = TRUE)) %>%
          dplyr::arrange(dplyr::desc(!!rlang::sym(scoreCol)))
        tmp <- values$df
        tmp[[methodCol]] <- factor(tmp[[methodCol]], 
                                   levels = rowAnnot[[methodCol]])
        mat <- tmp %>%
          dplyr::select(c(!!rlang::sym(methodCol),
                          !!rlang::sym(metricCol),
                          !!rlang::sym(valueCol))) %>%
          tidyr::spread(key = !!rlang::sym(metricCol),
                        value = !!rlang::sym(valueCol), fill = NA) %>%
          as.data.frame() %>%
          tibble::column_to_rownames(var = methodCol) %>%
          as.matrix()
        ## TODO: Multiply weight by sign for visualization
        rowAnnot <- rowAnnot[match(rownames(mat), rowAnnot[[methodCol]]), ,
                             drop = FALSE] %>% as.data.frame() %>%
          tibble::column_to_rownames(var = methodCol)
        colAnnot <- values$df %>%
          dplyr::filter(!duplicated(!!rlang::sym(metricCol))) %>%
          dplyr::select(c(!!rlang::sym(metricCol), weightCol)) 
        colAnnot <- tibble::tibble(colAnnot[match(colnames(mat), 
                                                  colAnnot[[metricCol]]), , 
                             drop = FALSE]) %>% as.data.frame() %>%
          tibble::column_to_rownames(var = metricCol)
        
        rowAnnot <- ComplexHeatmap::rowAnnotation(
          Score = ComplexHeatmap::anno_barplot(rowAnnot[[scoreCol]],
                                               width = grid::unit(2, "cm"), 
                                               baseline = 0)
        )
        colAnnot <- ComplexHeatmap::columnAnnotation(
          Weight = ComplexHeatmap::anno_barplot(colAnnot[[weightCol]],
                                                height = grid::unit(2, "cm"), 
                                                baseline = 0)
        )

        heatmapCols = circlize::colorRamp2(seq(min(mat), max(mat), length = 3), 
                                           c("blue", "#EEEEEE", "red"))
        
        ComplexHeatmap::Heatmap(matrix = mat, name = "Relative\nvalue",
                                col = heatmapCols,
                                na_col = "lightgrey",
                                rect_gp = grid::gpar(col = "white", lwd = 1),
                                cluster_rows = FALSE, 
                                cluster_columns = FALSE, 
                                row_names_side = "left",
                                top_annotation = colAnnot, 
                                right_annotation = rowAnnot)
      }
    })
    
    ## TODO: Add 'sign' checkbox to allow changing the sign of the influence 
    ## of a given metric
    
    ## Define weight controls -------------------------------------------------
    output$weights <- shiny::renderUI({
      if (is.null(values$df)) {
        NULL
      } else {
        do.call(shiny::tagList,
                lapply(values$metrics, function(i) {
                  shiny::numericInput(
                    inputId = paste0(i, "_weight"),
                    label = i,
                    value = values$df[values$df[[metricCol]] == i, 
                                      weightCol][1],
                    min = 0.001,
                    max = 1 - 0.001 * (length(values$metrics) - 1),
                    step = 0.000001
                  )
                }))
      }
    })
    
  }
  
  # Generate app ------------------------------------------------------------
  shiny::shinyApp(ui = p_layout, server = server_function)
}