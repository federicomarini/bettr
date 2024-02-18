#' Create a summary heatmap
#' 
#' Create a summary heatmap. The input arguments for this functions are 
#' typically generated using \code{\link{bettrPrepare}}, which ensures that 
#' all required columns are available. 
#' 
#' @param bettrList A \code{list}, the output object from \code{prepData}. 
#'     If \code{bettrList} is provided, arguments \code{plotdata}, 
#'     \code{scoredata}, \code{idCol}, \code{metricCol}, \code{valueCol},
#'     \code{weightCol}, \code{scoreCol}, \code{metricGroupCol}, 
#'     \code{metricInfo}, \code{metricColors}, \code{idInfo}, \code{idColors},
#'     \code{metricCollapseGroup}, \code{metricGrouping} and \code{methods} 
#'     will be ignored and the corresponding values will be extracted from 
#'     \code{bettrList}. This is the recommended way of calling the plotting
#'     functions, as it ensures compatibility of all components. 
#' @param plotdata A \code{data.frame} with columns representing methods, 
#'     metrics, scores, and weights. Typically obtained as 
#'     \code{prepData$plotdata}, where \code{prepData} is the output from 
#'     \code{bettrPrepare}. 
#' @param scoredata A \code{data.frame} with columns representing methods, 
#'     aggregated scores, and any other method annotations. Typically 
#'     obtained as \code{prepData$scoredata}, where \code{prepData} is the 
#'     output from \code{bettrPrepare}.
#' @param idCol Character scalar indicating which column of \code{plotdata} and 
#'     \code{scoredata} contains the method IDs. 
#' @param metricCol Character scalar indicating which column of \code{plotdata} 
#'     contains the metric IDs. Typically, \code{"Metric"}.
#' @param valueCol Character scalar indicating which column of \code{plotdata} 
#'     contains the metric values. Typically, \code{"ScaledValue"}.
#' @param weightCol Character scalar indicating which column of \code{plotdata} 
#'     contains the weight values. Typically, \code{"Weight"}.
#' @param scoreCol Character scalar indicating which column of \code{scoredata} 
#'     contains the aggregated score values. Typically, \code{"Score"}.
#' @param metricGroupCol Character scalar indicating which column of 
#'     \code{plotdata} contains the information about the metric group. 
#'     Typically, \code{"metricGroup"}.
#' @param metricInfo `data.frame` with annotations for metrics. Typically 
#'     obtained as \code{prepData$metricInfo}, where \code{prepData} is the 
#'     output from \code{bettrPrepare}.
#' @param idInfo `data.frame` with annotations for entities. Typically 
#'     obtained as \code{prepData$idInfo}, where \code{prepData} is the 
#'     output from \code{bettrPrepare}.
#' @param labelSize Numeric scalar providing the size of the labels in the plot.
#' @param idColors Named list with colors used for methods and any other 
#'     method annotations. Typically obtained as \code{prepData$idColors}, 
#'     where \code{prepData} is the output from \code{bettrPrepare}. 
#' @param metricColors Named list with colors used for the metrics and 
#'     any other metric annotations. Typically obtained as 
#'     \code{prepData$metricColors}, where \code{prepData} is the output from 
#'     \code{bettrPrepare}. 
#' @param metricCollapseGroup Logical scalar indicating whether metrics 
#'     should be collapsed by the group variable provided by 
#'     \code{metricGrouping}. Typically obtained as 
#'     \code{prepData$metricCollapseGroup}, where \code{prepData} is the 
#'     output from \code{bettrPrepare}.
#' @param metricGrouping Character scalar indicating the column of 
#'     \code{metricInfo} that was used to group metrics. Typically obtained as 
#'     \code{prepData$metricGrouping}, where \code{prepData} is the output
#'     from \code{bettrPrepare}.
#' @param showRowNames Logical scalar indicating whether to show row (method)
#'     names in the heatmap. 
#' @param plotType Either \code{"Heatmap"} or \code{"Dot plot"} indicating the 
#'     type of plot to construct.
#' @param rownamewidth_cm,colnameheight_cm Numeric scalars defining the width 
#'     of row names and height of column names, in cm. 
#' 
#' @author Charlotte Soneson
#' @export
#' 
#' @returns
#' A \code{\link[ComplexHeatmap]{HeatmapList}} object.
#' 
#' @importFrom dplyr arrange filter select contains all_of
#' @importFrom tidyr spread
#' @importFrom tibble column_to_rownames tibble
#' @importFrom rlang .data :=
#' @importFrom ComplexHeatmap rowAnnotation anno_barplot columnAnnotation 
#'   Heatmap draw pindex
#' @importFrom grid gpar unit grid.rect grid.circle unit.c
#' @importFrom circlize colorRamp2
#' 
#' @examples
#' ## Generate example data
#' df <- data.frame(Method = c("M1", "M2", "M3"), 
#'                  metric1 = c(1, 2, 3),
#'                  metric2 = c(3, 1, 2))
#' metricInfo <- data.frame(Metric = c("metric1", "metric2", "metric3"),
#'                          Group = c("G1", "G2", "G2"))
#' idInfo <- data.frame(Method = c("M1", "M2", "M3"), 
#'                      Type = c("T1", "T1", "T2"))
#' prepData <- bettrPrepare(df = df, idCol = "Method", 
#'                          metricInfo = metricInfo, idInfo = idInfo)
#' makeHeatmap(bettrList = prepData, plotType = "Heatmap")
#' makeHeatmap(bettrList = prepData, plotType = "Dot plot")
#'                  
makeHeatmap <- function(bettrList = NULL, 
                        plotdata, scoredata, idCol, metricCol = "Metric", 
                        valueCol = "ScaledValue", weightCol = "Weight", 
                        scoreCol = "Score", metricGroupCol = "metricGroup", 
                        metricInfo, metricColors,
                        idInfo, idColors, 
                        metricCollapseGroup = FALSE, metricGrouping = "---", 
                        labelSize = 10, showRowNames = TRUE, 
                        plotType = "Heatmap",
                        rownamewidth_cm = 6, colnameheight_cm = 6) {
    
    ## If bettrList is provided, extract arguments from there
    if (!is.null(bettrList)) {
        stopifnot(all(c("plotdata", "scoredata", "idCol", "metricCol", 
                        "valueCol", "weightCol", "scoreCol", "metricGroupCol", 
                        "metricInfo", "metricColors", "idInfo", "idColors",
                        "metricCollapseGroup", "metricGrouping") %in% 
                          names(bettrList)))
        plotdata <- bettrList$plotdata
        scoredata <- bettrList$scoredata
        idCol <- bettrList$idCol
        metricCol <- bettrList$metricCol
        valueCol <- bettrList$valueCol
        weightCol <- bettrList$weightCol
        scoreCol <- bettrList$scoreCol
        metricGroupCol <- bettrList$metricGroupCol
        metricInfo <- bettrList$metricInfo
        metricColors <- bettrList$metricColors
        idInfo <- bettrList$idInfo
        idColors <- bettrList$idColors
        metricCollapseGroup <- bettrList$metricCollapseGroup
        metricGrouping <- bettrList$metricGrouping
    }

    if (metricCollapseGroup && !is.null(metricInfo[[metricGrouping]])) {
        metricInfo <- metricInfo |> 
            dplyr::select(dplyr::all_of(metricGrouping)) |>
            dplyr::distinct() |> 
            dplyr::mutate("{ metricCol }" := .data[[metricGrouping]])
    }
    
    ## Matrix -----------------------------------------------------------------
    mat <- plotdata |>
        dplyr::select(dplyr::all_of(c(idCol, metricCol, valueCol))) |>
        tidyr::spread(key = .data[[metricCol]],
                      value = .data[[valueCol]], fill = NA) |>
        as.data.frame() |>
        tibble::column_to_rownames(var = idCol) |>
        as.matrix()
    
    ## Match order of row annotations
    rowAnnot <- scoredata
    rowAnnot <- rowAnnot[match(rownames(mat), 
                               rowAnnot[[idCol]]), ,
                         drop = FALSE] |> as.data.frame()
    rownames(rowAnnot) <- rowAnnot[[idCol]]
    rowAnnot[[idCol]] <- NULL
    if (!is.null(idInfo)) {
        idInfo <- idInfo[match(rownames(mat), 
                               idInfo[[idCol]]), , 
                         drop = FALSE] |> as.data.frame()
        rownames(idInfo) <- idInfo[[idCol]]
        idInfo[[idCol]] <- NULL
    }
    
    ## Match order of column annotations
    colAnnot <- plotdata |>
        dplyr::filter(!duplicated(.data[[metricCol]])) |>
        dplyr::select(dplyr::all_of(c(metricCol, weightCol)),
                      dplyr::contains(metricGroupCol)) 
    if (metricGroupCol %in% colnames(colAnnot)) {
        colAnnot <- colAnnot |>
            dplyr::arrange(.data[[metricGroupCol]])
        mat <- mat[, match(colAnnot[[metricCol]], 
                           colnames(mat)), drop = FALSE]
    }
    colAnnot <- tibble::tibble(
        colAnnot[match(colnames(mat), 
                       colAnnot[[metricCol]]), , 
                 drop = FALSE]) |> 
        dplyr::select(-contains(metricGroupCol)) |>
        as.data.frame()
    rownames(colAnnot) <- colAnnot[[metricCol]]
    colAnnot[[metricCol]] <- NULL
    
    if (!is.null(metricInfo)) {
        metricInfo <- metricInfo[match(colnames(mat), 
                                       metricInfo[[metricCol]]), , 
                                 drop = FALSE] |> as.data.frame()
        rownames(metricInfo) <- metricInfo[[metricCol]]
        metricInfo[[metricCol]] <- NULL
    }
    
    ## Create annotations -----------------------------------------------------
    rowAnnotRight <- ComplexHeatmap::rowAnnotation(
        Score = ComplexHeatmap::anno_barplot(
            rowAnnot[[scoreCol]],
            width = grid::unit(2, "cm"), 
            baseline = 0,
            axis = TRUE,
            axis_param = list(side = "top")),
        annotation_name_side = "top", 
        annotation_name_rot = 0
    )
    
    if (!is.null(idInfo)) {
        rowAnnotLeft <- ComplexHeatmap::rowAnnotation(
            df = idInfo,
            col = idColors
        )
    } else {
        rowAnnotLeft <- NULL
    }
    
    colAnnotTop <- ComplexHeatmap::columnAnnotation(
        Weight = ComplexHeatmap::anno_barplot(
            colAnnot[[weightCol]],
            height = grid::unit(2, "cm"), 
            baseline = 0),
        annotation_name_side = "left",
        annotation_name_rot = 90
    )
    if (!is.null(metricInfo)) {
        colAnnotBottom <- ComplexHeatmap::columnAnnotation(
            df = metricInfo,
            col = metricColors
        )
    } else {
        colAnnotBottom <- NULL
    }
    
    minmat <- min(mat, na.rm = TRUE)
    maxmat <- max(mat, na.rm = TRUE)
    if (minmat < 0) {
        if (maxmat <= 0) {
            heatmapCols <- circlize::colorRamp2(
                c(minmat, maxmat), 
                c("blue", "#EEEEEE")
            )
        } else if (maxmat > 0) {
            heatmapCols <- circlize::colorRamp2(
                c(minmat, 0, maxmat), 
                c("blue", "#EEEEEE", "red")
            )
        }
    } else {
        heatmapCols <- circlize::colorRamp2(
            c(minmat, maxmat), 
            c("#EEEEEE", "red")
        )
    }
    
    if (plotType == "Heatmap") {
        hm <- ComplexHeatmap::Heatmap(
            matrix = mat, name = "Relative\nvalue",
            col = heatmapCols,
            na_col = "white",
            # rect_gp = grid::gpar(col = "white", lwd = 1),
            cluster_rows = FALSE, 
            cluster_columns = FALSE, 
            show_row_names = showRowNames,
            row_names_side = "left",
            row_names_gp = grid::gpar(fontsize = labelSize),
            row_names_max_width = grid::unit(rownamewidth_cm, "cm"),
            column_names_gp = grid::gpar(fontsize = labelSize),
            column_names_max_height = grid::unit(colnameheight_cm, "cm"),
            row_title = idCol,
            column_title = metricCol,
            column_title_side = "bottom",
            top_annotation = colAnnotTop,
            bottom_annotation = colAnnotBottom, 
            right_annotation = rowAnnotRight,
            left_annotation = rowAnnotLeft
        )
    } else if (plotType == "Dot plot") {
        layer_fun <- function(j, i, x, y, w, h, fill) {
            grid::grid.rect(x = x, y = y, width = w, height = h, 
                            gp = grid::gpar(col = NA, fill = NA))
            grid::grid.circle(
                x = x, y = y, r = ComplexHeatmap::pindex(mat, i, j) / 
                    (max(abs(mat), na.rm = TRUE) * 2) * min(grid::unit.c(w, h)),
                gp = grid::gpar(fill = heatmapCols(
                    ComplexHeatmap::pindex(mat, i, j)), 
                    col = "black"))
        }
        hm <- ComplexHeatmap::Heatmap(
            matrix = mat, name = "Relative\nvalue",
            layer_fun = layer_fun, 
            rect_gp = gpar(type = "rect", fill = NA, col = "lightgrey"),
            col = heatmapCols,
            na_col = "white",
            # rect_gp = grid::gpar(col = "white", lwd = 1),
            cluster_rows = FALSE, 
            cluster_columns = FALSE, 
            show_row_names = showRowNames,
            row_names_side = "left",
            row_names_gp = grid::gpar(fontsize = labelSize),
            row_names_max_width = grid::unit(rownamewidth_cm, "cm"),
            column_names_gp = grid::gpar(fontsize = labelSize),
            column_names_max_height = grid::unit(colnameheight_cm, "cm"),
            row_title = idCol,
            column_title = metricCol,
            column_title_side = "bottom",
            top_annotation = colAnnotTop,
            bottom_annotation = colAnnotBottom, 
            right_annotation = rowAnnotRight,
            left_annotation = rowAnnotLeft
        )
    } else {
        stop("Unknown plot type ", plotType)
    }
    ComplexHeatmap::draw(hm)
}