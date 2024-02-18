#' @importFrom dplyr arrange filter select contains all_of
#' @importFrom tidyr spread
#' @importFrom tibble column_to_rownames tibble
#' @importFrom rlang .data :=
#' @importFrom ComplexHeatmap rowAnnotation anno_barplot columnAnnotation 
#'   Heatmap draw pindex
#' @importFrom grid gpar unit grid.rect grid.circle unit.c
#' @importFrom circlize colorRamp2
.makeHeatmap <- function(df, scores, idCol, metricCol, valueCol, weightCol, 
                         scoreCol, metricGroupCol, metricInfo, idInfo,
                         labelSize, idColors, metricColors,
                         metricCollapseGroup, metricGrouping, showRowNames, 
                         plotType = "Heatmap",
                         rownamewidth_cm = 6, colnameheight_cm = 6) {

    if (metricCollapseGroup && !is.null(metricInfo[[metricGrouping]])) {
        metricInfo <- metricInfo |> 
            dplyr::select(dplyr::all_of(metricGrouping)) |>
            dplyr::distinct() |> 
            dplyr::mutate("{ metricCol }" := .data[[metricGrouping]])
    }
    
    ## Matrix -----------------------------------------------------------------
    mat <- df |>
        dplyr::select(dplyr::all_of(c(idCol, metricCol, valueCol))) |>
        tidyr::spread(key = .data[[metricCol]],
                      value = .data[[valueCol]], fill = NA) |>
        as.data.frame() |>
        tibble::column_to_rownames(var = idCol) |>
        as.matrix()
    
    ## Match order of row annotations
    rowAnnot <- scores
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
    colAnnot <- df |>
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