#' @noRd
#' 
#' @importFrom dplyr group_by summarize arrange desc filter select contains
#' @importFrom tidyr spread
#' @importFrom tibble column_to_rownames tibble
#' @importFrom rlang .data :=
#' @importFrom ComplexHeatmap rowAnnotation anno_barplot columnAnnotation 
#'   Heatmap
#' @importFrom grid gpar
#' @importFrom circlize colorRamp2
#' 
.makeHeatmap <- function(df, idCol, metricCol, valueCol, weightCol, scoreCol, 
                         groupCol, metricInfo, idInfo, labelSize, 
                         ordering = "high-to-low", idColors, metricColors,
                         collapseGroup) {
    if (!(ordering %in% c("high-to-low", "low-to-high"))) {
        stop("ordering must be 'high-to-low' or 'low-to-high'")
    }
    
    if (collapseGroup && !is.null(df[[groupCol]])) {
        df <- df %>%
            dplyr::group_by(.data[[idCol]], .data[[groupCol]]) %>%
            dplyr::summarize("{ valueCol }" := mean(.data[[valueCol]]),
                             "{ weightCol }" := mean(.data[[weightCol]])) %>%
            dplyr::mutate("{ metricCol }" := .data[[groupCol]]) %>%
            dplyr::ungroup() %>%
            as.data.frame()
        metricInfo <- metricInfo %>% 
            dplyr::select(.data[[groupCol]]) %>%
            dplyr::distinct() %>% 
            dplyr::mutate("{ metricCol }" := .data[[groupCol]])
    }
    
    ## Get ordering by score --------------------------------------------------
    rowAnnot <- df %>%
        dplyr::group_by(.data[[idCol]]) %>%
        dplyr::summarize(
            "{scoreCol}" := sum(.data[[weightCol]] *
                                    .data[[valueCol]],
                                na.rm = TRUE)
        ) 
    if (ordering == "high-to-low") {
        rowAnnot <- rowAnnot %>%
            dplyr::arrange(dplyr::desc(.data[[scoreCol]]))
    } else {
        rowAnnot <- rowAnnot %>%
            dplyr::arrange(.data[[scoreCol]])
    }
    
    ## Matrix -----------------------------------------------------------------
    tmp <- df
    tmp[[idCol]] <- factor(tmp[[idCol]], 
                           levels = rowAnnot[[idCol]])
    mat <- tmp %>%
        dplyr::select(c(.data[[idCol]],
                        .data[[metricCol]],
                        .data[[valueCol]])) %>%
        tidyr::spread(key = .data[[metricCol]],
                      value = .data[[valueCol]], fill = NA) %>%
        as.data.frame() %>%
        tibble::column_to_rownames(var = idCol) %>%
        as.matrix()

    ## Match order of row annotations
    rowAnnot <- rowAnnot[match(rownames(mat), 
                               rowAnnot[[idCol]]), ,
                         drop = FALSE] %>% as.data.frame()
    rownames(rowAnnot) <- rowAnnot[[idCol]]
    rowAnnot[[idCol]] <- NULL
    if (!is.null(idInfo)) {
        idInfo <- idInfo[match(rownames(mat), 
                               idInfo[[idCol]]), , 
                         drop = FALSE] %>% as.data.frame()
        rownames(idInfo) <- idInfo[[idCol]]
        idInfo[[idCol]] <- NULL
    }

    ## Match order of column annotations
    colAnnot <- df %>%
        dplyr::filter(!duplicated(.data[[metricCol]])) %>%
        dplyr::select(c(metricCol, weightCol,
                        dplyr::contains(groupCol))) 
    if (groupCol %in% colnames(colAnnot)) {
        colAnnot <- colAnnot %>%
            dplyr::arrange(.data[[groupCol]])
        mat <- mat[, match(colAnnot[[metricCol]], 
                           colnames(mat)), drop = FALSE]
    }
    colAnnot <- tibble::tibble(
        colAnnot[match(colnames(mat), 
                       colAnnot[[metricCol]]), , 
                 drop = FALSE]) %>% 
        dplyr::select(-contains(groupCol)) %>%
        as.data.frame()
    rownames(colAnnot) <- colAnnot[[metricCol]]
    colAnnot[[metricCol]] <- NULL

    if (!is.null(metricInfo)) {
        metricInfo <- metricInfo[match(colnames(mat), 
                                       metricInfo[[metricCol]]), , 
                                 drop = FALSE] %>% as.data.frame()
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
            heatmapCols = circlize::colorRamp2(
                c(minmat, maxmat), 
                c("blue", "#EEEEEE")
            )
        } else if (maxmat > 0) {
            heatmapCols = circlize::colorRamp2(
                c(minmat, 0, maxmat), 
                c("blue", "#EEEEEE", "red")
            )
        }
    } else {
        heatmapCols = circlize::colorRamp2(
            c(minmat, maxmat), 
            c("#EEEEEE", "red")
        )
    }

    ComplexHeatmap::Heatmap(
        matrix = mat, name = "Relative\nvalue",
        col = heatmapCols,
        na_col = "white",
        rect_gp = grid::gpar(col = "white", lwd = 1),
        cluster_rows = FALSE, 
        cluster_columns = FALSE, 
        row_names_side = "left",
        row_names_gp = grid::gpar(fontsize = labelSize),
        column_names_gp = grid::gpar(fontsize = labelSize),
        row_title = idCol,
        column_title = metricCol,
        column_title_side = "bottom",
        top_annotation = colAnnotTop,
        bottom_annotation = colAnnotBottom, 
        right_annotation = rowAnnotRight,
        left_annotation = rowAnnotLeft
    )
}