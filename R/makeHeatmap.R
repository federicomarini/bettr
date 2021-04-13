#' @keywords internal
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
                         groupCol, ordering = "high-to-low") {
    if (!(ordering %in% c("high-to-low", "low-to-high"))) {
        stop("ordering must be 'high-to-low' or 'low-to-high'")
    }
    
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
    rowAnnot <- rowAnnot[match(rownames(mat), 
                               rowAnnot[[idCol]]), ,
                         drop = FALSE] %>% as.data.frame() %>%
        tibble::column_to_rownames(var = idCol)
    
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
                 drop = FALSE]) %>% as.data.frame() %>%
        tibble::column_to_rownames(var = metricCol)
    
    rowAnnot <- ComplexHeatmap::rowAnnotation(
        Score = ComplexHeatmap::anno_barplot(
            rowAnnot[[scoreCol]],
            width = grid::unit(2, "cm"), 
            baseline = 0,
            axis = TRUE,
            axis_param = list(side = "top"))
    )
    if (groupCol %in% colnames(colAnnot)) {
        colAnnot <- ComplexHeatmap::columnAnnotation(
            Weight = ComplexHeatmap::anno_barplot(
                colAnnot[[weightCol]],
                height = grid::unit(2, "cm"), 
                baseline = 0),
            Group = colAnnot[[groupCol]]
        )
    } else {
        colAnnot <- ComplexHeatmap::columnAnnotation(
            Weight = ComplexHeatmap::anno_barplot(
                colAnnot[[weightCol]],
                height = grid::unit(2, "cm"), 
                baseline = 0)
        )
    }
    
    heatmapCols = circlize::colorRamp2(
        seq(min(mat), max(mat), length = 3), 
        c("blue", "#EEEEEE", "red")
    )
    
    ComplexHeatmap::Heatmap(
        matrix = mat, name = "Relative\nvalue",
        col = heatmapCols,
        na_col = "lightgrey",
        rect_gp = grid::gpar(col = "white", lwd = 1),
        cluster_rows = FALSE, 
        cluster_columns = FALSE, 
        row_names_side = "left",
        top_annotation = colAnnot, 
        right_annotation = rowAnnot
    )
}