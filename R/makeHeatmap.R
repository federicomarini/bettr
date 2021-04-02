#' @keywords internal
#' 
#' @importFrom dplyr group_by summarize arrange desc filter select contains
#' @importFrom tidyr spread
#' @importFrom tibble column_to_rownames tibble
#' @importFrom rlang sym
#' @importFrom ComplexHeatmap rowAnnotation anno_barplot columnAnnotation 
#'   Heatmap
#' @importFrom grid gpar
#' @importFrom circlize colorRamp2
#' 
.makeHeatmap <- function(df, idCol, scoreCol, weightCol, valueCol, metricCol,
                         groupCol) {
    rowAnnot <- df %>%
        dplyr::group_by(!!rlang::sym(idCol)) %>%
        dplyr::summarize(
            "{scoreCol}" := sum(!!rlang::sym(weightCol) *
                                    !!rlang::sym(valueCol),
                                na.rm = TRUE)
        ) %>%
        dplyr::arrange(dplyr::desc(!!rlang::sym(scoreCol)))
    tmp <- df
    tmp[[idCol]] <- factor(tmp[[idCol]], 
                           levels = rowAnnot[[idCol]])
    mat <- tmp %>%
        dplyr::select(c(!!rlang::sym(idCol),
                        !!rlang::sym(metricCol),
                        !!rlang::sym(valueCol))) %>%
        tidyr::spread(key = !!rlang::sym(metricCol),
                      value = !!rlang::sym(valueCol), fill = NA) %>%
        as.data.frame() %>%
        tibble::column_to_rownames(var = idCol) %>%
        as.matrix()
    rowAnnot <- rowAnnot[match(rownames(mat), 
                               rowAnnot[[idCol]]), ,
                         drop = FALSE] %>% as.data.frame() %>%
        tibble::column_to_rownames(var = idCol)
    
    colAnnot <- df %>%
        dplyr::filter(!duplicated(!!rlang::sym(metricCol))) %>%
        dplyr::select(c(metricCol, weightCol,
                        dplyr::contains(groupCol))) 
    if (groupCol %in% colnames(colAnnot)) {
        colAnnot <- colAnnot %>%
            dplyr::arrange(!!rlang::sym(groupCol))
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