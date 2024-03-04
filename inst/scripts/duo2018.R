suppressPackageStartupMessages({
    library(DuoClustering2018)
    library(ExperimentHub)
    library(dplyr)
    library(tidyr)
    library(bettr)
})

shannon_entropy <- function(cluster_assignments) {
    p <- c(table(cluster_assignments)) / length(cluster_assignments)
    -1 * sum(p * log2(p))
}

## Query the ExperimentHub package for all relevant resources
eh <- ExperimentHub()
eh <- query(eh, "DuoClustering")
eh <- query(eh, "_v2")
eh <- query(eh, "clustering_summary")

## Use only Expr filtering
eh <- query(eh, "filteredExpr")

length(eh)
names(eh)

res <- lapply(names(eh), function(e) {
    eh[[e]]
})
res <- do.call(dplyr::bind_rows, res)

## Summarize performance
res_summary <- res %>%
    dplyr::group_by(dataset, method, run, k) %>%
    dplyr::filter(!is.na(cluster)) %>%
    dplyr::summarize(ARI = mclust::adjustedRandIndex(cluster, trueclass),
                     truenclust = length(unique(trueclass)),
                     estnclust = unique(est_k),
                     elapsed = stats::median(elapsed),
                     s = shannon_entropy(cluster),
                     s.true = shannon_entropy(trueclass),
                     s.norm = s/log2(unique(k)),
                     s.true.norm = s.true/log2(unique(k))) %>%
    dplyr::ungroup()

## Summarize across runs
res_summary_medians <- res_summary %>%
    dplyr::group_by(dataset, method, k) %>%
    dplyr::summarize(across(everything(), ~ stats::median(.x)))

## Only keep results for the true k
res_summary_truek <- res_summary_medians %>%
    dplyr::filter(k == truenclust)

## Compare to 'true' values of entropy and nclust
res_summary_truek <- res_summary_truek %>%
    dplyr::mutate(s.norm.vs.true = abs(s.norm - s.true.norm),
                  nclust.vs.true = abs(estnclust - truenclust)) %>%
    dplyr::select(dataset, method, ARI, elapsed, s.norm.vs.true, 
                  nclust.vs.true)

## Reshape to one metric per dataset
res_summary_wide <- res_summary_truek %>%
    dplyr::mutate(dataset = sub("sce_filteredExpr10_", "", dataset)) %>%
    tidyr::pivot_wider(names_from = c("dataset"),
                       values_from = c("ARI", "elapsed", "s.norm.vs.true", 
                                       "nclust.vs.true"))

## Create metric info
metricInfo <- data.frame(Metric = setdiff(colnames(res_summary_wide), "method")) %>%
    dplyr::mutate(Class = sapply(strsplit(Metric, "_"), .subset, 1))

## Define colors
metric_colors <- list(Class = c(ARI = "purple", elapsed = "forestgreen", 
                                nclust.vs.true = "blue", 
                                s.norm.vs.true = "orange"))
method_colors <- c(
    CIDR = "#332288", FlowSOM = "#6699CC", PCAHC = "#88CCEE", 
    PCAKmeans = "#44AA99", pcaReduce = "#117733",
    RtsneKmeans = "#999933", Seurat = "#DDCC77", SC3svm = "#661100", 
    SC3 = "#CC6677", TSCAN = "grey34", ascend = "orange", SAFE = "black",
    monocle = "red", RaceID2 = "blue"
)

## Define initial transformations
## elapsed - z-score, flip
## nclust.vs.true - [0, 1], flip
## s.norm.vs.true - [0, 1], flip
initialTransforms <- list()
for (nm in grep("elapsed", colnames(res_summary_wide), value = TRUE)) {
    initialTransforms[[nm]] <- list(flip = TRUE, transform = '[0,1]')
}
for (nm in grep("nclust.vs.true", colnames(res_summary_wide), value = TRUE)) {
    initialTransforms[[nm]] <- list(flip = TRUE, transform = '[0,1]')
}
for (nm in grep("s.norm.vs.true", colnames(res_summary_wide), value = TRUE)) {
    initialTransforms[[nm]] <- list(flip = TRUE, transform = '[0,1]')
}

saveRDS(list(df = res_summary_wide, metricInfo = metricInfo, 
             initialTransforms = initialTransforms, 
             idColors = list(method = method_colors), 
             metricColors = metric_colors), 
        file = "inst/extdata/duo2018.rds")

se <- assembleSE(df = res_summary_wide, idCol = "method", 
                 initialWeights = NULL, 
                 initialTransforms = initialTransforms, 
                 idColors = list(method = method_colors), 
                 metricColors = metric_colors, metricInfo = metricInfo)
saveRDS(se, file = "inst/extdata/duo2018se.rds")
