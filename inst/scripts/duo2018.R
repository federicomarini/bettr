suppressPackageStartupMessages({
    library(DuoClustering2018)
    library(ExperimentHub)
    library(dplyr)
    library(tidyr)
    library(bettr)
})

shannonEntropy <- function(clusterAssignments) {
    p <- c(table(clusterAssignments)) / length(clusterAssignments)
    -1.0 * sum(p * log2(p))
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
resSummary <- res |>
    dplyr::group_by(dataset, method, run, k) |>
    dplyr::filter(!is.na(cluster)) |>
    dplyr::summarize(ARI = mclust::adjustedRandIndex(cluster, trueclass),
                     truenclust = length(unique(trueclass)),
                     estnclust = unique(est_k),
                     elapsed = stats::median(elapsed),
                     s = shannonEntropy(cluster),
                     s.true = shannonEntropy(trueclass),
                     s.norm = s / log2(unique(k)),
                     s.true.norm = s.true / log2(unique(k))) |>
    dplyr::ungroup()

## Summarize across runs
resSummaryMedians <- resSummary |>
    dplyr::group_by(dataset, method, k) |>
    dplyr::summarize(across(everything(), ~ stats::median(.x)))

## Only keep results for the true k
resSummaryTrueK <- dplyr::filter(resSummaryMedians, k == truenclust)

## Compare to 'true' values of entropy and nclust
resSummaryTrueK <- resSummaryTrueK |>
    dplyr::mutate(s.norm.vs.true = abs(s.norm - s.true.norm),
                  nclust.vs.true = abs(estnclust - truenclust)) |>
    dplyr::select(dataset, method, ARI, elapsed, s.norm.vs.true,
                  nclust.vs.true)

## Reshape to one metric per dataset
resSummaryWide <- resSummaryTrueK |>
    dplyr::mutate(dataset = sub("sce_filteredExpr10_", "", dataset,
                                fixed = TRUE)) |>
    tidyr::pivot_wider(names_from = "dataset",
                       values_from = c("ARI", "elapsed", "s.norm.vs.true",
                                       "nclust.vs.true"))

## Create metric info
metricInfo <- data.frame(Metric = setdiff(colnames(resSummaryWide), "method")) |>
    dplyr::mutate(Class = vapply(strsplit(Metric, "_"), .subset, 1L, FUN.VALUE = ""))

## Define colors
metricColors <- list(Class = c(ARI = "purple", elapsed = "forestgreen",
                               nclust.vs.true = "blue",
                               s.norm.vs.true = "orange"))
methodColors <- c(
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
for (nm in grep("elapsed", colnames(resSummaryWide), value = TRUE,
                fixed = TRUE)) {
    initialTransforms[[nm]] <- list(flip = TRUE, transform = "[0,1]")
}
for (nm in grep("nclust.vs.true", colnames(resSummaryWide), value = TRUE)) {
    initialTransforms[[nm]] <- list(flip = TRUE, transform = "[0,1]")
}
for (nm in grep("s.norm.vs.true", colnames(resSummaryWide), value = TRUE)) {
    initialTransforms[[nm]] <- list(flip = TRUE, transform = "[0,1]")
}

saveRDS(list(df = resSummaryWide, metricInfo = metricInfo,
             initialTransforms = initialTransforms,
             idColors = list(method = methodColors),
             metricColors = metricColors),
        file = file.path("inst", "extdata", "duo2018.rds"))

write.table(resSummaryWide, file = file.path("inst", "extdata",
                                             "duo2018_results.csv"),
            sep = ",", quote = FALSE, row.names = FALSE,
            col.names = TRUE)

se <- assembleSE(df = resSummaryWide, idCol = "method",
                 initialWeights = NULL,
                 initialTransforms = initialTransforms,
                 idColors = list(method = methodColors),
                 metricColors = metricColors, metricInfo = metricInfo)
saveRDS(se, file = file.path("inst", "extdata", "duo2018se.rds"))
