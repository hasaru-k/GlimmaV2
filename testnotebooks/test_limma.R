library(Glimma)
library(limma)

data(lymphomaRNAseq)
lymphomaRNAseq$samples$group <- gsub(
  "Smchd1-null", "Mut", lymphomaRNAseq$samples$group
)

lymphomaRNAseq <- calcNormFactors(lymphomaRNAseq)

des <- model.matrix(~lymphomaRNAseq$samples$genotype)
v <- voomWithQualityWeights(lymphomaRNAseq, design = des, plot = FALSE)
fit <- lmFit(v, des)
fit <- eBayes(fit)

temp_dir <- temp_dir()

with(
  lymphomaRNAseq,
  glMDPlot(
    fit,
    counts = counts,
    anno = genes,
    groups = samples$group,
    samples = samples$sample,
    status = decideTests(fit)[, 2],
    sample.cols = as.numeric(samples$group),
    path = temp_dir
  )
)

with(lymphomaRNAseq,
glimmaMA(fit, counts=counts, groups=samples$group, width=1200, height=1000)
)
