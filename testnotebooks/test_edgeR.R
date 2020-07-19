library(Glimma)
library(edgeR)

data(lymphomaRNAseq)
lymphomaRNAseq$samples$group <- gsub(
  "Smchd1-null", "Mut", lymphomaRNAseq$samples$group
)

lymphomaRNAseq <- calcNormFactors(lymphomaRNAseq)

des <- model.matrix(~lymphomaRNAseq$samples$group)
fit <- estimateDisp(lymphomaRNAseq, design=design)
et <- exactTest(fit, pair=c("WT", "Mut"))

temp_dir <- tempdir()

with(
  lymphomaRNAseq,
  glMDPlot(
    et,
    counts = counts,
    anno = genes,
    groups = samples$group,
    samples = samples$sampleID,
    display.columns = c("Symbols", "GeneID"),
    status = decideTestsDGE(et),
    path = temp_dir
  )
)

glimmaMA(fit, counts=counts, groups=groups, width=1200, height=1000)
