library(Glimma)
library(GlimmaV2)
library(limma)
library(edgeR)

data(lymphomaRNAseq)
lymphomaRNAseq$samples$group <- gsub(
  "Smchd1-null", "Mut", lymphomaRNAseq$samples$group
)

lymphomaRNAseq <- calcNormFactors(lymphomaRNAseq)

des <- model.matrix(~lymphomaRNAseq$samples$genotype)
v <- voomWithQualityWeights(lymphomaRNAseq, design = des, plot = FALSE)
fit <- lmFit(v, des)
fit <- eBayes(fit)

glimmaMA(fit, counts=lymphomaRNAseq$counts, groups=lymphomaRNAseq$samples$group, width=1200, height=1000)

glimmaMA(fit, dge=lymphomaRNAseq, width=1200, height=1000)
