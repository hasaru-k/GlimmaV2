library(Glimma)
library(GlimmaV2)
library(edgeR)

data(lymphomaRNAseq)
lymphomaRNAseq$samples$group <- gsub(
  "Smchd1-null", "Mut", lymphomaRNAseq$samples$group
)

lymphomaRNAseq <- calcNormFactors(lymphomaRNAseq)

des <- model.matrix(~lymphomaRNAseq$samples$group)
fit <- estimateDisp(lymphomaRNAseq, design=des)
et <- exactTest(fit, pair=c("WT", "Mut"))

glimmaMA(et, counts=lymphomaRNAseq$counts, groups=lymphomaRNAseq$samples$group, width=1200, height=1000)
glimmaMA(et, dge=lymphomaRNAseq, width=1200, height=1000)
