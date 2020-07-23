library(Glimma)
library(GlimmaV2)
library(DESeq2)

data(lymphomaRNAseq)
lymphomaRNAseq$samples$genotype <- gsub(
  "Smchd1-null", "Mut", lymphomaRNAseq$samples$genotype
)
lymphomaRNAseq$samples$genotype <- as.factor(lymphomaRNAseq$samples$genotype)

dds <- DESeqDataSetFromMatrix(
  countData = lymphomaRNAseq$counts,
  colData = lymphomaRNAseq$samples,
  rowData = lymphomaRNAseq$genes,
  design = ~genotype
)
dds <- DESeq(dds, quiet=TRUE)

glimmaMA(dds, width=1200, height=1000)
