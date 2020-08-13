context("glimmaVolcano")
library(GlimmaV2)
library(edgeR)
library(Glimma)
library(DESeq2)

setup
({
    data(lymphomaRNAseq)
    dge <- lymphomaRNAseq
    dge <- calcNormFactors(dge)
    des <- model.matrix(~dge$samples$group)

    # MArrayLM
    v <- voomWithQualityWeights(dge, design = des, plot = FALSE)
    limmaFit <- lmFit(v, des)
    limmaFit <- eBayes(limmaFit)

    # DGEExact
    fit <- estimateDisp(dge, design=des)
    dgeexact <- exactTest(fit)

    # DESeqDataset
    dds <- DESeqDataSetFromMatrix(
        countData = dge$counts,
        colData = dge$samples,
        rowData = dge$genes,
        design = ~genotype)
    dds <- DESeq(dds)

})

test_that("Volcano plot returns widget", 
{
    # MArrayLM, DGEExact/DGELRT
    for (x in list(limmaFit, dgeexact))
    {
        result <- glimmaVolcano(x, dge=dge)
        expect_equal(is.null(result), FALSE)
    }
    # DESeqDataset
    result <- glimmaVolcano(dds)
    expect_equal(is.null(result), FALSE)
})

test_that("Saving MA plot works", 
{
    testname <- "testMAabc.html"
    # MArrayLM, DGEExact/DGELRT
    for (x in list(limmaFit, dgeexact))
    {
        result <- glimmaVolcano(x, dge=dge, html=testname)
        expect_equal(result, NULL)
        expect_equal(file.exists(testname), TRUE)
        unlink(testname)
    }
    # DESeqDataset
    result <- glimmaVolcano(dds, html=testname)
    expect_equal(result, NULL)
    expect_equal(file.exists(testname), TRUE)
    unlink(testname)
})