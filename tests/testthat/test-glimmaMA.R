context("glimmaMA")
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

test_that("MA Plot returns widget", 
{
    # MArrayLM, DGEExact/DGELRT
    for (x in list(limmaFit, dgeexact))
    {
        result <- glimmaMA(x, dge=dge)
        expect_equal(is.null(result), FALSE)
    }
    # DESeqDataset
    result <- glimmaMA(dds)
    expect_equal(is.null(result), FALSE)
})

test_that("Saving MA plot works", 
{
    testname <- "testMAabc.html"
    # MArrayLM, DGEExact/DGELRT
    for (x in list(limmaFit, dgeexact))
    {
        result <- glimmaMA(x, dge=dge, html=testname)
        expect_equal(result, NULL)
        expect_equal(file.exists(testname), TRUE)
        unlink(testname)
    }
    # DESeqDataset
    result <- glimmaMA(dds, html=testname)
    expect_equal(result, NULL)
    expect_equal(file.exists(testname), TRUE)
    unlink(testname)
})