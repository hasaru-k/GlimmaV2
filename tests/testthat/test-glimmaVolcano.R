context("glimmaVolcano")
library(Glimma)
library(edgeR)
library(DESeq2)

setup
({
    dge <- readRDS(system.file("RNAseq123/dge.rds", package = "Glimma"))
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
    dds <- DESeq2::DESeqDataSetFromMatrix(
        countData = dge$counts,
        colData = dge$samples,
        rowData = dge$genes,
        design = ~group)
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

test_that("Saving volcano plot works",
{
    testname <- "testVolcano.html"
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
