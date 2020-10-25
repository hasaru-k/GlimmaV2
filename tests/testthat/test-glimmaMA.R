context("glimmaMA")
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

test_that("Length of the status.colours must be exactly 3",
{
    # MArrayLM, DGEExact/DGELRT
    for (x in list(limmaFit, dgeexact))
    {
        expect_error(glimmaMA(x, dge=dge, status.cols=c("green","red")))
    }
    # DESeqDataset
    expect_error(glimmaMA(dds, status.cols=c("green","red")))
})

test_that("Length of status vector must match the other args",
{
    rand <- 25000
    # MArrayLM, DGEExact/DGELRT
    for (x in list(limmaFit, dgeexact))
    {
        expect_error(glimmaMA(x, dge=dge, status=rep(0, rand)))
        expect_silent(glimmaMA(x, dge=dge, status=rep(0, nrow(x))))
    }
    # DESeqDataset
    expect_error(glimmaMA(dds, status=rep(0, rand)))
    expect_message(glimmaMA(dds, status=rep(0, nrow(dds))), "genes were filtered out in DESeq2 tests")
})

test_that("User cannot provide counts argument without groups argument for edgeR/limma objects",
{
    # MArrayLM, DGEExact/DGELRT
    for (x in list(limmaFit, dgeexact))
    {
        expect_error(glimmaMA(x, counts=dge$counts))
    }
})
