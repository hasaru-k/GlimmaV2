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

test_that("HTML arg exports the MA plot",
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

test_that("DGE argument must have same length as limma/edgeR objects",
{
    sample <- 1:(nrow(dge)-10)
    # MArrayLM, DGEExact/DGELRT
    for (x in list(limmaFit, dgeexact))
    {
        expect_error(glimmaMA(x, dge=dge[sample,]))
    }
})

test_that("Providing counts warns the user of log transformation",
{
    # MArrayLM, DGEExact/DGELRT
    for (x in list(limmaFit, dgeexact))
    {
        expect_message(glimmaMA(x, counts=dge$counts, groups=dge$samples$group))
    }
})

test_that("NULL sample.colours arg leads to -1 transmitted to JS frontend",
{
    # MArrayLM, DGEExact/DGELRT
    for (x in list(limmaFit, dgeexact))
    {
        xData <- glimmaMA(x, dge=dge)$x$data
        expect_equal(xData$sampleColours, -1)
    }
    xData <- glimmaMA(dds)$x$data
    expect_equal(xData$sampleColours, -1)
})

test_that("Vector sample.colours arg leads to vector transmitted to JS frontend",
{
    testcols <- rep("red", ncol(dge))
    # MArrayLM, DGEExact/DGELRT
    for (x in list(limmaFit, dgeexact))
    {
        xData <- glimmaMA(x, dge=dge, sample.cols=testcols)$x$data
        expect_equal(xData$sampleColours, testcols)
    }
    xData <- glimmaMA(dds, sample.cols=testcols)$x$data
    expect_equal(xData$sampleColours, testcols)
})

test_that("Setting transform.counts = cpm transforms accordingly",
{
    expected_counts <- edgeR::cpm(dge$counts, log=FALSE)
    # MArrayLM, DGEExact/DGELRT
    for (x in list(limmaFit, dgeexact))
    {
        xData <- glimmaMA(x, dge=dge, transform.counts="cpm")$x$data
        expect_true(all(xData$counts==expected_counts))
    }
    # can't test DESeq2 because some genes are filtered out
})

test_that("Specifying RPKM transformation without length column gives error",
{
    # MArrayLM, DGEExact/DGELRT
    for (x in list(limmaFit, dgeexact))
    {
        expect_error(glimmaMA(x, dge=dge, transform.counts="rpkm"))
    }
    expect_error(glimmaMA(dds, transform.counts="rpkm"))
})


test_that("Specifying RPKM transformation with non-numeric length gives an error",
{
    # MArrayLM, DGEExact/DGELRT
    for (x in list(limmaFit, dgeexact))
    {
        x$genes$length <- "123abc"
        expect_error(glimmaMA(x, dge=dge, transform.counts="rpkm"))
        x$genes$length <- NULL
    }
})

test_that("Setting transform.counts = rpkm transforms accordingly",
{
    expected_counts <- edgeR::rpkm(dge$counts, gene.length=1234)
    # MArrayLM, DGEExact/DGELRT
    for (x in list(limmaFit, dgeexact))
    {
        x$genes$length <- 1234
        xData <- glimmaMA(x, dge=dge, transform.counts="rpkm")$x$data
        expect_true(all(xData$counts==expected_counts))
    }
    # can't test DESeq2 because some genes are filtered out
})

test_that("Setting transform.counts = logrpkm transforms accordingly",
{
    expected_counts <- edgeR::rpkm(dge$counts, gene.length=1234, log = TRUE)
    # MArrayLM, DGEExact/DGELRT
    for (x in list(limmaFit, dgeexact))
    {
        x$genes$length <- 1234
        xData <- glimmaMA(x, dge=dge, transform.counts="logrpkm")$x$data
        expect_true(all(xData$counts==expected_counts))
    }
    # can't test DESeq2 because some genes are filtered out
})

test_that("Display.columns arg provides a minimal set of [xlab, ylab, geneID]",
{
    # MArrayLM, DGEExact/DGELRT
    for (x in list(limmaFit, dgeexact))
    {
        xData <- glimmaMA(x, dge=dge, xlab="x", ylab="y", display.columns=c("SYMBOL"))$x$data
        expect_true(setequal(xData$cols, c("x", "y", "gene", "SYMBOL")))
    }
    # can't test DESeq2 because some genes are filtered out
    xData <- glimmaMA(dds, xlab="x", ylab="y", display.columns=c("SYMBOL"))$x$data
    expect_true(setequal(xData$cols, c("x", "y", "gene", "SYMBOL")))
})
