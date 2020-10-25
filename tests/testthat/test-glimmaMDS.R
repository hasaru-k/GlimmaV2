context("glimmaMDS")
library(Glimma)
library(edgeR)
library(DESeq2)

setup
({
    dge <- readRDS(system.file("RNAseq123/dge.rds", package = "Glimma"))

    # DESeqDataset
    dds <- DESeq2::DESeqDataSetFromMatrix(
        countData = dge$counts,
        colData = dge$samples,
        rowData = dge$genes,
        design = ~group)
})

test_that("MDS error when x has < 3 dimensions",
{
    for (x in list(dge, dds))
    {
        expect_error(glimmaMDS(x[, 1:2]))
        expect_silent(glimmaMDS(x[, 1:3]))
    }
})

test_that("glimmaMDS doesn't export HTML file unless you ask it to",
{
    for (x in list(dge, dds))
    {
        result <- glimmaMDS(x)
        expect_equal(is.null(result), FALSE)
    }
})

test_that("Saving MDS works",
{
    testname <- "testMDSabc.html"
    for (x in list(dge, dds))
    {
        result <- glimmaMDS(dge, html=testname)
        expect_equal(result, NULL)
        expect_equal(file.exists(testname), TRUE)
        unlink(testname)
    }
})
