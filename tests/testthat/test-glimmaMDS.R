context("glimmaMDS")
library(GlimmaV2)
library(edgeR)
library(Glimma)
library(DESeq2)

setup
({
    data(lymphomaRNAseq)
    dge <- lymphomaRNAseq
    dds <- DESeqDataSetFromMatrix(countData = dge$counts,
                                  colData = dge$samples,
                                  rowData = dge$genes,
                                  design = ~genotype)
})

test_that("MDS error when x has < 3 dimensions", 
{
    for (x in list(dge, dds))
    {
        expect_error(glimmaMDS(x[, 1:2]))
        expect_silent(glimmaMDS(x[, 1:3]))
    }
})

test_that("Saving MDS with no filename arg works",
{
    for (x in list(dge, dds))
    {
        glimmaMDS(x, save=TRUE)
        expect_equal(file.exists("glimmaMDS.html"), TRUE)
        unlink("glimmaMDS.html")
    }
})

test_that("Saving MDS with filename argument works", 
{
    testname <- "testMDSabc.html"
    for (x in list(dge, dds))
    {
        glimmaMDS(dge, save=TRUE, filename=testname)
        expect_equal(file.exists(testname), TRUE)
        unlink(testname)
    }
})