context("glimmaXY")
library(Glimma)
library(edgeR)

setup
({
    dge <- readRDS(system.file("RNAseq123/dge.rds", package = "Glimma"))
    dge <- calcNormFactors(dge)
    des <- model.matrix(~dge$samples$group)
    v <- voomWithQualityWeights(dge, design = des, plot = FALSE)
    limmaFit <- lmFit(v, des)
    limmaFit <- eBayes(limmaFit)
    coef <- 1
    fc <- limmaFit$coefficients[, coef]
    sig <- -log10(limmaFit$p.value[, coef])
})

test_that("XY plot returns widget",
{
    result <- glimmaXY(x=fc, y =sig, counts=dge$counts, groups=dge$samples$group)
    expect_equal(is.null(result), FALSE)
})

test_that("Saving XY plot works",
{
    testname <- "testXYabc.html"
    result <- glimmaXY(x=fc, y =sig, counts=dge$counts, groups=dge$samples$group, html=testname)
    expect_equal(result, NULL)
    expect_equal(file.exists(testname), TRUE)
    unlink(testname)
})

test_that("X and Y args must have the same length",
{
    expect_error(glimmaXY(x=1:3, y=1:4))
    expect_silent(glimmaXY(x=1:4, y=1:4))
})
