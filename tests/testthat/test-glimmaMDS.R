context("glimmaMDS")
library(GlimmaV2)
library(edgeR)

test_that("MDS error when x has < 3 dimensions", 
{
    files_under <- c("testdata/GSM1545535_10_6_5_11.txt", "testdata/GSM1545536_9_6_5_11.txt")
    files_enough <- c("testdata/GSM1545535_10_6_5_11.txt", "testdata/GSM1545536_9_6_5_11.txt",
                        "testdata/GSM1545538_purep53.txt")
    x1 <- readDGE(files_under, columns=c(1,3))
    x2 <- readDGE(files_enough, columns=c(1,3))
    expect_error(glimmaMDS(x1))
    expect_silent(glimmaMDS(x2))
})