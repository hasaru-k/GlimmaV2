context("helperFunctions")

test_that("Documentation functions return character vectors",
{
    expect_equal(class(MA_details()), "character")
    expect_equal(class(volcano_details()), "character")
    expect_equal(class(XY_details()), "character")
    expect_equal(class(MDS_details()), "character")
})
