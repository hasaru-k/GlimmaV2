context("Test as.hexcol")

test_that("hex colour tools are correct", {
    expect_false(Glimma:::is.hex("#12345"))
    expect_false(Glimma:::is.hex("#1234567"))
    expect_false(Glimma:::is.hex("#123456789"))
    expect_false(Glimma:::is.hex("#1122gg"))

    expect_true(Glimma:::is.hex("#123456"))
    expect_true(Glimma:::is.hex("#ffffff"))
    expect_true(Glimma:::is.hex("#FFFFFF"))

    expect_equal(Glimma:::as.hexcol("red"), "#ff0000")
    expect_equal(Glimma:::as.hexcol("green"), "#00ff00")
    expect_equal(Glimma:::as.hexcol("blue"), "#0000ff")

    expect_warning(Glimma:::as.hexcol(0))
    expect_equal(Glimma:::as.hexcol(1), "#000000")

    # R4.0.0 changed palette
    if (getRversion() < "4.0.0") {
        expect_equal(Glimma:::as.hexcol(2), "#ff0000")
    } else {
        expect_equal(Glimma:::as.hexcol(2), "#df536b")
    }
})
