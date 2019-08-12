context("check : scalar : validNames")

test_that("TRUE", {
    ## Dots (periods) and underscores are valid.
    expect_true(validNames(c("sample.1", "sample_1")))
    ## Can't begin with a number.
    expect_false(validNames("293cells"))
})

test_that("FALSE", {
    ## Spaces, dashes (hyphens), and other non-alphanumerics aren't valid.
    expect_false(validNames("sample 1"))
    expect_false(validNames("cell-AAAAAAAA"))
    expect_false(validNames("GFP+"))
    expect_false(validNames(c("gene1", "gene1", "gene2", "gene2")))
})
