context("hasRownames")

test_that("TRUE", {
    x <- data.frame(
        "sample1" = c(1L, 2L),
        "sample2" = c(3L, 4L),
        row.names = c("gene1", "gene2")
    )
    expect_true(hasRownames(x))
})

test_that("FALSE : data.frame sequence row names", {
    x <- data.frame(a = seq_len(2L))
    ok <- hasRownames(x)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("x has sequence row names (soft NULL).")
    )
})

test_that("FALSE : DataFrame NULL", {
    x <- DataFrame(a = seq_len(2L))
    ok <- hasRownames(x)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("x has NULL row names.")
    )
})
