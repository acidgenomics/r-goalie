context("check : scalar : hasRownames")

test_that("hasRownames", {
    mapply(
        fun = list(
            data.frame,
            DataFrame,
            data.table,
            tibble
        ),
        cause = c(
            "{.var data} has integer row names (soft {.val NULL}).",
            "{.var data} has {.val NULL} row names.",
            "{.cls data.table} class doesn't support row names.",
            "{.cls tbl_df} class doesn't support row names."
        ),
        FUN = function(fun, cause) {
            data <- fun()
            ok <- hasRownames(data)
            expect_false(ok)
            expect_s4_class(ok, "goalie")
            expect_identical(cause(ok), cause)
        },
        SIMPLIFY = FALSE
    )
})

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
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "{.var x} has integer row names (soft {.val NULL})."
    )
})

test_that("FALSE : DataFrame NULL", {
    x <- DataFrame(a = seq_len(2L))
    ok <- hasRownames(x)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "{.var x} has {.val NULL} row names."
    )
})
