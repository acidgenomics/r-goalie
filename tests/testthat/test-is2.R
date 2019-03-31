context("is2")

test_that("multiple classes", {
    ok <- is2(1L, class = c("atomic", "numeric", "integer"))
    expect_identical(
        object = ok,
        expected = c(
            atomic = TRUE,
            numeric = TRUE,
            integer = TRUE
        )
    )
})

test_that("scalar failure", {
    ok <- is2(data.frame(), class = "DataFrame")
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        cause(ok),
        noquote(paste(
            "data.frame() is not of class 'DataFrame';",
            "it has class 'data.frame'."
        ))
    )
})

test_that("vector failure", {
    ok <- is2(data.frame(), class = c("data.frame", "DataFrame"))
    expect_s3_class(ok, "goalie")
    expect_identical(
        nocause(ok),
        c(data.frame = TRUE, DataFrame = FALSE)
    )
    expect_identical(
        cause(ok),
        noquote(c("", "class 'data.frame' is not 'DataFrame'"))
    )
})

test_that("no class", {
    expect_error(
        object = is2(1L, class = character()),
        expected = "`class` must be non-empty character."
    )
})
