context("internal : capitalize")

test_that("capitalize", {
    expect_null(.capitalize(x = NULL))
    expect_identical(
        .capitalize(c("hello", NA)),
        c("Hello", NA)
    )
})



context("internal : dim")

test_that("dim", {
    expect_identical(.dim(seq(2L)), 2L)
    expect_identical(.dim(mtcars), c(32L, 11L))
})




context("internal : is2")

test_that("multiple classes", {
    ok <- .is2(1L, class = c("atomic", "numeric", "integer"))
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
    ok <- .is2(data.frame(), class = "DataFrame")
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        cause(ok),
        noquote(paste(
            "'data.frame()' is not of class 'DataFrame';",
            "it has class 'data.frame'."
        ))
    )
})

test_that("vector failure", {
    ok <- .is2(data.frame(), class = c("data.frame", "DataFrame"))
    expect_s3_class(ok, "goalie")
    expect_identical(
        nocause(ok),
        c(data.frame = TRUE, DataFrame = FALSE)
    )
    expect_identical(
        cause(ok),
        noquote(c(
            "data.frame" = "",
            "DataFrame" = "class 'data.frame' is not 'DataFrame'"
        ))
    )
})

test_that("no class", {
    expect_error(
        object = .is2(1L, class = character()),
        expected = "'class' must be non-empty character."
    )
})



context("internal : typeDescription")

test_that("typeDescription", {
    expect_identical(
        object = .typeDescription(base::save),
        expected = "class 'closure function'"
    )
})
