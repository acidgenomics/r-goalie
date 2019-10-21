context("base : methodDefinition")

test_that("as.data.frame", {
    ## Use `substitute()` to put a missing argument in pairlist.
    ## https://stackoverflow.com/questions/3892580
    formals <- pairlist(
        "x" = substitute(),
        row.names = NULL,
        optional = FALSE,
        "..." = substitute()
    )

    ## Function
    x <- methodFunction(
        f = "as.data.frame",
        signature = "ANY",
        package = "S4Vectors"
    )
    expect_is(x, "function")
    expect_identical(formals(x), formals)

    ## Formals
    x <- methodFormals(
        f = "as.data.frame",
        signature = "ANY",
        package = "S4Vectors"
    )
    expect_identical(x, formals)
})

test_that("Expected failure", {
    expect_error(
        methodFunction("XXX", signature = "ANY", package = "S4Vectors"),
        "Failed to locate"
    )
})

test_that(".local handling", {
    md <- getMethod(
        f = "aggregate",
        signature = "Vector",
        where = asNamespace("S4Vectors")
    )
    expect_true(.hasLocal(md))
    expect_is(.extractLocal(md), "function")

    md <- getMethod(
        f = "as.data.frame",
        signature = "ANY",
        where = asNamespace("BiocGenerics")
    )
    expect_false(.hasLocal(md))
    expect_error(.extractLocal(md))
})
