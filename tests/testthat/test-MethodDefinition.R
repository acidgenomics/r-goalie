context("MethodDefinition")

test_that("MethodDefinition", {
    # Use `substitute()` to put a missing argument in pairlist.
    # https://stackoverflow.com/questions/3892580
    formals <- pairlist(
        "x" = substitute(),
        row.names = NULL,
        optional = FALSE,
        "..." = substitute()
    )

    # Function
    x <- methodFunction(
        f = "as.data.frame",
        signature = "ANY",
        package = "S4Vectors"
    )
    expect_is(x, "function")
    expect_identical(formals(x), formals)

    # Formals
    x <- methodFormals(
        f = "as.data.frame",
        signature = "ANY",
        package = "S4Vectors"
    )
    expect_identical(x, formals)
})
