context("Scalar checks")



# allAreAtomic =================================================================
test_that("allAreAtomic : TRUE", {
    expect_true(allAreAtomic(data.frame(a = "foo", b = "bar")))
    expect_true(allAreAtomic(list(a = "foo", b = "bar")))
})

test_that("allAreAtomic : FALSE", {
    object <- allAreAtomic(data.frame())
    expect_s3_class(object, "goalie")
    expect_false(object)
    expect_identical(
        cause(object),
        noquote("data.frame() has length 0.")
    )

    object <- allAreAtomic(list(a = "x", b = list()))
    expect_s3_class(object, "goalie")
    expect_false(object)
    expect_identical(
        cause(object),
        noquote('Not all elements in list(a = "x", b = list()) are atomic.')
    )
})



# areSameLength ================================================================
test_that("areSameLength : TRUE", {
    x <- list(a = 1L, b = 2L)
    y <- list(c = 3L, d = 4L)
    object <- areSameLength(x = x, y = y)
    expect_true(object)
})

test_that("areSameLength : FALSE", {
    x <- list(a = 1L)
    y <- list(b = 2L, c = 3L)
    object <- areSameLength(x = x, y = y)
    expect_s3_class(object, "goalie")
    expect_false(object)
    expect_identical(
        cause(object),
        noquote("x does not have the same length as y.")
    )
})



# formalCompress ===============================================================
test_that("formalCompress : TRUE", {
    expect_true(formalCompress("gzip"))
    expect_true(formalCompress(TRUE))
})

test_that("formalCompress : FALSE", {
    object <- formalCompress(NULL)
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_identical(
        cause(object),
        noquote("compress is not any of: character, logical.")
    )

    object <- formalCompress(NA)
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_identical(
        cause(object),
        noquote("compress is NA")
    )

    object <- formalCompress("xxx")
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_identical(
        cause(object),
        noquote('"xxx" is not in c("bzip2", "gzip", "xz").')
    )
})



# hasDimnames ==================================================================
with_parameters_test_that(
    "hasDimnames : TRUE", {
        x <- datasets::mtcars
        expect_true(fun(x))
    },
    fun = list(
        hasDimnames,
        hasRownames,
        hasColnames
    )
)

with_parameters_test_that(
    "hasDimnames : FALSE", {
        x <- data.frame()
        object <- fun(x)
        expect_false(object)
        expect_s3_class(object, "goalie")
    },
    fun = list(
        hasDimnames,
        hasRownames,
        hasColnames
    )
)

with_parameters_test_that(
    "hasRownames", {
        data <- fun()
        object <- hasRownames(data)
        expect_false(object)
        expect_s3_class(object, "goalie")
        expect_identical(cause(object), noquote(cause))
    },
    fun = list(
        data.frame,
        S4Vectors::DataFrame,
        data.table::data.table,
        tibble::tibble
    ),
    cause = c(
        "data has sequence row names (soft NULL).",
        "data has NULL row names.",
        "data.table class doesn't support row names.",
        "tibble (tbl_df) class doesn't support row names."
    )
)



# hasDims



# hasDuplicates



# hasElements



# hasInternet



# hasLength



# hasNames



# hasNonZeroRowsAndCols



# hasRownames



# hasUniqueCols



# hasValidNames



# isAll



# isAlpha



# isAny



# isCharacter



# isFlag



# isGGScale



# isHeaderLevel



# isHexColorFunction



# isNumber



# isScalar



# isString



# matchesUniqueGeneNames



# sets



# validNames
