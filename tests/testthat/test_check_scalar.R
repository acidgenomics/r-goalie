context("Scalar checks")



test_that("allAreAtomic", {
    expect_true(allAreAtomic(data.frame(a = "foo", b = "bar")))
    expect_true(allAreAtomic(list(a = "foo", b = "bar")))

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



test_that("areSameLength", {
    x <- list(a = 1L, b = 2L)
    y <- list(c = 3L, d = 4L)
    object <- areSameLength(x = x, y = y)
    expect_true(object)

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



test_that("formalCompress", {
    expect_true(formalCompress("gzip"))
    expect_true(formalCompress(TRUE))

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



with_parameters_test_that(
    "hasDimnames", {
        x <- datasets::mtcars
        expect_true(fun(x))

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



test_that("hasDims", {
    expect_true(hasDims(datasets::mtcars))

    # Note that dims don't have to be non-zero, just not NULL.
    expect_true(hasDims(data.frame()))

    object <- hasDims(list())
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_identical(
        cause(object),
        noquote("The dimensions of list() are NULL.")
    )
})



with_parameters_test_that(
    "hasRows, hasCols", {
        x <- datasets::mtcars
        expect_true(fun(x))

        x <- data.frame()
        expect_false(fun(x))
    },
    fun = list(hasRows, hasCols)
)



test_that("hasDuplicates", {
    expect_true(hasDuplicates(c("a", "a")))
    expect_true(hasNoDuplicates(c("a", "b")))

    object <- hasDuplicates(c("a", "b"))
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_identical(
        cause(object),
        noquote('c("a", "b") has no duplicates.')
    )

    object <- hasNoDuplicates(c("a", "a", "b", "b"))
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_identical(
        cause(object),
        noquote('c("a", "a", "b", "b") has duplicates at positions 2, 4.')
    )
})



test_that("hasElements", {
    expect_true(hasElements("hello", n = 1))
    expect_true(hasElements(list(a = 1, b = 2), n = 2))

    object <- hasElements(list(), n = 1)
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_identical(
        cause(object),
        noquote('list() has 0 elements, not 1.')
    )
})



test_that("hasInternet", {
    expect_true(hasInternet())
})



test_that("hasLength", {
    expect_true(hasLength(1L))
    expect_true(hasLength(FALSE))
    expect_true(hasLength(datasets::mtcars))
    expect_true(hasLength(""))

    object <- hasLength(NULL)
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_identical(
        cause(object),
        noquote("NULL has length 0.")
    )

    object <- hasLength(character())
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_identical(
        cause(object),
        noquote("character() has length 0.")
    )

    object <- hasLength(data.frame())
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_identical(
        cause(object),
        noquote("data.frame() has length 0.")
    )
})



test_that("hasNames", {
    expect_true(hasNames(datasets::mtcars))

    object <- hasNames(matrix())
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_identical(
        cause(object),
        noquote("The names of matrix() are NULL.")
    )

    object <- hasNames(data.frame())
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_identical(
        cause(object),
        noquote("The names of data.frame() are all empty.")
    )
})



test_that("hasNonZeroRowsAndCols", {
    x <- matrix(data = seq_len(4), nrow = 2)
    expect_true(hasNonZeroRowsAndCols(x))

    x <- matrix(data = rep(1, times = 2), byrow = TRUE)
    expect_true(hasNonZeroRowsAndCols(x))

    x <- matrix(data = rep(1, times = 2), byrow = FALSE)
    expect_true(hasNonZeroRowsAndCols(x))

    x <- matrix(nrow = 0, ncol = 0)
    object <- hasNonZeroRowsAndCols(x)
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_identical(
        cause(object),
        noquote("The number of rows in x is zero.")
    )

    x <- matrix(nrow = 1, ncol = 0)
    object <- hasNonZeroRowsAndCols(x)
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_identical(
        cause(object),
        noquote("The number of columns in x is zero.")
    )

    x <- matrix(nrow = 0, ncol = 1)
    object <- hasNonZeroRowsAndCols(x)
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_identical(
        cause(object),
        noquote("The number of rows in x is zero.")
    )
})



test_that("hasRownames", {
    x <- data.frame(
        "sample1" = c(1L, 2L),
        "sample2" = c(3L, 4L),
        row.names = c("gene1", "gene2")
    )
    expect_true(hasRownames(x))

    x <- data.frame(a = seq_len(2L))
    object <- hasRownames(x)
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_identical(
        cause(object),
        noquote("x has sequence row names (soft NULL).")
    )

    x <- S4Vectors::DataFrame(a = seq_len(2L))
    object <- hasRownames(x)
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_identical(
        cause(object),
        noquote("x has NULL row names.")
    )
})



test_that("hasUniqueCols", {
    x <- matrix(data = seq_len(20L), ncol = 2L)
    expect_true(hasUniqueCols(x))

    x <- matrix(data = rep(seq_len(10L), times = 2L), ncol = 2L)
    object <- hasUniqueCols(x)
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_match(
        as.character(cause(object)),
        "has duplicated columns"
    )
})



test_that("hasValidNames", {
    x <- list(a = 1, b = 2)
    expect_true(hasValidNames(x))

    x <- datasets::iris
    expect_true(hasValidDimnames(x))

    x <- list(
        `1`       = 1,  # can't start with number
        `foo bar` = 2,  # no spaces
        `foo-bar` = 3   # no hyphens
    )
    object <- hasValidNames(x)
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_identical(
        cause(object),
        noquote("x does not have valid names.")
    )

    # Note the spaces in the row names here.
    x <- datasets::mtcars
    object <- hasValidDimnames(x)
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_identical(
        cause(object),
        noquote("x has invalid row names.")
    )
})



test_that("isAll", {
    x <- 1L
    expect_true(isAll(x, classes = c("integer", "numeric")))

    object <- isAll(x, classes = c("integer", "NULL"))
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_identical(
        cause(object),
        noquote("x is not all: integer, NULL")
    )
})



test_that("isAlpha", {
    expect_true(isAlpha(0.05))
    expect_true(isAlpha(1e-10))

    object <- isAlpha(0L)
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_identical(
        cause(object),
        noquote("0L is not scalar double.")
    )

    expect_identical(cause(isAlpha(0)), noquote("too low"))
    expect_identical(cause(isAlpha(1)), noquote("too high"))

    # Must be scalar.
    expect_identical(
        cause(isAlpha(c(0.1, 0.1))),
        noquote("c(0.1, 0.1) is not scalar double.")
    )
})



test_that("isAny", {
    x <- 1L

    expect_true(isAny(x, classes = c("integer", "NULL")))
    expect_true(isAny(x, classes = c("numeric", "NULL")))
    expect_true(isAny(x, classes = c("atomic", "NULL")))

    object <- isAny(x, classes = c("character", "data.frame"))
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_identical(
        cause(object),
        noquote("x is not any of: character, data.frame.")
    )
})



test_that("isCharacter", {
    expect_true(isCharacter("a"))
    expect_true(isCharacter(letters))

    object <- isCharacter(seq_len(5L))
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_identical(
        cause(object),
        noquote("seq_len(5L) is not character.")
    )

    expect_false(isCharacter(NULL))
    expect_false(isCharacter(character()))
    expect_false(isCharacter(""))
    expect_false(isCharacter(NA_character_))
})



test_that("isFlag", {
    expect_true(isFlag(TRUE))
    expect_true(isFlag(FALSE))

    object <- isFlag(c(TRUE, TRUE))
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_identical(
        cause(object),
        noquote("c(TRUE, TRUE) is not a boolean flag (TRUE/FALSE).")
    )

    object <- isFlag(1)
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_identical(
        cause(object),
        noquote("1 is not a boolean flag (TRUE/FALSE).")
    )

    object <- isFlag(NA)
    expect_false(object)
    expect_s3_class(object, "goalie")
    expect_identical(
        cause(object),
        noquote("NA is not a boolean flag (TRUE/FALSE).")
    )
})



test_that("isGGScale", {
    library(ggplot2)
    colour_c <- scale_colour_gradient(low = "red", high = "blue")
    colour_d <- scale_colour_manual(values = c("red", "blue"))
    fill_c <- scale_fill_gradient(low = "red", high = "blue")
    fill_d <- scale_fill_manual(values = c("red", "blue"))

    expect_true(isGGScale(x = colour_c, scale = "continuous", aes = "colour"))
    expect_true(isGGScale(x = colour_d, scale = "discrete", aes = "colour"))
    expect_true(isGGScale(x = fill_c, scale = "continuous", aes = "fill"))
    expect_true(isGGScale(x = fill_d, scale = "discrete", aes = "fill"))

    expect_false(isGGScale(x = colour_d, scale = "continuous", aes = "colour"))
    expect_false(isGGScale(x = colour_c, scale = "discrete", aes = "colour"))
    expect_false(isGGScale(x = fill_d, scale = "continuous", aes = "fill"))
    expect_false(isGGScale(x = fill_c, scale = "discrete", aes = "fill"))
})



# isHeaderLevel



# isHexColorFunction



# isNumber



# isScalar



# isString



# matchesUniqueGeneNames



# sets



# validNames
