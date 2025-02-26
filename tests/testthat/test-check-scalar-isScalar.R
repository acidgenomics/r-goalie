test_that("isScalar", {
    expect_true(isScalar("X"))
    expect_true(isScalar(""))
    expect_true(isScalar(1L))
    expect_true(isScalar(1.1))
    expect_true(isScalar(NA))
    expect_true(isScalar(NaN))
    expect_false(nocause(isScalar(NULL)))
    expect_false(nocause(isScalar(character())))
})

test_that("isNonScalar", {
    expect_true(isNonScalar(seq_len(2L)))
    expect_true(isNonScalar(NULL))
    expect_false(nocause(isNonScalar(1L)))
})

test_that("isScalarAtomic", {
    expect_true(isScalarAtomic("X"))
    expect_true(isScalarAtomic(""))
    expect_true(isScalarAtomic(1L))
    expect_true(isScalarAtomic(1.1))
    expect_true(isScalarAtomic(NA))
    expect_false(nocause(isScalarAtomic(NULL)))
    expect_false(nocause(isScalarAtomic(character())))
})

test_that("isScalarCharacter", {
    expect_true(isScalarCharacter("X"))
    expect_true(isScalarCharacter(NA_character_))
    ## Note that `isCharacter()` is stricter and returns false for this one.
    expect_true(isScalarCharacter(""))
    expect_false(nocause(isScalarCharacter(c("A", "B"))))
    expect_false(nocause(isScalarCharacter(1L)))
    expect_false(nocause(isScalarCharacter(list(a = "a"))))
    expect_false(nocause(isScalarCharacter(NULL)))
})

test_that("isScalarDouble", {
    expect_true(isScalarDouble(0.1))
    expect_true(isScalarDouble(1.0))
    expect_true(isScalarDouble(1)) # nolint
    expect_false(nocause(isScalarDouble(c(0.1, 0.2))))
    expect_false(nocause(isScalarDouble(1L)))
    expect_false(nocause(isScalarDouble(numeric())))
    expect_false(nocause(isScalarDouble(NULL)))
})

test_that("isScalarInteger", {
    expect_true(isScalarInteger(1L))
    expect_true(isScalarInteger(NA_integer_))
    expect_false(nocause(isScalarInteger(NULL)))
    expect_false(nocause(isScalarInteger(1))) # nolint
    expect_false(nocause(isScalarInteger(integer())))
    expect_false(nocause(isScalarInteger(c(1L, 2L))))
})

test_that("isScalarIntegerish", {
    expect_true(isScalarIntegerish(1)) # nolint
    expect_true(isScalarIntegerish(1L))
    expect_true(isScalarIntegerish(1.0))
    expect_false(nocause(isScalarIntegerish(NULL)))
    expect_false(nocause(isScalarIntegerish(seq_len(2L))))
    expect_false(nocause(isScalarIntegerish(1.000001)))
})

test_that("isScalarList", {
    expect_true(isScalarList(list(a = 1L)))
    expect_false(nocause(isScalarList(list())))
    expect_false(nocause(isScalarList(list(a = 1L, b = 2L))))
    expect_false(nocause(isScalarList(NULL)))
    expect_false(nocause(isScalarList(FALSE)))
})

test_that("isScalarLogical", {
    expect_true(isScalarLogical(FALSE))
    expect_true(isScalarLogical(TRUE))
    ## Note that `isFlag()` is stricter and will return false for this.
    expect_true(isScalarLogical(NA))
    expect_false(nocause(isScalarLogical(c(FALSE, TRUE))))
    expect_false(nocause(isScalarLogical(NaN)))
    expect_false(nocause(isScalarLogical(NULL)))
    expect_false(nocause(isScalarLogical(logical())))
})

test_that("isScalarNumeric", {
    expect_true(isScalarNumeric(0.1))
    expect_true(isScalarNumeric(1L))
    expect_true(isScalarNumeric(NA_integer_))
    expect_false(nocause(isScalarNumeric(c(0.1, 0.2))))
    expect_false(nocause(isScalarNumeric(NULL)))
})

test_that("isScalarVector", {
    expect_true(isScalarVector("X"))
    expect_true(isScalarVector(TRUE))
    expect_true(isScalarVector(NA))
    expect_true(isScalarVector(NaN))
    expect_false(nocause(isScalarVector(vector())))
})
