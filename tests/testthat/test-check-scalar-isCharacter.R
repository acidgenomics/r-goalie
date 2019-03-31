context("check : scalar : isCharacter")

test_that("TRUE", {
    expect_true(isCharacter("a"))
    expect_true(isCharacter(letters))
})

test_that("FALSE : numeric sequence", {
    ok <- isCharacter(seq_len(5L))
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("seq_len(5L) is not character.")
    )
})

# Note that `isCharacter()` is intentionally more strict than `is.character()`.
test_that("FALSE : empty", {
    expect_false(isCharacter(NULL))
    expect_false(isCharacter(character()))
    expect_false(isCharacter(""))
    expect_false(isCharacter(NA_character_))
})