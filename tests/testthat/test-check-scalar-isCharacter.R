test_that("TRUE", {
    expect_true(isCharacter("a"))
    expect_true(isCharacter(letters))
})

test_that("FALSE : numeric sequence", {
    ok <- isCharacter(seq_len(5L))
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "{.var integer} is not character."
    )
})

## Note that `isCharacter()` is intentionally more strict than `is.character()`.
test_that("FALSE : empty", {
    expect_false(isCharacter(NULL))
    expect_false(isCharacter(character()))
    expect_false(isCharacter(""))
    expect_false(isCharacter(NA_character_))
})

test_that("nullOk", {
    expect_false(isCharacter(NULL, nullOk = FALSE))
    expect_true(isCharacter(NULL, nullOk = TRUE))
})
