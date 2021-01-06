context("check : scalar : isOrganism")

test_that("TRUE", {
    expect_true(isOrganism("Homo sapiens"))
    expect_true(isOrganism("Mus musculus"))
})

test_that("FALSE", {
    expect_false(isOrganism("Human"))
    expect_false(isOrganism("Homo Sapiens"))
    expect_false(isOrganism("homo_sapiens"))
})

test_that("nullOK", {
    expect_false(isOrganism(NULL, nullOK = FALSE))
    expect_true(isOrganism(NULL, nullOK = TRUE))
})
