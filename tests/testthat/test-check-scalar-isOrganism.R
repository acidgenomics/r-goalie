test_that("TRUE", {
    expect_true(isOrganism("Homo sapiens"))
    expect_true(isOrganism("Mus musculus"))
    expect_true(isOrganism("Canis lupus familiaris"))
})

test_that("FALSE", {
    expect_false(nocause(isOrganism("Human")))
    expect_false(nocause(isOrganism("Homo Sapiens")))
    expect_false(nocause(isOrganism("homo_sapiens")))
})

test_that("nullOk", {
    expect_false(nocause(isOrganism(NULL, nullOk = FALSE)))
    expect_true(isOrganism(NULL, nullOk = TRUE))
})
