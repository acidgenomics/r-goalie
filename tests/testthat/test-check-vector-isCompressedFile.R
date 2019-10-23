pass <- c("sample1.fastq.gz", "sample2.fastq.bz2")
fail <- c("sample1.fastq", "sample2.fastq")
file.create(pass, fail)



context("check : vector : isCompressedFile")

test_that("TRUE", {
    expect_true(all(isCompressedFile(pass)))
})

test_that("FALSE", {
    expect_false(any(isCompressedFile(fail)))
})



context("check : scalar : isACompressedFile")

test_that("TRUE", {
    expect_true(isACompressedFile(pass[[1L]]))
})

test_that("FALSE", {
    expect_false(isACompressedFile(pass))
    expect_false(isACompressedFile(fail[[1L]]))
})



context("check : scalar : allAreCompressedFiles")

test_that("TRUE", {
    expect_true(allAreCompressedFiles(pass))
})

test_that("FALSE", {
    expect_false(allAreCompressedFiles(fail))
})



file.remove(pass, fail)
