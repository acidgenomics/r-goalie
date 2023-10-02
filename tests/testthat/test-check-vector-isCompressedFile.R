pass <- file.path(
    tempdir(),
    c("sample1.fastq.gz", "sample2.fastq.bz2")
)
fail <- file.path(
    tempdir(),
    c("sample1.fastq", "sample2.fastq")
)
invisible(file.create(pass, fail))

test_that("TRUE", {
    expect_true(all(isCompressedFile(pass)))
    expect_true(isACompressedFile(pass[[1L]]))
    expect_true(allAreCompressedFiles(pass))
})

test_that("FALSE", {
    expect_false(any(isCompressedFile(fail)))
    expect_false(isACompressedFile(pass))
    expect_false(isACompressedFile(fail[[1L]]))
    expect_false(allAreCompressedFiles(fail))
})

invisible(file.remove(pass, fail))
