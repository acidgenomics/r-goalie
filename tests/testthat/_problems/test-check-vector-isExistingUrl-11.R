# Extracted from test-check-vector-isExistingUrl.R:11

# prequel ----------------------------------------------------------------------
urls <- c(
    "https://www.r-project.org/",
    ## Corporate firewalls sometimes block FTP, so disabling check here.
    ## > "ftp://r-project.org/",
    "https://ndownloader.figshare.com/files/35020903",
    "https://figshare.com/ndownloader/files/40448834"
)

# test -------------------------------------------------------------------------
ok <- isExistingUrl(urls)
expect_true(all(ok))
