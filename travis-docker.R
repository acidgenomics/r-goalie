Sys.setenv(TZ = "America/New_York")
rcmdcheck::rcmdcheck(args = "--no-manual")
BiocCheck::BiocCheck()
lintr::lint_package()
