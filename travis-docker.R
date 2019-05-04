Sys.setenv(TZ = "America/New_York")
rcmdcheck::rcmdcheck(args = "--no-manual")
BiocCheck::BiocCheck(`quit-with-status` = TRUE)
lintr::lint_package()
