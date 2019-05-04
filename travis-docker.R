Sys.setenv(TZ = "America/New_York")
rcmdcheck::rcmdcheck(args = "--no-build-vignettes --no-manual --timings")
BiocCheck::BiocCheck(`quit-with-status` = TRUE)
lintr::lint_package()
