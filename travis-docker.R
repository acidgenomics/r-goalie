Sys.setenv(TZ = "America/New_York")
rcmdcheck::rcmdcheck()
BiocCheck::BiocCheck(`quit-with-status` = TRUE)
lintr::lint_package()
