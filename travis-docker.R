Sys.setenv(TZ = "America/New_York")
sessioninfo::session_info()
rcmdcheck::rcmdcheck(
    args = c(
        "--no-build-vignettes",
        "--no-manual",
        "--no-vignettes",
        "--timings"
    ),
    build_args = c(
        "--no-build-vignettes",
        "--no-manual"
    ),
    error_on = "error"
)
BiocCheck::BiocCheck(`quit-with-status` = TRUE)
lintr::lint_package()
