Sys.setenv(TZ = "America/New_York")
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
    )
)
BiocCheck::BiocCheck(`quit-with-status` = TRUE)
lintr::lint_package()
