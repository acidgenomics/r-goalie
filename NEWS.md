# Release notes

## goalie 0.6.13 (2023-07-27)

Minor changes:

- `isURL` now returns `FALSE` for non-encoded URLs (e.g. containing spaces,
  other invalid characters). This helps protect against unwanted input into
  httr2 engine for REST API calls. Refer to `utils::URLencode` for encoding
  checks.

## goalie 0.6.12 (2023-07-24)

New functions:

- `isTximport`: New check function to perform validity checks on tximport `list`
  return, which isn't currently classed.

Minor changes:

- Updated lintr checks to be compatible with new release.

## goalie 0.6.11 (2023-07-13)

New functions:

- `isTempFile` / `isATempFile` / `allAreTempFiles`: Check functions to determine
  if a file exists on disk as a temporary file, defined by `tempfile`. Checks
  directory path internally against `tempdir` return. This function will be
  used in pipette update for compressed file cleanup during `import` calls.

Minor changes:

- Now requiring R 4.3 / Bioconductor 3.17.

## goalie 0.6.10 (2023-05-31)

Minor changes:

- `compressExtPattern`: Updated to ensure that maching is case insensitive.
- Relaxed suggested package dependencies.

## goalie 0.6.9 (2023-04-26)

New functions:

- `quietly`: Useful expression wrapper that suppresses all warnings, messages,
  and console output. Now used internally in `requireNamespaces` to suppress
  unwanted warnings from some Bioconductor packages, such as AnnotationHub,
  which now masks utils in the 3.17 release.

Minor changes:

- `validateClasses`: Improved assert check for input of empty list.

## goalie 0.6.8 (2023-01-12)

New functions:

- `requireNamespaces`: Migrated this useful base function from AcidBase package
  here to goalie instead, as this is often used inside of assert checks.

Minor changes:

- `hasNoDuplicates`: Improve support for Rle handling. Note that S4Vectors
  currently doesn't define a `duplicated` method for `Rle`, so we have to
  work around this at the moment.

## goalie 0.6.7 (2022-12-14)

New functions:

- `isIntegerish`: New check function that will also return true for vector-like
  classes, including `factor` and S4 `Rle`.

Minor changes:

- `hasRownames`: Should now return `TRUE` for `data.frame` with integer row
  names out of order. This is useful for `matrix` export method defined in
  pipette package.
- Added support for S4 `Rle` class handling in `isSubset`, `areDisjointSets`,
  `areIntersectingSets`, `areSetEqual`.
- Removed unused `skip_on_docker` test function.

## goalie 0.6.6 (2022-10-18)

New functions:

- `isVSCode`: Check if R session is running inside of Visual Studio Code.

## goalie 0.6.5 (2022-10-18)

Minor changes:

- Using `isSubset` in place of `%in%` where possible.
- `isInstalled`: Now calling `.packages` internally instead of checking rownames
  on `installed.packages` return.
- `hasDuplicates`, `hasNoDuplicates`: Hardened internal calls to `anyDuplicated`
  to check whether values return non-zero or not.
- `isCleanSystemLibrary`: NAMESPACE fix for `installed.packages` call.

## goalie 0.6.4 (2022-05-31)

Minor changes:

- `isGitRepo`: Return with classed cause in the event that Git is not installed.

## goalie 0.6.3 (2022-05-13)

Minor changes:

- Reworked package to support new lintr and testthat releases.
- Now using `Map` internally instead of `mapply`, as recommended by lintr.

## goalie 0.6.2 (2022-05-09)

Minor changes:

- `validateClasses`: Reworked internal code to use `isAny`, to allowing "or"
  checking for multiple acceptable class types. Currently needed to update
  validity checks against `sessionInfo` (utils package) vs. `session_info`
  (sessioninfo package) in our S4 classes that extend `SummarizedExperiment`.

## goalie 0.6.1 (2022-04-29)

Minor changes:

- Updated R dependency to 4.2.

## goalie 0.6.0 (2022-03-11)

Major changes:

- `assert`: Engine will use `AcidCLI::abort` if installed, to return richer
  error messages in the console. Also improved named message handling, similar
  to `stopifnot` conventions.
- Updated minimum R version dependency to 4.1, matching Bioconductor 3.14.

Minor changes:

- Improved some cause attributes to use better CLI formatting.
- Migrated some check functions from pointillism / AcidSingleCell packages:
  `hasClusters`, `hasMultipleSamples`, and `isBiocParallelParam`.
- Migrated `isDark` check here from AcidPlots package.
- Updated pkgdown website to use bootstrap 5 template.

## goalie 0.5.5 (2021-09-21)

Minor changes:

- `isDockerEnabled`: Bug fix for properly checking successful status on
  internal `docker info` call via `system2`.

## goalie 0.5.4 (2021-08-22)

Minor changes:

- `isInstalled`: Added support for `lib` argument, which allows the user to
  check whether an R package is installed in a specific package library.
  Defaults to checking against all libraries, calling `installed.packages`
  internally. This is a non-breaking change.

## goalie 0.5.3 (2021-08-19)

New functions:

- `isCondaEnabled`: Scalar check to determine if Conda is active inside of the
  current R session. Evaluates `CONDA_DEFAULT_ENV` and `CONDA_SHLVL` system
  environment variables internally.
- `isDockerEnabled`: Scalar check to determine if Docker is installed and is
  actively running on the current machine.
- `isGitRepo`: Vectorized checks for the existence of Git repositories. This
  is a little tricky to check inside of CI, so may need to think of a more
  clever approach for code coverage in the future.

Minor changes:

- `assert`, `validate`: Improved cause return when attribute is set from
  internal `falseFromVector` call. This was detected when `isSystemCommand`
  was not returning the expected program name in the assert call.
- `falseFromVector`: Improved internal cause attribute handling for scalars.
- Reworked no coverage exclusions with `nocov`, for platform- and configuration-
  specific checks.

## goalie 0.5.2 (2021-07-19)

Minor changes:

- `isAURL`: Improve setting of cause attribute when string is encoded. Bug fix
  is applied internally to `falseFromVector`, which passes `false` to internal
  `sprintf` call.
- Got package coverage back to 100%.

## goalie 0.5.1 (2021-03-09)

Minor changes:

- Miscellaneous documentation updates, to pass build checks.

## goalie 0.5.0 (2021-02-23)

Major changes:

- Reworked internal engine to redefine the goalie class as an S4 instead of
  an S3. This allows us to provide better validity checks with `setValidity`.
- Reworked internal handling of cause attribute, to provide better compatibility
  inside of R Markdown renders.

## goalie 0.4.14 (2021-01-06)

Minor changes:

- `isOrganism`: Added `nullOK` argument.

## goalie 0.4.13 (2021-01-06)

New functions:

- `isOrganism`: New assert check to verify that user precisely entered expected
  organism format of `<genus>` `<species>` (e.g. "Homo sapiens"). Case
  sensitivity and spaces are required here.

## goalie 0.4.12 (2021-01-04)

Minor changes:

- Migrated some base functions that were previously defined in AcidBase, so we
  can pin the dependencies to base R only: `compressExtPattern`, `extPattern`,
  `printString`, `safeDeparse`, `shorten`, `toNames`.

## goalie 0.4.11 (2020-11-05)

Minor changes:

- Decreased the number of suggested packages, removing DelayedArray and
  DelayedMatrixStats. These packages were called internally but basically
  never used in practice inside the `hasNonzeroRowsAndCols` check function.
  DelayedArray can have some HDF5-related build issues on some machines, so
  removing it as a suggested package makes goalie easier to install consistently
  across platforms.

## goalie 0.4.10 (2020-10-29)

New functions:

- `isDevel`: Check if session is running inside R-devel.
  Internally checks against `R.version.string`.
- `isBiocDevel`: Check if Bioconductor installation is under development.
  Requires BiocManager and yaml packages, which have been added to "Suggests".

## goalie 0.4.9 (2020-10-06)

- Updated dependency package names (e.g. AcidBase).

## goalie 0.4.8 (2020-08-12)

New functions:

- Migrated `isCleanSystemLibrary` here from bb8.

Minor changes:

- Switched vignette back to default instead of using BiocStyle.

## goalie 0.4.7 (2020-08-11)

New functions:

- `isPackageVersion`: Migrated check that was previously defined in koopa.
- `isVanilla`: Check if R session is running with `--vanilla` flag enabled.

## goalie 0.4.6 (2020-07-24)

Minor changes:

- Maintenance release, increasing minimum R dependency to 4.0.

## goalie 0.4.5 (2020-06-26)

New functions:

- `isSymlink`, `isASymlink`, `allAreSymlinks`: Added logical check functions
  for symbolic links. Note that these functions won't work on Windows.

## goalie 0.4.4 (2020-04-12)

New functions:

- `isRStudio`: Checks whether current R session is running inside RStudio.

Minor changes:

- Now setting cause attribute on failure for: `isLinux`, `isMacOS`, `isUnix`,
  and `isWindows`.

## goalie 0.4.3 (2020-04-08)

New functions:

Migrated some functions that were previously defined in the [koopa][] package:

- `hasGitHubPAT`: Check if the user has a GitHUB personal access token (PAT)
  defined as `GITHUB_PAT` environment variable.
- `isMacOS`, `isLinux`, `isUnix`, `isWindows`. The Linux, Unix, and Windows
  checks are new but inspired by the macOS check from koopa.
- `isSystemCommand` (previously named `isCommand` in koopa). This check will
  look to see if a system (shell) command is available.

Minor changes:

- `isDocker`: Hardened the check function to look at `/proc/1/cgroup`.

## goalie 0.4.2 (2020-01-28)

Minor changes:

- Switched license from MIT to GPL-3.

## goalie 0.4.1 (2019-12-09)

Minor changes:

- `hasRownames`: Improved internal check for integer (sequence) row names for
  `data.frame` class, which previously returned `TRUE` unexpectedly if the rows
  are reordered or subset. Now this function uses an internal `grepl` check
  via `allAreMatchingRegex`, which hardens this check, and now should always
  return `FALSE` for soft NULL row names in `data.frame` class.

## goalie 0.4.0 (2019-10-22)

The increase in version number here denotes new import of [acidbase][] package.

Major changes:

- Migrated base (system) functions to new low-level acidbase package.
  Some of these functions are not specific to an assert check engine and should
  be defined in a separate package.

New functions:

- Added assert checks, based on file extension:
  `isCompressedFile`, `isACompressedFile`, `allAreCompressedFiles`.
  These perform similarly to `isFile` but also check to see if the file
  extension contains a compression format (e.g. bz2, gz, xz, zip).

## goalie 0.3.12 (2019-10-21)

New functions:

- `isFileSystemCaseSensitive`: Checks whether file system is case sensitive
  (`TRUE`) or insensitive (`FALSE`). Linux tends to default to case sensitive,
  where as macOS and Windows default to case insensitive. Note that case
  insensitive file systems can lead to issues with Git repos. This check will
  be incorporated into a new `rename` mode for [syntactic][] naming functions
  in a future release.

## goalie 0.3.11 (2019-10-18)

Minor changes:

- Improved consistency of scalar and vector print messages.

## goalie 0.3.10 (2019-10-10)

Minor changes:

- `toNames`: Bug fix for `na.omit` return causing assert check to fail because
  class returns `FALSE` for `is.vector` check but `TRUE` for `is.atomic`.
- `isIntegerish` now returns logical vector for `na.omit` return. This supports
  `isScalarIntegerish` and `isInt` check functions.

## goalie 0.3.9 (2019-10-04)

New functions:

- `nElements`: Return the number of elements in an object. Previously this was
  defined internally in the package but is generally useful.

Minor changes:

- `hasElements`: No longer requiring the `n` argument. The function now performs
  similarly to `hasLength`.
- `isEmpty` and `isNonEmpty` are now soft deprecated. `isEmpty` is defined in
  the Bioconductor S4Vectors package, and we don't want to mask in interactive
  scripting sessions.
- `is2`: No longer exported, since this function isn't really intended for use
  outside of the package. Refer to `isAll` or `isAny` checks instead.

## goalie 0.3.8 (2019-09-15)

Minor changes:

- `isGGScale`: Update `aes` argument to support American "color" pass-in in
  addition to British "colour" spelling.

Minor changes:

- Switched to consistent use of "color" instead of "colour" in documentation,
  working examples, and unit tests.

## goalie 0.3.7 (2019-09-06)

Minor changes:

- `hasInternet`: Now allowing a user to check a specific URL, using the `url`
  argument. Currently defaults to Bioconductor.

## goalie 0.3.6 (2019-08-27)

Updated R dependency to 3.6.

Minor changes:

- `appendToBody`: Now allowing pass-in of multiple values as a `list`. The list
  must contains `call` elements.
- `hasNonzeroRowsAndCols`: Renamed function, improving consistency with generic
  defined in bioverbs. Also added support for `DelayedMatrix`, improving
  consistency with basejump `nonzeroRowsAndCols` function.

## goalie 0.3.5 (2019-08-13)

Minor changes:

- `isURL` now returns true on `url` class input.
- Improved message consistency.

## goalie 0.3.4 (2019-08-12)

New functions:

- `hasMetrics`, `hasSubset`: New functions that were previously used internally
  in basejump. Requires S4Vectors / SummarizedExperiment.

## goalie 0.3.3 (2019-08-11)

Minor changes:

- `validNames`: Bug fix and improvement for function, clearly showing position
  where names are invalid.

## goalie 0.3.2 (2019-08-10)

New functions:

- Now exporting `toNames`, which was previously used internally by the package.
  Improved handling in this function for floating points. Also now using
  `toNames` more consistently internally to sanitize names for `cause` return.

Minor changes:

- `standardizeCall`: Now returns with default arguments included. Can disable
  using `defaults = FALSE`.
- `matchArgsToDoCall`: Tweaked internal code slightly based on `standardizeCall`
  update.
- Updated messages to use quotes more consistently.
- Improved vector name handling for `cause` return.

## goalie 0.3.1 (2019-08-05)

Minor changes:

- Improved documentation consistency, using shared params defined in new
  AcidRoxygen package. This will be used across the basejump packages in a
  forthcoming update.

## goalie 0.3.0 (2019-07-29)

Version bump, reflecting start of a new release series.

Minor changes:

- Hardened `hasColnames`, `hasDimnames`, `hasNames`, `hasRownames`,
  `hasValidDimnames`, and `hasValidNames` against invalid objects that don't
  return either a value or `NULL`. This can happen in rare cases with invalid
  SummarizedExperiment objects (e.g. `bcb_invalid` object in bcbioRNASeq tests).

## goalie 0.2.19 (2019-07-21)

Minor changes:

- Improved full line comments to use "##" instead of "#", as recommended by
  current Bioconductor coding style.

## goalie 0.2.18 (2019-07-18)

New functions:

- Added `isDocker` and `skip_on_docker` functions for improved conditional
  testing inside Docker R images.

## goalie 0.2.17 (2019-07-15)

Major changes:

- Tweaked the internal code slightly for `assert` and `validate` to match the
  new conventions used in `stopifnot` for R 3.6 release. This should be
  completely backward compatible and transparency, but file an issue if you run
  into any unexpected bugs with this change.
- Improved `n` argument handling in `hasElements` checks. Previously this didn't
  return scalar consistently and could cause some build check issues to pop up
  in R 3.6, which are now fixed.
- Removed support for R 3.4 and no longer need to import backports.

Minor changes:

- No longer exporting `extractLocal` and `hasLocal`. These are currently only
  used by MethodDefinition utility functions defined here in the goalie package.
- Improved code coverage back up to 100%.
- Improved the documentation file names.

## goalie 0.2.16 (2019-05-04)

Minor changes:

- Improved pkgdown NEWS configuration.
- Improved Travis Docker configuration.
- Initial commit of draft vignette.
- Improved comments regarding call standardization and name handling inside
  `assert` function.

## goalie 0.2.15 (2019-04-22)

Minor changes:

- Bug fix for name handling in `assert` and `validate` calls. Now using `unname`
  internally to prevent unexpected errors with named logical vectors.

## goalie 0.2.14 (2019-04-02)

Minor changes:

- `standardizeCall`: Improved `isNonNegative` assert check by wrapping in
  `unname` call, to keep backward compatibility with R 3.4.
- Improved code coverage, getting closer to 100%.

## goalie 0.2.13 (2019-03-31)

Minor changes:

- Overhauled and improved assert checks.
- Miscellaneous bug fixes for assert checks.

## goalie 0.2.12 (2019-03-22)

Minor changes:

- Migrated domain to [Acid Genomics][].

## goalie 0.2.11 (2019-03-17)

Minor changes:

- `isDirectory`, `isFile`: Now return with `cause` attribute set on failure.
- Updated assert checks and reworked comments to pass lintr checks.

## goalie 0.2.10 (2019-03-10)

Minor changes:

- `isSubset`: Improved cause message, which would error using internal `deparse`
  call on long vectors.

## goalie 0.2.9 (2019-02-25)

Minor changes:

- Needed to tweak internal code in `assignToBody` to fix backports handling of
  `...elt` for R 3.4.
- `hasAccess`: Needed to add `uname` call internally for R 3.4 backward
  compatibility, for checks that rely upon `isTRUE` to return `TRUE`, as
  expected. The handling of this situation has changed in R 3.5.

## goalie 0.2.8 (2019-02-11)

Minor changes:

- Bioconda unit tests revealed that `...elt` approach used in `assert` isn't
  backward compatible with R 3.4. The backports package was added as a
  dependency, which provides legacy support for `...elt` in R 3.4. This is
  called internally inside `.onLoad` specifically for R releases prior to 3.5.

## goalie 0.2.7 (2019-02-11)

Minor changes:

- `assert`: `traceback` argument is disabled by default. Note that this can be
  enabled globally using `options(goalie.traceback = TRUE)`, which can be useful
  for code debugging in some situations.
- `isSubset`: Improved cause message if user attempts to pass in `NULL` for
  either `x` or `y` arguments.
- `hasNonZeroRowsAndCols`: Fixed typo in internal code that didn't check using
  `hasCols` properly.
- `isFlag`: Improved cause message if user passes in `NA`.

## goalie 0.2.6 (2019-01-22)

New functions:

- Migrated `printString` from [basejump][] package.

## goalie 0.2.5 (2019-01-15)

Minor changes:

- `isADirectory`, `isAFile`: Added `nullOK` argument support.

## goalie 0.2.4 (2019-01-06)

New functions:

- New exports: `allAreDirectories`, `allAreFiles`, which return scalar.

Minor changes:

- Documentation improvements.

## goalie 0.2.3 (2019-01-04)

New functions:

- Migrated some low-level functions from [basejump][] that are useful for
  assertive checks: `matchArgsToDoCall`, `standardizeCall`, and the
  `MethodDefinition` family, which includes `methodFunction`,
  `methodFormals`, `hasLocal`, `extractLocal`. These will get re-exported
  in [basejump][].

Minor changes:

- Reorganized base engine function files, getting rid of the `base-` prefix.
- Added conditional `NULL` support where applicable, with `nullOK` formal.
  Applies to `isInt`, `isNumber`, `isString`, for example.
- General documentation improvements.

## goalie 0.2.2 (2018-12-22)

Major changes:

- `assert`: Added `msg` formal. Improved error message when a user inputs
  check functions that don't return boolean.
- `capitalize`: Simplified documentation, referring the user to the
  documentation provided in [syntactic][] package.
- `falseFromVector`: Newly exported function.
- `isInRange` family: Added scalar variants for all vectorized functions.
- Improved scalar return consistency in functions where applicable, using
  `falseFromVector` internally.

## goalie 0.2.1 (2018-12-20)

Major changes:

- The approach to the package update is heavily influenced by the conventions
  defined in the assertive package. In goalie, I'm attempting to create a
  minimal assert check engine that behaves similar to `stopifnot`, but with
  more informative error messages. This approach is inspired by the assertthat
  package, but neither assertive or assertthat are working quite right for my
  needs.
- Overhauled the `assert` and `validate` engines, re-writing the code from
  scratch based on the internal code of `stopifnot`.
- Reworked all assertive check functions to reduce the number of dependencies.
  Now the package is lean and mean, only importing methods and utils into the
  NAMESPACE.
- Assertive checks now return as `goalie` class in addition to `logical`.

## goalie 0.2.0 (2018-12-12)

Major changes:

- Removed all `assert*` functions in favor of a simpler, assertthat style
  approach using `assert` for all checks. The package will export check
  functions that return `logical`, typically `logical(1)` (boolean flag) for
  most checks. These checks can then be wrapped in `assert` or `validate`
  (for S4 class validity checks) calls.

## goalie 0.1.2 (2018-11-21)

Minor changes:

- `isDir` and `isFile` now import `R.utils::isDirectory` and `R.utils::isFile`.

## goalie 0.1.1 (2018-11-14)

Minor changes:

- `areUniqueGeneNames` now returns `boolean` as expected.
- Improved error message for `assertAreUniqueGeneNames`.

## goalie 0.1.0 (2018-11-11)

- Initial release, migrating assertive check functions from [basejump][]
  package.

[acid genomics]: https://acidgenomics.com/
[acidbase]: https://r.acidgenomics.com/packages/acidbase/
[basejump]: https://r.acidgenomics.com/packages/basejump/
[koopa]: https://koopa.acidgenomics.com/
[syntactic]: https://r.acidgenomics.com/packages/syntactic/
