## goalie 0.3.8 (2019-09-15)

### Minor changes

- `isGGScale`: Update `aes` argument to support American "color" pass-in in
  addition to British "colour" spelling.
  
### Minor changes

- Switched to consistent use of "color" instead of "colour" in documentation,
  working examples, and unit tests.

## goalie 0.3.7 (2019-09-06)

### Minor changes

- `hasInternet`: Now allowing a user to check a specific URL, using the `url`
  argument. Currently defaults to Bioconductor.

## goalie 0.3.6 (2019-08-27)

Updated R dependency to 3.6.

### Minor changes

- `appendToBody`: Now allowing pass-in of multiple values as a `list`. The list
  must contains `call` elements.
- `hasNonzeroRowsAndCols`: Renamed function, improving consistency with generic
  defined in bioverbs. Also added support for `DelayedMatrix`, improving
  consistency with basejump `nonzeroRowsAndCols` function.

## goalie 0.3.5 (2019-08-13)

### Minor changes

- `isURL` now returns true on `url` class input.
- Improved message consistency.

## goalie 0.3.4 (2019-08-12)

### New functions

- `hasMetrics`, `hasSubset`: New functions that were previously used internally
  in basejump. Requires S4Vectors / SummarizedExperiment.

## goalie 0.3.3 (2019-08-11)

### Minor changes

- `validNames`: Bug fix and improvement for function, clearly showing position
  where names are invalid.

## goalie 0.3.2 (2019-08-10)

### New functions

- Now exporting `toNames`, which was previously used internally by the package.
  Improved handling in this function for floating points. Also now using
  `toNames` more consistently internally to sanitize names for `cause` return.

### Minor changes

- `standardizeCall`: Now returns with default arguments included. Can disable
  using `defaults = FALSE`.
- `matchArgsToDoCall`: Tweaked internal code slightly based on `standardizeCall`
  update.
- Updated messages to use quotes more consistently.
- Improved vector name handling for `cause` return.

## goalie 0.3.1 (2019-08-05)

### Minor changes

- Improved documentation consistency, using shared params defined in new
  acidroxygen package. This will be used across the basejump packages in a
  forthcoming update.

## goalie 0.3.0 (2019-07-29)

Version bump, reflecting start of a new release series.

### Minor changes

- Hardened `hasColnames`, `hasDimnames`, `hasNames`, `hasRownames`,
  `hasValidDimnames`, and `hasValidNames` against invalid objects that don't
  return either a value or `NULL`. This can happen in rare cases with invalid
  SummarizedExperiment objects (e.g. `bcb_invalid` object in bcbioRNASeq tests).

## goalie 0.2.19 (2019-07-21)

### Minor changes

- Improved full line comments to use "##" instead of "#", as recommended by
  current Bioconductor coding style.

## goalie 0.2.18 (2019-07-18)

### New functions

- Added `isDocker` and `skip_on_docker` functions for improved conditional
  testing inside Docker R images.

## goalie 0.2.17 (2019-07-15)

### Major changes

- Tweaked the internal code slightly for `assert` and `validate` to match the
  new conventions used in `stopifnot` for R 3.6 release. This should be
  completely backward compatible and transparency, but file an issue if you run
  into any unexpected bugs with this change.
- Improved `n` argument handling in `hasElements` checks. Previously this didn't
  return scalar consistently and could cause some build check issues to pop up
  in R 3.6, which are now fixed.
- Removed support for R 3.4 and no longer need to import backports.

### Minor changes

- No longer exporting `extractLocal` and `hasLocal`. These are currently only
  used by MethodDefinition utility functions defined here in the goalie package.
- Improved code coverage back up to 100%.
- Improved the documentation file names.

## goalie 0.2.16 (2019-05-04)

### Minor changes

- Improved pkgdown NEWS configuration.
- Improved Travis Docker configuration.
- Initial commit of draft vignette.
- Improved comments regarding call standardization and name handling inside
  `assert` function.

## goalie 0.2.15 (2019-04-22)

### Minor changes

- Bug fix for name handling in `assert` and `validate` calls. Now using `unname`
  internally to prevent unexpected errors with named logical vectors.

## goalie 0.2.14 (2019-04-02)

### Minor changes

- `standardizeCall`: Improved `isNonNegative` assert check by wrapping in
  `unname` call, to keep backward compatibility with R 3.4.
- Improved code coverage, getting closer to 100%.

## goalie 0.2.13 (2019-03-31)

### Minor changes

- Overhauled and improved assert checks.
- Miscellaneous bug fixes for assert checks.

## goalie 0.2.12 (2019-03-22)

### Minor changes

- Migrated domain to [Acid Genomics][].

## goalie 0.2.11 (2019-03-17)

### Minor changes

- `isDirectory`, `isFile`: Now return with `cause` attribute set on failure.
- Updated assert checks and reworked comments to pass lintr checks.

## goalie 0.2.10 (2019-03-10)

### Minor changes

- `isSubset`: Improved cause message, which would error using internal `deparse`
  call on long vectors.

## goalie 0.2.9 (2019-02-25)

### Minor changes

- Needed to tweak internal code in `assignToBody` to fix backports handling of
  `...elt` for R 3.4.
- `hasAccess`: Needed to add `uname` call internally for R 3.4 backward
  compatibility, for checks that rely upon `isTRUE` to return `TRUE`, as
  expected. The handling of this situation has changed in R 3.5.

## goalie 0.2.8 (2019-02-11)

### Minor changes

- Bioconda unit tests revealed that `...elt` approach used in `assert` isn't
  backward compatible with R 3.4. The backports package was added as a
  dependency, which provides legacy support for `...elt` in R 3.4. This is
  called internally inside `.onLoad` specifically for R releases prior to 3.5.

## goalie 0.2.7 (2019-02-11)

### Minor changes

- `assert`: `traceback` argument is disabled by default. Note that this can be
  enabled globally using `options(goalie.traceback = TRUE)`, which can be useful
  for code debugging in some situations.
- `isSubset`: Improved cause message if user attempts to pass in `NULL` for
  either `x` or `y` arguments.
- `hasNonZeroRowsAndCols`: Fixed typo in internal code that didn't check using
  `hasCols` properly.
- `isFlag`: Improved cause message if user passes in `NA`.

## goalie 0.2.6 (2019-01-22)

### New functions

- Migrated `printString` from [basejump][] package.

## goalie 0.2.5 (2019-01-15)

### Minor changes

- `isADirectory`, `isAFile`: Added `nullOK` argument support.

## goalie 0.2.4 (2019-01-06)

### New functions

- New exports: `allAreDirectories`, `allAreFiles`, which return scalar.

### Minor changes

- Documentation improvements.

## goalie 0.2.3 (2019-01-04)

### New functions

- Migrated some low-level functions from [basejump][] that are useful for
  assertive checks: `matchArgsToDoCall`, `standardizeCall`, and the
  `MethodDefinition` family, which includes `methodFunction`,
  `methodFormals`, `hasLocal`, `extractLocal`. These will get re-exported
  in [basejump][].

### Minor changes

- Reorganized base engine function files, getting rid of the `base-` prefix.
- Added conditional `NULL` support where applicable, with `nullOK` formal.
  Applies to `isInt`, `isNumber`, `isString`, for example.
- General documentation improvements.

## goalie 0.2.2 (2018-12-22)

### Major changes

- `assert`: Added `msg` formal. Improved error message when a user inputs
  check functions that don't return boolean.
- `capitalize`: Simplified documentation, referring the user to the
  documentation provided in [syntactic][] package.
- `falseFromVector`: Newly exported function.
- `isInRange` family: Added scalar variants for all vectorized functions.
- Improved scalar return consistency in functions where applicable, using
  `falseFromVector` internally.

## goalie 0.2.1 (2018-12-20)

### Major changes

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

### Major changes

- Removed all `assert*` functions in favor of a simpler, assertthat style
  approach using `assert` for all checks. The package will export check
  functions that return `logical`, typically `logical(1)` (boolean flag) for
  most checks. These checks can then be wrapped in `assert` or `validate`
  (for S4 class validity checks) calls.

## goalie 0.1.2 (2018-11-21)

### Minor changes

- `isDir` and `isFile` now import `R.utils::isDirectory` and `R.utils::isFile`.

## goalie 0.1.1 (2018-11-14)

### Minor changes

- `areUniqueGeneNames` now returns `boolean` as expected.
- Improved error message for `assertAreUniqueGeneNames`.

## goalie 0.1.0 (2018-11-11)

- Initial release, migrating assertive check functions from [basejump][]
  package.

[basejump]: https://basejump.acidgenomics.com/
[Acid Genomics]: https://acidgenomics.com/
[syntactic]: https://syntactic.acidgenomics.com/
