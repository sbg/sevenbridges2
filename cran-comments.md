## Comments

#### 2024-07-01

This is a new release (0.2.0).

It introduces support for bulk actions in the API client library:

* Added new methods for bulk actions to facilitate batch processing.
* Enhanced error handling in the `api()` function, replacing `setequal()` with `list_eq()` for compatibility with newer R versions.
* Improved logic for setting API request headers when using Seven Bridges single sign-on token.
* Updated default behavior of the `fields` parameter in `api()` to include all available fields by default.

These changes aim to enhance functionality and maintain compatibility with current R environments.

## Test environments

* Local machine, macOS, R 4.4.0
* devtools::
  * check_win_devel()

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 1 note ✖

* checking installed package size ... NOTE
    installed size is  7.8Mb
    sub-directories of 1Mb or more:
      R     2.0Mb
      doc   5.2Mb

## revdepcheck results
