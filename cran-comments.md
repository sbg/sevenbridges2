## Comments

### 2024-07-01

This is a new release (0.2.0).

It introduces support for bulk actions in the API client library:

* Added new methods for bulk actions to facilitate batch processing.
* Enhanced error handling in the `api()` function, replacing `setequal()` with `list_eq()` for compatibility with newer R versions.
* Improved logic for setting API request headers when using Seven Bridges single sign-on token.
* Updated default behavior of the `fields` parameter in `api()` to include all available fields by default.

These changes aim to enhance functionality and maintain compatibility with current R environments.

#### Test environments

* Local machine, macOS, R 4.4.0
* devtools::
  * check_win_devel()

#### R CMD check results

0 errors ✔ | 0 warnings ✔ | 1 note ✖

```
checking installed package size ... NOTE
    installed size is  7.8Mb
    sub-directories of 1Mb or more:
      R     2.0Mb
      doc   5.2Mb
```

---

### 2025-02-24

This release (0.3.0) introduces support for asynchronous bulk actions in the API client library:

* Added new methods for async bulk actions, enabling non-blocking operations on multiple items.
* Introduced `async_bulk_copy()`, `async_bulk_delete()`, and `async_bulk_move()` for bulk file operations.
* Added methods to retrieve details of asynchronous jobs: `async_get_copy_job()`, `async_get_delete_job()`, `async_get_move_job()`, and `async_list_file_jobs()`.
* Updated the `quickstart.Rmd` vignette:
  - Replaced the outdated example app for `Auth$apps$copy()`.
  - Updated example input files.
  - Fixed typos and improved readability.

#### Test environments

* Local machine, macOS, R 4.4.0
* devtools::
  * check_win_devel()

#### R CMD check results

0 errors ✔ | 0 warnings ✔ | 1 note ✖

```
checking installed package size ... NOTE
    installed size is  7.8Mb
    sub-directories of 1Mb or more:
      R     2.0Mb
      doc   5.2Mb
```

There is one more NOTE, but only for `devtools::check_win_devel()`:

```
Found the following (possibly) invalid URLs:
  URL: https://portal.azure.com/
    From: man/Volumes.Rd
    Status: 403
    Message: Forbidden
```

- This is a valid URL which passes `urlchecker::url_check()`, but it requires 
login access.
- The `Volumes` class and its methods have not been modified in this release.

---

### 2025-03-24

This release (0.4.0) introduces support for Enterprise API actions in the API
client library:

* Added new methods for managing organizational structures, including divisions
and teams, with support for listing, creating, modifying, and removing these
entities.

* Introduced support for managing volume access with enterprise members by
adding methods for assigning users, teams, and divisions to volumes.

* Internally refactored volume access logic by introducing a shared method used
across volume member-related operations.

* Added a new vignette `Enterprise_actions.Rmd` and updated
`Files_upload_and_Volumes.Rmd` to document the new functionality.

* Performed a package-wide review to fix typos and improve clarity across R
scripts, tests, and vignettes.

#### Test environments

* Local machine, macOS, R 4.4.0
* devtools::
  * check_win_devel()

#### R CMD check results

0 errors ✔ | 0 warnings ✔ | 1 note ✖

```
checking installed package size ... NOTE
    installed size is  9.1Mb
    sub-directories of 1Mb or more:
      R     2.0Mb
      doc   6.4Mb
```

There is one more NOTE, but only for `devtools::check_win_devel()`:

```
Found the following (possibly) invalid URLs:
  URL: https://portal.azure.com/
    From: man/Volumes.Rd
    Status: 403
    Message: Forbidden
```

- This is a valid URL which passes `urlchecker::url_check()`, but it is not
accessible without authentication.
