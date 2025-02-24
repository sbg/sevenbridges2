# sevenbridges2 0.1.0

* Initial CRAN submission.

---

# sevenbridges2 0.2.0

## New features

* The package now includes support for another category of API calls: bulk
actions.
* Bulk actions allow users to perform actions on multiple items with a single
call. The following bulk API methods have been added:
  - `bulk_submit_import()` in the `Auth$imports` path allows you to import
  multiple volume files or folders into a project using a single API call.
  - `bulk_get()` in the `Auth$imports` path retrieves details about a bulk
  import job.
  - `bulk_submit_export()` in the `Auth$exports` path enables you to export
  multiple project files into a volume using a single API call.
  - `bulk_get()` in the `Auth$exports` path fetches details about a bulk export
  job.
  - `bulk_get()` in the `Auth$files` path retrieves details of multiple
  specified files, including file names and metadata.
  - `bulk_update()` in the `Auth$files` path updates the details for multiple
  specified files, replacing existing information and clearing omitted
  parameters.
  - `bulk_edit()` in the `Auth$files` path modifies existing information or
  adds new information for multiple specified files, while preserving omitted
  parameters.
  - `bulk_delete()` in the `Auth$files` path deletes multiple specified files
  in a single API call.
  - `bulk_get()` in the `Auth$tasks` path retrieves details of multiple tasks
  in a single API call.
* Additionally, the package includes two utility functions:
`prepare_items_for_bulk_import()` and `prepare_items_for_bulk_export()`. These
functions are designed to facilitate the creation of item lists for bulk import
and export operations.


## Enhancements and fixes

* Fix error handling checkers in the core `api()` function to ensure
compatibility with newer versions of R. The `setequal()` function has been
replaced with the custom `list_eq()` function.
* Fix logic for setting API request header when using Seven Bridges single
sign-on token for authentication.
* Set the default value of the `fields` parameter in the core `api()` function
to "_all", ensuring that all available fields are included in the API response
for each resource.

---

# sevenbridges2 0.3.0

## New features

* Introduced support for **asynchronous bulk actions**, enabling asynchronous 
operations on multiple items without blocking execution.
* The following **async bulk API methods** have been added:
  - `async_bulk_copy()` in the `Auth$files` path: Copies multiple files and
  folders while preserving the folder structure. Supports:
    - Copying to a subfolder within the same project.
    - Copying to another project.
    - Copying to a subfolder in a different project.
  - `async_bulk_delete()` in the `Auth$files` path: Deletes multiple files or
  folders asynchronously. Empty and non-empty folders can be deleted.
  - `async_bulk_move()` in the `Auth$files` path: Moves multiple files and
  folders. Supports:
    - Moving to the root folder of a project.
    - Moving to a subfolder within the same project or a different project.
  - `async_get_copy_job()` in the `Auth$files` path: Retrieves details of an
  asynchronous bulk copy job.
  - `async_get_delete_job()` in the `Auth$files` path: Retrieves details of an
  asynchronous bulk deletion job.
  - `async_get_move_job()` in the `Auth$files` path: Retrieves details of an
  asynchronous bulk move job.
  - `async_list_file_jobs()`in the `Auth$files` path: Lists all active and
  completed asynchronous bulk jobs initiated by the user.

## Enhancements and fixes

* Updated the **quickstart.Rmd vignette**:
  - Replaced the example app illustrating the `Auth$apps$copy()` method, as the
  previous example was outdated.
  - Updated example input files for improved clarity and consistency.
  - Fixed typos and improved wording for better readability.
