# sevenbridges2 0.1.0

* Initial CRAN submission.

# sevenbridges2 0.2.0

## Breaking changes

* The package now includes support for another category of API calls: bulk actions.
* Bulk actions allow users to perform actions on multiple items with a single call. The following bulk API methods have been added:
  - `bulk_submit_import()` in the `Auth$imports` path allows you to import multiple volume files or folders into a project using a single API call.
  - `bulk_get()` in the `Auth$imports` path retrieves details about a bulk import job.
  - `bulk_submit_export()` in the `Auth$exports` path enables you to export multiple project files into a volume using a single API call.
  - `bulk_get()` in the `Auth$exports` path fetches details about a bulk export job.
  - `bulk_get()` in the `Auth$files` path retrieves details of multiple specified files, including file names and metadata.
  - `bulk_update()` in the `Auth$files` path updates the details for multiple specified files, replacing existing information and clearing omitted parameters.
  - `bulk_edit()` in the `Auth$files` path modifies existing information or adds new information for multiple specified files, while preserving omitted parameters.
  - `bulk_delete()` in the `Auth$files` path deletes multiple specified files in a single API call.
  - `bulk_get()` in the `Auth$tasks` path retrieves details of multiple tasks in a single API call.
* Additionally, the package includes two utility functions: `prepare_items_for_bulk_import()` and `prepare_items_for_bulk_export()`. These functions are designed to facilitate the creation of item lists for bulk import and export operations.


## Enhancements and fixes

* Fix error handling checkers in the core `api()` function to ensure compatibility with newer versions of R. The `setequal()` function has been replaced with the custom `list_eq()` function.
* Fix logic for setting API request header when using Seven Bridges single sign-on token for authentication.
* Set the default value of the `fields` parameter in the core `api()` function to "_all", ensuring that all available fields are included in the API response for each resource.
