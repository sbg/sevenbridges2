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
  
---
  
# sevenbridges2 0.4.0

## New features

* Introduced support for **Enterprise API actions**, enabling users to manage
organizational structures such as divisions and teams on the Seven Bridges
Platform.

* The following Enterprise API classes and methods have been added:
  - **Division-related methods**:
    - `query()` and `get()` in the `Auth$divisions` path allow listing all
      divisions and retrieving a division by ID.
    - Fetched Division objects support additional methods:
      - `list_teams()` lists all teams within a division.
      - `list_members()` retrieves division members, with optional filtering by
        role.
      - `remove_member()` removes a member from the division.
        
  - **Team-related methods**:
    - `query()`, `get()`, `create()`, and `delete()` in the `Auth$teams` path
      allow managing teams within divisions.
    - Fetched Team objects support the following actions:
      - `list_members()`, `add_member()`, and `remove_member()` for managing
        team membership.
      - `rename()` and `delete()` for modifying or removing a team.
      
  - **Added support for managing volume access using Enterprise members:**
    - New methods in the Volume class:
      - `add_member_team()` and `add_member_division()` allow adding entire
        teams or divisions as volume members (for users with appropriate
        `ADMIN` privileges).
    - Introduced an internal method `add_member_general()`, which handles
      the underlying logic for adding volume members. All three public methods
      — `add_member()`, `add_member_team()`, and `add_member_division()` — now
      use `add_member_general()` internally to ensure consistency and reduce
      duplication.

* Added a new vignette `Enterprise_actions.Rmd` covering the full set of
Enterprise API actions.

* Updated the `Files_upload_and_Volumes.Rmd` vignette to document volume
sharing with teams and divisions.

## Enhancements and fixes

Fixed typos and refined wording across documentation and code comments for
improved readability and maintainability.

