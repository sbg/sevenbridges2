# nolint start
#' @title R6 Class representing an app
#'
#' @description
#' R6 Class representing a resource for managing apps.
#'
#' @importFrom R6 R6Class
#'
#' @export
App <- R6::R6Class(
  # nolint end
  "App",
  inherit = Item,
  portable = FALSE,
  public = list(
    #' @field URL List of URL endpoints for this resource.
    URL = list(
      "get" = "apps/{id}/{revision}",
      "get_revision" = "apps/{self$id}/{revision}",
      "create_revision" = "apps/{self$id}/{revision}/raw",
      "copy" = "apps/{id}/actions/copy",
      "sync" = "apps/{self$id}/actions/sync"
    ),
    #' @field id Character used as an app ID - short app name.
    id = NULL,
    #' @field project Project ID if any, when returned by an API call.
    project = NULL,
    #' @field name App name.
    name = NULL,
    #' @field revision App's revision number.
    revision = NULL,
    #' @field copy_of The original application of which this is a copy.
    copy_of = NULL,
    #' @field latest_revision App's latest revision number.
    latest_revision = NULL,
    #' @field raw App's raw CWL (JSON or YAML).
    raw = NULL,

    # Initialize App object --------------------------------------------------
    #' @description Create a new App object.
    #'
    #' @param res Response containing App object information.
    #'
    #' @param ... Other response arguments.
    #'
    #' @return A new `App` object.
    initialize = function(res = NA, ...) {
      # Initialize Item class
      super$initialize(...)

      self$id <- sub("/[^/]*$", "", res$id)
      self$project <- res$project
      self$name <- res$name
      self$revision <- res$revision
      self$raw <- res$raw
      self$copy_of <- ifelse(!is.null(res$raw$`sbg:copyOf`),
        res$raw$`sbg:copyOf`, NA
      )
      self$latest_revision <-
        ifelse(!is.null(res$raw$`sbg:latestRevision`),
          res$raw$`sbg:latestRevision`,
          NA
        )
    },

    # nocov start
    # Print App object ------------------------------------------------------
    #' @description Print method for App class.
    #'
    #' @importFrom purrr discard
    #' @importFrom glue glue_col
    #' @importFrom cli cli_h1 cli_li cli_end
    #'
    #' @examples
    #' \dontrun{
    #'  # x is API response when app is requested
    #'  app_object <- App$new(
    #'    res = x,
    #'    href = x$href,
    #'    auth = auth,
    #'    response = attr(x, "response")
    #'  )
    #'  app_object$print()
    #' }
    print = function() {
      x <- as.list(self)

      # Extract all except 'raw'
      x$raw <- NULL

      x <- purrr::discard(x, .p = is.function)
      x <- purrr::discard(x, .p = is.environment)
      x <- purrr::discard(x, .p = is.null)
      x <- purrr::discard(x, .p = is.list)

      # Remove copy_of field if it's empty (NA)
      x <- purrr::discard(x, .p = is.na)

      string <- glue::glue_col("{green {names(x)}}: {x}")

      cli::cli_h1("App")

      cli::cli_li(string)

      # Close container elements
      cli::cli_end()
    },

    # Reload App object ------------------------------------------------------
    #' @description Reload App object information.
    #'  Suitable also for loading raw CWL in the 'raw' field, if it's not
    #'  already populated.
    #'
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @examples
    #' \dontrun{
    #'  # x is API response when app is requested
    #'  app_object <- App$new(
    #'    res = x,
    #'    href = x$href,
    #'    auth = auth,
    #'    response = attr(x, "response")
    #'  )
    #'  app_object$reload()
    #' }
    #' @return \code{\link{App}} object.
    reload = function(...) {
      super$reload(
        cls = self,
        ...
      )
      rlang::inform("App object is refreshed!")
    },
    # nocov end

    # Copy App ---------------------------------------------------------------
    #' @description A method that copies the current app to the
    #'  specified project.
    #'
    #' @param project Project object or project ID. If you opt for the latter,
    #'  remember that the project ID should be specified in
    #'  `<project_owner>/<project-name>` format, e.g. \cr
    #'  `rfranklin/my-project`, or as `<division>/<project-name>`
    #'  depending on the account \cr type.
    #' @param name The new name the app will have in the target project.
    #'  Optional.
    #' @param strategy The method for copying the app. Supported strategies:
    #' \itemize{
    #'    \item `clone` - copy all revisions; get updates from the same app as
    #'    the copied app (default)
    #'    \item `direct`: copy latest revision; get updates from the copied app
    #'    \item `clone_direct`: copy all revisions; get updates from the copied
    #'    app
    #'    \item `transient`: copy latest revision; get updates from the same
    #'    app as the copied app.
    #' }
    #' @param use_revision Parameter specifying which app's revision should be
    #'  copied. If set to `FALSE` (default), the latest revision of the app will
    #'  be copied.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom rlang abort
    #' @importFrom checkmate assert_string assert_logical
    #' @importFrom glue glue
    #'
    #' @examples
    #' \dontrun{
    #'  # x is API response when app is requested
    #'  app_object <- App$new(
    #'    res = x,
    #'    href = x$href,
    #'    auth = auth,
    #'    response = attr(x, "response")
    #'  )
    #'  app_object$copy(project)
    #' }
    #'
    #' @return Copied \code{\link{App}} object.
    copy = function(project,
                    name = NULL,
                    strategy = "clone",
                    use_revision = FALSE,
                    ...) {
      if (is_missing(project)) {
        rlang::abort("Project parameter must be provided!")
      }
      # Check (and transform) project argument
      project <- check_and_transform_id(project,
        class_name = "Project",
        field_name = "id"
      )

      # Check name argument
      checkmate::assert_string(name, null.ok = TRUE)

      # Check strategy argument
      check_app_copy_strategy(strategy)

      # Check use_revision argument
      checkmate::assert_logical(use_revision, len = 1)

      # nocov start
      # Create body
      body <- list(
        project = project,
        name = name,
        strategy = strategy
      )

      # Use full app ID (with revision number) or omit revision number (copy
      # the latest version of the app)
      id <- ifelse(use_revision,
        glue::glue(self$id, "/", self$revision),
        self$id
      )

      path <- glue::glue(self$URL[["copy"]])

      res <- sevenbridges2::api(
        path = path,
        method = "POST",
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      rlang::inform(
        glue::glue_col(
          "App {green {self$name}} has been copied to {green {project}} project." # nolint
        )
      )

      # Return newly created app
      return(asApp(res, auth = self$auth))
      # nocov end
    },

    # Get App's revision -----------------------------------------------------
    #' @description Get app's revision.
    #'
    #' @details
    #'  This call allows you to obtain a particular revision of an
    #'  app, which is not necessarily the most recent version.
    #'
    #' @param revision Revision of the app.
    #' @param in_place If `TRUE`, replace current app object with new for
    #'  specified app revision.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom checkmate assert_numeric assert_logical
    #' @importFrom glue glue
    #'
    #' @examples
    #' \dontrun{
    #'  # x is API response when app is requested
    #'  app_object <- App$new(
    #'    res = x,
    #'    href = x$href,
    #'    auth = auth,
    #'    response = attr(x, "response")
    #'  )
    #'  app_object$get_revision()
    #' }
    #'
    #' @return \code{\link{App}} object.
    get_revision = function(revision = self$revision,
                            in_place = FALSE,
                            ...) {
      # Check if revision is positive number and convert it to integer
      checkmate::assert_numeric(revision, lower = 0, len = 1)
      revision <- as.integer(revision)

      # Check in_place parameter to be logical
      checkmate::assert_logical(in_place,
        len = 1,
        any.missing = FALSE,
        null.ok = FALSE
      )

      path <- glue::glue(self$URL[["get_revision"]])

      # nocov start
      res <- sevenbridges2::api(
        path = path,
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      if (in_place) {
        self$initialize(
          res = res,
          href = res$href,
          response = attr(res, "response"),
          auth = self$auth
        )
      } else {
        # Return new object
        return(asApp(res, self$auth))
      } # nocov end
    },

    # Create App revision ----------------------------------------------------
    #' @description Create a new app revision.
    #'
    #' @details
    #'  This call creates a new revision for an existing app. It adds a new CWL
    #'  app description, and stores it as the named revision for the specified
    #'  app. The revision number must not already exist and should follow the
    #'  sequence of previously created revisions. \cr \cr
    #'  More documentation about how to create the app via API can be found
    #' [here](https://docs.sevenbridges.com/reference/add-an-app-using-raw-cwl).
    #' @param raw A list containing a raw CWL for the app revision you are
    #'  about to create. To generate such a list, you might want to load some
    #'  existing JSON / YAML file. In case that your CWL file is in JSON format,
    #'  please use the `fromJSON` function from the `jsonlite` package to
    #'  minimize potential problems with parsing the JSON file. If you want to
    #'  load a CWL file in YAML format, it is highly recommended to use the
    #'  `read_yaml` function from the `yaml` package. Keep in mind that this
    #'  parameter should not be used together with the `file_path` parameter.
    #' @param from_path A path to a file containing the raw CWL for the app
    #'  (JSON or YAML). This parameter should not be used together with the
    #'  `raw` parameter.
    #' @param raw_format The type of format used (JSON or YAML).
    #' @param in_place If `TRUE`, replace current app object with newly
    #'  created revision.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom rlang abort
    #' @importFrom glue glue glue_col
    #' @importFrom checkmate assert_list assert_character
    #' @importFrom stringr str_extract
    #' @importFrom tools file_ext
    #' @importFrom jsonlite validate fromJSON
    #' @importFrom yaml yaml.load
    #' @importFrom readr read_file
    #'
    #' @examples
    #' \dontrun{
    #'  # x is API response when app is requested
    #'  app_object <- App$new(
    #'    res = x,
    #'    href = x$href,
    #'    auth = auth,
    #'    response = attr(x, "response")
    #'  )
    #'  # Create App object using raw CWL
    #'  app_object$create_revision(raw)
    #' }
    #'
    #' @return \code{\link{App}} object.
    create_revision = function(raw = NULL,
                               from_path = NULL,
                               raw_format = c("JSON", "YAML"),
                               in_place = FALSE,
                               ...) {
      if (is_missing(raw) && is_missing(from_path)) {
        rlang::abort(
          glue::glue_col(
            "Both parameters {magenta raw} and {magenta from_path} are missing. Please provide one of them." # nolint
          )
        )
      }

      if (!is_missing(raw) && !is_missing(from_path)) {
        rlang::abort(
          glue::glue_col(
            "Both parameters {magenta raw} and {magenta from_path} are provided. Please use only one of them." # nolint
          )
        )
      }

      raw_format <- match.arg(raw_format)

      # Check in_place parameter to be logical
      checkmate::assert_logical(in_place,
        len = 1,
        any.missing = FALSE,
        null.ok = FALSE
      )

      if (!is_missing(raw)) {
        # Check if raw parameter is a list
        checkmate::assert_list(raw)
        raw_cwl <- raw
      }

      if (!is_missing(from_path)) {
        # Check if the provided file path is a string
        checkmate::assert_character(from_path, len = 1)

        # Check if the file with the provided path really exists on local disk
        check_file_path(from_path)

        raw_body <- readr::read_file(file = from_path)

        # Check raw_format and read the file with the appropriate function
        if (raw_format == "JSON") {
          jsonlite::validate(raw_body)
          raw_cwl <-
            jsonlite::fromJSON(raw_body, simplifyDataFrame = FALSE)
        }

        if (raw_format == "YAML") {
          raw_cwl <- yaml::yaml.load(raw_body)
        }
      }

      # nocov start
      revision <- self$latest_revision + 1
      path <- glue::glue(self$URL[["create_revision"]])

      res <- sevenbridges2::api(
        path = path,
        method = "POST",
        body = raw_cwl,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      rlang::inform(
        glue::glue_col(
          "New {green {self$name}} app revision with number {green {self$latest_revision + 1}} has been created." # nolint
        )
      )

      # Return new or reload current object with newly created revision
      return(self$get_revision(
        revision = self$latest_revision + 1,
        in_place = in_place
      ))
    },

    # Synchronize Apps -------------------------------------------------------
    #' @description Synchronize a copied app with its parent app.
    #'
    #' @details
    #'  This call synchronizes a copied app with the source app from which it
    #'  has been copied.
    #'
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom glue glue
    #'
    #' @examples
    #' \dontrun{
    #'  # x is API response when app is requested
    #'  app_object <- App$new(
    #'    res = x,
    #'    href = x$href,
    #'    auth = auth,
    #'    response = attr(x, "response")
    #'  )
    #'
    #'   app_object$sync()
    #' }
    #'
    #' @return \code{\link{App}} object.
    sync = function(...) {
      path <- glue::glue(self$URL[["sync"]])
      res <- sevenbridges2::api(
        path = path,
        method = "POST",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      rlang::inform(glue::glue_col("App {green {self$name}} has been updated.")) # nolint

      # Reload object
      self$initialize(
        res = res,
        href = res$href,
        response = attr(res, "response"),
        auth = self$auth
      )
    },

    # Get inputs info ---------------------------------------------------------
    #' @description Get inputs matrix for the app - what are expected inputs
    #'  required or not, with their details about the expected types,
    #'  descriptions etc.
    #'
    #' @return Data frame.
    input_matrix = function() {
      if (is.null(self$raw)) {
        self$reload()
      }
      sevenbridges2:::input_matrix(self$raw)
    },

    # Get outputs info --------------------------------------------------------
    #' @description Get outputs matrix for the app - what are the expected
    #'  outputs of the task running this app, with their details about the
    #'  expected types, descriptions etc.
    #'
    #' @return Data frame.
    output_matrix = function() {
      if (is.null(self$raw)) {
        self$reload()
      }
      sevenbridges2:::output_matrix(self$raw)
    },

    # Create task ------------------------------------------------------------
    #' @description This call creates a new task. You can create either a single
    #'  task or a batch task by using the app's default batching, override
    #'  batching, or disable batching completely. A parent task is a task that
    #'  specifies criteria by which to batch its inputs into a series of further
    #'  sub-tasks, called child tasks. The documentation on
    # nolint start
    #'  [batching tasks](https://docs.sevenbridges.com/docs/about-batch-analyses)
    # nolint end
    #'  for more details on batching criteria.
    #'
    #' @param project The ID string of a project or a Project object where you
    #'  want to create the task in.
    #' @param revision The app
    #'  [revision (version)](https://docs.sevenbridges.com/docs/app-versions)
    #'  number.
    #' @param name The name of the task.
    #' @param description An optional description of the task.
    #' @param execution_settings Named list with detailed task execution
    #'  parameters. Detailed task execution parameters:
    #'  \itemize{
    #'    \item `instance_type`: Possible value is the specific instance type,
    #'      e.g. `"instance_type" = "c4.2xlarge;ebs-gp2;2000"`;
    #'    \item `max_parallel_instances`: Maximum number of instances
    #'      running at the same time. Takes any integer value equal to or
    #'      greater than 1, e.g. `"max_parallel_instances" = 2.`;
    #'    \item `use_memoization`: Set to `FALSE` by default. Set to `TRUE`
    #'      to enable
    #'      [memoization](https://docs.sevenbridges.com/docs/about-memoization);
    #'    \item `use_elastic_disk`: Set to `TRUE` to enable
    #'      [Elastic Disk](https://docs.sevenbridges.com/page/elastic-disk).
    #'  }
    #'
    #' Here is an example:
    #' ```{r}
    #' execution_settings <- list(
    #'   "instance_type" = "c4.2xlarge;ebs-gp2;2000",
    #'   "max_parallel_instances" = 2,
    #'   "use_memoization" = TRUE,
    #'   "use_elastic_disk" = TRUE
    #'   )
    #' ```
    #' @param inputs List of objects. See the section on
    # nolint start
    #'  [specifying task inputs](https://docs.sevenbridges.com/docs/the-api#section-inputs)
    # nolint end
    #'  for information on creating task input objects. Here is an example with
    #'  various input types:
    #'  ```{r}
    #'  inputs <- list(
    #'    "input_file"= "<file_id/file_object>",
    #'    "input_directory" = "<folder_id/folder_object>",
    #'    "input_array_string" = list("<string_elem_1>", "<string_elem_2>"),
    #'    "input_boolean" = TRUE,
    #'    "input_double" = 54.6,
    #'    "input_enum" = "enum_1",
    #'    "input_float" = 11.2,
    #'    "input_integer" = "asdf",
    #'    "input_long" = 4212,
    #'    "input_string" = "test_string",
    #'    "input_record" = list(
    #'      "input_record_field_file" = "<file_id/file_object>",
    #'      "input_record_field_integer" = 42
    #'     )
    #'    )
    #'  ```
    #' @param output_location The output location list allows you to
    #'  define the exact location where your task outputs will be stored.
    #'  The location can either be defined for the entire project using the
    #'  main_location parameter, or individually per each output node, by
    #'  setting the nodes_override parameter to true and defining individual
    #'  output node locations within nodes_location.
    #'  See below for more details.
    #'  \itemize{
    #'    \item `main_location` - Defines the output location for all
    #'      output nodes in the task. Can be a string path within the project in
    #'      which the task is created, for example \cr
    #'      `/Analysis/<task_id>_<task_name>/`
    #'      or a path on an attached volume, \cr such as
    #'      `volumes://volume_name/<project_id>/html`.
    #'      Parts of the path enclosed in angle brackets <> are tokens that are
    #'      dynamically replaced with corresponding values during task
    #'      execution.
    #'    \item `main_location_alias`: The string location (path) in the
    #'      project that will point to the actual location where the outputs are
    #'      stored. Used if main_location is defined as a volume path (starting
    #'      with volumes://), to provide an easy way of accessing output data
    #'      directly from project files.
    #'    \item `nodes_override`: Enables defining of output locations
    #'      for output nodes individually through nodes_location (see below).
    #'      Set to `TRUE` to be able to define individual locations per output
    #'      node. Default: `FALSE`.
    #'      Even if nodes_override is set to `TRUE`, it is not necessary to
    #'      define output locations for each of the output nodes individually.
    #'      Data from those output nodes that don't have their locations
    #'      explicitly defined through nodes_location is either placed in
    #'      main_location (if defined) or at the project files root if a main
    #'      output location is not defined for the task.
    #'    \item `nodes_location`: List of output paths for individual
    #'      task output nodes in the following format for each output node: \cr
    #'      \code{<output-node-id> = \cr list(
    #'        "output_location" = "<output-path>", \cr
    #'        "output_location_alias" = "<alias-path>" \cr
    #'      )} \cr
    #'      ```{r}
    #'      b64html = list( \cr
    #'      "output_location" = "volumes://outputs/tasks/mar-19", \cr
    #'      "output_location_alias" = "/rfranklin/tasks/picard" \cr
    #'      )
    #'      ```
    #'
    #'      In the example above, b64html is the ID of the output node for which
    #'      you want to define the output location, while the parameters are
    #'      defined as follows:
    #'    \itemize{
    #'      \item `output_location` - Can be a path within the project in which
    #'        the task is created, for example \cr
    #'        `/Analysis/<task_id>_<task_name>/`
    #'        or a path on an attached volume, \cr such as
    #'        `volumes://volume_name/<project_id>/html`. Also accepts tokens.
    #'      \item `output_location_alias` - The location (path) in the project
    #'        that will point to the exact location where the output is stored.
    #'        Used if output_location is defined as a volume path
    #'        (starting with volumes://).
    #'      }
    #' }
    #' @param batch This is set to `FALSE` by default. Set to `TRUE` to
    #'  create a batch task and specify the `batch_input` and `batch-by`
    #'  criteria as described below.
    #' @param batch_input The ID of the input on which you wish to batch.
    #'  You would typically batch on the input consisting of a list of files.
    #'  If this parameter is omitted, the default batching criteria defined for
    #'  the app will be used.
    #' @param batch_by Batching criteria in form of list. For example:
    #'  ```{r}
    #'  batch_by = list(
    #'    type = "CRITERIA",
    #'    criteria = list("metadata.condition")
    #'  )
    #'  ```
    #' @param use_interruptible_instances This field can be `TRUE` or `FALSE`.
    #'  Set this field to `TRUE` to allow the use of
    # nolint start
    #' [spot instances](https://docs.sevenbridges.com/docs/about-spot-instances).
    # nolint end
    #' @param action If set to `run`, the task will be run immediately upon
    #'  creation.
    #' @param ... Other arguments that can be passed to core `api()` function
    #'  like 'fields', etc.
    #'
    #' @importFrom checkmate assert_string
    #' @importFrom rlang abort
    #'
    #' @examples
    #' \dontrun{
    #'  # x is API response when app is requested
    #'  app_object <- App$new(
    #'    res = x,
    #'    href = x$href,
    #'    auth = auth,
    #'    response = attr(x, "response")
    #'  )
    #'  # Create a DRAFT task
    #'  app_object$create_task(project = project)
    #' }
    #'
    #' @return \code{\link{Task}} object.
    create_task = function(project,
                           revision = NULL,
                           name = NULL,
                           description = NULL,
                           execution_settings = NULL,
                           inputs = NULL,
                           output_location = output_location,
                           batch = NULL,
                           batch_input = NULL,
                           batch_by = NULL,
                           use_interruptible_instances = NULL,
                           action = NULL,
                           ...) {
      self$auth$tasks$create(
        app = self,
        project = project,
        revision = revision,
        name = name,
        description = description,
        execution_settings = execution_settings,
        inputs = inputs,
        output_location = output_location,
        batch = batch,
        batch_input = batch_input,
        batch_by = batch_by,
        use_interruptible_instances = use_interruptible_instances,
        action = action,
        ...
      )
    }
    # nocov end
  )
)
# nocov start
# Helper functions for creating App objects ---------------------------------
asApp <- function(x = NULL, auth = NULL) {
  App$new(
    res = x,
    href = x$href,
    auth = auth,
    response = attr(x, "response")
  )
}

asAppList <- function(x, auth) {
  obj <- lapply(x$items, asApp, auth = auth)
  obj
}
# nocov end
