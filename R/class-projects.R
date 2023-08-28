# nolint start
#' @title R6 Class representing a projects endpoints.
#'
#' @description
#' R6 Class representing Projects resource.
#'
#' @importFrom R6 R6Class
#' @export
Projects <- R6::R6Class(
  # nolint end
  "Projects",
  inherit = Resource,
  portable = FALSE,
  public = list(
    #' @field URL URL endpoint fields
    URL = list(
      "query" = "projects",
      "get" = "projects/{id}",
      "create" = "projects"
    ),

    #' @param ... Other arguments.
    initialize = function(...) {
      # Initialize Resource class
      super$initialize(...)
    },
    # list all projects -------------------------------------------------------
    #' @description A method to list all projects available to particular user.
    #' if the username is not provided, all projects available to the currently
    #' authenticated user will be listed. Otherwise, projects will be listed
    #' for the user whose username is provided. Please keep in mind that this
    #' way you will only be able to list projects you are a member of.
    #' @param name Project's name.
    #' @param owner The username of the owner whose projects you want to query.
    #' @param tags The list of project tags.
    #' @param limit Defines the number of items you want to get from your API
    #' request. By default, `limit` is set to `50`. Maximum is `100`.
    #' @param offset Defines where the retrieved items started.
    #' By default, `offset` is set to `0`.
    #' @param ... Other arguments that can be passed to this method.
    #' Such as query parameters.
    #' @importFrom checkmate assert_string
    query = function(name = NULL,
                     owner = NULL,
                     tags = NULL,
                     limit = getOption("sevenbridges2")$limit,
                     offset = getOption("sevenbridges2")$offset,
                     ...) {
      checkmate::assert_string(name, null.ok = TRUE)
      checkmate::assert_string(owner, null.ok = TRUE)
      check_tags(tags)

      res <- super$query(
        path = self$URL[["query"]],
        name = name,
        tags = tags,
        limit = limit,
        offset = offset,
        ...
      )
      res$items <- asProjectList(res, auth = self$auth)

      return(asCollection(res, auth = self$auth))
    },
    # get specific project ----------------------------------------------------
    #' @description This call creates an object containing the details
    #' of a specified project.
    #' @param project_owner If you are using Enterprise, use the name of the
    #' Division that owns the project; otherwise, enter the project owner's
    #' Platform username. By default, your username is used and properly handled
    #' for the usage in this operation.
    #' @param project The short name of the project you are querying.
    #' @details
    #' Note that project_owner is always case-sensitive, and that project is
    #' not the project's name but its short name. For full details of
    #' identifying objects using the API, please see the API overview.
    #' @param ... Other arguments.
    #' @importFrom stringr str_split
    #' @return Project object.
    get = function(project_owner = NULL, project, ...) {
      if (is_missing(project_owner)) {
        user <- suppressMessages(self$auth$user()$username)
        project_owner <- stringr::str_split(
          user,
          pattern = "/"
        )[[1]][1]
      }
      checkmate::assert_string(project_owner, null.ok = FALSE)
      checkmate::assert_string(project, null.ok = FALSE)

      id <- glue::glue(project_owner, "/", tolower(project))

      res <- super$get(
        cls = self,
        id = id,
        ...
      )
      return(asProject(res, auth = self$auth))
    },
    # create new project
    #' @description A method for creating a new project.
    #'
    #' @param name The name of the project you are creating.
    #' @param billing_group The Billing object or ID of the billing group for
    #'   the project.
    #' @param description Description of the project.
    #' @param tags The list of project tags.
    #' @param locked (boolean) Set this field to true to lock down a project.
    #'    Locking down a project prevents any Seven Bridges team member from
    #'    viewing any information about the task.
    #' @param controlled Set this field to true to define this project as
    #' controlled i.e. one which will contain controlled data. Set false to
    #' define the project as open i.e. one which will contain open data.
    #' @param location Specify the location for this project:
    #' aws:us-east-1 or aws:us-west-2
    #' @param use_interruptible_instances Defines the use of spot instances.
    #' @param use_memoization Set to false by default. Set to true to enable
    #' memoization.
    #' @param use_elastic_disk Set to true to enable Elastic disk.
    #' @param intermediate_files A list defining the retention period for
    #' intermediate files. Expected elements:
    #' \itemize{
    #' \item retention - Specifies that intermediate files should be retained
    #' for a limited amount of time. The value is always LIMITED.
    #' \item duration - Specifies intermediate files retention period in hours.
    #' The minimum value is 1. The maximum value is 120 and the default value
    #' is 24.
    #' }
    #' @param ... Other arguments.
    #' @importFrom rlang inform abort
    #' @importFrom glue glue
    #' @importFrom checkmate assert_string test_character
    create = function(name,
                      billing_group = NULL,
                      description = name,
                      tags = NULL,
                      locked = FALSE,
                      controlled = FALSE,
                      location = NULL,
                      use_interruptible_instances = TRUE,
                      use_memoization = FALSE,
                      use_elastic_disk = FALSE,
                      intermediate_files = list(
                        "retention" = "LIMITED",
                        "duration" = 24
                      ),
                      ...) {
      if (is_missing(name)) {
        rlang::abort("You must provide at least a name for the project you want to create.") # nolint
      }
      checkmate::assert_string(name, null.ok = FALSE)
      checkmate::assert_string(description, null.ok = TRUE)
      if (!is_missing(billing_group)) {
        billing_group <- check_and_transform_id(billing_group, "Billing")
      }
      check_tags(tags)
      checkmate::assert_logical(locked)
      checkmate::assert_logical(controlled)
      checkmate::assert_string(location, null.ok = TRUE)
      checkmate::assert_logical(use_interruptible_instances)
      checkmate::assert_logical(use_memoization)
      checkmate::assert_logical(use_elastic_disk)
      checkmate::assert_list(intermediate_files)

      body <- list(
        "name" = name,
        "description" = description,
        "tags" = tags,
        "billing_group" = billing_group,
        "settings" = list(
          "locked" = locked,
          "controlled" = controlled,
          "location" = location,
          "use_interruptible_instances" = use_interruptible_instances,
          "use_memoization" = use_memoization,
          "use_elastic_disk" = use_elastic_disk,
          "intermediate_files" = intermediate_files
        )
      )

      res <- sevenbridges2::api(
        path = self$URL[["create"]],
        token = self$auth$get_token(),
        body = body,
        method = "POST",
        base_url = self$auth$url,
        ...
      )
      res <- status_check(res)

      rlang::inform(
        glue::glue_col(
          "New project {green {name}} has been created on the {green {self$platform}} platform." # nolint
        )
      )
      return(asProject(res, auth = self$auth))
    }
  )
)
