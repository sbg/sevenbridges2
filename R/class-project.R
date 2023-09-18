# nolint start
#' @title R6 Class representing a project.
#'
#' @description
#' R6 Class representing a central resource for managing projects.
#'
#' @importFrom R6 R6Class
#' @details
#' This is main object for Projects
Project <- R6::R6Class(
  # nolint end
  "Project",
  inherit = Item,
  portable = FALSE,
  public = list(
    #' @field URL URL endpoint fields
    URL = list(
      "get" = "projects/{id}",
      "project" = "projects/{self$id}",
      "members" = "projects/{self$id}/members",
      "member" = "projects/{self$id}/members/{username}",
      "member_permissions" = "projects/{self$id}/members/{username}/permissions", # nolint
      "files" = "projects/{self$id}/files"
    ),
    #' @field id Project's ID.
    id = NULL,
    #' @field name Project's name.
    name = NULL,
    #' @field billing_group The ID of the billing group for the project.
    billing_group = NULL,
    #' @field description Project's description.
    description = NULL,
    #' @field type Project's type. All projects have type v2.
    type = NULL,
    #' @field tags The list of project tags.
    tags = NULL,
    #' @field settings A list which contains detailed project settings.
    #' The following fields are part of the settings object:
    #' \itemize{
    #'   \item locked (boolean) Set this field to true to lock down a project.
    #'    Locking down a project prevents any Seven Bridges team member from
    #'    viewing any information about the task.
    #'   \item use_interruptible_instances (boolean) Defines the use of spot
    #'    instances. If not included in the request, spot instances are enabled
    #'    by default.
    #'   \item use_memoization (boolean) Set to false by default. Set to true
    #'    to enable memoization.
    #'   \item use_elastic_disk (boolean) Set to true to enable Elastic disk.
    #'   \item intermediate_files (list) Contains the following two sub fields:
    #'   retention - Specifies that intermediate files should be retained for a
    #'   limited amount of time. The value is always LIMITED.
    #'   duration - Specifies intermediate files retention period in hours.
    #'   The minimum value is 1. The maximum value is 120 and the default value
    #'   is 24.
    #' }
    settings = NULL,
    #' @field root_folder ID for of the project's root folder.
    root_folder = NULL,
    #' @field created_by Username of the person who created the project.
    created_by = NULL,
    #' @field created_on Date and time of project creation.
    created_on = NULL,
    #' @field modified_on Date and time describing when the project was
    #' last modified.
    modified_on = NULL,
    #' @field permissions An object containing the information about user's
    #' permissions within the project.
    permissions = NULL,
    #' @field category Project's category. By default projects are PRIVATE.
    category = NULL,
    #' @description Create a new Project object.
    #' @param res Response containing Project object information.
    #' @param ... Other arguments.
    initialize = function(res = NA, ...) {
      # Initialize Item class
      super$initialize(...)

      self$id <- res$id
      self$name <- res$name
      self$billing_group <- res$billing_group
      self$description <- res$description
      self$type <- "v2"
      self$tags <- res$tags
      self$settings <- res$settings
      self$root_folder <- res$root_folder
      self$created_by <- res$created_by
      self$created_on <- res$created_on
      self$modified_on <- res$modified_on
      self$permissions <- res$permissions
      self$category <- res$category
    },
    #' @description  Basic print method for Project class.
    #'
    #' @importFrom glue glue_col
    print = function() {
      cat(glue::glue_col("{blue  Project name: } {self$name}"), "\n") # nocov
      cat(glue::glue_col("{blue  Project id: } {self$id}"), "\n") # nocov
    },
    #' @description Detailed print method for Project class.
    #'
    #' @details This method allows users to print all the fields from the
    #' Project object more descriptively.
    #'
    #' @importFrom purrr discard
    #' @importFrom glue glue
    #' @importFrom cli cli_h1 cli_li cli_ul cli_end
    detailed_print = function() {
      x <- as.list(self)

      if (!is.null(x$settings) && length(x$settings) != 0) {
        project_settings <- x$settings
        string_project_settings <-
          glue::glue("{names(project_settings)}: {project_settings}") # nolint
      }
      if (!is.null(x$tags) && length(x$tags) != 0) {
        project_tags <- x$tags
        names(project_tags) <-
          paste0("tag_", seq_along(project_tags))
        string_project_tags <-
          glue::glue("{names(project_tags)}: {project_tags}") # nolint
      }
      if (!is.null(x$permissions) && length(x$permissions) != 0) {
        project_permissions <- x$permissions
        # Convert permissions env to a list and keep only those elements that
        # are logical
        permissions <- as.list(project_permissions)
        permissions <- purrr::keep(permissions, .p = is.logical)
        string_permissions <-
          glue::glue("{names(permissions)}: {permissions}")
      }
      x <- purrr::discard(x, .p = is.function)
      x <- purrr::discard(x, .p = is.environment)
      x <- purrr::discard(x, .p = is.null)
      x <- purrr::discard(x, .p = is.list)

      # Handle POSIX format
      x$modified_on <- as.character(x$modified_on)
      x$created_on <- as.character(x$created_on)

      x <- purrr::discard(x, .p = ~ is.character(.x) && .x == "")
      string <- glue::glue("{names(x)}: {x}")
      names(string) <- rep("*", times = length(string))

      cli::cli_h1("Project")
      cli::cli_li(string)


      ifelse(exists("project_settings") &&
        !is.null(project_settings),
      {
        cli::cli_li("settings")
        cli::cli_ul(string_project_settings)
      },
      ""
      )
      ifelse(exists("project_tags") && !is.null(project_tags),
        {
          cli::cli_li("tags")
          cli::cli_ul(string_project_tags)
        },
        ""
      )
      ifelse(exists("permissions") && !is.null(permissions),
        {
          cli::cli_li("permissions")
          cli::cli_ul(string_permissions)
        },
        ""
      )
      # Close container elements
      cli::cli_end()
    },
    #' @description
    #' Reload Project.
    #' @param ... Other query parameters.
    #' @return Project
    reload = function(...) {
      super$reload(
        cls = self,
        ...
      )
      rlang::inform("Project object is refreshed!")
    },
    # Update project ---------------------------------------------------------
    #' @description Method that allows you to edit an already existing project.
    #' As a project Admin you can use it to change the name, settings,
    #' tags or billing group of the project. Users with write permissions in
    #' the project can change the project description.
    #' @param name The name of the project you are creating.
    #' @param description Description of the project.
    #' @param billing_group Billing object or ID of a particular billing
    #'   group for the project.
    #' @param settings Contains detailed project settings.
    #' @param tags The list of project tags.
    #' @param ... Other arguments that can be passed to api() function
    #' like 'limit', 'offset', 'fields', etc.
    #' @importFrom checkmate assert_string
    #' @importFrom rlang abort
    #' @importFrom glue glue
    update = function(name = NULL,
                      description = NULL,
                      billing_group = NULL,
                      settings = NULL,
                      tags = NULL,
                      ...) {
      checkmate::assert_string(name, null.ok = TRUE)
      checkmate::assert_string(description, null.ok = TRUE)
      check_tags(tags)
      check_settings(settings)
      if (!is_missing(billing_group)) {
        billing_group <-
          check_and_transform_id(billing_group, "Billing")
      }
      # nocov start
      body <- list(
        "name" = name,
        "description" = description,
        "billing_group" = billing_group,
        "settings" = settings,
        "tags" = tags
      )

      body <- body[!sapply(body, is.null)]
      if (length(body) == 0) {
        rlang::abort("Please provide updated information.")
      }

      res <- sevenbridges2::api(
        path = glue::glue(self$URL[["project"]]),
        method = "PATCH",
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )



      # Reload object
      self$initialize(
        res = res,
        href = res$href,
        response = attr(res, "response"),
        auth = self$auth
      )
    },
    # nocov end
    # Delete project ---------------------------------------------------------
    #' @description Method that allows you to delete project from a platform.
    #' It can only be successfully made if you have admin status for the
    #' project.
    #' Please be careful when using this method and note that calling it will
    #' permanently delete the project from the platform.
    #' @importFrom rlang abort inform
    #' @importFrom httr content
    #' @importFrom glue glue
    delete = function() {
      # nocov start
      res <- sevenbridges2::api(
        path = glue::glue(self$URL[["project"]]),
        method = "DELETE",
        token = self$auth$get_token(),
        base_url = self$auth$url
      )



      rlang::inform(message = glue::glue("Project {self$id} has been deleted.")) # nolint
    },
    # nocov end
    # Project members ---------------------------------------------------------
    #' @description Method for listing all the project members.
    #' @param limit Defines the number of items you want to get from your API
    #' request. By default, `limit` is set to `50`. Maximum is `100`.
    #' @param offset Defines where the retrieved items started.
    #' By default, `offset` is set to `0`.
    #' @param ... Other arguments.
    #' @importFrom glue glue
    #' @return List of Member class objects.
    list_members = function(limit = getOption("sevenbridges2")$limit,
                            offset = getOption("sevenbridges2")$offset,
                            ...) {
      # nocov start
      res <- sevenbridges2::api(
        path = glue::glue(self$URL[["members"]]),
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        limit = limit,
        offset = offset,
        ...
      )


      res$items <- asMemberList(res, self$auth)

      return(asCollection(res, auth = self$auth))
    },
    # nocov end
    #' @description Method for adding new members to a specified project.
    #' The call can only be successfully made by a user who has admin
    #' permissions in the project.
    #' @param user The Seven Bridges Platform username of the person
    #' you want to add to the project or object of class Member containing
    #' user's username.
    #' @param email The email address of the person you want to add to the
    #' project. This has to be the email address that the person used when
    #' registering for an account on the Seven Bridges Platform.
    #' @param permissions List of permissions that will be associated with the
    #' project's member. It can contain fields: 'read', 'copy', 'write',
    #' 'execute' and 'admin' with logical fields - TRUE if certain permission
    #' is allowed to the user, or FALSE if it's not.
    #' Requests to add a project member must include the key permissions.
    #' However, if you do not include a value for some permission, it will be
    #' set to FALSE by default. The exception to this rule is the 'read'
    #' permission, which is the default permission on a project. It enables a
    #' user to read project data, including file names, but access file
    #' contents.
    #'
    #' Example: list(read = TRUE, copy = TRUE, write = FALSE, execute = FALSE,
    #' admin = FALSE)
    #' @importFrom rlang abort
    #' @importFrom glue glue glue_col
    #' @importFrom checkmate assert_character assert_list assert_subset
    #' @return Member object.
    add_member = function(user = NULL,
                          email = NULL,
                          permissions = list(
                            read = TRUE,
                            copy = FALSE,
                            write = FALSE,
                            execute = FALSE,
                            admin = FALSE
                          )) {
      if (is_missing(user) && is_missing(email)) {
        rlang::abort(
          "Neither username nor email are provided. You must provide at least one of these parameters before you can add a user to a project." # nolint
        )
      }
      if (!is_missing(user)) {
        username <- check_and_transform_id(user,
          class_name = "Member",
          field_name = "username"
        )
      }
      if (!is_missing(email)) {
        checkmate::assert_character(email, len = 1, null.ok = TRUE)
      }
      checkmate::assert_list(
        permissions,
        null.ok = FALSE,
        max.len = 5,
        min.len = 1,
        types = "logical"
      )
      checkmate::assert_subset(
        names(permissions),
        empty.ok = FALSE,
        choices = c(
          "read", "copy", "execute", "write",
          "admin"
        )
      )

      # nocov start
      body <- list(
        "username" = username,
        "email" = email,
        "permissions" = permissions
      )

      res <- sevenbridges2::api(
        path = glue::glue(self$URL[["members"]]),
        method = "POST",
        token = self$auth$get_token(),
        body = body,
        authorization = self$auth$authorization,
        base_url = self$auth$url
      )



      return(asMember(res, auth = self$auth))
    },
    # nocov end
    #' @description A method for removing members from the project. It can only
    #' be successfully run by a user who has admin privileges in the project.
    #' @param user The Seven Bridges Platform username of the person
    #' you want to remove from the project or object of class Member containing
    #' user's username.
    #' @importFrom rlang abort inform
    #' @importFrom glue glue glue_col
    remove_member = function(user) {
      if (is_missing(user)) {
        rlang::abort(
          "Please provide a username for the user or project member you want to remove from the project." # nolint
        )
      }
      # nocov start
      username <- check_and_transform_id(user,
        class_name = "Member",
        field_name = "username"
      )
      res <- sevenbridges2::api(
        path = glue::glue(self$URL[["member"]]),
        method = "DELETE",
        token = self$auth$get_token(),
        authorization = self$auth$authorization,
        base_url = self$auth$url
      )



      rlang::inform(
        message = glue::glue_col(
          "User {green {username}} has been deleted
          from the {green {self$id}} project.",
          .literal = TRUE
        )
      )
    },
    # nocov end
    #' @description This method returns the information about the member of
    #' the specified project.
    #' @param user The Seven Bridges Platform username of the project member
    #' you want to get information about or object of class Member containing
    #' user's username.
    #' @importFrom rlang abort
    #' @importFrom glue glue
    #' @param ... Other arguments.
    get_member = function(user, ...) {
      if (is_missing(user)) {
        rlang::abort("Please provide a username or Member object.")
      }
      # nocov start
      username <- check_and_transform_id(user,
        class_name = "Member",
        field_name = "username"
      )
      res <- sevenbridges2::api(
        path = glue::glue(self$URL[["member"]]),
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )



      return(asMember(res, self$auth))
    },
    # nocov end
    #' @description This method can be used to edit a user's permissions in a
    #' specified  project. It can only be successfully made by a user who
    #' has admin permissions in the project.
    #' @param user The Seven Bridges Platform username of the person
    #' you want to modify permissions on the volume for or object of class
    #' Member containing user's username.
    #' @param permissions List of permissions that will be updated for the
    #' user. It can contain fields: 'read', 'copy', 'execute', 'write' and
    #' 'admin' with logical values - TRUE if certain permission is allowed to
    #' the user, or FALSE if it's not.
    #' Example: list(read = TRUE, copy = TRUE)
    #' @importFrom rlang abort
    #' @importFrom glue glue glue_col
    #' @importFrom checkmate assert_list assert_subset
    #' @return Permission object.
    modify_member_permissions = function(user = NULL,
                                         permissions = list(
                                           read = TRUE,
                                           copy = FALSE,
                                           write = FALSE,
                                           execute = FALSE,
                                           admin = FALSE
                                         )) {
      if (is_missing(user)) {
        rlang::abort("Please provide a username or Member object.")
      }
      username <- check_and_transform_id(user,
        class_name = "Member",
        field_name = "username"
      )
      checkmate::assert_list(
        permissions,
        null.ok = FALSE,
        max.len = 5,
        min.len = 1,
        types = "logical"
      )
      checkmate::assert_subset(
        names(permissions),
        empty.ok = FALSE,
        choices = c(
          "read", "copy", "execute", "write",
          "admin"
        )
      )
      # nocov start
      body <- flatten_query(permissions)
      body <- body[!sapply(body, is.null)]

      if (length(body) == 0) {
        rlang::abort("Please provide updated information.")
      }

      res <- sevenbridges2::api(
        path = glue::glue(self$URL[["member_permissions"]]),
        method = "PATCH",
        token = self$auth$get_token(),
        body = body,
        authorization = self$auth$authorization,
        base_url = self$auth$url
      )



      rlang::inform(glue::glue_col("Permissions for {green {username}} have been changed.")) # nolint

      return(asPermission(res, auth = self$auth))
    },
    # nocov end
    # Project files ---------------------------------------------------------
    #' @description  List all project's files and folders.
    #' @param limit The maximum number of collection items to return for a
    #'   single request. Minimum value is 1. The maximum value is 100 and the
    #'   default value is 50. This is a pagination-specific attribute.
    #' @param offset The zero-based starting index in the entire collection of
    #'   the first item to return. The default value is 0. This is a
    #'   pagination-specific attribute.
    #' @param ... Other arguments that can be passed to api() function
    #' like 'limit', 'offset', 'fields', etc.
    #' @importFrom glue glue
    list_files = function(limit = getOption("sevenbridges2")$limit,
                          offset = getOption("sevenbridges2")$offset,
                          ...) {
      # nocov start
      res <- sevenbridges2::api(
        path = glue::glue(self$URL[["files"]]),
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        limit = limit,
        offset = offset,
        ...
      )


      res$items <- asFileList(res, self$auth)

      return(asCollection(res, auth = self$auth))
    },
    # nocov end
    #' @description  Create a new folder under the project's root directory.
    #' Every project on the Seven Bridges Platform is represented
    #' by a root folder which contains all the files associated
    #' with a particular project. You can create first level folders
    #' within this root folder by using this function.
    #'
    #' @param name Folder name.
    #' @param ... Other arguments that can be passed to api() function
    #' like 'limit', 'offset', 'fields', etc.
    #' @importFrom glue glue_col
    #' @importFrom rlang inform
    create_folder = function(name, ...) {
      check_folder_name(name)
      # nocov start
      body <- list(
        "name" = name,
        "project" = self$id,
        "type" = "FOLDER"
      )

      res <- sevenbridges2::api(
        path = "files",
        method = "POST",
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )



      rlang::inform(glue::glue_col("New folder {green {name}} has been created.")) # nolint

      return(asFile(res, self$auth))
    },
    #' @description  Get project's root folder object
    get_root_folder = function() {
      self$auth$files$get(id = self$root_folder)
    },
    # Project apps ---------------------------------------------------------
    #' @description This call lists all apps in project.
    #'
    #' @param query_terms Enter one or more search terms to query Project's
    #'   apps.
    #' @param id Use this parameter to query Project's apps based on their ID.
    #' @param limit The maximum number of collection items to return for a
    #'   single request. Minimum value is 1. The maximum value is 100 and the
    #'   default value is 50. This is a pagination-specific attribute.
    #' @param offset The zero-based starting index in the entire collection of
    #'   the first item to return. The default value is 0. This is a
    #'   pagination-specific attribute.
    #' @param ... Other arguments that can be passed to this method.
    #' Such as query parameters.
    list_apps = function(query_terms = NULL,
                         id = NULL,
                         limit = getOption("sevenbridges2")$limit,
                         offset = getOption("sevenbridges2")$offset,
                         ...) {
      self$auth$apps$query(
        project = self$id,
        visibility = "private",
        query_terms = query_terms,
        id = id,
        limit = limit,
        offset = offset,
        ...
      )
    },
    #' @description This call creates app in project.
    #'
    #' @param raw The body of the request should be a CWL app description saved
    #'   as a `JSON` or `YAML` file. For a template of this description, try
    #'   making the call to get raw CWL for an app about an app already in one
    #'   of your projects. Shouldn't be used together with `from_path`
    #'   parameter.
    #' @param from_path File containing CWL app description. Shouldn't be used
    #'   together with raw parameter.
    #' @param name A short name for the app (without any non-alphanumeric
    #'   characters or spaces).
    #' @param raw_format The type of format used (`JSON` or `YAML`).
    create_app = function(raw = NULL,
                          from_path = NULL,
                          name,
                          raw_format = c("JSON", "YAML")) {
      self$auth$apps$create(
        raw = raw,
        from_path = from_path,
        project = self$id,
        name = name,
        raw_format = raw_format
      )
    },
    # Project tasks ---------------------------------------------------------
    #' @description This call lists all tasks from project you can access.
    #'
    #' @param status String. You can filter the returned tasks by their status.
    #' Set the value of status to one of the following values: `QUEUED`, `DRAFT`
    #' , `RUNNING`, `COMPLETED`, `ABORTED`, `FAILED`.
    #' @param parent Provide task ID or task object of the parent task to return
    #'  all child tasks from that parent. A parent task is a task that specifies
    #' criteria by which to batch its inputs into a series of further sub-tasks,
    #'  called child tasks. See the documentation on
    #' [batching tasks](https://docs.sevenbridges.com/docs/about-batch-analyses)
    #'  for more details on how to run tasks in batches.
    #' @param created_from String. Enter the starting date for querying tasks
    #' created on the specified date and onwards.
    #' @param created_to String. Enter the ending date for querying tasks
    #' created until the specified date. You can use it in combination with
    #' `created_from` to specify a time interval.
    #' @param started_from String. Enter the starting date for querying tasks
    #' started on the specified date and onwards.
    #' @param started_to String. Enter the ending date for querying tasks
    #' started until the specified date.
    #' @param ended_from String. Enter the starting date for querying tasks
    #' that ended on a specified date.
    #' @param ended_to String. Enter the ending date for querying tasks that
    #' ended until a specified date.
    #' @param order_by String. Order returned results by the specified field.
    #' Allowed values: `created_time`, `start_time`, `name`, `end_time` and
    #' `created_by`. Sort can be done only by one column. The default value is
    #' `created_time`.
    #' @param order String. Sort results in ascending or descending order by
    #' specifying `asc` or `desc`, respectively. Only taken into account if
    #' `order_by` is explicitly specified. The default value is `asc`.
    #' @param origin_id String. Enter an automation run ID to list all tasks
    #' created from the specified automation run.
    #' @param limit The maximum number of collection items to return for a
    #' single request. Minimum value is 1. The maximum value is 100 and the
    #' default value is 50. This is a pagination-specific attribute.
    #' @param offset The zero-based starting index in the entire collection of
    #' the first item to return. The default value is 0. This is a
    #' pagination-specific attribute.
    #' @param ... Other arguments such as `fields` which can be used to specify
    #' a subset of fields to include in the response.
    list_tasks = function(status = NULL,
                          parent = NULL,
                          created_from = NULL,
                          created_to = NULL,
                          started_from = NULL,
                          started_to = NULL,
                          ended_from = NULL,
                          ended_to = NULL,
                          order_by = c(
                            "created_time", "start_time", "name",
                            "end_time", "created_by"
                          ),
                          order = c("asc", "desc"),
                          origin_id = NULL,
                          limit = getOption("sevenbridges2")$limit,
                          offset = getOption("sevenbridges2")$offset,
                          ...) {
      self$auth$tasks$query(
        status = status,
        parent = parent,
        project = self$id,
        created_from = created_from,
        created_to = created_to,
        started_from = started_from,
        started_to = started_to,
        ended_from = ended_from,
        ended_to = ended_to,
        order_by = order_by,
        order = order,
        origin_id = origin_id,
        limit = limit,
        offset = offset,
        ...
      )
    },

    #' @description This call lists imports initiated by particular user
    #' into this destination project.
    #'
    #' @param volume String volume id or Volume object. List all imports
    #' from particular volume. Optional.
    #' @param state String. The state of the import job. Possible values are:
    #' \itemize{
    #'    \item `PENDING`: the import is queued;
    #'    \item `RUNNING`: the import is running;
    #'    \item `COMPLETED`: the import has completed successfully;
    #'    \item `FAILED`: the import has failed.
    #' }
    #' Example: state = c("RUNNING", "FAILED")
    #' @param limit Defines the number of items you want to get from your API
    #' request. By default, `limit` is set to `50`. Maximum is `100`.
    #' @param offset Defines where the retrieved items started.
    #' By default, `offset` is set to `0`.
    #' @param ... Other arguments that can be passed to api() function
    #' like 'fields', etc.
    #' @return Collection of import jobs (Import class objects).
    list_imports = function(volume = NULL,
                            state = NULL,
                            limit = getOption("sevenbridges2")$limit,
                            offset = getOption("sevenbridges2")$offset,
                            ...) {
      self$auth$imports$query(
        project = self,
        volume = volume,
        state = state,
        limit = limit,
        offset = offset,
        ...
      )
    },
    #' @description This call creates a new task. You can create either a single
    #'  task or a batch task by using the app's default batching, override
    #'  batching, or disable batching completely. A parent task is a task that
    #'  specifies criteria by which to batch its inputs into a series of further
    #'  sub-tasks, called child tasks. the documentation on
    # nolint start
    #' [batching tasks] (https://docs.sevenbridges.com/docs/about-batch-analyses)
    # nolint end
    #' for more details on batching criteria.
    #'
    #' @param app The ID string of an app or an App object you want to run.
    #' Recall that apps are specified by their projects, in the form
    #' `{project_owner}/{project}/{app_name}`
    #' @param revision Numeric. The app
    #' [revision (version)] (https://docs.sevenbridges.com/docs/app-versions)
    #'  number.
    #' @param name String. The name of the task.
    #' @param description String. An optional description of the task.
    #' @param execution_settings Named list with detailed task execution
    #' parameters. Detailed task execution parameters:
    #' * `instance_type`: String. Possible value is the specific instance type,
    #' e.g. `"instance_type" = "c4.2xlarge;ebs-gp2;2000"`.
    #' * `max_parallel_instances`: Integer. Maximum number of instances
    #' running at the same time. Takes any integer value equal to or greater
    #' than 1, e.g. `"max_parallel_instances" = 2.`
    #' * `use_memoization`: Boolean. Set to `FALSE` by default. Set to `TRUE`
    #' to enable
    #' [memoization](https://docs.sevenbridges.com/docs/about-memoization).
    #' * `use_elastic_disk`: Boolean. Set to `TRUE` to enable
    #' [Elastic Disk](https://docs.sevenbridges.com/page/elastic-disk).
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
    #' [specifying task inputs](https://docs.sevenbridges.com/docs/the-api#section-inputs)
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
    #'  ````
    #' @param batch Boolean. This is set to `FALSE` by default. Set to `TRUE` to
    #' create a batch task and specify the `batch_input` and `batch-by`
    #' criteria as described below.
    #' @param batch_input String. The ID of the input on which you wish to
    #' batch. You would typically batch on the input consisting of a list of
    #' files. If this parameter is omitted, the default batching criteria
    #' defined for the app will be used.
    #' @param use_interruptible_instances Boolean. This field can be `TRUE` or
    #' `FALSE`. Set this field to `TRUE` to allow the use of
    # nolint start
    #' [spot instances](https://docs.sevenbridges.com/docs/about-spot-instances).
    # nolint end
    #' @param action String. If set to `run`, the task will be run immediately
    #' upon creation.
    #' @param ... Other arguments such as `fields` which can be used to specify
    #' a subset of fields to include in the response.
    #' @importFrom checkmate assert_string
    #' @importFrom rlang abort
    create_task = function(app,
                           revision = NULL,
                           name = NULL,
                           description = NULL,
                           execution_settings = NULL,
                           inputs = NULL,
                           batch = NULL,
                           batch_input = NULL,
                           use_interruptible_instances = NULL,
                           action = NULL,
                           ...) {
      self$auth$tasks$create(
        app = app,
        project = self,
        revision = revision,
        name = name,
        description = description,
        execution_settings = execution_settings,
        inputs = inputs,
        batch = batch,
        batch_input = batch_input,
        use_interruptible_instances = use_interruptible_instances,
        action = action,
        ...
      ) # nocov end
    }
  )
)

# Helper function for creating Project objects --------------------------------
asProject <- function(x = NULL, auth = NULL) {
  Project$new(
    res = x,
    href = x$href,
    auth = auth,
    response = attr(x, "response")
  )
}


asProjectList <- function(x, auth) {
  obj <- lapply(x$items, asProject, auth = auth)
  obj
}
