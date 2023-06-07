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
    #' @param id Project's ID.
    #' @param name Project's name.
    #' @param billing_group The ID of the billing group for the project.
    #' @param description Project's description.
    #' @param type Project's type. All projects have type v2.
    #' @param tags The list of project tags. By default, projects do not have
    #' any tags.
    #' @param settings A list which contains detailed project settings.
    #' @param root_folder ID for of the project's root folder.
    #' @param created_by Username of the person who created the project.
    #' @param created_on Date and time of project creation.
    #' @param modified_on Date and time describing when the project was
    #' last modified.
    #' @param permissions An object containing the information about user's
    #' permissions within the project.
    #' @param category Project's category. By default projects are PRIVATE.
    #' @param ... Other arguments.
    initialize = function(id = NA,
                          name = NA,
                          billing_group = NA,
                          description = NA,
                          type = "v2",
                          tags = NA,
                          settings = NA,
                          root_folder = NA,
                          created_by = NA,
                          created_on = NA,
                          modified_on = NA,
                          permissions = NA,
                          category = NA,
                          ...) {
      # Initialize Item class
      super$initialize(...)

      self$id <- id
      self$name <- name
      self$billing_group <- billing_group
      self$description <- description
      self$type <- type
      self$tags <- tags
      self$settings <- settings
      self$root_folder <- root_folder
      self$created_by <- created_by
      self$created_on <- created_on
      self$modified_on <- modified_on
      self$permissions <- permissions
      self$category <- category
    },
    #' @description  Basic print method for Project class.
    #'
    #' @importFrom glue glue_col
    print = function() {
      cat(glue::glue_col("{blue  Project name: } {self$name}"), "\n") # nocov
      cat(glue::glue_col("{blue  Project id: } {self$id}"), "\n") # nocov
    },
    # nocov start

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
        string_project_settings <- glue::glue(
          "{names(project_settings)}: {project_settings}"
        )
      }
      if (!is.null(x$tags) && length(x$tags) != 0) {
        project_tags <- x$tags
        names(project_tags) <- paste0("tag_", seq_along(project_tags))
        string_project_tags <- glue::glue(
          "{names(project_tags)}: {project_tags}"
        )
      }
      if (!is.null(x$permissions) && length(x$permissions) != 0) {
        project_permissions <- x$permissions
        string_project_permissions <- glue::glue(
          "{names(project_permissions)}: {project_permissions}"
        )
      }
      x <- purrr::discard(x, .p = is.function)
      x <- purrr::discard(x, .p = is.environment)
      x <- purrr::discard(x, .p = is.null)
      x <- purrr::discard(x, .p = is.list)
      x <- purrr::discard(x, .p = ~ .x == "")
      string <- glue::glue("{names(x)}: {x}")
      names(string) <- rep("*", times = length(string))

      cli::cli_h1("Project")
      cli::cli_li(string)


      ifelse(exists("project_settings") && !is.null(project_settings),
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
      ifelse(exists("project_permissions") && !is.null(project_permissions),
        {
          cli::cli_li("permissions")
          cli::cli_ul(string_project_permissions)
        },
        ""
      )
      # Close container elements
      cli::cli_end()
    },

    # edit project ---------------------------------------------------------
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
    edit = function(name = NULL,
                    description = NULL,
                    billing_group = NULL,
                    settings = NULL,
                    tags = NULL, ...) {
      check_tags(tags)
      check_settings(settings)
      billing_group <- check_and_transform_id(billing_group, "Billing")

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
        path = paste0("projects/", self$id),
        method = "PATCH",
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      res <- status_check(res)

      asProject(res, self$auth)
    },
    # delete project ---------------------------------------------------------
    #' @description Method that allows you to delete project from a platform.
    #' It can only be successfully made if you have admin status for the
    #' project.
    #' Please be careful when using this method and note that calling it will
    #' permanently delete the project from the platform.
    delete = function() {
      res <- sevenbridges2::api(
        path = paste0("projects/", self$id),
        method = "DELETE",
        token = self$auth$get_token(),
        base_url = self$auth$url
      )

      if (res$status_code == 204) {
        rlang::inform(message = glue::glue(
          "Project {self$id} has been deleted."
        ))
      } else if (res$status_code %in% c("401", "403", "404", "503")) {
        msg <- httr::content(res, as = "parsed")$message
        rlang::abort(glue::glue("HTTP Status {res$status_code} : {msg}"))
      }
    },
    #' @description Method for listing all the project members.
    #' @param ... Other arguments.
    member_list = function(...) {
      res <- sevenbridges2::api(
        path = paste0("projects/", self$id, "/members"),
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      res <- status_check(res)

      members_list <- asMemberList(res, self$auth)
      members_list
    },
    #' @description Method for adding new members to a specified project.
    #' The call can only be successfully made by a user who has admin
    #' permissions in the project.
    #' @param username The Seven Bridges Platform username of the person
    #' you want to add to the project.
    #' @param email The email address of the person you want to add to the
    #' project. This has to be the email address that the person used when
    #' registering for an account on the Seven Bridges Platform.
    #' @param write Whether the user should have the write permission.
    #' @param read Whether the user should have the read permission.
    #' @param copy Whether the user should have the copy permission.
    #' @param execute Whether the user should have the execute permission.
    #' @param admin Whether the user should have the admin permission.
    #' @param ... Other arguments that can be passed to api() function
    #' like 'limit', 'offset', 'fields', etc.
    member_add = function(username = NULL,
                          email = NULL,
                          write = TRUE,
                          read = TRUE,
                          copy = TRUE,
                          execute = TRUE,
                          admin = FALSE,
                          ...) {
      if (is_missing(username) && is_missing(email)) {
        rlang::abort("Neither username nor email are provided. You must
                       provide at least one of these parameters before you can
                       add a user to a project.")
      }
      body <- list(
        "username" = username,
        "email" = email,
        "permissions" = list(
          "write" = write,
          "read" = read,
          "copy" = copy,
          "execute" = execute,
          "admin" = admin
        )
      )

      req <- sevenbridges2::api(
        path = paste0("projects/", self$id, "/members"),
        method = "POST",
        token = self$auth$get_token(),
        body = body,
        authorization = self$auth$authorization,
        base_url = self$auth$url,
        ...
      )

      res <- status_check(req)

      asMember(res)
    },
    #' @description A method for deleting members from the project. It can only
    #' be successfully run by a user who has admin privileges in the project.
    #' @param username The Seven Bridges Platform username of the user you
    #' are about to remove.
    member_delete = function(username = NULL) {
      if (is_missing(username)) {
        rlang::abort("Please provide a username for the user you want to remove
                     from the project.")
      }
      req <- sevenbridges2::api(
        path = paste0("projects/", self$id, "/members", "/", username),
        method = "DELETE",
        token = self$auth$get_token(),
        authorization = self$auth$authorization,
        base_url = self$auth$url
      )


      if (req$status_code == 204) {
        rlang::inform(message = glue::glue_col(
          "User {green {username}} has been deleted
          from the {green {self$id}} project.",
          .literal = TRUE
        ))
      }
    },
    #' @description This method returns the permissions of a specified user
    #' within a specified project.
    #' @param username Username of the user whose permissions you are
    #' enquiring about.
    #' @param ... Other arguments.
    member_permissions_get = function(username = NULL, ...) {
      if (is_missing(username)) {
        rlang::abort("Please provide a username.")
      }
      req <- sevenbridges2::api(
        path = paste0("projects/", self$id, "/members", "/", username),
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      res <- status_check(req)

      asMember(res, self$auth)
    },
    #' @description This method can be used to edit a user's permissions in a
    #' specified  project.
    #' @param username The project member whose permissions you are editing.
    #' @param write Whether the user should have the write permission.
    #' @param read Whether the user should have the read permission.
    #' @param copy Whether the user should have the copy permission.
    #' @param execute Whether the user should have the execute permission.
    #' @param admin Whether the user should have the admin permission.
    #' @param ... Other arguments that can be passed to api() function
    #' like 'limit', 'offset', 'fields', etc.
    member_permissions_modify = function(username = NULL,
                                         write = TRUE,
                                         read = TRUE,
                                         copy = TRUE,
                                         execute = TRUE,
                                         admin = FALSE, ...) {
      if (is_missing(username)) {
        rlang::abort("Please provide a username.")
      }
      body <- list(
        "write" = write,
        "read" = read,
        "copy" = copy,
        "execute" = execute,
        "admin" = admin
      )

      body <- body[!sapply(body, is.null)]

      if (length(body) == 0) {
        rlang::abort("Please provide updated information.")
      }

      req <- sevenbridges2::api(
        path = paste0(
          "projects/", self$id, "/members", "/", username,
          "/permissions"
        ),
        method = "PATCH",
        token = self$auth$get_token(),
        body = body,
        authorization = self$auth$authorization,
        base_url = self$auth$url,
        ...
      )

      res <- status_check(req)


      if (req$status_code == 200) {
        rlang::inform(glue::glue_col(
          "Permissions for {green {username}} have been changed."
        ))
      } else {
        rlang::abort("Oops, something went wrong. ")
        res
      }
    },
    #' @description  List all project's files and folders.
    #' @param ... Other arguments that can be passed to api() function
    #' like 'limit', 'offset', 'fields', etc.
    files = function(...) {
      req <- sevenbridges2::api(
        path = paste0("projects/", self$id, "/files"),
        method = "GET",
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      res <- status_check(req)

      asFileList(res, self$auth)
    },
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
    create_folder = function(name, ...) {
      check_folder_name(name)

      body <- list(
        "name" = name,
        "project" = self$id,
        "type" = "FOLDER"
      )

      req <- sevenbridges2::api(
        path = "files",
        method = "POST",
        body = body,
        token = self$auth$get_token(),
        base_url = self$auth$url,
        ...
      )

      res <- status_check(req)

      if (attr(res, "response")$status_code == 201) {
        # nolint start
        rlang::inform(glue::glue_col("New folder {green {name}} has been created."))
        # nolint end
      }
      asFile(res, self$auth)
    },
    #' @description  Get project's root folder object
    get_root_folder = function() {
      self$auth$get_file(id = self$root_folder)
    },
    # nocov end
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
    list_apps = function(query_terms = NULL,
                         id = NULL,
                         limit = getOption("sevenbridges2")$limit,
                         offset = getOption("sevenbridges2")$offset) {
      self$auth$apps$query(
        project = self$id,
        visibility = "private",
        query_terms = query_terms,
        id = id,
        limit = limit,
        offset = offset
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
    }
  )
)

# Helper function for creating Project objects
asProject <- function(x, auth = NULL) {
  Project$new(
    href = x$href,
    id = x$id,
    name = x$name,
    type = x$type,
    description = x$description,
    settings = x$settings,
    tags = x$tags,
    permissions = x$permissions,
    root_folder = x$root_folder,
    billing_group = x$billing_group,
    created_by = x$created_by,
    category = x$category,
    created_on = x$created_on,
    modified_on = x$modified_on,
    auth = auth,
    response = attr(x, "response")
  )
}


asProjectList <- function(x, auth) {
  obj <- lapply(x$items, asProject, auth = auth)
  obj
}
