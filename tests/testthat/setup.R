### Script for setting testing variables
# devtools::load_all()

# Dummy token
dummy_token <-
  stringi::stri_rand_strings(1, 32, pattern = "[a-z0-9]")

# Different bad token types
bad_tokens <- list(NA, "", NULL, list())

# Different bad url types
bad_urls <- list(NA, "", NULL, list())

# Different bad method types
bad_methods <- c(NA, "", NULL, "PUTT")

# Different bad encoding types
bad_encodings <- c(NA, "", NULL, "something else")

credentials_path <- testthat::test_path(
  "test_data",
  "sbg_credentials_test_file"
)

# Auth object
setup_auth_object <-
  Auth$new(from = "file", config_file = credentials_path)

# Permission obj
setup_permission_obj <-
  Permission$new(
    write = TRUE,
    read = TRUE,
    copy = TRUE,
    execute = TRUE,
    admin = TRUE,
    href = NULL,
    response = list("raw-response"),
    auth = setup_auth_object
  )
# Project obj
setup_project_obj <-
  Project$new(
    id = "project_id",
    name = "Project name",
    billing_group = "billing group",
    description = "Project description",
    type = "v2",
    tags = list("Tag1", "Tag2"),
    settings = list("locked" = FALSE),
    root_folder = "root_folder_id",
    created_by = "user1",
    created_on = as.POSIXct.POSIXlt(
      strptime("2023-06-10 13:36:00", "%Y-%m-%d %H:%M:%S")
    ),
    modified_on = as.POSIXct.POSIXlt(
      strptime("2023-07-10 13:36:00", "%Y-%m-%d %H:%M:%S")
    ),
    permissions = setup_permission_obj,
    category = "PRIVATE",
    auth = setup_auth_object
  )
# Project member object
setup_project_member_object <- Member$new(
  username = "test-member",
  email = "test-member@gmail.com", type = "USER",
  id = "test-member",
  permissions = Permission$new(
    read = TRUE, copy = FALSE, write = FALSE,
    execute = FALSE, admin = FALSE,
    href = NULL,
    auth = setup_auth_object,
    response = list(raw = "raw-response-list")
  ),
  href = "link/to/resource",
  auth = setup_auth_object,
  response = list(raw = "raw-response-list")
)

setup_file_obj <-
  File$new(
    id = "file-id",
    name = "File name",
    size = 100,
    project = "user1/project-id",
    parent = "parent-id",
    type = "file",
    created_on = "2023-06-06T11:14:11Z",
    modified_on = "2023-06-06T11:14:11Z",
    href = "https://api.sbgenomics.com/v2/files/file-id",
    auth = setup_auth_object,
    tags = list("tag_1"),
    metadata = list(
      sbg_public_files_category = "test",
      reference_genome = "HG19_Broad_variant",
      sample_id = "HCC1143_1M",
      case_id = "CCLE-HCC1143",
      investigation = "CCLE-BRCA"
    ),
    origin = list(task = "123a1a1a-12a1-1234-a123-1234567a1a12"),
    storage = list(
      type = "PLATFORM",
      hosted_on_locations = list("aws:us-east-1")
    )
  )


setup_folder_obj <-
  File$new(
    id = "folder_id",
    name = "Folder_name",
    project = "user1/project-id",
    parent = "parent-id",
    type = "folder",
    created_on = "2023-06-06T11:14:11Z",
    modified_on = "2023-06-06T11:14:11Z",
    href = "https://api.sbgenomics.com/v2/files/folder_id",
    url = NA
  )

# Load raw cwl app
setup_app_path <- testthat::test_path(
  "test_data",
  "raw_app_list.RDS"
)
setup_raw_cwl <- readRDS(setup_app_path)

setup_app_obj <- App$new(
  id = "user_free_1/user-free-1-s-demo-project/fastqc-analysis", # nolint
  project = "user_free_1/user-free-1-s-demo-project",
  name = "FastQC Analysis",
  revision = 0,
  raw = setup_raw_cwl,
  copy_of = NA,
  latest_revision = 0,
  auth = setup_auth_object
)

# Resource_obj
setup_resource_obj <- Resource$new(auth = setup_auth_object)

# Apps obj
setup_apps_obj <- Apps$new(auth = setup_auth_object)

# Volumes obj
setup_volumes_obj <- Volumes$new(auth = setup_auth_object)

# Volume obj
setup_s3_volume_obj <- Volume$new(
  id = "volume-id",
  name = "my_new_volume",
  access_mode = "RW",
  service = list(
    type = "s3",
    bucket = "bucket-name",
    prefix = "",
    endpoint = "s3.amazonaws.com",
    credentials = list(
      access_key_id = "access_key_id"
    ),
    properties = list(
      sse_algorithm = "AES256"
    ),
    export_enabled = TRUE,
    direct_export_enabled = FALSE
  ),
  created_on = "2023-06-15T14:50:16Z",
  modified_on = "2023-06-15T14:50:16Z",
  active = TRUE
)

# Collection object
setup_collection_obj <- Collection$new(
  href = "some-href",
  items = list(
    item1 = list(field1 = "value11", field2 = "value12"),
    item2 = list(field1 = "value21", field2 = "value22"),
    item3 = list(field1 = "value31", field2 = "value32")
  ),
  links = list(
    list(href = "link-to-next-page", method = "GET", rel = "next"),
    list(href = "link-to-prev-page", method = "GET", rel = "prev")
  ),
  response = list(raw = "raw-response-list"),
  auth = setup_auth_object
)

# VolumeFile object type file
setup_volume_file_obj <- VolumeFile$new(
  href = "resource-href",
  location = "my_new_file.txt",
  type = "FILE",
  storage_type = "s3",
  volume = "my_s3_volume",
  metadata = list(metadata_field = "metadata-value")
)
# VolumeFile object type folder
setup_volume_file_dir_obj <- VolumeFile$new(
  href = "resource-href",
  location = "my_new_folder",
  type = "PREFIX",
  storage_type = NULL,
  volume = "my_s3_volume",
  metadata = NULL
)

# VolumeFileCollection object
setup_volfile_collection_obj <- VolumeFileCollection$new(
  href = "some-href",
  items = list(
    list(
      href = "resource-href",
      location = "my_new_file1.txt",
      type = "FILE",
      storage_type = "s3",
      volume = "my_s3_volume",
      metadata = list(metadata_field = "metadata-value")
    ),
    list(
      href = "resource-href",
      location = "my_new_file2.txt",
      type = "FILE",
      storage_type = "s3",
      volume = "my_s3_volume",
      metadata = list(metadata_field = "metadata-value")
    )
  ),
  prefixes = list(list(
    href = "resource-href",
    prefix = "my_new_folder",
    volume = "my_s3_volume"
  )),
  links = list(list("next" = "link-to-next-page")),
  response = list(raw = "raw-response-list"),
  auth = setup_auth_object
)

# Volume member object
setup_volume_member_object <- Member$new(
  username = "test-member",
  email = "test-member@gmail.com", type = "USER",
  id = "test-member",
  permissions = Permission$new(
    read = TRUE, copy = FALSE, write = FALSE,
    execute = NULL, admin = FALSE,
    href = NULL,
    auth = setup_auth_object,
    response = list(raw = "raw-response-list")
  ),
  href = "link/to/resource",
  auth = setup_auth_object,
  response = list(raw = "raw-response-list")
)

# Imports obj
setup_imports_obj <- Imports$new(auth = setup_auth_object)

file_obj_params_list <- list(
  id = "file-id",
  name = "File_name",
  size = 100,
  project = "user1/project-id",
  parent = "parent-id",
  type = "file",
  created_on = "2023-06-06T11:14:11Z",
  modified_on = "2023-06-06T11:14:11Z",
  href = "https://api.sbgenomics.com/v2/files/file-id",
  auth = setup_auth_object
)
# Import obj
setup_import_obj <- Import$new(
  href = "link-to-the-resource",
  id = "import-job-id",
  state = "COMPLETED",
  overwrite = FALSE,
  autorename = TRUE,
  preserve_folder_structure = NULL,
  source = list(volume = "volume-id", location = "location-name"),
  destination = list(project = "project-id", name = "file_name.txt"),
  started_on = "2023-07-13T12:34:56Z",
  finished_on = "2023-07-13T12:34:56Z",
  error = NULL,
  result = file_obj_params_list,
  auth = setup_auth_object
)


# Close session at the end of tests
withr::defer(teardown_env())
