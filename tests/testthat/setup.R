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

# Rate limit object
rate_limit_res <- list(
  rate = list(
    limit = 1000,
    remaining = 990,
    reset = 1693846218
  ),
  instance = list(
    limit = -1,
    remaining = 987654342
  )
)
setup_rate_limit_obj <- asRate(
  x = rate_limit_res,
  auth = setup_auth_object
)

# User obj
user_res <- list(
  username = "luna_lovegood",
  email = "luna.lovegood@hogwarts.com",
  first_name = "Luna",
  last_name = "Lovegood",
  affiliation = "Hogwarts",
  country = "United Kingdom"
)
setup_user_object <- asUser(
  x = user_res,
  auth = setup_auth_object
)

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

# Projects obj
setup_projects_obj <- Projects$new(auth = setup_auth_object)

# Project obj response
project_res <- list(
  id = "project_id",
  name = "Project name",
  billing_group = "billing group",
  description = "Project description",
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
  category = "PRIVATE"
)
# Project obj
setup_project_obj <- asProject(
  x = project_res,
  auth = setup_auth_object
)

# Project member object
proj_member_res <- list(
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
  response = list(raw = "raw-response-list")
)
setup_project_member_object <- asMember(
  x = proj_member_res,
  auth = setup_auth_object
)

# Files obj
setup_files_obj <- Files$new(auth = setup_auth_object)

file_res <- list(
  id = "file-id",
  name = "File name",
  size = 100,
  project = "user1/project-id",
  parent = "parent-id",
  type = "file",
  created_on = "2023-06-06T11:14:11Z",
  modified_on = "2023-06-06T11:14:11Z",
  href = "https://api.sbgenomics.com/v2/files/file-id",
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
setup_file_obj <- asFile(
  x = file_res,
  auth = setup_auth_object
)

folder_res <- list(
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
setup_folder_obj <- asFile(
  x = folder_res,
  auth = setup_auth_object
)

# Load raw cwl app
setup_app_path <- testthat::test_path(
  "test_data",
  "raw_app_list.RDS"
)
setup_raw_cwl <- readRDS(setup_app_path)

app_res <- list(
  id = "user_free_1/user-free-1-s-demo-project/fastqc-analysis/7",
  project = "user_free_1/user-free-1-s-demo-project",
  name = "FastQC Analysis",
  revision = 0,
  raw = setup_raw_cwl,
  copy_of = NA,
  latest_revision = 0
)
setup_app_obj <- asApp(
  x = app_res,
  auth = setup_auth_object
)

# Resource_obj
setup_resource_obj <- Resource$new(auth = setup_auth_object)

# Apps obj
setup_apps_obj <- Apps$new(auth = setup_auth_object)

# Volumes obj
setup_volumes_obj <- Volumes$new(auth = setup_auth_object)

# Volume obj
volume_res <- list(
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
setup_s3_volume_obj <- asVolume(
  x = volume_res,
  auth = setup_auth_object
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
volume_file_res <- list(
  location = "my_new_file.txt",
  type = "s3",
  volume = "my_s3_volume",
  metadata = list(metadata_field = "metadata-value"),
  href = "resource-href"
)
setup_volume_file_obj <- asVolumeFile(
  x = volume_file_res,
  auth = setup_auth_object
)
# VolumeFile object type folder
volume_file_folder_res <- list(
  prefix = "my_new_folder",
  type = "s3",
  volume = "my_s3_volume",
  metadata = NULL
)
setup_volume_file_dir_obj <- asVolumeFile(
  x = volume_file_folder_res,
  auth = setup_auth_object
)

# VolumeFileCollection object
vol_file_collection_res <- list(
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
  response = list(raw = "raw-response-list")
)
setup_volfile_collection_obj <- asVolumeFileCollection(
  x = vol_file_collection_res,
  auth = setup_auth_object
)

# Volume member object
volume_member_res <- list(
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
  response = list(raw = "raw-response-list")
)
setup_volume_member_object <- asMember(
  x = volume_member_res,
  auth = setup_auth_object
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
import_res <- list(
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
  result = file_obj_params_list
)
setup_import_obj <- asImport(
  x = import_res,
  auth = setup_auth_object
)

# Tasks obj
setup_tasks_obj <- Tasks$new(auth = setup_auth_object)

task_res <- list(
  id = "task-id",
  name = "Task name",
  description = "My new test task",
  project = "project-id",
  app = "app-id",
  created_by = "user1",
  executed_by = "user1",
  created_on = "12-08-2023",
  start_time = "12-08-2023",
  end_time = "12-08-2023"
)
# Task obj
setup_task_obj <- asTask(
  x = task_res,
  auth = setup_auth_object
)

# Exports obj
setup_exports_obj <- Exports$new(auth = setup_auth_object)

# Export obj
export_res <- list(
  href = "link-to-the-resource",
  id = "export-job-id",
  state = "COMPLETED",
  overwrite = FALSE,
  properties = NULL,
  source = list(file = "file-id"),
  destination = list(volume = "volume-id", location = "file_name.txt"),
  started_on = "2023-07-14T12:34:56Z",
  finished_on = "2023-07-14T12:34:56Z",
  error = NULL,
  result = file_obj_params_list
)
setup_export_obj <- asExport(
  x = export_res,
  auth = setup_auth_object
)

setup_task_inputs_raw <- list(
  input_reads = list(
    list(
      path = "input_reads_nested_file_1_id",
      metadata = list(paired_end = "1", sample_id = "TCRBOA3-T"),
      size = 12580652281,
      contents = NULL,
      name = "TCRBOA3-T-WEX.read1.fastq",
      checksum = NULL,
      location = "input_reads_file_1_id_location",
      class = "File",
      dirname = "/Projects/parent_id/"
    ),
    list(
      path = "input_reads_nested_file_2_id",
      metadata = list(paired_end = "2", sample_id = "TCRBOA3-T"),
      size = 12580652281,
      contents = NULL,
      name = "TCRBOA3-T-WEX.read2.fastq",
      checksum = NULL,
      location = "input_reads_nested_file_2_location",
      class = "File",
      dirname = "/Projects/parent-id/"
    ),
    another_level_nested = list(
      list(
        path = "input_reads_nested_file_3_id",
        class = "File"
      ),
      list(
        path = "input_reads_nested_file_4_id",
        class = "File"
      )
    )
  ),
  target_bed = list(
    path = "target_bed_file_id",
    metadata = structure(list(), names = character(0)),
    size = 5312162L,
    contents = NULL,
    name = "v5_core_targets.refse-ccds-gencode-ucsc.bed",
    checksum = NULL,
    location = "target_bed_file_location",
    class = "File",
    dirname = "/Projects/parent_id/reference_files/"
  ),
  bait_bed = list(
    path = "bait_bed_id",
    metadata = structure(list(), names = character(0)),
    size = 5506630L,
    contents = NULL,
    name = "SureSelect_XT_Human_All_Exon_V5_annot.bed",
    checksum = NULL,
    location = "bait_bed_location",
    class = "File",
    dirname = "/Projects/parent_id/reference_files/"
  ),
  kgsnp_database = list(
    path = "kgsnp_database_id",
    metadata = structure(list(), names = character(0)),
    size = 7398865818,
    contents = NULL,
    name = "1000G_phase1.snps.high_confidence.hg19.sites.vcf",
    checksum = NULL,
    location = "kgsnp_database_location",
    class = "File",
    dirname = "/Projects/parent_id/reference_files/"
  ),
  input_tar_with_reference = list(
    path = "input_tar_with_reference_id",
    metadata = list(library_id = "UCSC"),
    size = 8689960960,
    contents = NULL,
    name = "ucsc.hg19.fasta.tar",
    checksum = NULL,
    location = "input_tar_with_reference_location",
    class = "File",
    dirname = "/Projects/parent_id/reference_files/"
  ),
  mgindel_database = list(
    path = "mgindel_database_id",
    metadata = structure(list(), names = character(0)),
    size = 20718745L,
    contents = NULL,
    name = "Mills_and_1000G_gold_standard.tab.indels.hg19.sites.vcf.gz",
    checksum = NULL,
    location = "mgindel_database_location",
    class = "File",
    dirname = "/Projects/parent_id/reference_files/"
  ),
  char = "char_value",
  double = 235.6,
  some_vars = list(
    int = 10,
    str = "text"
  )
)

setup_task_outputs_raw <- list(
  alignment_metrics = list(
    path = "alignment_metrics_id",
    size = 2337L,
    name = "_1_TCRBOA3-T.ALN_METRIC.txt",
    checksum = "sum",
    location = NULL,
    secondaryFiles = list(),
    class = "File",
    dirname = "/mnt/nosbgfs/workspaces/wp-id/tasks/task-id/SBG_Rename_App_4"
  ),
  per_target_coverage = list(
    path = "per_target_coverage_id",
    size = 18757531L,
    name = "_1_TCRBOA3-T.per_target_coverage.txt",
    checksum = "sum",
    location = NULL,
    class = "File",
    dirname = "/mnt/nosbgfs/workspaces/wp-id/tasks/task-id/Picard_CollectHsMetrics" # nolint
  ),
  hs_metrics = list(
    path = "hs_metrics_id",
    size = 5523L,
    name = "_1_TCRBOA3-T.hsMetrics.txt",
    checksum = "sum",
    location = NULL,
    class = "File",
    dirname = "/mnt/nosbgfs/workspaces/wp-id/tasks/task-id/Picard_CollectHsMetrics" # nolint
  ),
  dedup_metrics = list(
    path = "dedup_metrics_id",
    size = 2952L,
    name = "_1_TCRBOA3-T.DEDUP.txt",
    checksum = "sum",
    location = NULL,
    secondaryFiles = list(),
    class = "File",
    dirname = "/mnt/nosbgfs/workspaces/wp-id/tasks/task-id/SBG_Rename_App"
  ),
  output_bam = list(
    path = "output_bam_id",
    size = 6632805966,
    name = "_1_TCRBOA3-T.sorted.dedup.recal.bam",
    checksum = "sum",
    location = NULL,
    secondaryFiles = list(
      list(
        path = "sec_file_id",
        metadata = list(
          `__inherit__` = "input_bam",
          intervals_file = "v5_core_targets.refse-ccds-gencode-ucsc.bed",
          reference_genome = "ucsc.hg19",
          sample_id = "TCRBOA3-T"
        ),
        size = 2950000L,
        contents = NULL,
        name = "_1_TCRBOA3-T.sorted.dedup.recal.bam.bai",
        checksum = "sum",
        location = "parent-id",
        class = "File",
        dirname = "/mnt/nosbgfs/workspaces/wp-id/tasks/task-id/SBG_Rename_App_2" # nolint
      )
    ),
    class = "File",
    dirname = "/mnt/nosbgfs/workspaces/wp-id/tasks/task-id/SBG_Rename_App_2" # nolint
  ),
  recal_table = list(
    path = "recal_table_id", size = 861779L,
    name = "_1_TCRBOA3-T.recal_result.txt",
    checksum = "sum",
    location = NULL, secondaryFiles = list(), class = "File",
    dirname = "/mnt/nosbgfs/workspaces/wp-id/tasks/task-id/SBG_Rename_App_1" # nolint
  )
)

# Invoices obj
setup_invoices_obj <- Invoices$new(auth = setup_auth_object)

# Invoice obj
invoice_res <- list(
  id = "some-id",
  href = "some-href",
  pending = FALSE,
  approval_date = "2020-01-01T00:00:00Z",
  invoice_period = list(
    from = "2020-01-01T11:00:00Z",
    to = "2020-01-31T23:59:59Z"
  ),
  analysis_costs = list(
    currency = "USD",
    amount = "1244.1"
  ),
  analysis_costs = list(
    currency = "USD",
    amount = "117.4"
  ),
  total = list(
    currency = "USD",
    amount = "1361.5"
  )
)

setup_invoice_obj <- asInvoice(x = invoice_res, auth = setup_auth_object)

# Close session at the end of tests
withr::defer(teardown_env())
