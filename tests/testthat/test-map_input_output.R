test_that("Task helper function for mapping inputs to proper objects works", {
  # Check if input is of list type
  transformed_inputs <- setup_task_obj$private$map_input_output(
    input = setup_task_inputs_raw
  )

  testthat::expect_equal(length(transformed_inputs), 9)
  testthat::expect_equal(
    names(transformed_inputs),
    c(
      "input_reads", "target_bed", "bait_bed", "kgsnp_database",
      "input_tar_with_reference", "mgindel_database", "char", "double",
      "some_vars"
    )
  )
  testthat::expect_equal(length(transformed_inputs$input_reads), 3)
  testthat::expect_true(inherits(transformed_inputs$input_reads[[1]], "File"))
  testthat::expect_true(!is.null(transformed_inputs$input_reads[[1]]$id))
  testthat::expect_true("another_level_nested" %in% names(transformed_inputs$input_reads)) # nolint
  testthat::expect_equal(
    length(transformed_inputs$input_reads$another_level_nested), 2
  )
  testthat::expect_true(inherits(transformed_inputs$input_reads$another_level_nested[[1]], "File")) # nolint
  testthat::expect_true(!is.null(transformed_inputs$input_reads$another_level_nested[[1]]$id)) # nolint

  testthat::expect_true(inherits(transformed_inputs$target_bed, "File"))
  testthat::expect_true(inherits(transformed_inputs$mgindel_database, "File"))

  testthat::expect_equal(typeof(transformed_inputs$char), "character")
  testthat::expect_equal(typeof(transformed_inputs$double), "double")
  testthat::expect_equal(length(transformed_inputs$some_vars), 2)
  testthat::expect_equal(typeof(transformed_inputs$some_vars$int), "double")
  testthat::expect_equal(typeof(transformed_inputs$some_vars$str), "character")


  # Check if input is simple type
  simple_input <- c("simple-input-name" = "value")
  transformed_inputs <- setup_task_obj$private$map_input_output(
    input = simple_input
  )
  testthat::expect_equal(transformed_inputs, simple_input)

  # Check if input is already some object
  object_input <- setup_file_obj
  transformed_inputs <- setup_task_obj$private$map_input_output(
    input = object_input
  )
  testthat::expect_equal(transformed_inputs, object_input)
  testthat::expect_true(inherits(transformed_inputs, "File"))
})

test_that("Task helper function for mapping outputs to proper objects works", {
  # Check if output is of list type
  transformed_outputs <- setup_task_obj$private$map_input_output(
    input = setup_task_outputs_raw
  )

  testthat::expect_equal(length(transformed_outputs), 6)
  testthat::expect_equal(
    names(transformed_outputs),
    c(
      "alignment_metrics", "per_target_coverage", "hs_metrics", "dedup_metrics",
      "output_bam", "recal_table"
    )
  )

  testthat::expect_true(checkmate::test_list(transformed_outputs, types = "File")) # nolint

  # Check secondary files handling
  output_file <- transformed_outputs[[5]]
  testthat::expect_true(!is.null(output_file$secondary_files))
  testthat::expect_equal(length(output_file$secondary_files), 1)
  testthat::expect_true(inherits(output_file$secondary_files[[1]], "File"))
  testthat::expect_true(!is.null(output_file$secondary_files[[1]]$id))

  # Check if output is simple type
  simple_output <- c("simple-output-name" = "value")
  transformed_outputs <- setup_task_obj$private$map_input_output(
    input = simple_output
  )
  testthat::expect_equal(transformed_outputs, simple_output)

  # Check if output is already some object
  object_output <- setup_file_obj
  transformed_outputs <- setup_task_obj$private$map_input_output(
    input = object_output
  )
  testthat::expect_equal(transformed_outputs, object_output)
  testthat::expect_true(inherits(transformed_outputs, "File"))
})
