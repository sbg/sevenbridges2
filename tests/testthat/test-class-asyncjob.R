test_that("AsyncJob initialization works", {
  # Item object creation works
  testthat::expect_no_error(asAsyncJob(auth = setup_auth_object))

  # Item object class and methods are set
  checkmate::assert_r6(
    setup_async_job_obj,
    classes = c("Item", "AsyncJob"),
    public = c(
      "id", "type", "state", "result", "total_files",
      "completed_files", "failed_files", "started_on", "finished_on",
      "print", "reload"
    )
  )
})

test_that("AsyncJob print method works", {
  testthat::skip_on_ci()
  testthat::skip_on_cran()
  testthat::expect_snapshot(setup_async_job_obj$print())
})
