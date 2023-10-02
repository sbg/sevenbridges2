test_that("Item initialization works", {
  # Item object creation works
  testthat::expect_no_error(Item$new(auth = setup_auth_object))

  # Item object class and methods are set
  checkmate::assert_r6(
    setup_item_object,
    classes = c("Item"),
    public = c("reload")
  )
})
