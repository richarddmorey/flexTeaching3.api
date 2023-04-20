test_that("empty assignments lists are empty", {
  td <- tempdir(check=TRUE)
  empty_assignments_list <- ft3_get_assignments(td)
  expect_length(empty_assignments_list, 0)
  expect_type(empty_assignments_list, 'list')
})

test_that("the package contains assignments", {
  assignments_dir <- system.file(package = packageName(), 'ft3_pkg/assignments')
  assignments_list <- ft3_get_assignments(assignments_dir)
  expect_gt(length(assignments_list), 0)
  expect_type(assignments_list, 'list')
})