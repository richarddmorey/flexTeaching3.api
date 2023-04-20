test_that("http_error throws an error", {
  expect_error(ft3_http_error(400,'This is an error'))
})
