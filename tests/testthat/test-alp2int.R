test_that("alp2int works", {
  a <- ft3_alp2int('a')
  b <- ft3_alp2int('b')
  expect_length(a, 1)
  expect_type(a, "integer")
  expect_equal(a, 1081383251)
  expect_equal(b, 1501415656)
})
