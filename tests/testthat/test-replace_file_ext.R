test_that("replacing file extensions works", {
  expect_equal(ft3_replace_file_ext('test.csv', 'txt'), 'test.txt')
  expect_equal(ft3_replace_file_ext('test', 'txt'), 'test.txt')
  expect_equal(ft3_replace_file_ext('test.csv', ''), 'test')
  expect_equal(ft3_replace_file_ext('test.second.csv', 'txt'), 'test.second.txt')
})
