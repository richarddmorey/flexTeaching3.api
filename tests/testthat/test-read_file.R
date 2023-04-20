test_that("reading files as raw", {
  tf <- tempfile(fileext = '.Rda')
  save(sleep, file = tf)
  contents <- ft3_read_file_raw(tf)
  expect_type(contents, 'raw')
  expect_equal(length(contents), file.size(tf))
})

test_that("reading files as text", {
  tf <- tempfile(fileext = '.txt')
  text = 'This is a test. 😀'
  cat(text, file = tf)
  contents <- ft3_read_file_text(tf)
  expect_type(contents, 'character')
  expect_equal(contents, text)
})
