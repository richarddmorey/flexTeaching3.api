test_that("creating filenames for assignment output files", {
  
  tf <- tempfile(fileext = '.Rda')
  save(sleep, file = tf)
  contents <- ft3_read_file_raw(tf)
  
  seed = 'seed0'
  
  fn_assignment <- ft3_create_filename(
    filename = 'filename', 
    assignment = 'a1', 
    id = 'id', 
    seed = seed, 
    assignment_mode = TRUE,
    solutions = FALSE, 
    file_content = contents)
  
  fn_practice <- ft3_create_filename(
    filename = 'filename', 
    assignment = 'a1', 
    id = 'id', 
    seed = seed, 
    assignment_mode = FALSE,
    solutions = FALSE, 
    file_content = contents)
  
  expect_type(fn_assignment, 'character')
  expect_length(fn_assignment, 1)
  expect_match(fn_assignment, '_assignment_')
  # Make sure assignment file names DO NOT include the seed
  expect_false(
    grepl(
      pattern = paste0('_',seed,'_'),
      x = fn_assignment,
      fixed = TRUE
      )
    )
  
  expect_match(fn_practice, '_practice_')
  # Make sure practice file names include the seed
  expect_match(fn_practice, paste0('_',seed,'_'), fixed = TRUE)
  
  
})
