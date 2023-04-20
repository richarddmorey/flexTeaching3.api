test_that("animal seeds work", {
  ab <- ft3_animal_seed('a','b')
  ac <- ft3_animal_seed('a','c')
  expect_length(ab, 1)
  expect_type(ab, "character")
  expect_equal(ab, "dazzling_buzzard_kqs5hy7iqk")
  expect_equal(ac, "ossified_badger_gms8smqmdm")
})
