context('Testig checking duplicates...')

DT <- data.table(x = c(1, 2, 1), y = letters[1:3])

test_that("Checking if duplicated entries give error...", {
  
  expect_error(detect_duplicates(DT, cols = 'x'))
})

test_that("Checking if unique tables gives no error...", {
  expect_silent(detect_duplicates(DT, cols = c('x', 'y')))
})
