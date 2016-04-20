library(dplyr, quietly = TRUE, verbose = FALSE)

context("Vectorised addition")

A = 1:5
B = 5:1
C = rep(NA, 5)
D = c(6, -2, NA, 0, 7)
E = c(NA, NA, 8, NA, NA)

df <- data_frame(A = A, B = B, C = C, D = D, E = E)

test_that("Vectorised binary addition adds numbers correctly", {
  expect_equal(A %+% B, A + B)
})

test_that("Vectorised binary addition replaces NA correctly", {
  expect_equal(A %+% C, A)
  expect_equal(`%+%`(A, C), A)
  expect_equal(`%+%`(A, C, na_replacement = 100), A + 100)
})


test_that("Vectorised n-ary addition replaces NA correctly", {
  expect_equal(plus_(list(B)), B)
  expect_equal(plus_(list(A, B)), A + B)
  expect_equal(plus_(list(A, B, C)), A + B)
  expect_equal(plus_(list(A, B, D)), A + B + replace(D, is.na(D), 0))
  expect_equal(plus_(list(A, B, D), na_replacement = -5), A + B + replace(D, is.na(D), -5))
  expect_false(any(is.na(plus_(list(D, E)))))
})

test_that("Vectorised NSE n-ary addition replaces NA correctly", {
  expect_equal(plus(B), B)
  expect_equal(plus(A, B), A + B)
  expect_equal(plus(A, B, C), A + B)
  expect_equal(plus(A, B, D), A + B + replace(D, is.na(D), 0))
  expect_equal(plus(A, B, D, na_replacement = -5), A + B + replace(D, is.na(D), -5))
  expect_false(any(is.na(plus(D, E))))
})
