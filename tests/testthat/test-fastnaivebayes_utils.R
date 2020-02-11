context("Test Util Functions")

test_that("Cast", {

  y <- as.factor(c("Ham", "Ham", "Spam", "Spam", "Spam"))
  x <- matrix(
    c(1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1),
    nrow = 5,
    ncol = 4,
    dimnames = list(NULL, c("wo", "mo", "bo", "so")))

  # Test matrix casting
  expect_equal(fnb.utils.cast(Matrix(x, sparse=TRUE), sparse=FALSE)$sparse, TRUE)
  expect_equal(fnb.utils.cast(as.data.frame(x), sparse = FALSE)$x, x)
  expect_equal(fnb.utils.cast(x, sparse = TRUE)$x, Matrix(x, sparse=TRUE))
})
