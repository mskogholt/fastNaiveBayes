context("Test Check Args")

test_that("Check Arg Distribution", {
  x <- matrix(
    c(1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1),
    nrow = 5,
    ncol = 4,
    dimnames = list(NULL, c("wo", "mo", "bo", "so")))

  expect_error(fnb.check.args.dist(x, -1))
})

test_that("Check Arg Model", {

  y <- as.factor(c("Ham", "Ham", "Spam", "Spam", "Spam"))
  x <- matrix(
    c(1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1),
    nrow = 5,
    ncol = 4,
    dimnames = list(NULL, c("wo", "mo", "bo", "so")))

  # Test matrix casting
  expect_equal(fnb.check.args.model(Matrix(x, sparse=TRUE), y, priors = NULL)$sparse, TRUE)
  expect_equal(fnb.check.args.model(as.data.frame(x), y, priors = NULL, sparse = FALSE)$x, x)
  expect_equal(fnb.check.args.model(x, y, priors = NULL, sparse = TRUE)$x, Matrix(x, sparse=TRUE))

  # Test
  expect_error(fnb.check.args.model(matrix("", nrow=10, ncol=0), y, priors = NULL, sparse = TRUE))

  # Test NA conversion
  nax <- x
  nax[1,3] <- NA
  expect_warning(args <- fnb.check.args.model(nax, y, priors = NULL, sparse = FALSE))
  expect_equal(args$x, x)

  # Test factor conversion
  expect_equal(fnb.check.args.model(x, as.character(y), priors = NULL, sparse = FALSE)$y, y)

  # Test y at least 2 or more levels
  expect_error(fnb.check.args.model(x, as.factor(as.character(y[1:2])), priors = NULL, sparse = FALSE))

  # Test NA removal from Y
  nay <- y
  nay[5] <- NA
  expect_warning(args <- fnb.check.args.model(x, nay, priors = NULL, sparse = FALSE))
  expect_equal(args$y, nay[!is.na(nay)])
  expect_equal(args$x, x[!is.na(nay),])

  # Test row/label mismatch
  expect_error(fnb.check.args.model(x[1:2,], y, priors = NULL, sparse = FALSE))

  # Test priors numeric
  expect_error(fnb.check.args.model(x, y, priors = list("ABC"), sparse = FALSE))

  # Test priors equal to levels of y
  expect_error(fnb.check.args.model(x, y, priors = c(0.1, 0.3, 0.6), sparse = FALSE))

  # Test priors add up to 1
  expect_error(fnb.check.args.model(x, y, priors = c(0.2, 0.2), sparse = FALSE))

})

test_that("Check Arg Predict", {

  y <- as.factor(c("Ham", "Ham", "Spam", "Spam", "Spam"))
  x <- matrix(
    c(1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1),
    nrow = 5,
    ncol = 4,
    dimnames = list(NULL, c("wo", "mo", "bo", "so")))
  object <- fnb.bernoulli(x, y, laplace = 1)
  newdata <- x

  # Test threshold
  expect_error(fnb.check.args.predict(object, newdata, "raw", FALSE, threshold=-1))

  # Test threshold
  expect_error(fnb.check.args.predict(object, matrix("", nrow=10, ncol=0), "raw", FALSE, threshold=0.1))

  # Test empty
  expect_error(fnb.check.args.predict(object, matrix("", nrow=10, ncol=0), "raw", FALSE, threshold=-1))

  # Test NA conversion
  nax <- newdata
  nax[1,3] <- NA
  expect_warning(args <- fnb.check.args.predict(object, nax, "raw", FALSE, threshold=0.1))
  expect_equal(args$newdata, newdata)

  # Test new names warning
  object <- fnb.bernoulli(x[,1:3], y, laplace = 1)
  expect_warning(fnb.check.args.predict(object, newdata, "raw", FALSE, threshold=0.1))

})
