context("Test Check Args")

test_that("Train checks args", {
  y <- as.factor(c("Ham", "Ham", "Spam", "Spam", "Spam"))
  x <- matrix(c(2, 3, 0, 1, 0, 5, 3, 0, 2, 0, 0, 1, 3, 1, 0, 0, 0, 4, 3, 5),
              nrow = 5, ncol = 4)
  colnames(x) <- c("wo", "mo", "bo", "so")
  x <- as.data.frame(x)

  expect_equal(fastNaiveBayes:::fnb.check.args.dist(as.data.frame(x), nrow(x)), fastNaiveBayes:::fnb.check.args.dist(x, nrow(x)))
  expect_error(fastNaiveBayes:::fnb.check.args.dist(x, nrows = 0))

  mod <- fnb.train(x, y)
  expect_error(fastNaiveBayes:::fnb.check.args.mixed_predict(object=mod, newdata=x, type="raw", sparse = FALSE, threshold = -1))
  expect_error(fastNaiveBayes:::fnb.check.args.mixed_predict(object=mod, newdata=x[,1:2], type="raw", sparse = TRUE, threshold = -1))

  expect_error(fastNaiveBayes:::fnb.check.args.train(x, y, priors = NULL, laplace = -1, sparse = FALSE, distribution = NULL))
  expect_error(fastNaiveBayes:::fnb.check.args.train(x, y, priors = list(1,2), laplace = 0, sparse = FALSE, distribution = NULL))
  expect_error(fastNaiveBayes:::fnb.check.args.train(x, y, priors = c(1,2,3), laplace = 0, sparse = FALSE, distribution = NULL))
  expect_error(fastNaiveBayes:::fnb.check.args.train(x, y, priors = c(1,2), laplace = 0, sparse = FALSE, distribution = NULL))
  expect_error(fastNaiveBayes:::fnb.check.args.train(x, y, priors = NULL, laplace = 0, sparse = FALSE,
                                                     distribution = c("bernoulli")))

  expect_error(fastNaiveBayes:::fnb.check.args.train(x, y, priors = NULL, laplace = 0, sparse = FALSE,
                                                     distribution = list("martin"=c("h","a"))))

  expect_warning(fastNaiveBayes:::fnb.check.args.train(x, y, priors = NULL, laplace = 0, sparse = FALSE,
                                                       distribution = list("bernoulli"=c("h","a"),
                                                                           "martin"=c("a","b"))))

  expect_error(fastNaiveBayes:::fnb.check.args.train(x, y, priors = NULL, laplace = 0, sparse = FALSE,
                                                     distribution = c("bernoulli")))

  expect_error(fastNaiveBayes::fnb.check.args.train(x[,1], y, priors=NULL, laplace = 0, sparse = FALSE))
  expect_error(fastNaiveBayes::fnb.check.args.train(NULL, y, priors=NULL, laplace = 0, sparse = FALSE))
  expect_error(fastNaiveBayes::fnb.check.args.train(x[2:5,], y, priors=NULL, laplace = 0, sparse = FALSE))

  # 100%
  expect_error(fastNaiveBayes::fnb.check.args.train(
    matrix("", ncol = 0, nrow = 10), y, priors=NULL, laplace = 0, sparse = FALSE))

  expect_error(fastNaiveBayes::fnb.check.args.mixed_predict(mod,
                                                                matrix("", ncol = 0, nrow = 10)))


  predictions <- predict(mod, x, type="raw")
  x[1,3] <- NA
  expect_warning(alt_mod <- fnb.train(x, as.character(y)))
  expect_warning(alt_predictions <- predict(alt_mod, x, type="raw"))
  expect_equal(sum(round(abs(predictions-alt_predictions), digits = 12)), 0)


  expect_warning(fastNaiveBayes:::fnb.check.args.train(x, y, priors = NULL, laplace = 0, sparse = FALSE))

  # Test removal of nas from y
  x[1,3] <- 0
  y[5] <- NA

  mod <- fnb.train(x[1:4,], y[1:4], laplace = 1)
  expect_warning(alt_mod <- fnb.train(x, y, laplace = 1))

  predictions <- predict(mod, x[1:4,], type="raw")
  alt_predictions <- predict(alt_mod, x[1:4,], type="raw")
  expect_equal(sum(round(abs(predictions-alt_predictions), digits = 12)), 0)

  # Test single level error
  expect_error(fastNaiveBayes::fnb.check.args.train(x[1:2,], as.factor(as.character(y[1:2])), priors=NULL, laplace = 0, sparse = FALSE))

  # Test single level error
  expect_error(fastNaiveBayes::fnb.check.args.train(x, y[1:4], priors=NULL, laplace = 0, sparse = FALSE))

})


test_that("Bernoulli checks args", {
  y <- as.factor(c("Ham", "Ham", "Spam", "Spam", "Spam"))
  x <- matrix(
    c(1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1),
    nrow = 5,
    ncol = 4,
    dimnames = list(NULL, c("wo", "mo", "bo", "so")))
  df <- as.data.frame(x)

  mod <- fnb.bernoulli(x, y, laplace = 1)
  expect_error(fastNaiveBayes:::fnb.bernoulli.check.args.predict(object=mod, newdata=x, type="raw", sparse = FALSE, threshold = -1))
  expect_error(fastNaiveBayes:::fnb.bernoulli.check.args.predict(object=mod, newdata=x[,1:2], type="raw", sparse = TRUE, threshold = -1))

  expect_error(fastNaiveBayes:::fnb.bernoulli.check.args.model(x, y, priors = NULL, laplace = -1, sparse = FALSE))
  expect_error(fastNaiveBayes:::fnb.bernoulli.check.args.model(x, y, priors = list(1,2), laplace = 0, sparse = FALSE))
  expect_error(fastNaiveBayes:::fnb.bernoulli.check.args.model(x, y, priors = c(1,2,3), laplace = 0, sparse = FALSE))
  expect_error(fastNaiveBayes:::fnb.bernoulli.check.args.model(x, y, priors = c(1,2), laplace = 0, sparse = FALSE))

  expect_error(fastNaiveBayes::fnb.bernoulli.check.args.model(x[,1], y, priors=NULL, laplace = 0, sparse = FALSE))
  expect_error(fastNaiveBayes::fnb.bernoulli.check.args.model(x[2:5,], y, priors=NULL, laplace = 0, sparse = FALSE))
  expect_error(fastNaiveBayes::fnb.bernoulli.check.args.model(NULL, y, priors=NULL, laplace = 0, sparse = FALSE))

  # 100%
  expect_error(fastNaiveBayes::fnb.bernoulli.check.args.model(
    matrix("", ncol = 0, nrow = 10), y, priors=NULL, laplace = 0, sparse = FALSE))

  expect_error(fastNaiveBayes::fnb.bernoulli.check.args.predict(mod,
                                                                  matrix("", ncol = 0, nrow = 10)))


  predictions <- predict(mod, x, type="raw")
  x[1,3] <- NA
  expect_warning(alt_mod <- fnb.bernoulli(x, as.character(y), laplace=1))
  expect_warning(alt_predictions <- predict(alt_mod, x, type="raw"))
  expect_equal(sum(round(abs(predictions-alt_predictions), digits = 12)), 0)


  expect_warning(fastNaiveBayes:::fnb.bernoulli.check.args.model(x, y, priors = NULL, laplace = 0, sparse = FALSE))

  # Test removal of nas from y
  x[1,3] <- 0
  y[5] <- NA

  mod <- fnb.bernoulli(x[1:4,], y[1:4], laplace = 1)
  expect_warning(alt_mod <- fnb.bernoulli(x, y, laplace = 1))

  predictions <- predict(mod, x[1:4,], type="raw")
  alt_predictions <- predict(alt_mod, x[1:4,], type="raw")
  expect_equal(sum(round(abs(predictions-alt_predictions), digits = 12)), 0)

  # Test single level error
  expect_error(fastNaiveBayes::fnb.bernoulli.check.args.model(x[1:2,], as.factor(as.character(y[1:2])), priors=NULL, laplace = 0, sparse = FALSE))

  # Test single level error
  expect_error(fastNaiveBayes::fnb.bernoulli.check.args.model(x, y[1:4], priors=NULL, laplace = 0, sparse = FALSE))

})

test_that("Multinomial checks args", {
  y <- as.factor(c("Ham", "Ham", "Spam", "Spam", "Spam"))
  x <- matrix(c(2, 3, 0, 1, 0, 5, 3, 0, 2, 0, 0, 1, 3, 1, 0, 0, 0, 4, 3, 5),
              nrow = 5, ncol = 4)
  colnames(x) <- c("wo", "mo", "bo", "so")
  x <- as.data.frame(x)

  mod <- fnb.multinomial(x, y)
  expect_error(fastNaiveBayes:::fnb.multinomial.check.args.predict(object=mod, newdata=x, type="raw", sparse = FALSE, threshold = -1))
  expect_error(fastNaiveBayes:::fnb.multinomial.check.args.predict(object=mod, newdata=x[,1:2], type="raw", sparse = TRUE, threshold = -1))

  expect_error(fastNaiveBayes:::fnb.multinomial.check.args.model(x, y, priors = NULL, laplace = -1, sparse = FALSE))
  expect_error(fastNaiveBayes:::fnb.multinomial.check.args.model(x, y, priors = list(1,2), laplace = 0, sparse = FALSE))
  expect_error(fastNaiveBayes:::fnb.multinomial.check.args.model(x, y, priors = c(1,2,3), laplace = 0, sparse = FALSE))
  expect_error(fastNaiveBayes:::fnb.multinomial.check.args.model(x, y, priors = c(1,2), laplace = 0, sparse = FALSE))

  expect_error(fastNaiveBayes::fnb.multinomial.check.args.model(x[,1], y, priors=NULL, laplace = 0, sparse = FALSE))
  expect_error(fastNaiveBayes::fnb.multinomial.check.args.model(NULL, y, priors=NULL, laplace = 0, sparse = FALSE))
  expect_error(fastNaiveBayes::fnb.multinomial.check.args.model(x[2:5,], y, priors=NULL, laplace = 0, sparse = FALSE))

  # 100%
  expect_error(fastNaiveBayes::fnb.multinomial.check.args.model(
    matrix("", ncol = 0, nrow = 10), y, priors=NULL, laplace = 0, sparse = FALSE))

  expect_error(fastNaiveBayes::fnb.multinomial.check.args.predict(mod,
                                                               matrix("", ncol = 0, nrow = 10)))

  predictions <- predict(mod, x, type="raw")
  x[1,3] <- NA
  expect_warning(alt_mod <- fnb.multinomial(x, as.character(y)))
  expect_warning(alt_predictions <- predict(alt_mod, x, type="raw"))
  expect_equal(sum(round(abs(predictions-alt_predictions), digits = 12)), 0)


  expect_warning(fastNaiveBayes:::fnb.multinomial.check.args.model(x, y, priors = NULL, laplace = 0, sparse = FALSE))

  # Test removal of nas from y
  x[1,3] <- 0
  y[5] <- NA

  mod <- fnb.multinomial(x[1:4,], y[1:4], laplace = 1)
  expect_warning(alt_mod <- fnb.multinomial(x, y, laplace = 1))

  predictions <- predict(mod, x[1:4,], type="raw")
  alt_predictions <- predict(alt_mod, x[1:4,], type="raw")
  expect_equal(sum(round(abs(predictions-alt_predictions), digits = 12)), 0)

  # Test single level error
  expect_error(fastNaiveBayes::fnb.multinomial.check.args.model(x[1:2,], as.factor(as.character(y[1:2])), priors=NULL, laplace = 0, sparse = FALSE))

  # Test single level error
  expect_error(fastNaiveBayes::fnb.multinomial.check.args.model(x, y[1:4], priors=NULL, laplace = 0, sparse = FALSE))

})

test_that("Gaussian checks args", {
  y <- as.factor(c("Ham", "Ham", "Spam", "Spam", "Spam"))
  x <- matrix(
    c(2, 3, 2, 1, 2,
      5, 3, 4, 2, 4,
      0, 1, 3, 1, 0,
      3, 4, 4, 3, 5),
    nrow = 5,
    ncol = 4,
    dimnames = list(NULL, c("wo", "mo", "bo", "so")))
  df <- as.data.frame(x)

  mod <- fnb.gaussian(x, y)
  expect_error(fastNaiveBayes:::fnb.gaussian.check.args.predict(object=mod, newdata=x, type="raw", sparse = FALSE, threshold = -1))
  expect_error(fastNaiveBayes:::fnb.gaussian.check.args.predict(object=mod, newdata=x[,1:2], type="raw", sparse = TRUE, threshold = -1))

  expect_error(fastNaiveBayes:::fnb.gaussian.check.args.model(x, y, priors = list(1,2), sparse = FALSE))
  expect_error(fastNaiveBayes:::fnb.gaussian.check.args.model(x, y, priors = c(1,2,3), sparse = FALSE))
  expect_error(fastNaiveBayes:::fnb.gaussian.check.args.model(x, y, priors = c(1,2), sparse = FALSE))

  expect_error(fastNaiveBayes::fnb.gaussian.check.args.model(x[,1], y, priors=NULL, sparse = FALSE))
  expect_error(fastNaiveBayes::fnb.gaussian.check.args.model(NULL, y, priors=NULL, sparse = FALSE))
  expect_error(fastNaiveBayes::fnb.gaussian.check.args.model(x[2:5,], y, priors=NULL, laplace = 0, sparse = FALSE))

  # 100%
  expect_error(fastNaiveBayes::fnb.gaussian.check.args.model(
    matrix("", ncol = 0, nrow = 10), y, priors=NULL, sparse = FALSE))

  expect_error(fastNaiveBayes::fnb.gaussian.check.args.predict(mod,
                                                               matrix("", ncol = 0, nrow = 10)))

  predictions <- predict(mod, x, type="raw")
  x[1,3] <- NA
  expect_warning(alt_mod <- fnb.gaussian(x, as.character(y)))
  expect_warning(alt_predictions <- predict(alt_mod, x, type="raw"))
  expect_equal(sum(round(abs(predictions-alt_predictions), digits = 12)), 0)


  expect_warning(fastNaiveBayes:::fnb.gaussian.check.args.model(x, y, priors = NULL, sparse = FALSE))

  # Test removal of nas from y
  x[1,3] <- 0
  y[5] <- NA

  mod <- fnb.gaussian(x[1:4,], y[1:4])
  expect_warning(alt_mod <- fnb.gaussian(x, y))

  predictions <- predict(mod, x[1:4,], type="raw")
  alt_predictions <- predict(alt_mod, x[1:4,], type="raw")
  expect_equal(sum(round(abs(predictions-alt_predictions), digits = 12)), 0)

  # Test single level error
  expect_error(fastNaiveBayes::fnb.gaussian.check.args.model(x[1:2,], as.factor(as.character(y[1:2])), priors=NULL, sparse = FALSE))

  # Test single level error
  expect_error(fastNaiveBayes::fnb.gaussian.check.args.model(x, y[1:4], priors=NULL, sparse = FALSE))

})
