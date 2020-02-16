context("Test fnb.update")

test_that("Not implemented error", {
  data <- tweetsDTM[1:10, 1:10]
  y <- as.factor(data[,1])
  x <- as.matrix(data[,2:ncol(data)])

  mod <- fnb.train(x, y)
  expect_error(fnb.update(mod, x, y))
})

test_that("", {

  test <- function(priors, sparse){

    y <- as.factor(c("Ham", "Ham", "Spam", "Spam", "Spam"))
    x <- matrix(
      c(2, 3, 2, 1, 2,
        5, 3, 4, 2, 4,
        0, 1, 3, 1, 0,
        3, 4, 4, 3, 5),
      nrow = 5,
      ncol = 4,
      dimnames = list(NULL, c("wo", "mo", "bo", "so")))

    base <- fnb.gaussian(x, y, priors=priors, sparse = sparse)
    altmod <- fnb.update(base, x, y, sparse = sparse)

    y <- factor(c(as.character(y), as.character(y)))
    x <- rbind(x, x)

    mod <- fnb.gaussian(x, y, priors=priors, sparse=sparse)

    mod_preds <- predict(mod, newdata = x, type = "raw")
    alt_preds <- predict(altmod, newdata = x, type = "raw")

    expect_equal(sum(abs(round(mod_preds - alt_preds, digits = 8))), 0)
  }

  test(NULL, FALSE)
  test(NULL, TRUE)
  test(c(0.5, 0.5), FALSE)
  test(c(0.5, 0.5), TRUE)
})
test_that("Bernoulli Normal case", {

  test <- function(laplace, sparse){
    data <- tweetsDTM[1:200,1:200]
    y <- as.factor(data[,1])
    x <- as.matrix(data[,2:ncol(data)])

    # Bernoulli model test with laplace = 1
    mod <- fnb.bernoulli(x, y, laplace = laplace, sparse = sparse)
    base <- fnb.bernoulli(x[1:100,], y[1:100], laplace = laplace, sparse = sparse)
    altmod <- fnb.update(base, x[101:nrow(x),], y[101:nrow(x)], sparse = sparse)

    mod_preds <- predict(mod, newdata = x, type = "raw")
    alt_preds <- predict(altmod, newdata = x, type = "raw")

    expect_equal(sum(abs(round(mod_preds - alt_preds, digits = 8))), 0)
  }


  test(1, FALSE)
  test(2, FALSE)

  test(1, TRUE)
  test(2, TRUE)

})

test_that("Bernoulli Iteratively", {

  test <- function(laplace, sparse){

    data <- tweetsDTM[1:25,1:200]

    y <- as.factor(data[,1])
    x <- as.matrix(data[,2:ncol(data)])

    # Bernoulli model test with laplace = 1
    mod <- fnb.bernoulli(x, y, laplace = laplace, sparse = sparse)
    base <- fnb.bernoulli(x[1, ,drop=FALSE], y[1], laplace = laplace, sparse = sparse)
    for(i in 2:nrow(x)){
      base <- fnb.update(base, x[i, ,drop=FALSE], y[i], sparse = sparse)
    }

    mod_preds <- predict(mod, newdata = x, type = "raw")
    alt_preds <- predict(base, newdata = x, type = "raw")

    expect_equal(sum(abs(round(mod_preds - alt_preds, digits = 8))), 0)

  }

  test(0, FALSE)
  test(1, FALSE)
  test(2, FALSE)

  test(0, TRUE)
  test(1, TRUE)
  test(2, TRUE)
})

test_that("Bernoulli New classes", {

  test <- function(laplace, sparse){

    data <- tweetsDTM[1:200,1:200]
    y <- as.factor(data[,1])
    x <- as.matrix(data[,2:ncol(data)])

    base_x <- x
    initial_x <- x[1:2,]
    next_x <- x[3:nrow(x),]

    mod <- fnb.bernoulli(base_x, y, laplace = laplace, sparse = sparse)
    initial <- fnb.bernoulli(initial_x, y[1:2], laplace = laplace, sparse = sparse)
    altmod <- fnb.update(initial, next_x, y[3:nrow(x)], sparse = sparse)

    mod_preds <- predict(mod, newdata = x, type = "raw")
    alt_preds <- predict(altmod, newdata = x, type = "raw")

    expect_equal(sum(abs(round(mod_preds - alt_preds, digits = 8))), 0)

  }

  test(1, FALSE)
  test(2, FALSE)

  test(1, TRUE)
  test(2, TRUE)
})

test_that("Bernoulli fewer columns", {

  test <- function(laplace, sparse){

    data <- tweetsDTM[1:200,1:200]
    y <- as.factor(data[,1])
    x <- as.matrix(data[,2:ncol(data)])

    base_x <- x
    initial_x <- x[1:100,]
    next_x <- x[101:nrow(x),1:100]

    base_x[101:nrow(x), 101:ncol(x)] <- 0

    mod <- fnb.bernoulli(base_x, y, laplace = laplace, sparse = sparse)
    initial <- fnb.bernoulli(initial_x, y[1:100], laplace = laplace, sparse = sparse)
    altmod <- fnb.update(initial, next_x, y[101:nrow(x)], sparse = sparse)

    mod_preds <- predict(mod, newdata = x, type = "raw")
    alt_preds <- predict(altmod, newdata = x, type = "raw")

    expect_equal(sum(abs(round(mod_preds - alt_preds, digits = 8))), 0)

  }


  test(1, FALSE)
  test(2, FALSE)

  test(1, TRUE)
  test(2, TRUE)
})

test_that("Bernoulli extra columns", {

  test <- function(laplace, sparse){

    data <- tweetsDTM[1:200, 1:200]
    y <- as.factor(data[,1])
    x <- as.matrix(data[,2:ncol(data)])

    base_x <- x
    initial_x <- x[1:100, 1:100]
    next_x <- x[101:nrow(x),]

    base_x[1:100, 101:ncol(x)] <- 0

    mod <- fnb.bernoulli(base_x, y, laplace = laplace, sparse = sparse)
    initial <- fnb.bernoulli(initial_x, y[1:100], laplace = laplace, sparse = sparse)
    altmod <- fnb.update(initial, next_x, y[101:nrow(x)], sparse = sparse)

    mod_preds <- predict(mod, newdata = x, type = "raw")
    alt_preds <- predict(altmod, newdata = x, type = "raw")

    expect_equal(sum(abs(round(mod_preds - alt_preds, digits = 8))), 0)

  }


  test(1, FALSE)
  test(2, FALSE)

  test(1, TRUE)
  test(2, TRUE)
})

test_that("Bernoulli completely new block", {

  test <- function(laplace, sparse){

    data <- tweetsDTM[1:200, 1:200]
    y <- as.factor(data[,1])
    x <- as.matrix(data[,2:ncol(data)])

    base_x <- x

    initial_x <- x[1:100, 1:100]
    initial_y <- y[1:100]

    next_x <- x[101:nrow(x),101:ncol(x)]
    next_y <- y[101:nrow(x)]

    base_x[1:100, 101:ncol(x)] <- 0
    base_x[101:nrow(x), 1:100] <- 0

    mod <- fnb.bernoulli(base_x, y, laplace = laplace, sparse = sparse)
    initial <- fnb.bernoulli(initial_x, initial_y, laplace = laplace, sparse = sparse)
    altmod <- fnb.update(initial, next_x, next_y, sparse = sparse)

    mod_preds <- predict(mod, newdata = x, type = "raw")
    alt_preds <- predict(altmod, newdata = x, type = "raw")

    expect_equal(sum(abs(round(mod_preds - alt_preds, digits = 8))), 0)

  }


  test(1, FALSE)
  test(2, FALSE)

  test(1, TRUE)
  test(2, TRUE)
})

test_that("Multinomial Normal case", {

  test <- function(laplace, sparse){
    data <- tweetsDTM[1:200,1:200]
    y <- as.factor(data[,1])
    x <- as.matrix(data[,2:ncol(data)])

    # multinomial model test with laplace = 1
    mod <- fnb.multinomial(x, y, laplace = laplace, sparse = sparse)
    base <- fnb.multinomial(x[1:100,], y[1:100], laplace = laplace, sparse = sparse)
    altmod <- fnb.update(base, x[101:nrow(x),], y[101:nrow(x)], sparse = sparse)

    mod_preds <- predict(mod, newdata = x, type = "raw")
    alt_preds <- predict(altmod, newdata = x, type = "raw")

    expect_equal(sum(abs(round(mod_preds - alt_preds, digits = 8))), 0)
  }


  test(1, FALSE)
  test(2, FALSE)

  test(1, TRUE)
  test(2, TRUE)

})

test_that("Multinomial Iteratively", {

  test <- function(laplace, sparse){

    data <- tweetsDTM[1:25,1:200]

    y <- as.factor(data[,1])
    x <- as.matrix(data[,2:ncol(data)])

    # multinomial model test with laplace = 1
    mod <- fnb.multinomial(x, y, laplace = laplace, sparse = sparse)
    base <- fnb.multinomial(x[1, ,drop=FALSE], y[1], laplace = laplace, sparse = sparse)
    for(i in 2:nrow(x)){
      base <- fnb.update(base, x[i, ,drop=FALSE], y[i], sparse = sparse)
    }

    mod_preds <- predict(mod, newdata = x, type = "raw")
    alt_preds <- predict(base, newdata = x, type = "raw")

    expect_equal(sum(abs(round(mod_preds - alt_preds, digits = 8))), 0)

  }

  test(0, FALSE)
  test(1, FALSE)
  test(2, FALSE)

  test(0, TRUE)
  test(1, TRUE)
  test(2, TRUE)
})

test_that("Multinomial New classes", {

  test <- function(laplace, sparse){

    data <- tweetsDTM[1:200,1:200]
    y <- as.factor(data[,1])
    x <- as.matrix(data[,2:ncol(data)])

    base_x <- x
    initial_x <- x[1:2,]
    next_x <- x[3:nrow(x),]

    mod <- fnb.multinomial(base_x, y, laplace = laplace, sparse = sparse)
    initial <- fnb.multinomial(initial_x, y[1:2], laplace = laplace, sparse = sparse)
    altmod <- fnb.update(initial, next_x, y[3:nrow(x)], sparse = sparse)

    mod_preds <- predict(mod, newdata = x, type = "raw")
    alt_preds <- predict(altmod, newdata = x, type = "raw")

    expect_equal(sum(abs(round(mod_preds - alt_preds, digits = 8))), 0)

  }

  test(1, FALSE)
  test(2, FALSE)

  test(1, TRUE)
  test(2, TRUE)
})

test_that("Multinomial fewer columns", {

  test <- function(laplace, sparse){

    data <- tweetsDTM[1:200,1:200]
    y <- as.factor(data[,1])
    x <- as.matrix(data[,2:ncol(data)])

    base_x <- x
    initial_x <- x[1:100,]
    next_x <- x[101:nrow(x),1:100]

    base_x[101:nrow(x), 101:ncol(x)] <- 0

    mod <- fnb.multinomial(base_x, y, laplace = laplace, sparse = sparse)
    initial <- fnb.multinomial(initial_x, y[1:100], laplace = laplace, sparse = sparse)
    altmod <- fnb.update(initial, next_x, y[101:nrow(x)], sparse = sparse)

    mod_preds <- predict(mod, newdata = x, type = "raw")
    alt_preds <- predict(altmod, newdata = x, type = "raw")

    expect_equal(sum(abs(round(mod_preds - alt_preds, digits = 8))), 0)

  }


  test(1, FALSE)
  test(2, FALSE)

  test(1, TRUE)
  test(2, TRUE)
})

test_that("Multinomial extra columns", {

  test <- function(laplace, sparse){

    data <- tweetsDTM[1:200, 1:200]
    y <- as.factor(data[,1])
    x <- as.matrix(data[,2:ncol(data)])

    base_x <- x
    initial_x <- x[1:100, 1:100]
    next_x <- x[101:nrow(x),]

    base_x[1:100, 101:ncol(x)] <- 0

    mod <- fnb.multinomial(base_x, y, laplace = laplace, sparse = sparse)
    initial <- fnb.multinomial(initial_x, y[1:100], laplace = laplace, sparse = sparse)
    altmod <- fnb.update(initial, next_x, y[101:nrow(x)], sparse = sparse)

    mod_preds <- predict(mod, newdata = x, type = "raw")
    alt_preds <- predict(altmod, newdata = x, type = "raw")

    expect_equal(sum(abs(round(mod_preds - alt_preds, digits = 8))), 0)

  }


  test(1, FALSE)
  test(2, FALSE)

  test(1, TRUE)
  test(2, TRUE)
})

test_that("Multinomial completely new block", {

  test <- function(laplace, sparse){

    data <- tweetsDTM[1:200, 1:200]
    y <- as.factor(data[,1])
    x <- as.matrix(data[,2:ncol(data)])

    base_x <- x

    initial_x <- x[1:100, 1:100]
    initial_y <- y[1:100]

    next_x <- x[101:nrow(x),101:ncol(x)]
    next_y <- y[101:nrow(x)]

    base_x[1:100, 101:ncol(x)] <- 0
    base_x[101:nrow(x), 1:100] <- 0

    mod <- fnb.multinomial(base_x, y, laplace = laplace, sparse = sparse)
    initial <- fnb.multinomial(initial_x, initial_y, laplace = laplace, sparse = sparse)
    altmod <- fnb.update(initial, next_x, next_y, sparse = sparse)

    mod_preds <- predict(mod, newdata = x, type = "raw")
    alt_preds <- predict(altmod, newdata = x, type = "raw")

    expect_equal(sum(abs(round(mod_preds - alt_preds, digits = 8))), 0)

  }


  test(1, FALSE)
  test(2, FALSE)

  test(1, TRUE)
  test(2, TRUE)
})
