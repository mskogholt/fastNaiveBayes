context("Test fnb.update")

test_that("Not implemented error", {
  data <- tweetsDTM[1:200]
  y <- as.factor(data[,1])
  x <- as.matrix(data[,2:ncol(data)])

  mod <- fnb.multinomial(x, y, laplace = 1)
  expect_error(fnb.update(mod, x, y))
})

test_that("Normal case", {

  test <- function(laplace, sparse){
    data <- tweetsDTM[1:200]
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

  test(0, FALSE)
  test(1, FALSE)
  test(2, FALSE)

  test(0, TRUE)
  test(1, TRUE)
  test(2, TRUE)

})

test_that("Iteratively", {

  test <- function(laplace, sparse){

    data <- tweetsDTM[1:200]

    y <- as.factor(data[1:25,1])
    x <- as.matrix(data[1:25,2:ncol(data)])

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

test_that("New classes", {

  test <- function(laplace, sparse){

    data <- tweetsDTM[1:200]
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

  test(0, FALSE)
  test(1, FALSE)
  test(2, FALSE)

  test(0, TRUE)
  test(1, TRUE)
  test(2, TRUE)
})

test_that("fewer columns", {

  test <- function(laplace, sparse){

    data <- tweetsDTM[1:200]
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

  test(0, FALSE)
  test(1, FALSE)
  test(2, FALSE)

  test(0, TRUE)
  test(1, TRUE)
  test(2, TRUE)
})

test_that("extra columns", {

  test <- function(laplace, sparse){

    data <- tweetsDTM[1:200]
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

  test(0, FALSE)
  test(1, FALSE)
  test(2, FALSE)

  test(0, TRUE)
  test(1, TRUE)
  test(2, TRUE)
})

test_that("completely new block", {

  test <- function(laplace, sparse){

    data <- tweetsDTM[1:200]
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

  test(0, FALSE)
  test(1, FALSE)
  test(2, FALSE)

  test(0, TRUE)
  test(1, TRUE)
  test(2, TRUE)
})
