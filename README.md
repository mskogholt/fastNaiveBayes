fastNaiveBayes
==============

[![CRAN status](https://www.r-pkg.org/badges/version/fastNaiveBayes)](https://cran.r-project.org/package=fastNaiveBayes) [![Travis build status](https://travis-ci.org/mskogholt/fastNaiveBayes.svg?branch=master)](https://travis-ci.org/mskogholt/fastNaiveBayes) [![Codecov test coverage](https://codecov.io/gh/mskogholt/fastNaiveBayes/branch/master/graph/badge.svg)](https://codecov.io/gh/mskogholt/fastNaiveBayes?branch=master) [![CRAN Downloads Total](http://cranlogs.r-pkg.org/badges/grand-total/fastNaiveBayes)](https://cran.r-project.org/package=fastNaiveBayes) [![CRAN Downloads Weekly](http://cranlogs.r-pkg.org/badges/last-week/fastNaiveBayes)](https://cran.r-project.org/package=fastNaiveBayes)

Overview
--------

This is an extremely fast implementation of a Naive Bayes classifier. This package is currently the only package that supports a Bernoulli distribution, a Multinomial distribution, and a Gaussian distribution, making it suitable for both binary features, frequency counts, and numerical features. Another unique feature is the support of a mix of different event models. Only numerical variables are allowed, however, categorical variables can be transformed into dummies and used with the Bernoulli distribution.

This implementation offers a huge performance gain compared to the 'e1071' implementation in R. The execution times were compared on a data set of tweets and was found to be around 1135 times faster. Compared to other implementations the minimum speed up was found to be 12.5 times faster for the Bernoulli distribution. See the vignette for more details. This performance gain is only realized using a Bernoulli event model. Furthermore, the Multinomial event model implementation is even slightly faster, but incomparable since it was not implemented in 'e1071'. Compared to other implementations of a Multinomial distribution, this package was found to give a speed up of 12.2 times. The implementation is largely based on the paper "A comparison of event models for Naive Bayes anti-spam e-mail filtering" written by K.M. Schneider (2003) <doi:10.3115/1067807>.

Any issues can be submitted to: <https://github.com/mskogholt/fastNaiveBayes/issues>.

Installation
------------

Install the package with:

``` r
install.packages("fastNaiveBayes")
```

Or install the development version using [devtools](https://github.com/hadley/devtools) with:

``` r
library(devtools)
devtools::install_github("mskogholt/fastNaiveBayes")
```

Usage
-----

``` r
rm(list=ls())
library(fastNaiveBayes)

cars <- mtcars
y <- as.factor(ifelse(cars$mpg>25,'High','Low'))
x <- cars[,2:ncol(cars)]

# Mixed event models
dist <- fastNaiveBayes::fastNaiveBayes.detect_distribution(x, nrows = nrow(x))
print(dist)
mod <- fastNaiveBayes.mixed(x,y,laplace = 1)
pred <- predict(mod, newdata = x)
mean(pred!=y)

# Bernoulli only
vars <- c(dist$bernoulli, dist$multinomial)
newx <- x[,vars]
for(i in 1:ncol(newx)){
 newx[[i]] <- as.factor(newx[[i]])
}
new_mat <- model.matrix(y ~ . -1, cbind(y,newx))
mod <- fastNaiveBayes.bernoulli(new_mat, y, laplace = 1)
pred <- predict(mod, newdata = new_mat)
mean(pred!=y)

# Construction sparse Matrix:
mod <- fastNaiveBayes.bernoulli(new_mat, y, laplace = 1, sparse = TRUE)
pred <- predict(mod, newdata = new_mat)
mean(pred!=y)

# OR:
new_mat <- Matrix::Matrix(as.matrix(new_mat), sparse = TRUE)
mod <- fastNaiveBayes.bernoulli(new_mat, y, laplace = 1)
pred <- predict(mod, newdata = new_mat)
mean(pred!=y)

# Multinomial only
vars <- c(dist$bernoulli, dist$multinomial)
newx <- x[,vars]
mod <- fastNaiveBayes.multinomial(newx, y, laplace = 1)
pred <- predict(mod, newdata = newx)
mean(pred!=y)

# Gaussian only
vars <- c('hp', dist$gaussian)
newx <- x[,vars]
mod <- fastNaiveBayes.gaussian(newx, y)
pred <- predict(mod, newdata = newx)
mean(pred!=y)
```
