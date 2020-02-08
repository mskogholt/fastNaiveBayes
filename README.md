fastNaiveBayes
==============

[![CRAN status](https://www.r-pkg.org/badges/version/fastNaiveBayes)](https://cran.r-project.org/package=fastNaiveBayes) [![Travis build status](https://travis-ci.org/mskogholt/fastNaiveBayes.svg?branch=master)](https://travis-ci.org/mskogholt/fastNaiveBayes) [![Codecov test coverage](https://codecov.io/gh/mskogholt/fastNaiveBayes/branch/master/graph/badge.svg)](https://codecov.io/gh/mskogholt/fastNaiveBayes?branch=master) [![CRAN Downloads Total](http://cranlogs.r-pkg.org/badges/grand-total/fastNaiveBayes)](https://cran.r-project.org/package=fastNaiveBayes) [![CRAN Downloads Weekly](http://cranlogs.r-pkg.org/badges/last-week/fastNaiveBayes)](https://cran.r-project.org/package=fastNaiveBayes)

Overview
--------

This is an extremely fast implementation of a Naive Bayes classifier. This package is currently the only package that supports a Bernoulli distribution, a Multinomial distribution, and a Gaussian distribution, making it suitable for both binary features, frequency counts, and numerical features. Another feature is the support of a mix of different event models. Only numerical variables are allowed, however, categorical variables can be transformed into dummies and used with the Bernoulli distribution.

This implementation offers a huge performance gain compared to other implementations in R. The execution times were compared on a data set of tweets and this package was found to be around 283 to 34,841 times faster for the Bernoulli event models and 17 to 60 times faster for the Multinomial model. For the Gaussian distribution this package was found to be between 2.8 and 1679 times faster. See the vignette for more details. The implementation is largely based on the paper "A comparison of event models for Naive Bayes anti-spam e-mail filtering" written by K.M. Schneider (2003).

Any issues can be submitted to: <https://github.com/mskogholt/fastNaiveBayes/issues>.

News
----

### v2.1.1 (2020-02-09)

-   Load and save option that enables storing model to file and loading it later
-   Minor fixes

### v2.1.0 (2019-08-27)

-   All new naming structure! All functions now start as fnb.\[name\]. This makes it easy to browse the functions with autocomplete. The one exception is the 'fastNaiveBayes' function which is a wrapper around fnb.train. This also allows easier bundled documentation, since a lot of the function arguments are the same across different event models
-   'check' argument that when set to false disables formal input checks. This means it is up to the user to ensure the input is proper, but gives a small speed improvement when skipping the checks. Only noticeable if a naive bayes model is trained thousands of times for example.
-   User can now specify prior probabilities for all models.

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
rm(list = ls())
library(fastNaiveBayes)
cars <- mtcars
y <- as.factor(ifelse(cars$mpg > 25, "High", "Low"))
x <- cars[,2:ncol(cars)]

mod <- fastNaiveBayes(x, y, laplace = 1)

pred <- predict(mod, newdata = x)
mean(y!=pred)

mod <- fnb.train(x, y, laplace = 1)

pred <- predict(mod, newdata = x)
mean(y!=pred)

dist <- fnb.detect_distribution(x)

bern <- fnb.bernoulli(x[,dist$bernoulli], y, laplace = 1)
pred <- predict(bern, x[,dist$bernoulli])
mean(y!=pred)

mult <- fnb.multinomial(x[,dist$multinomial], y, laplace = 1)
pred <- predict(mult, x[,dist$multinomial])
mean(y!=pred)

gauss <- fnb.gaussian(x[,dist$gaussian], y)
pred <- predict(gauss, x[,dist$gaussian])
mean(y!=pred)
```
