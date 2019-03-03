fastNaiveBayes
==============

\[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version-last-release/fastNaiveBayes)\]\[![Travis build status](https://travis-ci.org/mskogholt/fastNaiveBayes.svg?branch=master)\](<https://travis-ci.org/mskogholt/fastNaiveBayes>)

Overview
--------

This is a very fast implementation of the Naive Bayes classifier in R. It has the fastest execution time of any other Naive Bayes implementation in R. It's also the only implementation that makes it possible to use either a Bernoulli distribution or a multinomial distribution for the features.

This is an extremely fast implementation of a Naive Bayes classifier. The package currently supports a Bernoulli distribution, a Multinomial distribution, and a Gaussian distribution, making it suitable for both binary features, frequency counts, and numerical features. Only numerical variables are allowed, however, categorical variables can be transformed into dummies and used with the Bernoulli distribution. The implementation is based on the paper "A comparison of event models for Naive Bayes anti-spam e-mail filtering" written by K.M. Schneider (2003) <doi:10.3115/1067807>. This implementation offers a huge performance gain compared to the 'e1071' implementation in R. The execution times were compared on a data set of tweets and was found to be 331 times faster. See the vignette for more details. This performance gain is only realized using a Bernoulli event model. Furthermore, the Multinomial event model is equally fast and available in contrast to 'e1071'.

Any issues can be submitted to: <https://github.com/mskogholt/fastNaiveBayes/issues>

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
library(mlbench)
library(Matrix)
library(fastNaiveBayes)

# Load BreastCancer data
data(BreastCancer)
dim(BreastCancer)
levels(BreastCancer$Class)
head(BreastCancer)
# Select couple of columns
data_mat <- BreastCancer[,c("Class","Cl.thickness","Cell.size","Cell.shape","Marg.adhesion")]
y <- data_mat[,"Class"]
data_mat <- data_mat[,setdiff(colnames(data_mat),c("Class"))]
for(i in 1:ncol(data_mat)){
 data_mat[[i]] <- as.numeric(data_mat[[i]])
}
# Example using only Gaussian distribution
model <- fastNaiveBayes.mixed(data_mat[1:400,], y[1:400], laplace = 1, sparse = TRUE,
                             distribution = list(
                               gaussian = colnames(data_mat)
                             ))
preds <- predict(model, newdata = data_mat[401:nrow(data_mat),], type = "class")
mean(preds!=y[401:length(y)])
# Example mixing distributions
model <- fastNaiveBayes.mixed(data_mat[1:400,], y[1:400], laplace = 1, sparse = TRUE,
                             distribution = list(
                               multinomial = c("Cl.thickness","Cell.size"),
                               gaussian = c("Cell.shape","Marg.adhesion")
                             ))
preds <- predict(model, newdata = data_mat[401:nrow(data_mat),], type = "class")
mean(preds!=y[401:length(y)])
# Construct y and sparse matrix
# Bernoulli dummy example
data_mat <- BreastCancer[,c("Class","Cl.thickness","Cell.size","Cell.shape","Marg.adhesion")]
col_counter <- ncol(data_mat)+1
for(i in 2:ncol(data_mat)){
 for(val in unique(data_mat[,i])){
   data_mat[,col_counter] <- ifelse(data_mat[,i]==val,1,0)
   col_counter <- col_counter+1
 }
}
y <- data_mat[,"Class"]
data_mat <- data_mat[,setdiff(colnames(data_mat),c("Class","Cl.thickness", "Cell.size",
                                                  "Cell.shape","Marg.adhesion"))]
sparse_data <- Matrix(as.matrix(data_mat), sparse = TRUE)
data_mat <- as.matrix(data_mat)
# Example to estimate and predict once with Bernoulli distribution
model <- fastNaiveBayes.mixed(data_mat[1:400,], y[1:400], laplace = 1, sparse = TRUE,
                             distribution = list(
                               bernoulli = colnames(data_mat)
                             ))
preds <- predict(model, newdata = data_mat[401:nrow(data_mat),], type = "class")
mean(preds!=y[401:length(y)])
# Example using the direct model. This is much faster if all columns should have
# The same event model. It saves a lot of overhead
direct_model <- fastNaiveBayes.bernoulli(data_mat[1:400,], y[1:400], laplace = 1, sparse = TRUE)
direct_preds <- predict(direct_model, newdata = data_mat[401:nrow(data_mat),], type = "class")
mean(direct_preds!=y[401:length(y)])
```
