#' @title Fast Naive Bayes Classifier with a Multinomial event model
#' @description Extremely fast implementation of a Naive Bayes Classifier. This instance only uses the
#' Multinomial event model for all columns.
#'
#' @param x a numeric matrix with frequency counts. A sparse dgcMatrix is also accepted
#' @param y a factor of classes
#' @param laplace A number used for Laplace smoothing. Default is 0
#' @param sparse Use a sparse matrix? If true a sparse matrix will be constructed from x, which can give up to a 40% speed up.
#'     It's possible to directly feed a sparse dgcMatrix as x, which will set this parameter to TRUE
#' @param ... Not used.
#'
#' @details A Naive Bayes classifier that assumes independence between the feature variables. The multinomial distribution should
#'     be used when the features are the frequency that the feature occurs in each document.
#'
#'     By setting sparse = TRUE the numeric matrix x will be converted to a sparse dgcMatrix. This can be considerably faster
#'     in case few observations have a value different than 0.
#'
#'     It's also possible to directly supply a sparse dgcMatrix, which can be a lot faster in case a fastNaiveBayes model
#'     is trained multiple times on the same matrix or a subset of this. See examples for more details. Bear in mind that
#'     converting to a sparse matrix can actually be slower depending on the data.
#'
#' @return A fitted object of class "fastNaiveBayes.bernoulli". It has four components:
#'
#'     \describe{
#'         \item{probability_table}{Posterior probabilities}
#'         \item{priors}{calculated prior probabilities for each class}
#'         \item{names}{names of features used to train this fastNaiveBayes}
#'     }
#'
#' @export
#' @import Matrix
#' @examples
#'     rm(list=ls())
#'     require(mlbench)
#'     require(Matrix)
#'
#'     # Load BreastCancer data
#'     data(BreastCancer)
#'     dim(BreastCancer)
#'     levels(BreastCancer$Class)
#'     head(BreastCancer)
#'
#'     # Select couple of columns
#'     data_mat <- BreastCancer[,c("Class","Cl.thickness","Cell.size","Cell.shape","Marg.adhesion")]
#'
#'     y <- data_mat[,"Class"]
#'     data_mat <- data_mat[,setdiff(colnames(data_mat),c("Class"))]
#'     for(i in 1:ncol(data_mat)){
#'       data_mat[[i]] <- as.numeric(data_mat[[i]])
#'     }
#'
#'
#'     model <- fastNaiveBayes.multinomial(data_mat[1:400,], y[1:400], laplace = 1, sparse = FALSE)
#'     preds <- predict(model, newdata = data_mat[401:nrow(data_mat),], type = "class")
#'
#'     mean(preds!=y[401:length(y)])
#'
#' @seealso \code{\link{predict.fastNaiveBayes.multinomial}} for the predict function for the fastNaiveBayes.multinomial class,
#' \code{\link{fastNaiveBayes.mixed}} for the general fastNaiveBayes model, \code{\link{fastNaiveBayes.bernoulli}} for a Bernoulli
#' distribution only model, and finally, \code{\link{fastNaiveBayes.gaussian}} for a Gaussian only distribution model.
#' @rdname fastNaiveBayes.multinomial
fastNaiveBayes.multinomial <- function(x, y, laplace = 0, sparse = FALSE, ...){
  UseMethod("fastNaiveBayes.multinomial")
}
