#' @title Fast Naive Bayes Classifier for mixed Distributions
#' @description Extremely fast implementation of a Naive Bayes Classifier.
#'
#' @param x a numeric matrix, or a dgcMatrix
#' @param y a factor of classes
#' @param laplace A number used for Laplace smoothing. Default is 0
#' @param sparse Use a sparse matrix? If true a sparse matrix will be constructed from x, which can give up to a 40% speed up.
#'     It's possible to directly feed a sparse dgcMatrix as x, which will set this parameter to TRUE
#' @param distribution A list with distribution names and column names to for which to use the distribution, see examples.
#' @param ... Not used.
#'
#' @details A Naive Bayes classifier that assumes independence between the feature variables. Currently, either a Bernoulli,
#'     multinomial, or Gaussian distribution can be used. The bernoulli distribution should be used when the features are 0 or 1 to
#'     indicate the presence or absence of the feature in each document. The multinomial distribution should be used when the
#'     features are the frequency that the feature occurs in each document. NA's are simply treated as 0. Finally, the Gaussian distribution
#'     should be used with numerical variables. By setting the distribution parameter a mix of different distributions can be used
#'     for different columns in the input matrix
#'
#'     By setting sparse = TRUE the numeric matrix x will be converted to a sparse dgcMatrix. This can be considerably faster
#'     in case few observations have a value different than 0.
#'
#'     It's also possible to directly supply a sparse dgcMatrix, which can be a lot faster in case a fastNaiveBayes model
#'     is trained multiple times on the same matrix or a subset of this. See examples for more details. Bear in mind that
#'     converting to a sparse matrix can actually be slower depending on the data.
#'
#' @return A fitted object of class "fastNaiveBayes". It has four components:
#'
#'     \describe{
#'         \item{models}{The fitted models, one for each distribution specified}
#'         \item{priors}{calculated prior probabilities for each class}
#'         \item{names}{names of features used to train this fastNaiveBayes}
#'         \item{distribution}{the distribution assumed for probability calculations and predictions}
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
#'     # Example using only Gaussian distribution
#'     model <- fastNaiveBayes.mixed(data_mat[1:400,], y[1:400], laplace = 1, sparse = TRUE,
#'                                   distribution = list(
#'                                     gaussian = colnames(data_mat)
#'                                   ))
#'     preds <- predict(model, newdata = data_mat[401:nrow(data_mat),], type = "class")
#'
#'     mean(preds!=y[401:length(y)])
#'
#'     # Example mixing distributions
#'     model <- fastNaiveBayes.mixed(data_mat[1:400,], y[1:400], laplace = 1, sparse = TRUE,
#'                                   distribution = list(
#'                                     multinomial = c("Cl.thickness","Cell.size"),
#'                                     gaussian = c("Cell.shape","Marg.adhesion")
#'                                   ))
#'     preds <- predict(model, newdata = data_mat[401:nrow(data_mat),], type = "class")
#'
#'     mean(preds!=y[401:length(y)])
#'
#'     # Construct y and sparse matrix
#'     # Bernoulli dummy example
#'     data_mat <- BreastCancer[,c("Class","Cl.thickness","Cell.size","Cell.shape","Marg.adhesion")]
#'     col_counter <- ncol(data_mat)+1
#'     for(i in 2:ncol(data_mat)){
#'       for(val in unique(data_mat[,i])){
#'         data_mat[,col_counter] <- ifelse(data_mat[,i]==val,1,0)
#'         col_counter <- col_counter+1
#'       }
#'     }
#'
#'     y <- data_mat[,"Class"]
#'     data_mat <- data_mat[,setdiff(colnames(data_mat),c("Class","Cl.thickness", "Cell.size",
#'                                                        "Cell.shape","Marg.adhesion"))]
#'
#'     sparse_data <- Matrix(as.matrix(data_mat), sparse = TRUE)
#'     data_mat <- as.matrix(data_mat)
#'
#'     # Example to estimate and predict once with Bernoulli distribution
#'     model <- fastNaiveBayes.mixed(data_mat[1:400,], y[1:400], laplace = 1, sparse = TRUE,
#'                                   distribution = list(
#'                                     bernoulli = colnames(data_mat)
#'                                   ))
#'     preds <- predict(model, newdata = data_mat[401:nrow(data_mat),], type = "class")
#'
#'     mean(preds!=y[401:length(y)])
#'
#'     # Example using the direct model. This is much faster if all columns should have
#'     # The same event model. It saves a lot of overhead
#'     direct_model <- fastNaiveBayes.bernoulli(data_mat[1:400,], y[1:400], laplace = 1, sparse = TRUE)
#'     direct_preds <- predict(direct_model, newdata = data_mat[401:nrow(data_mat),], type = "class")
#'     mean(direct_preds!=y[401:length(y)])
#'
#' @seealso \code{\link{predict.fastNaiveBayes.mixed}} for the predict function for a fastNaiveBayes class,
#' \code{\link{fastNaiveBayes.bernoulli}} for a Bernoulli only model, \code{\link{fastNaiveBayes.gaussian}} for a Gaussian
#' distribution only model, and finally, \code{\link{fastNaiveBayes.multinomial}} for a multinomial only distribution model.
#' @rdname fastNaiveBayes.mixed
fastNaiveBayes.mixed <- function(x, y, laplace = 0, sparse = FALSE, distribution = NULL, ...){
  UseMethod("fastNaiveBayes.mixed")
}

