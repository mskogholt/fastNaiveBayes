#' @title Fast Naive Bayes Classifier
#' @description Extremely fast and highly scalable implementation of a Naive Bayes Classifier.
#'
#' @param x a numeric matrix with 1's and 0's to indicate the presence or absence of features. A sparse dgcMatrix is also accepted
#' @param y a factor of classes
#' @param laplace A number used for Laplace smoothing. Default is 0
#' @param sparse Use a sparse matrix? If true a sparse matrix will be constructed from x, which can give up to a 40% speed up.
#'     It's possible to directly feed a sparse dgcMatrix as x, which will set this parameter to TRUE
#' @param ... Not used.
#' @param distribution the type of distribution used to model the probabilites. Either bernoulli or multinomial.
#'
#' @details A Naive Bayes classifier that assumes independence between the feature variables. Currently, either a Bernoulli
#'     or a multinomial distribution can be used. The bernoulli distribution should be used when the features are 0 or 1 to
#'     indicate the presence or absence of the feature in each document. The multinomial distribution should be used when the
#'     features are the frequency that the feature occurs in each document. NA's are simply treated as 0.
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
#'         \item{probability_table}{The calculated conditional probabilities}
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
#'     # Convert to 0/1 dummies
#'     data_mat <- BreastCancer[,c("Class","Cl.thickness","Cell.size","Cell.shape")]
#'     col_counter <- ncol(data_mat)+1
#'     for(i in 2:ncol(data_mat)){
#'       for(val in unique(data_mat[,i])){
#'         data_mat[,col_counter] <- ifelse(data_mat[,i]==val,1,0)
#'         col_counter <- col_counter+1
#'       }
#'     }
#'
#'     # Construct y and sparse matrix
#'     y <- data_mat[,"Class"]
#'     data_mat <- data_mat[,setdiff(colnames(data_mat),c("Class","Cl.thickness",
#'     "Cell.size","Cell.shape"))]
#'
#'     sparse_data <- Matrix(as.matrix(data_mat), sparse = TRUE)
#'     data_mat <- as.matrix(data_mat)
#'
#'     # Example to estimate and predict once:
#'     model <- fastNaiveBayes(data_mat[1:400,], y[1:400], laplace = 1, sparse = TRUE)
#'     preds <- predict(model, newdata = data_mat[401:nrow(data_mat),], type = "class")
#'
#'     mean(preds!=y[401:length(y)])
#'
#' @seealso \code{\link{predict.fastNaiveBayes}} for the predict function for a fastNaiveBayes class.
#' @rdname fastNaiveBayes
fastNaiveBayes <- function(x, y, laplace = 0, sparse = FALSE, distribution =
                             c("bernoulli","multinomial"), ...){
  UseMethod("fastNaiveBayes")
}

