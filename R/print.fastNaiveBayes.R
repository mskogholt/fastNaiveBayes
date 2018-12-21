#' Prints fastNaiveBayes object
#'
#' @param x a fastNaiveBayes object
#' @param ... not used.
#' @export
#' @method print fastNaiveBayes
print.fastNaiveBayes <- function(x,...){
  cat("\n A very efficient, highly scalable Naive Bayes Classifier\n\n")
  cat("A total of", length(x$train_data[[1]]$total),"features we're used for determining",
      "the frequency table","\n")
  cat("The different classes used to train were:",paste(names(x$train_data), collapse = " & "),"\n")
}
