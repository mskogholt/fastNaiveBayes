#' @export
#' @import Matrix
#' @rdname predict.fastNaiveBayes
predict.fnb.mixed <- function(object, newdata, type = c("class", "raw"),
                              sparse = FALSE, threshold = .Machine$double.eps, ...) {
  type <- match.arg(type)
  if (class(newdata)[1] != "dgCMatrix") {
    if (!is.matrix(newdata)) {
      newdata <- as.matrix(newdata)
    }
    if (sparse) {
      newdata <- Matrix(newdata, sparse = TRUE)
    }
  } else {
    sparse <- TRUE
  }

  names <- object$names
  distribution <- object$distribution

  probs <- NULL
  for (i in 1:length(object$models)) {
    model <- object$models[[i]]
    newnames <- model$names
    newx <- newdata[, model$names]
    if (length(newnames) == 1) {
      newx <- as.matrix(newx)
      colnames(newx) <- newnames
    }
    if (is.null(probs)) {
      probs <- stats::predict(model, newx, type = "rawprob", sparse, threshold)
    } else {
      probs <- probs + stats::predict(model, newx, type = "rawprob", sparse, threshold)
    }
  }
  probs <- exp(probs)

  priors <- object$priors
  for(i in 1:length(priors)){
    probs[,i] <- probs[,i]*priors[i]
  }

  denom <- rowSums(probs)
  denom[denom==0] <- 1
  probs <- probs / denom

  if (type == "class") {
    class <- as.factor(object$levels[max.col(probs, ties.method = "first")])
    levels(class) <- object$levels
    return(class)
  }
  return(probs)
}
