#' @export
#' @import Matrix
#' @rdname predict.fastNaiveBayes
predict.fnb.gaussian <- function(object, newdata, type = c("class", "raw", "rawprob"),
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

  names <- intersect(object$names, colnames(newdata))
  if(length(object$names)!=length(names)){
    warning('Columns in test and train set not equal! Only the intersect of the two is used for prediction')
    newdata <- newdata[, names]
  }
  if(length(names)==1){
    newdata <- as.matrix(newdata)
  }

  data <- object$probability_table
  probs <- NULL
  newdata <- t(newdata)
  for (j in 1:length(data)) {
    level <- data[[j]]
    level_probs <- NULL

    if(length(names)==1){
      means <- level$means
      stddevs <- level$stddev
    }else{
      means <- level$means[names]
      stddevs <- level$stddev[names]
    }

    level_probs <- colSums((newdata-means)^2/stddevs^2)
    level_probs <- sum(log(1/(sqrt(2*pi*stddevs^2))))-0.5*level_probs

    level_probs[is.infinite(level_probs)] <- max(-100000, log(threshold))

    probs <- cbind(probs, level_probs)

    colnames(probs)[j] <- level$level
  }

  if (type == "rawprob") {
    return(probs)
  }

  priors <- object$priors
  probs <- exp(probs)
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
