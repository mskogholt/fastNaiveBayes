#' @export
#' @import Matrix
#' @rdname fastNaiveBayesF
fnb.gaussian <- function(x, y, priors = NULL, sparse = FALSE, check = TRUE, ...) {
  UseMethod("fnb.gaussian")
}

#' @export
#' @import Matrix
#' @rdname fastNaiveBayesF
fnb.gaussian.default <- function(x, y, priors = NULL, sparse = FALSE, check = TRUE, ...) {
  if(check){
    args <- fnb.check.args.model(x, y, priors, laplace=0, sparse)
    x <- args$x
    y <- args$y
    priors <- args$priors
    sparse <- args$sparse
  }

  n <- tabulate(y)
  if (sparse) {
    probability_table <- lapply(levels(y), function(level) {
      x_level <- x[y == level, ]
      if (ncol(x) == 1) {
        x_level <- as.matrix(x_level)
      }

      means <- Matrix::colMeans(x_level, na.rm = TRUE)
      mat_means <- matrix(means, nrow = nrow(x_level), ncol = ncol(x_level), byrow = TRUE)
      stddev <- sqrt(Matrix::colSums((x_level - mat_means)^2) / (nrow(x_level) - 1))
      return(list(level = level, means = means, stddev = stddev))
    })
  }else{
    rs <- rowsum(x,y)
    means <- rs/n
    stddev <- sqrt((rowsum(x^2,y)-2*means*rs+n*means^2)/(n-1))
    probability_table <- lapply(levels(y), function(level){
      return(list(level = level, means = means[level,],
                  stddev = stddev[level,]))
    })
  }

  if(is.null(priors)){
    priors <- n / nrow(x)
  }

  structure(list(
    probability_table = probability_table,
    priors = priors,
    names = colnames(x),
    levels = levels(y)),

    class = "fnb.gaussian"
  )
}

#' @export
#' @import Matrix
#' @rdname predict.fastNaiveBayes
predict.fnb.gaussian <- function(object, newdata, type = c("class", "raw", "rawprob"), sparse = FALSE,
                                 threshold = .Machine$double.eps, check = TRUE, ...) {

  type <- match.arg(type)
  if(check){
    args <- fnb.check.args.predict(object, newdata, type, sparse, threshold, ...)
    object <- args$object
    newdata <- args$newdata
    type <- args$type
    sparse <- args$sparse
    threshold <- args$threshold
  }

  data <- object$probability_table
  probs <- NULL

  names <- colnames(newdata)
  newdata <- t(newdata)
  for (j in 1:length(data)) {
    level <- data[[j]]
    level_probs <- NULL

    if(!check){
      means <- level$means
      stddevs <- level$stddev
    }else{
      if(length(level$means)==1 && length(names)==1){
        means <- level$means
        stddevs <- level$stddev
      }else{
        means <- level$means[names]
        stddevs <- level$stddev[names]
      }
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


