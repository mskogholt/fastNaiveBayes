#' @export
#' @import Matrix
#' @rdname fastNaiveBayesF
fnb.poisson <- function(x, y, priors = NULL, sparse = FALSE, check = TRUE) {
  UseMethod("fnb.poisson")
}

#' @export
#' @import Matrix
#' @rdname fastNaiveBayesF
fnb.poisson.default <- function(x, y, priors = NULL, sparse = FALSE, check = TRUE) {
  if(check){
    args <- fnb.check.args.model(x, y, priors, sparse)
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
      return(list(level = level, means = means))
    })
  }else{
    rs <- rowsum(x,y)
    means <- rs/n
    probability_table <- lapply(levels(y), function(level){
      return(list(level = level, means = means[level,]))
    })
  }

  structure(
    list(
      probability_table = probability_table,
      n = n,
      obs = nrow(x),
      priors = priors,
      names = colnames(x),
      levels = levels(y),
      x = x,
      y = y
    ),
    class = "fnb.poisson"
  )
}

#' @export
#' @import Matrix
#' @rdname predict.fastNaiveBayes
predict.fnb.poisson <- function(object, newdata, type = c("class", "raw", "rawprob"), sparse = FALSE,
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
  #newdata <- t(newdata)
  for (j in 1:length(data)) {
    level <- data[[j]]
    level_probs <- NULL

    if(!check){
      means <- level$means
    }else{
      if(length(level$means)==1 && length(names)==1){
        means <- level$means
      }else{
        means <- level$means[names]
      }
    }

    means <- matrix(means, nrow=nrow(newdata), ncol=length(means), byrow=TRUE)

    level_probs <- log(((means^newdata)*exp(-means))/factorial(newdata))
    level_probs[is.infinite(level_probs)] <- max(-100000, log(threshold))
    level_probs[is.na(level_probs)] <- max(-100000, log(threshold))

    level_probs <- rowSums(level_probs)

    probs <- cbind(probs, level_probs)
    colnames(probs)[j] <- level$level
  }

  if (type == "rawprob") {
    return(probs)
  }

  priors <- object$priors
  if(is.null(priors)){
    priors <- object$n / object$obs
  }

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
