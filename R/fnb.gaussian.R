#' @export
#' @import Matrix
#' @rdname fastNaiveBayesF
fnb.gaussian <- function(x, y, priors = NULL, sparse = FALSE, check = TRUE) {
  UseMethod("fnb.gaussian")
}

#' @export
#' @import Matrix
#' @rdname fastNaiveBayesF
fnb.gaussian.default <- function(x, y, priors = NULL, sparse = FALSE, check = TRUE) {
  if(check){
    args <- fnb.gaussian.check.args.model(x, y, priors, sparse)
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
    args <- fnb.gaussian.check.args.predict(object, newdata, type, sparse, threshold, ...)
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

#' @import Matrix
fnb.gaussian.check.args.model <- function(x, y, priors, sparse){
  # x
  if (class(x)[1] != "dgCMatrix") {
    if (!is.matrix(x)) {
      x <- as.matrix(x)
    }
    if (sparse) {
      x <- Matrix(x, sparse = TRUE)
    }
  } else {
    sparse <- TRUE
  }

  if(is.null(colnames(x))){
    stop("x must have column names!")
  }

  if(any(is.na(x))){
    warning("x contains na's. These will be set to 0")
    x[is.na(x)] <- 0
  }

  # y
  if(!is.factor(y)){
    y <- as.factor(y)
  }

  if(nlevels(y)<=1){
    stop('y does not have enough levels to classify.')
  }

  if(any(is.na(y))){
    warning("y contains na's. These observations will be removed")
    x <- x[!is.na(y),]
    y <- y[!is.na(y)]
  }

  # y with x
  if(nrow(x)!=length(y)){
    stop('Rows of x not equal to length of y')
  }

  if(any(rowsum(rep(1,times = length(y)), y)<2)){
    stop('Not enough rows. Should be at least 2 rows or more for each class')
  }

  # priors
  if(!is.null(priors)){
    if(!is.vector(priors, mode = "numeric")){
      stop(paste0("Priors should be a numeric vector with ",
                  nlevels(y), " prior probabilities"))
    }

    if (length(priors) != nlevels(y)){
      stop(paste0("Priors should be a vector with ",
                  nlevels(y), " prior probabilities"))
    }

    if(abs(sum(priors)-1) > .Machine$double.eps){
      stop(paste0('Sum of the priors should equal 1, not ', sum(priors)))
    }
  }

  return(list(x=x, y=y, priors = priors, sparse=sparse))
}

#' @import Matrix
fnb.gaussian.check.args.predict <- function(object, newdata, type, sparse, threshold, silent = FALSE){
  if(threshold<0){
    stop('Threshold must be a positive number')
  }
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

  if(is.null(colnames(newdata))){
    stop("newdata does not have any column names!")
  }

  if(any(is.na(newdata))){
    warning("newdata contains na's. These will be set to 0")
    newdata[is.na(newdata)] <- 0
  }

  names <- intersect(object$names, colnames(newdata))
  if(!silent){
    if(length(object$names)!=length(names)){
      warning('Columns in test and train set not equal! Only the intersect of the two is used for prediction')
    }
  }
  newdata <- newdata[, names, drop=FALSE]
  return(list(object=object, newdata=newdata, type=type, sparse=sparse, threshold = threshold))
}
