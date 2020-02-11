#' @export
#' @import Matrix
#' @rdname fastNaiveBayesF
fnb.bernoulli <- function(x, y, priors = NULL, laplace = 0, sparse = FALSE, check = TRUE) {
  UseMethod("fnb.bernoulli")
}

#' @export
#' @import Matrix
#' @rdname fastNaiveBayesF
fnb.bernoulli.default <- function(x, y, priors = NULL, laplace = 0, sparse = FALSE, check = TRUE) {
  if(check){
    args <- fnb.bernoulli.check.args.model(x, y, priors, laplace, sparse)
    x <- args$x
    y <- args$y
    priors <- args$priors
    laplace <- args$laplace
    sparse <- args$sparse
  }

  n <- tabulate(y)

  present <- fnb.bernoulli.calculate(x, y, sparse)

  structure(list(
    present = present,
    laplace = laplace,
    n = n,
    obs = nrow(x),
    priors = priors,
    names = colnames(x),
    levels = levels(y)),

    class = "fnb.bernoulli"
  )
}

#' @export
#' @import Matrix
#' @rdname predict.fastNaiveBayes
predict.fnb.bernoulli <- function(object, newdata, type = c("class", "raw", "rawprob"), sparse = FALSE,
                                  threshold = .Machine$double.eps, check = TRUE, ...) {

  type <- match.arg(type)
  if(check){
    args <- fnb.bernoulli.check.args.predict(object, newdata, type, sparse, threshold, ...)
    object <- args$object
    newdata <- args$newdata
    type <- args$type
    sparse <- args$sparse
    threshold <- args$threshold
  }

  present <- object$present + object$laplace
  present <- present / (object$n+2*object$laplace)

  nonpresent <- log(1-present)
  present <- log(present)

  present[is.infinite(present)] <- max(-100000, log(threshold))
  nonpresent[is.infinite(nonpresent)] <- max(-100000, log(threshold))

  presence_prob <- newdata %*% t(present)
  nonpresence_prob <- matrix(base::colSums(t(nonpresent)),
                             nrow = nrow(presence_prob),
                             ncol = ncol(presence_prob), byrow = TRUE) - newdata %*% t(nonpresent)

  priors <- object$priors
  if(is.null(priors)){
    priors <- object$n / object$obs
  }

  if (type == "rawprob") {
    return(presence_prob + nonpresence_prob)
  }

  probs <- exp((presence_prob + nonpresence_prob))
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

fnb.bernoulli.calculate <- function(x, y, sparse){
  if (sparse) {
    present <- lapply(levels(y), function(level) {
      Matrix::colSums(x[y == level, ,drop=FALSE])
    })
    present <- do.call(rbind, present)
  } else {
    present <- rowsum(x, y)
  }
  return(present)
}

#' @import Matrix
fnb.bernoulli.check.args.model <- function(x, y, priors, laplace, sparse){
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

  # laplace
  if(laplace < 0){
    stop('Laplace smoothing must a positive number.')
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
  return(list(x=x, y=y, priors = priors, laplace=laplace, sparse=sparse))
}

#' @import Matrix
fnb.bernoulli.check.args.predict <- function(object, newdata, type, sparse, threshold, silent = FALSE){
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

  names <- intersect(object$names, colnames(newdata))
  newdata <- newdata[, names, drop=FALSE]

  if(any(is.na(newdata))){
    warning("newdata contains na's. These will be set to 0")
    newdata[is.na(newdata)] <- 0
  }

  if(length(object$names)!=length(names)){
    if(!silent){
      warning('Columns in test and train set not equal! Newdata is padded with zeros')
    }

    other_names <- setdiff(object$names, colnames(newdata))
    if(length(other_names)>0){
      if (sparse) {
        other_mat <- Matrix(0L, nrow = nrow(newdata), ncol = length(other_names), sparse = TRUE)
      } else {
        other_mat <- matrix(0L, nrow = nrow(newdata), ncol = length(other_names))
      }
      colnames(other_mat) <- other_names

      newdata <- cbind(newdata, other_mat)
    }
    newdata <- newdata[, object$names, drop=FALSE]
  }

  return(list(object=object, newdata=newdata, type=type, sparse=sparse, threshold = threshold))
}

