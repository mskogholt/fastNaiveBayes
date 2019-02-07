#' @export
#' @import Matrix
#' @rdname fastNaiveBayes.mixed
fastNaiveBayes.mixed.default <- function(x, y, laplace = 0, sparse = FALSE, distribution = NULL, ...){
  if(nrow(x)!=length(y)){
    stop('X and Y must be equal length')
  }

  if(class(x)[1]!='dgCMatrix'){
    if(!is.matrix(x)){
      x <- as.matrix(x)
    }
    if(sparse){
      x <- Matrix(x, sparse = TRUE)
    }
  }else{
    sparse <- TRUE
  }

  if(is.null(distribution)){
    distribution <- list(bernoulli = colnames(x))
  }

  models <- lapply(names(distribution), function(dist){
    switch(dist,
           bernoulli = fastNaiveBayes.bernoulli(x[,distribution[[dist]]],y,laplace, sparse),
           multinomial = fastNaiveBayes.multinomial(x[,distribution[[dist]]],y,laplace, sparse),
           gaussian = fastNaiveBayes.gaussian(x[,distribution[[dist]]],y,laplace, sparse)
    )
  })

  priors <- table(y)/nrow(x)
  structure(list(models = models,
                 priors = priors,
                 names = colnames(x),
                 distribution = distribution
  ),
  class = 'fastNaiveBayes.mixed')
}

