#' @export
#' @import Matrix
#' @rdname fastNaiveBayes
fastNaiveBayes.default <- function(x, y, laplace = 0, sparse = FALSE, distribution =
                                     c("bernoulli","multinomial"), ...){
  distribution <- match.arg(distribution)
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

  if(distribution=="bernoulli"){
    if(sparse){
      present <- lapply(levels(y),function(level){
        Matrix::colSums(x[y==level,])
      })
      present <- do.call(rbind, present)
      x <- 1-x
      non_present <- lapply(levels(y), function(level){
        Matrix::colSums(x[y==level,])
      })
      non_present <- do.call(rbind, non_present)
    }else{
      present <- rowsum(x, y)
      non_present <- rowsum(1-x,y)
    }
    present <- present + laplace
    non_present <- non_present + laplace
    total <- present + non_present

    present <- present/total
    non_present <- non_present/total
    probability_table <- list(present = present,
                              non_present = non_present)
  }
  if(distribution=="multinomial"){
    if(sparse){
      present <- lapply(levels(y),function(level){
        Matrix::colSums(x[y==level,])
      })
      present <- do.call(rbind, present)
    }else{
      present <- rowsum(x, y)
    }
    present <- present + laplace
    total <- rowSums(present)

    present <- present/total
    probability_table <- list(present = present,
                              non_present = NULL)
  }

  priors <- table(y)/nrow(x)
  structure(list(probability_table = probability_table,
                 priors = priors,
                 names = colnames(x),
                 distribution = distribution
  ),
  class = 'fastNaiveBayes')
}
