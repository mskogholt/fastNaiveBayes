## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE----------------------------------------------------------
#  rm(list=ls())
#  ###################### LIBRARIES ###########################
#  library(tm) #used for text mining
#  library(e1071) #this package includes the naive Bayes algorithm
#  library(Matrix)
#  library(microbenchmark)
#  library(e1071)
#  library(fastNaiveBayes)
#  library(quanteda)
#  library(naivebayes)
#  library(bnlearn)
#  library(klaR)
#  library(data.table)
#  
#  ############################ Timing Script ################
#  results <- NULL
#  
#  # Bernoulli Event Model
#  tweets <- fastNaiveBayes::tweetsDTM
#  
#  
#  y_var <- tweets$airline_sentiment
#  y_var <- as.factor(ifelse(y_var=='negative','negative','non-negative'))
#  tweets <- tweets[,2:ncol(tweets)]
#  tweets[tweets>1] <- 1
#  
#  tweets <- tweets[,which(colSums(tweets)!=0)]
#  tweets <- tweets[,which(colSums(tweets)!=nrow(tweets))]
#  
#  tweet_mat <- as.matrix(tweets)
#  sparse_tweets <- Matrix(as.matrix(tweet_mat), sparse = TRUE)
#  
#  for(i in 1:ncol(tweets)){
#    tweets[[i]] <- as.factor(tweets[[i]])
#  }
#  
#  # BNLearn
#  bn_tweets <- cbind(y_var, tweets)
#  colnames(bn_tweets)[1] <- 'y_var'
#  
#  # Quanteda
#  dfm <- as.dfm(tweet_mat)
#  
#  res <- microbenchmark(
#    klar = predict(klaR::NaiveBayes(x=tweets, grouping = y_var, fL=1), tweets),
#    e1071 = predict(e1071::naiveBayes(tweets, y_var, laplace = 1), tweets),
#    fastNaiveBayes = predict(fastNaiveBayes.bernoulli(tweet_mat, y_var, laplace = 1), tweet_mat),
#    fastNaiveBayes_sparse = predict(fastNaiveBayes.bernoulli(sparse_tweets, y_var, laplace = 1), sparse_tweets),
#    bnlearn = predict(bnlearn::naive.bayes(bn_tweets, 'y_var'), bn_tweets),
#    quanteda = predict(quanteda::textmodel_nb(dfm, y_var, prior = "docfreq", distribution = "Bernoulli"),
#                       newdata = dfm),
#    naivebayes = predict(naivebayes::naive_bayes(tweets, y_var, laplace = 1), newdata = tweets),
#    times = 3,
#    unit = "ms"
#  )
#  
#  res <- as.data.table(res)
#  res[,nrows:=nrow(tweet_mat)]
#  res[,ncols:=ncol(tweet_mat)]
#  res[,model:='Bernoulli']
#  
#  results <- res
#  
#  # Multinomial Event Model
#  tweets <- fastNaiveBayes::tweetsDTM
#  
#  y_var <- tweets$airline_sentiment
#  y_var <- as.factor(ifelse(y_var=='negative','negative','non-negative'))
#  tweets <- tweets[,2:ncol(tweets)]
#  
#  tweets <- tweets[,which(colSums(tweets)!=0)]
#  
#  tweet_mat <- as.matrix(tweets)
#  sparse_tweets <- Matrix(as.matrix(tweet_mat), sparse = TRUE)
#  
#  # Quanteda
#  dfm <- as.dfm(tweet_mat)
#  
#  res <- microbenchmark(
#    fastNaiveBayes = predict(fastNaiveBayes.multinomial(tweet_mat, y_var, laplace = 1), tweet_mat),
#    fastNaiveBayes_sparse = predict(fastNaiveBayes.multinomial(sparse_tweets, y_var, laplace = 1), sparse_tweets),
#    quanteda = predict(quanteda::textmodel_nb(dfm, y_var, prior = "docfreq", distribution = "multinomial"),
#                       newdata = dfm),
#    Rfast = Rfast::multinom.nb(tweet_mat, tweet_mat, y_var),
#    times = 3,
#    unit = "ms"
#  )
#  
#  res <- as.data.table(res)
#  res[,nrows:=nrow(tweet_mat)]
#  res[,ncols:=ncol(tweet_mat)]
#  res[,model:='Multinomial']
#  
#  results <- rbind(results, res)
#  
#  # Gaussian Event Model
#  cars <- mtcars
#  for(i in 1:6){
#    cars <- rbind(cars, cars)
#  }
#  
#  y_var <- cars$mpg
#  y_var <- as.factor(ifelse(y_var>20,'negative','non-negative'))
#  
#  cars <- cars[,3:7]
#  for(i in 1:6){
#    cars <- cbind(cars, cars)
#  }
#  
#  cars_mat <- as.matrix(cars)
#  sparse_cars <- Matrix(as.matrix(cars_mat), sparse = TRUE)
#  
#  res <- microbenchmark(
#    klar = predict(klaR::NaiveBayes(x=cars_mat, grouping = y_var, fL=1), cars_mat),
#    e1071 = predict(e1071::naiveBayes(cars_mat, y_var, laplace = 1), cars_mat),
#    naivebayes = predict(naivebayes::naive_bayes(cars_mat, y_var, laplace = 1), newdata = cars_mat),
#    fastNaiveBayes = predict(fastNaiveBayes.gaussian(cars_mat, y_var), cars_mat),
#    fastNaiveBayes_sparse = predict(fastNaiveBayes.gaussian(sparse_cars, y_var), sparse_cars),
#    Rfast = Rfast::gaussian.nb(cars_mat, cars_mat, y_var),
#    times = 3,
#    unit = "ms"
#  )
#  
#  res <- as.data.table(res)
#  res[,nrows:=nrow(cars_mat)]
#  res[,ncols:=ncol(cars_mat)]
#  res[,model:='Gaussian']
#  
#  results <- rbind(results, res)
#  
#  print(results)
#  fwrite(results, file = "./package_timings.csv", row.names = FALSE)

## ----eval=FALSE----------------------------------------------------------
#  rm(list=ls())
#  library(fastNaiveBayes)
#  
#  cars <- mtcars
#  y <- as.factor(ifelse(cars$mpg>25,'High','Low'))
#  x <- cars[,2:ncol(cars)]
#  
#  # Mixed event models
#  dist <- fastNaiveBayes::fastNaiveBayes.detect_distribution(x, nrows = nrow(x))
#  print(dist)
#  mod <- fastNaiveBayes.mixed(x,y,laplace = 1)
#  pred <- predict(mod, newdata = x)
#  mean(pred!=y)
#  
#  # Bernoulli only
#  vars <- c(dist$bernoulli, dist$multinomial)
#  newx <- x[,vars]
#  for(i in 1:ncol(newx)){
#   newx[[i]] <- as.factor(newx[[i]])
#  }
#  new_mat <- model.matrix(y ~ . -1, cbind(y,newx))
#  mod <- fastNaiveBayes.bernoulli(new_mat, y, laplace = 1)
#  pred <- predict(mod, newdata = new_mat)
#  mean(pred!=y)
#  
#  # Construction sparse Matrix:
#  mod <- fastNaiveBayes.bernoulli(new_mat, y, laplace = 1, sparse = TRUE)
#  pred <- predict(mod, newdata = new_mat)
#  mean(pred!=y)
#  
#  # OR:
#  new_mat <- Matrix::Matrix(as.matrix(new_mat), sparse = TRUE)
#  mod <- fastNaiveBayes.bernoulli(new_mat, y, laplace = 1)
#  pred <- predict(mod, newdata = new_mat)
#  mean(pred!=y)
#  
#  # Multinomial only
#  vars <- c(dist$bernoulli, dist$multinomial)
#  newx <- x[,vars]
#  mod <- fastNaiveBayes.multinomial(newx, y, laplace = 1)
#  pred <- predict(mod, newdata = newx)
#  mean(pred!=y)
#  
#  # Gaussian only
#  vars <- c('hp', dist$gaussian)
#  newx <- x[,vars]
#  mod <- fastNaiveBayes.gaussian(newx, y)
#  pred <- predict(mod, newdata = newx)
#  mean(pred!=y)

