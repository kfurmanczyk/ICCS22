library(dplyr)
library(caret)
source("datasets_prep.R")
source("prep_functions.R")
source("scoring_methods.R")

dsets<-
  ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']

for ( i in dsets ){
  
  my_dataset <- get(i)
  
  my_dataset <- set_greater_class(my_dataset,1)
  
  assign(i,my_dataset)
  
  rm(my_dataset) 
}


results <- data.frame(
  
  dataset = character()
  , method = character()
  , c = numeric()
  , experiment_num = integer()
  , score = numeric()
  , Y_real = numeric()
  
)

append_results <- function(results_frame, dataset, method, c, experiment_num, score, Y_real){
  
  my_result_frame <- results_frame
  
  current_result <- data.frame(
    dataset = i
    , method = method
    , c = c
    , experiment_num = num
    , score = score
    , Y_real = y_test
  )
  
  my_result_frame <- rbind.data.frame(
    my_result_frame
    , current_result
  )
  
  my_result_frame
}


c_vector <- c(0.1,0.3,0.5,0.7,0.9)
c_vector <- sort(c_vector, decreasing = T)


for (i in dsets) {
  results <- data.frame(
    
    dataset = character()
    , method = character()
    , c = numeric()
    , experiment_num = integer()
    , score = numeric()
    , Y_real = numeric()
    
  )
  for (c in c_vector) {
    current_frame <- get(i)
    current_frame$Y <- as.numeric(current_frame$Y)
    current_frame$S <- current_frame$Y * rbinom(nrow(current_frame),1,c)
    current_frame <- as.data.frame(sapply(current_frame, as.numeric))
    for (num in 1:10) {
      
      #current_frame <- current_frame[,apply(current_frame, 2, function(x) sqrt(var(x))/mean(x)) != 0]
      sel_current_frame <- current_frame[,apply(current_frame, 2, function(x) sort(table(x), decreasing = T)[1]/length(x)) < 0.99]
      sel_current_frame$S <- NULL
      sel_current_frame$Y <- NULL
      
      pp <- caret::preProcess(sel_current_frame, method = c("range"))
      sel_current_frame <- predict(pp,sel_current_frame)
      
      sel_current_frame$S <- current_frame$S
      sel_current_frame$Y <- current_frame$Y
      current_frame <- sel_current_frame
      
      train <- sample_frac(current_frame,0.80)
      print(ncol(train))
      train <- train[,apply(train, 2, function(x){length(unique(x))}) != 1]
      print(ncol(train))
      current_frame <- current_frame[,colnames(train)]
      test <- dplyr::setdiff(current_frame,train)
      
      x_train <- as.matrix(train[,!colnames(current_frame) %in% c('Y','S')])
      y_train <- train$Y
      s_train <- train$S 
      
      x_test <- as.matrix(test[,!colnames(current_frame) %in% c('Y','S')])
      y_test <- test$Y
      
      ### Oracle -----
      
      print(paste0("Calculating dataset = ", i, " for c = ", c, " experiment no ", num))
      
      current_result <- data.frame(
        dataset = i
        , method = 'Oracle'
        , c = c
        , experiment_num = num
        , score = logistic(x_train = x_train, y_train = y_train, x_test = x_test)
        , Y_real = y_test
      )
      
      results <- rbind.data.frame(
        results
        , current_result
      )

      ### Naive -----
      
      current_result <- data.frame(
        dataset = i
        , method = 'Naive'
        , c = c
        , experiment_num = num
        , score = logistic(x_train = x_train, y_train = s_train, x_test = x_test)
        , Y_real = y_test
      )
      
      results <- rbind.data.frame(
        results
        , current_result
      )

      ### Joint BFGS-----
     
      current_result <- data.frame(
        dataset = i
        , method = 'Joint BFGS'
        , c = c
        , experiment_num = num
        , score = logistic_joint(x_train = x_train, y_train = s_train, x_test = x_test, optim_method = "BFGS") 
        , Y_real = y_test
      )
      
      results <- rbind.data.frame(
        results
        , current_result
      )
      ### Weighted BFGS ----
      current_result <- data.frame(
        dataset = i
        , method = 'Weighted BFGS'
        , c = c
        , experiment_num = num
        , score = logistic_w(x_train = x_train, y_train = s_train, x_test = x_test)
        , Y_real = y_test
      )
      
      results <- rbind.data.frame(
        results
        , current_result
      )
      
      ### Joint MM-----
      score = logistic_cdmm(x_train = x_train, y_train = s_train, x_test = x_test)
      score = round(score,4)
      
      results <- append_results(results_frame = results, dataset = i, method = "Joint MM", c = c, experiment_num = num, score = score, Y_real = y_test)
      
      ### LassoMM-----
      score = lassoCDMM(x_train = x_train, y_train = s_train, x_test = x_test)
      score = round(score,4)
      
      results <- append_results(results_frame = results, dataset = i, method = "LassoMM", c = c, experiment_num = num, score = score, Y_real = y_test)

      ### LassoMM_lambda.min-----
      score = lassoCDMM(x_train = x_train, y_train = s_train, x_test = x_test, nfolds = 10, lambda = "lambda.min")
      score = round(score,4)
      
      results <- append_results(results_frame = results, dataset = i, method = "LassoMM_lambda.min", c = c, experiment_num = num, score = score, Y_real = y_test)
      
      ### LassoMM_lambda.1se-----
      score = lassoCDMM(x_train = x_train, y_train = s_train, x_test = x_test, nfolds = 10, lambda = "lambda.1se")
      score = round(score,4)
      
      results <- append_results(results_frame = results, dataset = i, method = "LassoMM_lambda.1se", c = c, experiment_num = num, score = score, Y_real = y_test)
      
      ### LassoJoint_BFGS ----
      
      current_result <- data.frame(
        dataset = i
        , method = 'LassoJoint_BFGS'
        , c = c
        , experiment_num = num
        , score = lassojoint(x_train = x_train, y_train = s_train, x_test = x_test)
        , Y_real = y_test
      )
      
      results <- rbind.data.frame(
        results
        , current_result
      )
     
      ### LassoJoint_BFGS_lambda.min ----
      
      current_result <- data.frame(
        dataset = i
        , method = 'LassoJoint_BFGS_lambda.min'
        , c = c
        , experiment_num = num
        , score = lassojoint(x_train = x_train, y_train = s_train, x_test = x_test, nfolds = 10, lambda = "lambda.min")
        , Y_real = y_test
      )
      
      results <- rbind.data.frame(
        results
        , current_result
      )
      
      ### LassoJoint_BFGS_lambda.1se ----
      
      current_result <- data.frame(
        dataset = i
        , method = 'LassoJoint_BFGS_lambda.1se'
        , c = c
        , experiment_num = num
        , score = lassojoint(x_train = x_train, y_train = s_train, x_test = x_test, nfolds = 10, lambda = "lambda.1se")
        , Y_real = y_test
      )
      
      results <- rbind.data.frame(
        results
        , current_result
      )
      
      # # AdaS_svm ----

      current_result <- data.frame(
        dataset = i
        , method = 'AdaS_svm'
        , c = c
        , experiment_num = num
        , score = adasampling_score(x_train = x_train, y_train = s_train, x_test = x_test,  classifier = 'svm')
        , Y_real = y_test
      )

      results <- rbind.data.frame(
        results
        , current_result
      )



      # # AdaS_knn ====

      current_result <- data.frame(
        dataset = i
        , method = 'AdaS_knn'
        , c = c
        , experiment_num = num
        , score = adasampling_score(x_train = x_train, y_train = s_train, x_test = x_test, classifier = 'knn')
        , Y_real = y_test
      )

      results <- rbind.data.frame(
        results
        , current_result
      )





      
      print(warnings())
      assign("last.warning", NULL, envir = baseenv())
    }
    
    
  }
}






