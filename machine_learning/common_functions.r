library(robustbase) #colmedians
library(stringr) #str_match
library(randomForest)
library(R.matlab) #readMat
library(caret)
library(ggcorrplot)

get_best_coco <- function(folder, best){
  all_best <- c()
  sample_size = 250
  files <- list.files(folder, full.names = FALSE, pattern = toString(sample_size))
  pattern <- ("(\\d+)_(\\d+)_.*")
  for (file in files){
    regex_match <- str_match(file, pattern)
    func <- regex_match[1,2]
    instance <- regex_match[1,3]
    best_index <- ((as.numeric(func) - 1) * 15) + as.numeric(instance)
    file_best <- best[best_index]
    all_best <- c(all_best, file_best)
  }
  return(all_best)
}


transform_best_coco <- function(best){
  #best is a 360 by 10 matrix, we want to convert it to a list of c(), featuring all non-0 members of best per row
  best_list <- list()
  for (row_i in 1:nrow(best)){
    best_row <- best[row_i,]
    best_row <- best_row[best_row > 0] #take away zero elements
    best_list[[row_i]] <- best_row
  }
  return(best_list)
}

linear_matrix_factorization_embeddings<-function(X_temp,problems_names,number_of_singular_values){
  
  data_temp<-X_temp
  decomposition<-svd(data_temp)
  
  
  Sigma<-diag(decomposition$d[1:number_of_singular_values])
  U<-decomposition$u[,1:number_of_singular_values]
  V_t<-t(decomposition$v)[1:number_of_singular_values,]
  inv_Sigma<-diag(1/decomposition$d[1:number_of_singular_values])
  problem_embeddings<-list()
  
  for(i in 1:nrow(data_temp)){
    problem_embeddings[[i]]<-data_temp[i,]%*%t(V_t)%*%inv_Sigma
  }
  problems_embeded<-matrix(unlist(problem_embeddings)[!is.na(unlist(problem_embeddings))], ncol = length(problem_embeddings[[i]]), byrow = T)
  
  rownames(problems_embeded)<-problems_names
  return(list(problems_embeded=problems_embeded,U=U,Sigma=Sigma,V_t=V_t))
  
}

normalize_data <- function(df, means = NA, stds = NA, min = NA, max = NA, type="normalize") {
  classes <- df$class
  df$class <- NULL
  
  funcs <- df$func
  df$func <- NULL

  df2 <- as.data.frame(df)
  
  columns <- colnames(df2)
  if(is.na(means)){
    means <- colMeans(df2)
    saveRDS(means, "means")
  }
  
  
  if(is.na(stds)){
    stds <- apply(df2, 2, sd)
    saveRDS(stds, "stds")
  }
  
  if(is.na(min)){
    min <- apply(df2, 2, min)
    saveRDS(min, "max")
  }
  
  if(is.na(max)){
    max <- apply(df2, 2, max)
    saveRDS(max, "max")
  }
  
  if(type == "normalize" || type=="both"){
    df2 <- sweep(df2, 2, means, FUN = '-')
    df2 <- sweep(df2, 2, stds, FUN = '/')
  }
  
  if (type =="minmax" || type == "both"){
    df2 <- sweep(df2, 2, min, FUN = '-')
    df2 <- sweep(df2, 2, (max-min), FUN = '/')
  }
  
  ret <- c()
  ret$normalized <- df2
  ret$mean <- means
  ret$std <- stds
  df2$class <- classes
  df2$func <- funcs
  
  #remove constant columns that have been set to NaN during normalization
  df2 <- df2[ , colSums(is.na(as.matrix(df2))) == 0]
  df2 <- df2[ , colSums(is.nan(as.matrix(df2))) == 0]
  return(df2)
}

read_features_performance <- function(folder = "C:\\R_Code\\samples_10D\\features", sample_size = 250){
  files <- list.files(folder, full.names = TRUE, pattern = paste(sample_size, sep=""))
  allData <- NULL
  allBest <- c()
  i<-1
  for (file in files){
    #get class based on file name
    pattern <- "samples_(\\D*)_"
    res <- str_match(file, pattern)
    best_algorithm <- res[1,2]
    allBest <- c(allBest, best_algorithm)
    
    pattern_function <- ".+_(\\d*)_samples_"
    res <- str_match(file, pattern_function)
    file_function <- as.numeric(res[1,2])
    i <- i + 1
    
    data <- readRDS(file)
    columns <- readRDS("./landscape_features/processed/ML_cols.RDS")
    if (is.null(allData)){
      allData <- matrix(nrow = 0, ncol = length(columns))
    }
    data$func <- file_function
    columns <- readRDS("landscape_features/processed/ML_cols.RDS")
    data <- unlist(data)
    data <- data[columns]
    allData <- rbind(allData,unlist(data))
  }
  result <- c()
  result$data = allData
  result$best = allBest
  return(result)
}


read_features_performance_coco <- function(folder = "C:\\R_Code\\samples_10D\\features", sample_size = 250){
   files <- list.files(folder, full.names = TRUE, pattern = toString(sample_size))
  allData <- NULL
  allBest <- c()
  for (file in files){
    #get class based on file name
    pattern <- "samples_(\\D*)_"
    res <- str_match(file, pattern)
    best_algorithm <- res[1,2]
    allBest <- c(allBest, best_algorithm)
    
    
    pattern_function <- ".+/(\\d*)_.*_250.mat.RDS"
    res <- str_match(file, pattern_function)
    file_function <- as.numeric(res[1,2])
    
    
    pattern_instance <- ".+_(\\d*)_.*_250.mat.RDS"
    res <- str_match(file, pattern_instance)
    file_instance <- as.numeric(res[1,2])
    
    trial_function <- ".+_.+_(\\d*)_250.mat.RDS"
    res <- str_match(file, trial_function)
    trial <- as.numeric(res[1,2])
    
    if (trial > 10){
      next()
    }
    
    
    columns <- readRDS("landscape_features/processed/ML_cols.RDS")
    data <- readRDS(file)
    if (is.null(allData)){
      allData <- matrix(nrow = 0, ncol = length(columns))
    }
    data$func <- as.numeric(file_function) * 100 + as.numeric(file_instance)
    
    data <- unlist(data)
    data <- data[columns]
    
    allData <- rbind(allData,unlist(data))
  }
  result <- c()
  result$data = allData
  result$best = allBest
  return(result)
}

#Create k folds, so that all runs of a function are always in the same fold (create folds based on functions)
createFoldsCustom <- function(data, k){
  folds = 10
  funcs <- unique(data$func)
  times <- rep(length(funcs)/folds, folds)
  indices <- sample(rep(1:folds, times = times))
  splits <- split(funcs, indices)
  
  all_indexes <- c()
  i <- 1
  for (split in splits){
    indexes <- which(data$func %in% split)
    all_indexes[[i]] <- indexes
    i <- i + 1
  }
  return(all_indexes)
  
}


createFoldsCustomCoco <- function(data, k){
  #print(data)
  funcs <- unique(data$instance)
  funcs <- funcs[order(funcs)] #order is random otherwise
  
  
  all_indexes <- c()
  i <- 1
  for (func in funcs){
    indexes <- which(!(data$instance %in% func))
    all_indexes[[i]] <- indexes
    i <- i + 1
  }
  return(all_indexes)
  
}


train_ml <- function(ml_func, train_data){
  model <- c()
  model$all = ml_func(train_data$all)
  model$Cols = ml_func(train_data$Cols)
  
  return(model)
}
test_ml <- function(model, test_data, validation_data, model_name, train_data, predictor = function(x,y){predict(x,y, predcontrib = TRUE, approxcontrib = F)}, multilabel=F, classes = NA){
  accuracy <- list()
  precisions <- list()
  recalls <- list()
  f1s <- list()
  
  model_names <- c("all", "Cols")
  
  for (m in model_names){
    train <- train_data[[m]]
    validation <- validation_data
    predictions <- predictor(model[[m]], test_data)
    predictions_validate <- predictor(model[[m]], validation)
    t<-table(predictions, test_data$class)
    
    
    if(multilabel){
      correct <- 0
      for (pred_i in 1:length(predictions)){
        pred <- predictions[[pred_i]]
        class <- classes[[pred_i]]
        if (pred %in% class){
          correct <- correct + 1
        }
      }
    } else {
      correct <- sum(diag(t)) # the correct predictions are on the diagonals
    }
    
    accuracy[[m]] <- correct/sum(t)
    
    total_cols <- colSums(t)
    total_rows <- rowSums(t)
    
    precisions[[m]] <-  correct / total_rows
    recalls[[m]] <- correct / total_cols
    fp <- total_rows - correct
    fn <- total_cols - correct
    f1s[[m]] <- (2*correct) / (2*correct + fp + fn) #isto kot zgoraj. Isto kot prva enaÄŤba, samo da je 0 namesto NaN ko sta precision in recall oba 0 (drugaÄŤe da f1 veÄŤje kot precision in recall ko damo macro average)
    
  }
  
  
  
  
  accuracy <- unlist(accuracy)
  names(accuracy) <-  paste(model_name, names(accuracy), sep = "_")
  
  #table is: rows = prediction, columns = true
  
  
  results <- c()
  results$accuracy <- accuracy
  results$precision <- precisions
  results$recall <- recalls
  results$f1 <- f1s
  
  return(results)
  
}

test_ml_old <- function(model, test_data, model_name, predictor = function(x,y){predict(x,y)}){
  accuracy <- c()
  
  predictions <- predictor(model$all, test_data)
  predictions2 <- predictor(model$"2", test_data)
  predictions5 <- predictor(model$"5", test_data)
  predictions10 <- predictor(model$"10", test_data)
  predictions15 <- predictor(model$"15", test_data)
  predictions20 <- predictor(model$"20", test_data)
  predictionsCols <- predictor(model$Cols, test_data)
  
  
  t2<-table(predictions2, test_data$class)
  accuracy$"2" <- sum(diag(t2))/sum(t2)
  
  t5<-table(predictions5, test_data$class)
  accuracy$"5" <- sum(diag(t5))/sum(t5)
  
  t10<-table(predictions10, test_data$class)
  accuracy$"10" <- sum(diag(t10))/sum(t10)
  
  t15<-table(predictions15, test_data$class)
  accuracy$"15" <- sum(diag(t15))/sum(t15)
  
  t20<-table(predictions20, test_data$class)
  accuracy$"20" <- sum(diag(t20))/sum(t20)
  
  tall<-table(predictions, test_data$class)
  accuracy$"all" <- sum(diag(tall))/sum(tall)
  
  tCols<-table(predictionsCols, test_data$class)
  accuracy$"Cols" <- sum(diag(tCols))/sum(tCols)
  
  accuracy <- unlist(accuracy)
  names(accuracy) <-  paste(model_name, names(accuracy), sep = "_")
  
  return(accuracy)
  
}

get_func_coco <- function(folder, best){
  all_best <- c()
  sample_size = 250
  files <- list.files(folder, full.names = FALSE, pattern = toString(sample_size))
  pattern <- ("(\\d+)_(\\d+)_.*")
  for (file in files){
    trial_function <- ".+_.+_(\\d*)_250.mat.RDS"
    res <- str_match(file, trial_function)
    trial <- as.numeric(res[1,2])
    if (trial > 10){
      next()
    }
    regex_match <- str_match(file, pattern)
    func <- regex_match[1,2]
    instance <- regex_match[1,3]
    #func_coco <- as.numeric(func)*100 + as.numeric(instance)
    func_coco <- as.numeric(func)*100
    all_best <- c(all_best, func_coco)
  }
  return(all_best)
}



analyze_results <- function(results){
  accuracy <- matrix(nrow = 0, ncol = length(results[[1]][[1]]$accuracy))
  precision <- matrix(nrow = 0, ncol = length(results[[1]][[1]]$accuracy))
  recall <- matrix(nrow = 0, ncol = length(results[[1]][[1]]$accuracy))
  f1 <- matrix(nrow = 0, ncol = length(results[[1]][[1]]$accuracy))
  for (row in 1:length(results)){
    results_row = results[[row]]
    for (algorithm in length(results_row)){
      algorithm_row <- results_row[[algorithm]]
      #mean for macro scores
      accuracy <- rbind(accuracy, algorithm_row$accuracy)
      precision <- rbind(precision, lapply(algorithm_row$precision, mean))
      recall <- rbind(recall, lapply(algorithm_row$recall, mean))
      f1 <- rbind(f1, lapply(algorithm_row$f1, mean))
      
      
    }
  }
  ret <- c()
  ret$accuracy <- accuracy
  ret$precision <- precision
  ret$recall <- recall
  ret$f1 <- f1
  return(ret)
}

make_balanced <- function(data){
  data_ret <- c()
  data_ret$best <- c()
  data_ret$data <- matrix(nrow = 0, ncol = ncol(data$data))
  algorithms <- levels(as.factor(data$best))
  for (algorithm in algorithms){
    print(length(which(data$best == algorithm)))
    indices <- which(data$best == algorithm)[1:50]
    data_ret$best <- c(data_ret$best, data$best[indices])
    data_ret$data <- rbind(data_ret$data, data$data[indices,])
    
  }
  
  data_ret$data <- as.data.frame(data_ret$data)
  return(data_ret)
}

make_median <- function(data){
  funcs <- as.data.frame(data$data)
  funcs <- funcs$func
  funcs <- unique(funcs)
  data_ret <- c()
  data_ret$best <- c()
  data_ret$data <- matrix(nrow = 0, ncol = ncol(data$data))
  df <- as.data.frame(data$data)
  for (func in funcs){
    indices <- which(df$func == func)
    
    if (length(indices) < 10){
      next()
    }
    
    
    
    new_best <- data$best[indices]
    
    if(length(unique(new_best)) > 1){
      print("different best")
    }
    
    new_data <- data$data[indices,]
    new_data <- colMedians(new_data)
    data_ret$best <- c(data_ret$best, new_best[[1]])
    data_ret$data <- rbind(data_ret$data, new_data)
    
  }
  
  return(data_ret)
}



make_median_coco <- function(data){
  funcs <- as.data.frame(data$data)
  funcs <- funcs$func
  funcs <- unique(funcs)
  data_ret <- c()
  data_ret$best <- c()
  data_ret$data <- matrix(nrow = 0, ncol = ncol(data$data))
  df <- as.data.frame(data$data)
  for (func in funcs){
    #print(func)
    
    indices <- which(df$func == func)
    
    if (length(indices) < 10){
      next()
    }
    
    
    
    new_best <- data$best[indices]
    
    if(length(unique(new_best)) > 1){
      print("different best")
    }
    
    new_data <- data$data[indices,]
    new_data <- colMedians(new_data)
    #order(train_class)[!duplicated(sort(train_class))]
    data_ret$best <- c(data_ret$best, new_best[[1]])
    data_ret$data <- rbind(data_ret$data, new_data)
    
  }
  #remove the instance information from the func column, to group folds by function
  df <- as.data.frame(data_ret$data)
  df$func <- floor(df$func/1000)
  data_ret$data <- as.matrix(df)
  return(data_ret)
}


make_median_artificial <- function(data){
  funcs <- as.data.frame(data$data)
  funcs <- funcs$func
  funcs <- unique(funcs)
  data_ret <- c()
  data_ret$best <- c()
  data_ret$data <- matrix(nrow = 0, ncol = ncol(data$data))
  df <- as.data.frame(data$data)
  for (func in funcs){
    #print(func)
    
    indices <- which(df$func == func)
    
    if (length(indices) < 10){
      next()
    }
    
    
    
    new_best <- data$best[indices]
    
    if(length(unique(new_best)) > 1){
      print("different best")
    }
    
    new_data <- data$data[indices,]
    new_data <- colMedians(new_data)
    #order(train_class)[!duplicated(sort(train_class))]
    data_ret$best <- c(data_ret$best, new_best[[1]])
    data_ret$data <- rbind(data_ret$data, new_data)
    
  }
  
  return(data_ret)
}


calculate_accuracy_values<-function(data){
  precisions <- list()
  recalls <- list()
  f1s <- list()
  
  for (type in c("all", "Cols")){
    precision <- c()
    recall <-c() 
    f1 <- c()
    for (fold in c(1:10)){
      p_type <- data[[fold]]$precision[[type]]
      r_type <- data[[fold]]$recall[[type]]
      f1_type <- data[[fold]]$f1[[type]]
      
      precision <- c(precision, mean(p_type, na.rm=TRUE))
      recall <-c(recall, mean(r_type, na.rm=TRUE)) 
      f1 <- c(f1, mean(f1_type, na.rm=TRUE))
    }
    precisions[[type]] <- mean(precision, na.rm = TRUE)  
    recalls[[type]] <- mean(recall, na.rm = TRUE)   
    f1s[[type]] <- mean(f1, na.rm = TRUE) 
  }
  ret <- c()
  ret$precision <- precisions
  ret$recall <- recalls
  ret$f1 <- f1s
  
  return(ret)
  
}


#The invariant landscape features
columns <- c("cm_angle.angle.mean", 
             "ela_distr.skewness", 
             "ela_distr.kurtosis", 
             "ela_distr.number_of_peaks",
             "ela_meta.lin_simple.adj_r2",
             "ela_meta.lin_simple.intercept",
             "ela_meta.lin_simple.coef.min",
             "ela_meta.quad_w_interact.adj_r2",
             "ela_meta.quad_simple.adj_r2",
             "ela_meta.lin_w_interact.adj_r2",
             "disp.ratio_mean_02",
             "disp.ratio_median_25",
             "nbc.nb_fitness.cor",
             "pca.expl_var_PC1.cov_init",
             "pca.expl_var.cov_init",
             "pca.expl_var.cor_init")
