# R version 3.3.2 (2016-10-31)
library(robustbase) #colmedians
library(stringr) #str_match
library(randomForest)
library(R.matlab) #readMat

train <- readRDS("landscape_features/processed/artificial.RDS")
test <- readRDS("landscape_features/processed/coco.RDS")


NUMBER_OF_ALGORITHMS = 1
TRIALS_PER_ALGORITHM = 2
NUMBER_OF_FOLDS = 10
NUMBER_OF_COLUMNS = NUMBER_OF_ALGORITHMS * TRIALS_PER_ALGORITHM
results_matrix = matrix(nrow=NUMBER_OF_FOLDS, ncol = NUMBER_OF_COLUMNS)
full_results <- list()
matrix_index <- 1
all_results <- list()
for (fold in 1:10){
  print(matrix_index)
  
  columns <- intersect(colnames(train), columns)
  columns <- c(columns, "class")
  columns <- setdiff(columns, "class.1")
  
  trainCols <- train[columns]
  train_data <- c()
  train_data$all <- train
  train_data$Cols <- trainCols
  
  
  #define models here
  rf_functional = train_ml(function(x){randomForest(class ~ ., data=x, ntree=1000)}, train_data)
  rf_results = test_ml(rf_functional, test, test, "rf", train_data)
  
  resultsRow <- c(rf_results)
  results_matrix[matrix_index, ] <- resultsRow$accuracy
  full_results[[matrix_index]] <- rf_results
  matrix_index <- matrix_index + 1
  
}

final_results <- colMeans(results_matrix) #accuracy
other_results <- calculate_accuracy_values(full_results) #f1, precision, and recall
