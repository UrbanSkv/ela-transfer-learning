# R version 3.3.2 (2016-10-31)


X_temp <- readRDS("landscape_features/processed/coco.RDS")


number_of_splits <- 10
folds <- 1:nrow(X_temp)
#what we will be basing the folds on.
#here, it's instances so 1:15 repeated 24 times
X_temp$instance <- rep(1:15, times=24)
folds <- createFoldsCustomCoco(X_temp, k = 10)
X_temp$func <- NULL
X_temp$instance <- NULL

X_temp$class <- droplevels(X_temp$class) #drop classes 9 and 10 which aren't present in the COCO dataset

NUMBER_OF_ALGORITHMS = 1
TRIALS_PER_ALGORITHM = 2
NUMBER_OF_FOLDS = 15
NUMBER_OF_COLUMNS = NUMBER_OF_ALGORITHMS * TRIALS_PER_ALGORITHM
results_matrix = matrix(nrow=NUMBER_OF_FOLDS, ncol = NUMBER_OF_COLUMNS)
full_results <- list()
matrix_index <- 1
all_results <- list()
for (fold in folds){
  print(matrix_index)
  test_rows <- setdiff(1:nrow(X_temp), fold)
  train_rows <- fold
  
  train <- X_temp[train_rows,]
  
  test <- X_temp[test_rows,]
  
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
