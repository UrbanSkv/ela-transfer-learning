folder <- "E:\\ML_Samples_new_2\\features"
all_features <- read_features_performance(folder = folder, sample_size = 250)
all_features <- make_median_artificial(all_features)
all_features <- make_balanced(all_features)


best_data <- table(as.factor(all_features$best))
#folder <- "E:\\gecco_ml_samples_new\\features"
folder <- "E:\\gecco_ml_samples\\features"
coco_features <- read_features_performance_coco(folder = folder, sample_size = 250)
coco_features <- make_median_coco(coco_features)
coco_features <- as.data.frame(coco_features$data)

cnames <- colnames(coco_features)
cnames <- str_remove(cnames, "data.")
colnames(coco_features)<-cnames

best_coco_multiple <- readMat("E:\\matlab_results\\10\\all_best5.mat")$allBest
best_coco <- best_coco_multiple[,1]
best_coco_multiple <- transform_best_coco(best_coco_multiple)


algs <- c("ABC","ACO","CMAES","CSO","DE","FEP","GA","PSO","SA","Rand")
best_coco <- algs[best_coco]

combined_cols <- intersect(colnames(all_features$data), colnames(coco_features))

all_features$data <- all_features$data[,combined_cols]
coco_features <- coco_features[,combined_cols]
all_features$data <- rbind(all_features$data, coco_features)
all_features$best <- c(all_features$best, best_coco)

total = nrow(all_features$data)



X_temp <- all_features$data[1:total,]
best <- all_features$best[1:total]


X_temp <- as.data.frame(X_temp)
X_temp$basic.objective_min <- NULL
X_temp$basic.objective_max <- NULL
X_temp$basic.upper_max <- NULL
X_temp$basic.upper_min <- NULL
X_temp$basic.lower_min <- NULL
X_temp$basic.lower_max<- NULL
X_temp$basic.blocks_min <- NULL
X_temp$basic.blocks_max <- NULL
X_temp$basic.cells_total <- NULL
X_temp$basic.cells_filled <- NULL
X_temp$basic.minimize_fun <- NULL
X_temp$basic.costs_fun_evals <- NULL
X_temp$basic.dim <- NULL
X_temp$basic.observations <- NULL


X_temp$basic.lower_max <- NULL
X_temp$basic.lower_min <- NULL
X_temp$basic.costs_runtime <- NULL
X_temp$pca.costs_runtime <- NULL
X_temp$pca.costs_fun_evals <- NULL
X_temp$nbc.costs_fun_evals <- NULL
X_temp$nbc.costs_runtime<- NULL
X_temp$limo.costs_runtime<- NULL
X_temp$limo.costs_fun_evals<- NULL
X_temp$disp.costs_fun_evals<- NULL
X_temp$disp.costs_runtime<- NULL
X_temp$ic.costs_fun_evals<- NULL
X_temp$ic.costs_runtime<- NULL
X_temp$ela_meta.costs_fun_evals<- NULL
X_temp$ela_level.costs_fun_evals<- NULL
X_temp$ela_meta.costs_runtime<- NULL
X_temp$ela_level.costs_runtime<- NULL
X_temp$cm_grad.costs_fun_evals <- NULL
X_temp$cm_grad.costs_runtime<- NULL
X_temp$cm_angle.costs_runtime <- NULL
X_temp$cm_angle.costs_fun_evals<- NULL
X_temp$cm_grad.costs_fun_evals <- NULL


X_temp$ic.eps.ratio <- NULL #contians infinite values

combined_cols <- colnames(X_temp)
coco_features <- coco_features[,combined_cols]



X_temp <- sapply(X_temp, as.numeric)
X_temp <- as.data.frame(X_temp)

best <- as.numeric(as.factor(best))
classes <- best
classes <- as.factor(classes)
X_temp$class <- classes
X_temp <- X_temp[ , colSums(is.na(X_temp)) == 0]
X_temp <- X_temp[ , colSums(is.nan(as.matrix(X_temp))) == 0]
X_temp <- X_temp[ , colSums(is.infinite(as.matrix(X_temp))) == 0]

X_temp$func <- NULL

train_rows <- 1:500
test_rows <- 501:860
train <- X_temp[train_rows,]
test <- X_temp[test_rows,]
columns_2 <- intersect(colnames(train), colnames(test))
test <- test[columns_2]
train <- train[columns_2]


saveRDS(test, "landscape_features/processed/coco.RDS")
saveRDS(train, "landscape_features/processed/artificial.RDS")

