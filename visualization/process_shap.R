library(ggplot2)

rf_art <- readRDS("visualization/shap_R_data.rds")
ALGS <- c("ABC","ACO","CMAES","CSO","DE","FEP","GA","PSO","SA","Rand")
NUM_FEATURES <- 50
FEATURE_NAMES <- colnames(rf_art$shap)[1:NUM_FEATURES]
avg_per_class <- function(data){
  ret_matrix <- matrix(ncol=50, nrow=0)
  colnames(ret_matrix) <- colnames(data$class)[1:NUM_FEATURES]
  data <- data$shap
  num_classes <- length(unique(data$'_ylevel_'))
  for (class in 1:num_classes){
    class_data <- data[data$'_ylevel_'==class,]
    attributions <- class_data$'_attribution_'
    attribution_mat <- matrix(attributions, ncol=NUM_FEATURES)
    attribution_mat <- colMeans(attribution_mat)
    ret_matrix<-rbind(ret_matrix, attribution_mat)
  }
  return(ret_matrix)
}

avg_all_from_class <- function(class_data){
  return(colMeans(class_data))
}


plot_all <- function(){
  for (class in 1:8){
    legend_colors <- c("Artificial" = "lightblue", "COCO" = "red")
    
    
    artificial <- read.csv(paste("shapley/shap_values/artificial_", class, ".csv", sep=""))
    coco <- read.csv(paste("shapley/shap_values/coco_", class, ".csv", sep=""))
    
    artificial <- colMeans(artificial)
    coco <- colMeans(coco)
    
    artificial <- as.data.frame(artificial)
    coco <- as.data.frame(coco)
    fill_temp <- c(rep("TEST", nrow(coco) - 1), "TEST2")
    
    combined <- cbind(FEATURE_NAMES, artificial, coco, fill_temp)
    
    print(ggplot(data=combined,aes(x=FEATURE_NAMES, fill = fill_temp))+
      geom_bar(aes(y=artificial),stat="identity",position ="identity",alpha=1,color='lightblue4', fill="lightblue")+
      geom_bar(aes(y=coco),stat="identity",position ="identity",alpha=0.3,color='red') +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      labs(title=paste("Algorithm = ", ALGS[class], sep=""), y="SHAP Values", x="Feature Names",color="Problem Type"))
  }
}