number_of_singular_values<-50

ai <- readRDS("landscape_features/processed/artificial.RDS")
coco<- readRDS("landscape_features/processed/coco.RDS")


folds_ai <- createFolds(as.factor(ai$class), k = 25)
folds_coco <- createFolds(as.factor(coco$class), k = 15)


ai<-ai[,-ncol(ai)]
coco<-coco[,-ncol(coco)]

data_temp<-data.matrix(ai)

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

problems_ai <- problems_embeded
ai_emb<-problems_embeded[folds_ai[[1]],]
rownames(ai_emb)<-paste(seq(1,nrow(ai_emb),1),"a",sep="")


data_temp<-data.matrix(coco)

problem_embeddings<-list()

for(i in 1:nrow(data_temp)){
  problem_embeddings[[i]]<-data_temp[i,]%*%t(V_t)%*%inv_Sigma
}

problems_embeded<-matrix(unlist(problem_embeddings)[!is.na(unlist(problem_embeddings))], ncol = length(problem_embeddings[[i]]), byrow = T)


coco_emb<-problems_embeded[folds_coco[[1]],]

rownames(coco_emb)<-paste(seq(1,nrow(coco_emb),1),"c",sep="")





M <- rbind(ai_emb, coco_emb)
M_all <- rbind(problems_ai, problems_embeded)
cor_x_temp<-cor(t(M), method = "p")
ggcorrplot(cor_x_temp, hc.order = FALSE, outline.col = "white",tl.cex=5.5,tl.srt=90)
