#EDA
data = read.csv("/home/wenkang/Downloads/divorce/divorce.csv", sep=";")
head(data)
str(data)
data[,"Class"]<-as.factor(data[,"Class"])
dim(data)
class(data)
sapply(X = data, FUN = function(x) sum(is.na(x)))
summary(data)
str(data)
data2=data[,!grepl("Class",names(data))]
library(psych)
data2_freq=round(response.frequencies(data2),4) * 100
data2_freq
library(likert)
library(dplyr)
data2_f <- data2 %>% mutate_if(is.integer, as.factor)
result <- likert(data2_f)
dev.new()
plot(result, group.order = names(data2_f))
library (pastecs)
data2_desc=t(round(stat.desc(data2[,], basic=FALSE, desc=TRUE, norm=TRUE, p=0.95),4))
data2_desc
summary(data2_desc[,"skewness"])
summary(data2_desc[,"kurtosis"])
data2_desc<-as.data.frame(data2_desc)
data2_desc[data2_desc$skewness > 0.8, ]
data2_desc[data2_desc$skewness < -0.8, ]
data2_desc[data2_desc$kurtosis > 3, ]
data2_desc[data2_desc$kurtosis < -3, ]
data_class1<-filter(data, data$Class == 1)
data_class0<-filter(data, data$Class == 0)
data_class1_freq=round(response.frequencies(data_class1[,-55]),4) * 100
data_class0_freq=round(response.frequencies(data_class0[,-55]),4) * 100
data_class1_freq<-as.data.frame(data_class1_freq)
data_class0_freq<-as.data.frame(data_class0_freq)
par(mfrow = c(3, 2)) 
loop.vector <- 1:5
for (i in loop.vector) {
  x <- data_class0_freq[,i]
  dev.new()
  barplot(x,
          main = paste("Class 0 Score", i-1),
          ylab = "percentage%",
          ylim = c(0, 100),
          names.arg=row.names(data_class0_freq))
}
par(mfrow = c(3, 2)) 
loop.vector <- 1:5
for (i in loop.vector) {
  x <- data_class1_freq[,i]
  dev.new()
  barplot(x,
          main = paste("Class 1 Score", i-1),
          ylab = "percentage%",
          ylim = c(0, 100),
          names.arg=row.names(data_class1_freq))
  
} 
data_class1_desc<-as.data.frame(t(round(stat.desc(data_class1[,-55], basic=FALSE, desc=TRUE, norm=TRUE, p=0.95),4)))
data_class0_desc<-as.data.frame(t(round(stat.desc(data_class0[,-55], basic=FALSE, desc=TRUE, norm=TRUE, p=0.95),4)))

par(mfrow = c(1, 2))
dev.new()
barplot(data_class1_desc$mean,
        main = paste("Class 1 Score mean"),
        ylab = "Score",
        ylim = c(0,4),
        names.arg=row.names(data_class1_freq))
dev.new()
barplot(data_class0_desc$mean,
        main = paste("Class 0 Score mean"),
        ylab = "Score",
        ylim = c(0,4),
        names.arg=row.names(data_class0_freq))

data2_cor= cor(data2)
data2_cor
library("Hmisc")
data2_cor2<- rcorr(as.matrix(data2))
inds <- which(data2_cor < 0.4, arr.ind = TRUE)
less_cor=data.frame(Var1 = rownames(data2_cor)[inds[, 1]], 
                    Var2 = colnames(data2_cor)[inds[, 2]], 
                    Cor = data2_cor[inds])
less_cor
less_cor_tab<-table(less_cor$Var1)
less_cor_tab
df2<-less_cor[!(less_cor$Var1=="Atr6" | less_cor$Var1=="Atr7"|less_cor$Var2=="Atr7"|less_cor$Var2=="Atr6"),]
df2
inds2 <- which(data2_cor > 0.7, arr.ind = TRUE)
more_cor=data.frame(Var1 = rownames(data2_cor)[inds2[, 1]], 
                    Var2 = colnames(data2_cor)[inds2[, 2]], 
                    Cor = data2_cor[inds2])
more_cor_tab<-table(more_cor$Var1)
more_cor_tab
data2_cor2$P
inds <- which(data2_cor2$P>0.5, arr.ind = TRUE)
more_p=data.frame(Var1 = rownames(data2_cor2$P)[inds[, 1]], 
                  Var2 = colnames(data2_cor2$P)[inds[, 2]], 
                  P = data2_cor2$P[inds])
more_p
library(corrplot)
dev.new()
corrplot(cor(data2))

set.seed(1234)
length(which(data$Class=="1")) 
length(which(data$Class=="0")) 
class_1=data[data$Class=="1",]
class_0=data[data$Class=="0",]
class1_idx=sample(nrow(class_1),size=round(0.7*nrow(class_1)))
class0_idx=sample(nrow(class_0),size=round(0.7*nrow(class_0)))
data.train=rbind(class_1[class1_idx,],class_0[class0_idx,])
data.test=rbind(class_1[-class1_idx,],class_0[-class0_idx,])
table(data.train$Class)
table(data.test$Class)

performance=function(xtab,desc=""){
  cat("-------------------------------------------","\n")
  cat(desc,"\n")
  ACR=sum(diag(xtab)/sum(xtab))
  TPR=xtab[1,1]/sum(xtab[,1])
  TNR=xtab[2,2]/sum(xtab[,2])
  PPV=xtab[1,1]/sum(xtab[1,])
  NPV=xtab[2,2]/sum(xtab[2,])
  FPR=1-TNR
  FNR=1-TPR
  RandomAccuracy=(sum(xtab[,2])*sum(xtab[2,])+sum(xtab[,1])*sum(xtab[1,]))/(sum(xtab)^2)
  Kappa=(ACR-RandomAccuracy)/(1-RandomAccuracy)
  print(xtab)
  cat("\n Accuracy:",ACR)
  cat("\n Kappa:",Kappa)
  cat("\n Sensitivity:",TPR)
  cat("\n Specificity:",TNR)
  cat("\n Positive Prediction Value:",PPV)
  cat("\n Negative Prediction Value:",NPV)
  cat("\n FPR:",FPR)
  cat("\n FNR:",FNR)
  cat("\n")
  cat("-------------------------------------------","\n")
}
#kNN
data_knn_train2=rbind(class_1[class1_idx,],class_0[class0_idx,])
data_knn_test2=rbind(class_1[-class1_idx,],class_0[-class0_idx,])
table(data_knn_train2$Class)
table(data_knn_test2$Class)
library(class)
loop=function(k){
  prediction=knn(data_knn_train2[,-55],data_knn_test2[,-55],data_knn_train2[,55],k=k)
  table=table(prediction,data_knn_test2[,55])
  performance(table,cat("Confusion Matrix and performance with K =",k))}
k=seq(1:5)
sapply(k,loop)
library(caret)
k_acccuracy<-data.frame(k = numeric(), accuracy = numeric())
for(k in 1:5){
  prediction=knn(data_knn_train2[,-55],data_knn_test2[,-55],data_knn_train2[,55],k=k)
  cm<-confusionMatrix(prediction,data_knn_test2[,55])
  overall <- cm$overall
  accuracy <- overall['Accuracy']
  k_acccuracy[k,]<-c(k,accuracy)
}
dev.new()
plot(k_acccuracy)
lines(k_acccuracy)


library(caret)
set.seed(1234)
data_kfold=data[sample(nrow(data)),]
folds=cut(seq(1,nrow(data)),breaks=10,label=FALSE)
data_kfold_acc<-data.frame(fold = numeric(), accuracy = numeric())
#10%  test 90% train
for(i in 1:10){
  testIndexes=which(folds==i,arr.ind=TRUE)
  testData=data_kfold[testIndexes,]
  trainData=data_kfold[-testIndexes,]
  model= knn
  pred=knn(trainData[,-55],testData[,-55],trainData[,55],k=i)
  
  cm<-confusionMatrix(pred,testData[,55])
  print(cm)
  overall <- cm$overall
  overall.accuracy <- overall['Accuracy']
  data_kfold_acc[i,]<-c(i,overall.accuracy)
}
mean(data_kfold_acc$accuracy)

#Logistic Regression
library(nnet)
mlr_model = multinom(Class~., data=data.train)
summary(mlr_model)

yhat = predict(mlr_model,newdata=subset(data.test,select=c(1:55),type='class'))
cfmat = table(yhat, data.test$Class)
performance(cfmat)

logreg_model = glm(formula = Class ~Atr1+Atr2+Atr3+Atr4+Atr5+Atr6+Atr7+Atr8+Atr9+Atr10+
                     Atr11+Atr12+Atr13+Atr14+Atr15+Atr16+Atr17+Atr18+Atr19+Atr20+
                     Atr21+Atr22+Atr23+Atr24+Atr25+Atr26+Atr27+Atr28+Atr29+Atr30+
                     Atr31+Atr32+Atr33+Atr34+Atr35+Atr36+Atr37+Atr38+Atr39+Atr40+
                     Atr41+Atr42+Atr43+Atr44+Atr45+Atr46+Atr47+Atr48+Atr49+Atr50+
                     Atr51+Atr52+Atr53+Atr54, family = binomial, data = data.train)
summary(logreg_model)

data.test.prob = predict(logreg_model,
                         newdata=subset(data.test,select=c(1:55)), type='response')
data.test.pred = ifelse(data.test.prob >= 0.5,"pred_1","pred_0")
cfmat = table(data.test.pred, data.test$Class)
performance(cfmat)

library(caret)
library(class)
set.seed(1234)
data_kfold=data[sample(nrow(data)),]
folds=cut(seq(1,nrow(data)),breaks=10,label=FALSE)
data_kfold_acc<-data.frame(fold = numeric(), accuracy = numeric())
#10%  test 90% train
for(i in 1:10){
  testIndexes=which(folds==i,arr.ind=TRUE)
  testData=data_kfold[testIndexes,]
  trainData=data_kfold[-testIndexes,]
  model=glm(formula = Class ~., family = binomial, data =trainData) 
  prob=predict(model,testData[,-55],type="response")
  pred=as.factor(ifelse(prob<0.5,0,1))
  cm<-confusionMatrix(pred,testData[,55])
  overall <- cm$overall
  overall.accuracy <- overall['Accuracy']
  data_kfold_acc[i,]<-c(i,overall.accuracy)
}
mean(data_kfold_acc$accuracy)



#Decision Tree
library(tree)
tree.data = tree(Class~.,data.train)
dev.new()
plot(tree.data)
text(tree.data,cex=0.8)
title("Tree")
tree.pred = predict(tree.data,data.test,type="class")
cf.mat = table(tree.pred,data.test$Class)
performance(cf.mat, "\ndata with tree::tree")
  
  
library(rpart)
dev.new()
tree.data.rpart = tree(Class~.,data.train)
plot(tree.data.rpart)
text(tree.data.rpart,cex=0.8)
title("Rpart")
tree.pred.rpart = predict(tree.data.rpart,data.test,type="class")
cf.mat = table(tree.pred.rpart,data.test$Class)
performance(cf.mat, "\ndata with tree::Rpart")
  
  
set.seed(1234)
data.kfold = data[sample(nrow(data)),]
folds=cut(seq(1,nrow(data)),breaks=10,label=FALSE)
data.kfold.acc = data.frame(fold = numeric(), accuracy = numeric())
  
  for(i in 1:10){
    testIndexes=which(folds==i,arr.ind=TRUE)
    testData=data.kfold[testIndexes,]
    trainData=data.kfold[-testIndexes,]
    tree.data.pred=tree(Class~.,data=trainData)
    tree.pred=predict(tree.data.pred,testData,type="class")
    cm = confusionMatrix(tree.pred,testData[,55])
    overall = cm$overall
    overall.accuracy = overall['Accuracy']
    data.kfold.acc[i,] = c(i,overall.accuracy)
  }
mean(data.kfold.acc$accuracy)

#Random Forest

library(randomForest)
set.seed(1234)
rf.data=randomForest(Class~.,data=data.train,importance=TRUE)
rf.data
oob.error.data <- data.frame(
  Trees=rep(1:nrow(rf.data$err.rate), times=3),
  Type=rep(c("OOB", "0", "1"), each=nrow(rf.data$err.rate)),
  Error=c(rf.data$err.rate[,"OOB"], 
          rf.data$err.rate[,"0"], 
          rf.data$err.rate[,"1"]))
library(ggplot2)
dev.new()
ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))
oob.values <- vector(length=10)
for(i in 1:10) {
  temp.model <- randomForest(Class ~ ., data=data, mtry=i, ntree=500)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values
min(oob.values)
which(oob.values == min(oob.values))
rf.data
tree.pred=predict(rf.data,data.test,type="class")
library(caret)
confusionMatrix(tree.pred,data.test$Class)
print(importance(rf.data))
dev.new()
varImpPlot(rf.data)

set.seed(1234)
data_kfold=data[sample(nrow(data)),]
folds=cut(seq(1,nrow(data)),breaks=10,label=FALSE)
data_kfold_acc<-data.frame(fold = numeric(), accuracy = numeric())

for(i in 1:10){
  testIndexes=which(folds==i,arr.ind=TRUE)
  testData=data_kfold[testIndexes,]
  trainData=data_kfold[-testIndexes,]
  pred=randomForest(Class~.,data=trainData,importance=TRUE)
  tree.pred=predict(pred,testData,type="class")
  cm<-confusionMatrix(tree.pred,testData[,55])
  overall <- cm$overall
  overall.accuracy <- overall['Accuracy']
  data_kfold_acc[i,]<-c(i,overall.accuracy)
}
mean(data_kfold_acc$accuracy)


#LDA

library(MASS)
lda.fit=lda(Class~.,data=data.train)
lda.fit
dev.new()
plot(lda.fit)
lda.pred=predict(lda.fit,data.test)
lda.pred
library(caret)
confusionMatrix(lda.pred$class,data.test$Class)

#k-fold validation
library(class)
set.seed(1234)
data_kfold=data[sample(nrow(data)),]
folds=cut(seq(1,nrow(data)),breaks=10,label=FALSE)
data_kfold_acc<-data.frame(fold = numeric(), accuracy = numeric())
  
#10%  test 90% train
  for(i in 1:10){
    testIndexes=which(folds==i,arr.ind=TRUE)
    testData=data_kfold[testIndexes,]
    trainData=data_kfold[-testIndexes,]
    model=lda(Class~.,data=trainData)
    pred=predict(model,testData)
    cm<-confusionMatrix(pred$class,testData[,55])
    overall <- cm$overall
    overall.accuracy <- overall['Accuracy']
    data_kfold_acc[i,]<-c(i,overall.accuracy)
  }
mean(data_kfold_acc$accuracy)

#PCA
library(factoextra)
library(dplyr)

data.train = data.train %>%
  mutate(
    
    # label outcome column
    Class = case_when(
      Class == 0 ~ "divorced",
      Class == 1 ~ "married"
    )
  )

data.test = data.test %>%
  mutate(
    
    # label outcome column
    Class = case_when(
      Class == 0 ~ "divorced",
      Class == 1 ~ "married"
    )
  )

predictor_variables = setdiff(colnames(data), "Class")

## create pca object using train dataset
train.pca = prcomp(data.train[, predictor_variables], scale=TRUE, center=TRUE)

## view pca summary
summary(train.pca)

train.pc = as.data.frame(train.pca$x)
train.pc$Class = data.train$Class
head(train.pc)

test.pc = predict(train.pca, newdata = data.test)
test.pc = as.data.frame(test.pc)
test.pc$class = data.test$class

dev.new()
fviz_eig(train.pca, addlabels=TRUE, ylim=c(0,80), geom = c("bar", "line"), barfill="pink", barcolor="grey", linecolor="red", ncp=10) +
  labs(title = "Variance Explained By Each Principal Component",
       x = "Principal Components", y = "% of Variance")

## first and second principal components
dev.new()
ggplot(train.pc) + 
  geom_point(aes(x=PC1, y=PC2, color=Class))

## second and third principal components
dev.new()
ggplot(train.pc) + 
  geom_point(aes(x=PC2, y=PC3, color=Class))

dev.new()
fviz_pca_biplot(train.pca, col.ind = as.factor(data.train$Class), col="black",
                palette = "jco", geom = "point", repel=TRUE, geom.var = c("point", "text"),
                legend.title="Outcome", addEllipses = TRUE)

#k-mean clustering
library(cluster)
library(factoextra)
set.seed(1234)
dev.new()
fviz_nbclust(data2[1:54], kmeans, method = "silhouette")
data2Cluster<-kmeans(data2,2,nstart=25)
str(data2Cluster)
print(data2Cluster)
data_pred<-data2
data_pred$cluster<-data2Cluster$cluster
aggr<-aggregate(data2,list(data_pred$cluster),mean)
aggr<-as.data.frame(aggr)
aggr<-t(aggr)[2:55,]
aggr
par(mfrow = c(1, 2))
dev.new()
barplot(as.numeric(aggr[,1]),
        main = paste("Predict grp 1 Score mean"),
        ylab = "Score",
        ylim = c(0,4),
        names.arg=names(aggr[,1]))
dev.new()
barplot(as.numeric(aggr[,2]),
        main = paste("Predict grp2 Score mean"),
        ylab = "Score",
        ylim = c(0,4),
        names.arg=names(aggr[,2]))
#show the mean of each Atr in Group 1 and Group 2
aggr

  dev.new()
  fviz_cluster(data2Cluster, data = data2[1:54])
  input<-data2[1:54]
  dev.new()
  sil<-silhouette(data2Cluster$cluster,dist(input))
  fviz_silhouette(sil)
  data_pred$cluster[data_pred$cluster=="2"]<-0
  data_pred$cluster<-as.factor(data_pred$cluster)
  str(data_pred)
  library(gmodels)
  CrossTable(x=data_pred$cluster,y=data$Class,prop.chisq=FALSE)
  library(caret)
  confusionMatrix(data_pred$cluster,data$Class)

#Hierachical Clustering
distance_mat = dist(data2, method = 'euclidean')
  
Hierar_cl = hclust(distance_mat, method ="complete")
Hierar_cl
dev.new()
plot(Hierar_cl)
abline(h = 110, col = "green")
  
fit = cutree(Hierar_cl, k = 2)
fit
table (fit)
rect.hclust(Hierar_cl, k = 2, border = "green")
  
dev.new()
library(cluster)
sil<-silhouette(fit,distance_mat)
fviz_silhouette(sil)
library(fpc)
hierar_stats <- cluster.stats(distance_mat,fit)
hierar_stats
  
aggrhier<-aggregate(data2,list(fit),mean)
aggrhier<-as.data.frame(aggrhier)
aggrhier=t(aggrhier)
aggrhier=aggrhier[2:55,]
par(mfrow = c(1, 2))
  dev.new()
  barplot(height=as.numeric(aggrhier[,1]),
          main = paste("Predict grp 1 Score mean"),
          ylab = "Score",
          ylim = c(0,4),
          names.arg=names(aggrhier[,1]))
  dev.new()
  barplot(height=as.numeric(aggrhier[,2]),
          main = paste("Predict grp2 Score mean"),
          ylab = "Score",
          ylim = c(0,4),
          names.arg=names(aggrhier[,2]))
  
  
fit[fit==1]=0
fit[fit==2]=1
fit<-as.factor(fit)
str(fit)
  
library(gmodels)
CrossTable(x=fit,y=data$Class,prop.chisq=FALSE)
library(caret)
confusionMatrix(fit,as.factor(data$Class))

