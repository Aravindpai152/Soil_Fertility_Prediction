setwd("C:/Users/HP/Dropbox/PC/Desktop/Soil Quality")
A=read.csv("Soil Fertility Prediction.csv",header=TRUE)
Y=A[,17]

#Preliminary analysis
library(psych)
psych::describe(A[,-17])

#To calculate the interQuartile Range
library(dplyr)
sapply(A[,-17],IQR)


# Replace missing values in CaCO3 column with the mean
median=median(A$CaCO3[A$CaCO3 != 0])
A$CaCO3[A$CaCO3==0]=median

#Identifying the Outliers Using the BoxPlot
par(mfrow=c(2,5))
na=c("pH","EC","OC","OM","N","P","K","Zn","Fe","Cu","Mn","Sand","Silt","Clay","CaCO3","CEC")
for(i in c(1,2,3,4,6,7,8,13,15,16)){
boxplot(A[,i],main=na[i])
}

#Idenifying the Position Of the Outliers
for(i in 1:16){
bb=boxplot.stats(A[,i])
outliers=bb$out
pos=which(A[,i] %in% c(outliers))
if(length(outliers)>0){
A[c(pos),i]=median(A[-c(pos),i])
}
}

AA=cbind(A$CaCO3,A$CEC,A$Clay,A$Cu,A$EC,A$Fe,A$K,A$Mn,A$N,A$OC,A$OM,A$P,A$pH,A$Sand,A$Silt,A$Zn)
colnames(AA)=c("CaCO3","CEC","Clay","Cu","EC","Fe","K","Mn","N","OC","OM","P","pH","Sand","Silt","Zn")

cor.plot(AA,main="Correlation Plot")
#Identifying the Influential Observations
n=length(AA[,1])
k=16
library(MASS)
H=AA%*%ginv(t(AA)%*%AA)%*%t(AA)
hii=diag(H)
cat("The Influential Observations are\n")
A[hii>(2*k)/n,]

#Model performance without standardisation
glm.fit=glm(Y~pH+EC+OC+OM+N+P+K+Zn+Fe+Cu+Sand+CaCO3+CEC,data=data.frame(AA),family="binomial",maxit=100)
library(pROC)
library(ROCR)
roc(Y,glm.fit$fitted.values,plot=TRUE,legacy.axes=TRUE,percent=TRUE,
xlab="False Positive Percentage",ylab="True Positive Percentage",col="blue",lwd=1,
print.auc=TRUE)

#Model performance standardisation
m=apply(AA,2,mean)
sd=apply(AA,2,sd)
Z=(AA-m)/sd
attach(data.frame(Z))
models=glm(Y~pH+EC+OC+OM+N+P+K+Zn+Fe+Cu+Sand+CaCO3+CEC,data=data.frame(Z),family="binomial")
roc(Y,models$fitted.values,plot=TRUE,legacy.axes=TRUE,percent=TRUE,
xlab="False Positive Percentage",ylab="True Positive Percentage",col="blue",lwd=1,
print.auc=TRUE)


#Model performance without influential observation
Model=glm(Y[-91]~pH+EC+OC+OM+N+P+K+Zn+Fe+Cu+Sand+CaCO3+CEC,data=data.frame(AA[-91,]),family="binomial",maxit=100)
library(pROC)
library(ROCR)
roc(Y[-91],Model$fitted.values,plot=TRUE,legacy.axes=TRUE,percent=TRUE,
xlab="False Positive Percentage",ylab="True Positive Percentage",col="blue",lwd=1,
print.auc=TRUE)


#Creating Test Train Split
set.seed(12)
library(caTools)
data=data.frame(cbind(AA,Y))
s=sample.split(data,SplitRatio=0.8)
train=subset(data,s==TRUE)
test=subset(data,s==FALSE)
Xtrain=train[,-17]
Ytrain=train[,17]
Xtest=test[,-17]
Ytest=test[,17]

#Logistic Regression
model=glm(Ytrain~pH+EC+OC+OM+N+P+K+Zn+Fe+Cu+Sand+CaCO3+CEC,data=Xtrain,family="binomial")
summary(model)
step(model)
lr=glm(Ytrain~pH+EC+N+K+Fe+Sand+CEC,data=Xtrain,family="binomial")
summary(lr)
library(pROC)
library(ROCR)
roc.info=roc(Ytrain,lr$fitted.values,plot=TRUE,legacy.axes=TRUE,percent=TRUE,
xlab="False Positive Percentage",ylab="True Positive Percentage",print.auc=TRUE)
roc.df=data.frame(tpp=roc.info$sensitivities,fpp=100-roc.info$specificities,
thresholds=roc.info$thresholds)
roc.df  #Gives us the thresholds for cutoff selection
coef=lr$coef
exp(coef)

#Overall Significane of the model
null_model=glm(Ytrain~1,data=train,family ="binomial")
full_model=glm(Ytrain~EC+N+K+Fe+Sand+CaCO3+CEC,data=train,family="binomial")
library(lmtest)
lrtest(null_model,full_model)


#Test data accuracy
Ycap1=predict(lr,newdata=Xtest,type="response")
PC1=ifelse(Ycap1>0.5377,1,0)
library(caret)
confusionmatrix1=confusionMatrix(as.factor(PC1),as.factor(Ytest)) 
confusionmatrix1

selected_var=c("pH","EC","N","K","Sand","CaCO3","CEC")
Xtrain=Xtrain[,selected_var]
Xtest=Xtest[,selected_var]

#Decision Tree
library(rpart)
library(rpart.plot)
Decision_tree=rpart(Ytrain~.,data=Xtrain,method="class")
summary(Decision_tree)
printcp(Decision_tree)
rpart.plot(Decision_tree,type=1,extra=1)
pred=predict(Decision_tree,Xtest,type="class")
confusionmatrix2=confusionMatrix(as.factor(Ytest),pred)
confusionmatrix2

#KNN Classifier
library(class)
KN=knn(Xtrain,Xtest,Ytrain,k=3)
confusionmatrix3=confusionMatrix(as.factor(Ytest),as.factor(KN))
confusionmatrix3

#SVM Classifier
library(e1071)
SV=svm(as.factor(Ytrain)~.,data=Xtrain)
summary(SV)
pq=predict(SV,Xtest,type="class")
confusionmatrix4=confusionMatrix(as.factor(pq),as.factor(Ytest))
confusionmatrix4

#Naive Baysian Classifier
library(e1071)
NB=naiveBayes(Xtrain,Ytrain)
prob=predict(NB,Xtest)
confusionmatrix5=confusionMatrix(as.factor(Ytest),as.factor(prob))
confusionmatrix5

#Random forest Classifier
library(randomForest)
rf=randomForest(as.factor(Ytrain)~.,data=Xtrain)
py=predict(rf,Xtest)
confusionmatrix6=confusionMatrix(as.factor(py),as.factor(Ytest))
confusionmatrix6

confusionmatrix1$overall["Accuracy"] #Logistic
confusionmatrix2$overall["Accuracy"] #Decison Tree
confusionmatrix3$overall["Accuracy"] #KNN
confusionmatrix4$overall["Accuracy"] #SVM
confusionmatrix5$overall["Accuracy"] #Naive Baysian
confusionmatrix6$overall["Accuracy"] #Random Forest

#Hotellings T square Statistic
FS=A[A$Output==0,]
NFS=A[A$Output==1,]
n1=50
n2=50
G1=t(FS[,-17])
G2=t(NFS[,-17])
X1bar=rowMeans(G1)
X2bar=rowMeans(G2)
A1=cov(t(G1))*(n1-1)
A2=cov(t(G2))*(n2-1)
Su=(A1+A2)/(n1+n2-2)
Tsq=(n1*n2)/(n1+n2)*(t(X1bar-X2bar)%*%solve(Su)%*%(X1bar-X2bar))
p=length(G1[,1])
Fcal=((n1+n2-p-1)*Tsq)/(p*(n1+n2-2))
cat("Calculated F value",Fcal)
Fcri=qf(1-0.05,p,n1+n2-p-1)
cat("Critical value",Fcri)

#t-test to test difference between the variable
variables=c("pH","EC","OC","OM","N","P","K","Zn","Fe","Cu","Mn","Sand","Silt","Clay","CaCO3","CEC")
result_df=data.frame(Variable = character(),
                        P_Value = numeric(),
                        Test_Statistic = numeric(),
                        Conf_Int = character(),
                        stringsAsFactors = FALSE)
for (variable in variables) {
FS=A[A$Output==0,variable]
NFS=A[A$Output==1,variable]
result=t.test(FS, NFS)
result_df=rbind(result_df,data.frame(Variable = variable,
                                           P_Value = result$p.value,
                                           Test_Statistic = result$statistic,
                                           stringsAsFactors = FALSE))
}
print(result_df)