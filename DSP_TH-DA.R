#Enabling required packages for data modeling
library("tidyverse")
library("caret")
library("e1071")
library("class")
library("caTools")
library("pROC")
library("dplyr")
library('rpart')
#Setting a random seed
set.seed(256711)
#Setting working directory to where file is
setwd("F:/VIT/Sem 5/CSI3004(DSP) TH/TH-DA")
#Reading Dataset

dataset<-read.csv("ObesityDataSet.csv")
view(dataset)
summary(dataset)

#Structure of Dataset 
str(dataset)

#Data Cleaning
#Checking for NA values in Dataset
na.dataframe<-is.na(dataset)
view(na.dataframe)

#Changing the columns which are supposed to be factors to factors
#Some columns such as FCVC which are in decimals can be rounded  off to its closest
#integer to perform various classification algorithm
#Column Age,Height and Weight will be classified based on its values to be converted
#into factor
dataset$Age<-cut(dataset$Age,breaks = c(0,15,24,40,64,100),labels = c("Child",'Youth','Young Adult','Adult','Senior'))
dataset$Height<-cut(dataset$Height,breaks=c(1.40,1.6,1.78,1.98),labels = c("Short","Average","Tall"))
dataset$Weight<-cut(dataset$Weight,breaks = c(38,65,87,101,175),labels = c("39-65","66-87","88-101","102-175"))
dataset$Gender<-as.factor(dataset$Gender)
dataset$family_history_with_overweight<-as.factor(dataset$family_history_with_overweight)
dataset$FAVC<-as.factor(dataset$FAVC)
dataset$FCVC<-factor(round(dataset$FCVC))
dataset$NCP<-as.factor(round(dataset$NCP))
dataset$CAEC<-as.factor(dataset$CAEC)
dataset$SMOKE<-as.factor(dataset$SMOKE)
dataset$CH2O<-as.factor(round(dataset$CH2O))
dataset$SCC<-as.factor(dataset$SCC)
dataset$FAF<-as.factor(round(dataset$FAF))
dataset$TUE<-as.factor(round(dataset$TUE))
dataset$CALC<-as.factor(dataset$CALC)
dataset$MTRANS<-as.factor(dataset$MTRANS)
dataset$NObeyesdad<-as.factor(dataset$NObeyesdad)

#Structure of Dataset after datatype conversion
str(dataset)

#Data Visualization and Data Exploration

#Frequency Plot of Individual Instances(Univariate Analysis)
plot.1<-ggplot(dataset,aes(Gender,y=""))+geom_bar(stat = "identity",fill="white",color="steelblue")+labs(title="Gender",x="Gender",y="Frequency")+theme(plot.title = element_text(hjust = 0.5))
plot(plot.1)

plot.1<-ggplot(dataset,aes(Age,y=""))+geom_bar(stat = "identity",fill="white",color="darkcyan")+labs(title="Age",x="Age",y="Frequency")+theme(plot.title = element_text(hjust = 0.5))
plot(plot.1)

plot.1<-ggplot(dataset,aes(Weight,y=""))+geom_bar(stat = "identity",fill="white",color="brown3")+labs(title="Weight",x="Weight",y="Frequency")+theme(plot.title = element_text(hjust = 0.5))
plot(plot.1)

plot.1<-ggplot(dataset,aes(family_history_with_overweight,y=""))+geom_bar(stat = "identity",fill="white",color="chartreuse4")+labs(title="Family History With Overweight",x="family_history_with_overweight",y="Frequency")+theme(plot.title = element_text(hjust = 0.5))
plot(plot.1)

plot.1<-ggplot(dataset,aes(FAVC,y=""))+geom_bar(stat = "identity",fill="white",color="darkorchid")+labs(title="Frequent consumption of high caloric food",x="FAVC",y="Frequency")+theme(plot.title = element_text(hjust = 0.5))
plot(plot.1)

plot.1<-ggplot(dataset,aes(FCVC,y=""))+geom_bar(stat = "identity",fill="white",color="dodgerblue4")+labs(title="Frequency of consumption of vegetables",x="FCVC",y="Frequency")+theme(plot.title = element_text(hjust = 0.5))
plot(plot.1)

plot.1<-ggplot(dataset,aes(NCP,y=""))+geom_bar(stat = "identity",fill="white",color="steelblue")+labs(title="Number of main meals",x="NCP",y="Frequency")+theme(plot.title = element_text(hjust = 0.5))
plot(plot.1)

plot.1<-ggplot(dataset,aes(CAEC,y=""))+geom_bar(stat = "identity",fill="white",color="mediumpurple2")+labs(title="Consumption of food between meals",x="CAEC",y="Frequency")+theme(plot.title = element_text(hjust = 0.5))
plot(plot.1)

plot.1<-ggplot(dataset,aes(SMOKE,y=""))+geom_bar(stat = "identity",fill="white",color="limegreen")+labs(title="Smoke",x="SMOKE",y="Frequency")+theme(plot.title = element_text(hjust = 0.5))
plot(plot.1)

plot.1<-ggplot(dataset,aes(CH2O,y=""))+geom_bar(stat = "identity",fill="white",color="darkslateblue")+labs(title="Consumption of of water daily ",x="CH2O",y="Frequency")+theme(plot.title = element_text(hjust = 0.5))
plot(plot.1)

plot.1<-ggplot(dataset,aes(SCC,y=""))+geom_bar(stat = "identity",fill="white",color="forestgreen")+labs(title="Calories consumption monitoring",x="SCC",y="Frequency")+theme(plot.title = element_text(hjust = 0.5))
plot(plot.1)

plot.1<-ggplot(dataset,aes(FAF,y=""))+geom_bar(stat = "identity",fill="white",color="firebrick2")+labs(title="Physical activity frequency",x="FAF",y="Frequency")+theme(plot.title = element_text(hjust = 0.5))
plot(plot.1)

plot.1<-ggplot(dataset,aes(TUE,y=""))+geom_bar(stat = "identity",fill="white",color="orangered3")+labs(title="Time using technology devices",x="TUE",y="Frequency")+theme(plot.title = element_text(hjust = 0.5))
plot(plot.1)

plot.1<-ggplot(dataset,aes(CALC,y=""))+geom_bar(stat = "identity",fill="white",color="magenta3")+labs(title="Consumption of alcohol",x="CALC",y="Frequency")+theme(plot.title = element_text(hjust = 0.5))
plot(plot.1)

plot.1<-ggplot(dataset,aes(MTRANS,y=""))+geom_bar(stat = "identity",fill="white",color="violetred1")+labs(title="Means of Transportation",x="MTRANS",y="Frequency")+theme(plot.title = element_text(hjust = 0.5))
plot(plot.1)

#Bivariate Analysis between Independent Variables and Target Variable
plot.2<-ggplot(dataset,aes(Age,NObeyesdad,fill=NObeyesdad))+geom_bar(stat = "identity")+coord_flip()+labs(x="Age",Y="NObeyesdad")+theme_minimal()+theme(axis.text.x = element_blank())+scale_fill_brewer(palette = "Dark2")
plot(plot.2)

plot.2<-ggplot(dataset,aes(Height,NObeyesdad,fill=NObeyesdad))+geom_bar(stat = "identity")+coord_flip()+labs(x="Height",Y="NObeyesdad")+theme_minimal()+theme(axis.text.x = element_blank())+scale_fill_brewer(palette = "Dark2")
plot(plot.2)

plot.2<-ggplot(dataset,aes(Weight,NObeyesdad,fill=NObeyesdad))+geom_bar(stat = "identity")+coord_flip()+labs(x="Weight",Y="NObeyesdad")+theme_minimal()+theme(axis.text.x = element_blank())+scale_fill_brewer(palette = "Dark2")
plot(plot.2)

plot.2<-ggplot(dataset,aes(Gender,NObeyesdad,fill=NObeyesdad))+geom_bar(stat = "identity")+coord_flip()+labs(x="Gender",Y="NObeyesdad")+theme_minimal()+theme(axis.text.x = element_blank())+scale_fill_brewer(palette = "Dark2")
plot(plot.2)

plot.2<-ggplot(dataset,aes(family_history_with_overweight,NObeyesdad,fill=NObeyesdad))+geom_bar(stat = "identity")+coord_flip()+labs(x="family_history",Y="NObeyesdad")+theme(axis.text.x = element_blank())+scale_fill_brewer(palette = "Dark2")
plot(plot.2)

plot.2<-ggplot(dataset,aes(FAVC,NObeyesdad,fill=NObeyesdad))+geom_bar(stat = "identity")+coord_flip()+labs(x="FAVC",Y="NObeyesdad")+theme_minimal()+theme(axis.text.x = element_blank())+scale_fill_brewer(palette = "Dark2")
plot(plot.2)

plot.2<-ggplot(dataset,aes(FCVC,NObeyesdad,fill=NObeyesdad))+geom_bar(stat = "identity")+coord_flip()+labs(x="FCVC",Y="NObeyesdad")+theme_minimal()+theme(axis.text.x = element_blank())+scale_fill_brewer(palette = "Dark2")
plot(plot.2)

plot.2<-ggplot(dataset,aes(NCP,NObeyesdad,fill=NObeyesdad))+geom_bar(stat = "identity")+coord_flip()+labs(x="NCP",Y="NObeyesdad")+theme_minimal()+theme(axis.text.x = element_blank())+scale_fill_brewer(palette = "Dark2")
plot(plot.2)

plot.2<-ggplot(dataset,aes(CAEC,NObeyesdad,fill=NObeyesdad))+geom_bar(stat = "identity")+coord_flip()+labs(x="CAEC",Y="NObeyesdad")+theme_minimal()+theme(axis.text.x = element_blank())+scale_fill_brewer(palette = "Dark2")
plot(plot.2)

plot.2<-ggplot(dataset,aes(SMOKE,NObeyesdad,fill=NObeyesdad))+geom_bar(stat = "identity")+coord_flip()+labs(x="SMOKE",Y="NObeyesdad")+theme_minimal()+theme(axis.text.x = element_blank())+scale_fill_brewer(palette = "Dark2")
plot(plot.2)

plot.2<-ggplot(dataset,aes(CH2O,NObeyesdad,fill=NObeyesdad))+geom_bar(stat = "identity")+coord_flip()+labs(x="CH2O",Y="NObeyesdad")+theme_minimal()+theme(axis.text.x = element_blank())+scale_fill_brewer(palette = "Dark2")
plot(plot.2)

plot.2<-ggplot(dataset,aes(SCC,NObeyesdad,fill=NObeyesdad))+geom_bar(stat = "identity")+coord_flip()+labs(x="SCC",Y="NObeyesdad")+theme_minimal()+theme(axis.text.x = element_blank())+scale_fill_brewer(palette = "Dark2")
plot(plot.2)

plot.2<-ggplot(dataset,aes(FAF,NObeyesdad,fill=NObeyesdad))+geom_bar(stat = "identity")+coord_flip()+labs(x="FAF",Y="NObeyesdad")+theme_minimal()+theme(axis.text.x = element_blank())+scale_fill_brewer(palette = "Dark2")
plot(plot.2)

plot.2<-ggplot(dataset,aes(TUE,NObeyesdad,fill=NObeyesdad))+geom_bar(stat = "identity")+coord_flip()+labs(x="TUE",Y="NObeyesdad")+theme_minimal()+theme(axis.text.x = element_blank())+scale_fill_brewer(palette = "Dark2")
plot(plot.2)

plot.2<-ggplot(dataset,aes(CALC,NObeyesdad,fill=NObeyesdad))+geom_bar(stat = "identity")+coord_flip()+labs(x="CALC",Y="NObeyesdad")+theme_minimal()+theme(axis.text.x = element_blank())+scale_fill_brewer(palette = "Dark2")
plot(plot.2)

plot.2<-ggplot(dataset,aes(MTRANS,NObeyesdad,fill=NObeyesdad))+geom_bar(stat = "identity")+coord_flip()+labs(x="MTRANS",Y="NObeyesdad")+theme_minimal()+theme(axis.text.x = element_blank())+scale_fill_brewer(palette = "Dark2")
plot(plot.2)

#Classification Modeling

#Splitting Dataset
split=sample.split(dataset$NObeyesdad,SplitRatio = 0.8)
train_set<-subset(dataset,split==TRUE)
test_set<-subset(dataset,split==FALSE)

#Naive Bayes
Naive_model<-naiveBayes(NObeyesdad~.,data = train_set)

#Prediction
Naive.y_pred<-predict(object = Naive_model,newdata=test_set)

#Confusion Matrix
Naive_CM<-table(Naive.y_pred,test_set$NObeyesdad)
Naive_CM
#Evaluation Methods
#Accuracy
NB.Accuracy<-diag(Naive_CM)/sum(Naive_CM)
NB.Accuracy
#Average Accuracy
NB.AvgAccuracy<-sum(diag(Naive_CM))/sum(Naive_CM)
NB.AvgAccuracy

#Precision
NB.Precision<-diag(Naive_CM)/colSums(Naive_CM)
NB.Precision
print(mean(NB.Precision))

#Recall
NB.Recall<-diag(Naive_CM)/rowSums(Naive_CM)
NB.Recall
print(mean(NB.Recall))

#F1 Score
NB.F1<-(2 *NB.Precision * NB.Recall) / (NB.Precision + NB.Recall)
NB.F1
print(mean(NB.F1))


#Multi-level ROC Curve
NB.roc_obj<-multiclass.roc(test_set$NObeyesdad,as.numeric(Naive.y_pred))
NB.rs <- NB.roc_obj[['rocs']]
plot.roc(NB.rs[[1]],main="ROC Curve for Naive Bayes")
sapply(2:length(NB.rs),function(i) lines.roc(NB.rs[[i]],col=i))

#KNN
#Creating a numeric factor dataset for KNN as ot doesnt work on factor datatypes
knn.dataset<- mutate_all(dataset, function(x) as.numeric(x))
split=sample.split(knn.dataset$NObeyesdad,SplitRatio = 0.8)
knn.train_set<-subset(knn.dataset,split==TRUE)
knn.test_set<-subset(knn.dataset,split==FALSE)

#Prediction
KNN.y_pred<-knn(knn.train_set[-17],knn.test_set[-17],knn.train_set[,17],k=2)

#Confusion Matrix
KNN_CM<-table(factor(KNN.y_pred),knn.test_set$NObeyesdad)
KNN_CM

#Evaluation Methods
#Accuracy
KNN.Accuracy<-diag(KNN_CM)/sum(KNN_CM)
KNN.Accuracy

#Average Accuracy
KNN.AvgAccuracy<-sum(diag(KNN_CM))/sum(KNN_CM)
KNN.AvgAccuracy

#Precision
KNN.Precision<-diag(KNN_CM)/colSums(KNN_CM)
KNN.Precision
print(mean(KNN.Precision))

#Recall
KNN.Recall<-diag(KNN_CM)/rowSums(KNN_CM)
KNN.Recall
print(mean(KNN.Recall))

#F1 Score
KNN.F1<-(2 *KNN.Precision * KNN.Recall) / (KNN.Precision + KNN.Recall)
KNN.F1
print(mean(KNN.F1))

#Multi-level ROC Curve
KNN.roc_obj<-multiclass.roc(knn.test_set$NObeyesdad,as.numeric(KNN.y_pred))
KNN.rs <- KNN.roc_obj[['rocs']]
plot.roc(KNN.rs[[1]],main="ROC Curve for KNN Classifier")
sapply(2:length(KNN.rs),function(i) lines.roc(KNN.rs[[i]],col=i))

#Decision Tree
output.tree<-rpart(NObeyesdad ~ . , data = train_set,method = "class",control = rpart.control(minsplit = 20, xval = 81))

#'Fancifying' Decision Tree
library('rattle')      #Required library for it (fancyRpartPlot() fucnction)
fancyRpartPlot(output.tree,main="Fancy Decision Tree")
#Checking if we can Prune the Decision Tree
printcp(output.tree)
plotcp(output.tree)

#Prediction
tree.y_pred<-predict(object = output.tree,newdata=test_set,type="class")

#Confusion Matrix
DecTree_CM<-table(tree.y_pred,test_set$NObeyesdad)
DecTree_CM

#Evaluation Methods
#Accuracy
DecTree.Accuracy<-diag(DecTree_CM)/sum(DecTree_CM)
DecTree.Accuracy

#Average Accuracy
DecTree.AvgAccuracy<-sum(diag(DecTree_CM))/sum(DecTree_CM)
DecTree.AvgAccuracy

#Precision
DecTree.Precision<-diag(DecTree_CM)/colSums(DecTree_CM)
DecTree.Precision
print(mean(DecTree.Precision))

#Recall
DecTree.Recall<-diag(DecTree_CM)/rowSums(DecTree_CM)
DecTree.Recall
print(mean(DecTree.Recall))

#F1 Score
DecTree.F1<-(2 *DecTree.Precision * DecTree.Recall) / (DecTree.Precision + DecTree.Recall)
DecTree.F1
print(mean(DecTree.F1))

#Multi-level ROC Curve
DecTree.roc_obj<-multiclass.roc(test_set$NObeyesdad,as.numeric(tree.y_pred))
DecTree.rs <- DecTree.roc_obj[['rocs']]
plot.roc(DecTree.rs[[1]],main="ROC Curve for Decision Tree")
sapply(2:length(DecTree.rs),function(i) lines.roc(DecTree.rs[[i]],col=i))


#Comparing Metric of the 3 models
accuracy<-c(NB.AvgAccuracy,KNN.AvgAccuracy,DecTree.AvgAccuracy)
f1.score<-c(mean(NB.F1),mean(KNN.F1),mean(DecTree.F1))
accuracy
f1.score
args<-c("NB","KNN","DT")
barplot(accuracy,names.arg = args,main="Accuracy",ylab = "Metric",xlab = "Classifier",col="darkviolet")
barplot(f1.score,names.arg = args,main="F1 Score",ylab = "Metric",xlab = "Classifier",col="seagreen3")

