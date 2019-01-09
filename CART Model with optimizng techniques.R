library(rpart)
library(caret)
library(e1071)
data<-read.csv("WA_Fn-UseC_-Telco-Customer-Churn2.csv")

set.seed(11)

#samp<-sample(nrow(data),floor(0.8*(nrow(data))),replace=FALSE)

ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))

set.seed(12)

train_data <- data[ind==1,]

set.seed(13)

test_data<- data[ind==2,]


#train_data=data[samp,]

#test_data=data[-samp,]

fittree=rpart(Churn~.,method="class",data=train_data,control=rpart.control(cp=1e-5,minsplit=2,minbucket=1))

#Accuracy of original tree and pruned tree

pred1<-predict(fittree,newdata=test_data,type="class")

Accuracy1=sum(test_data$Churn==pred1)/length(pred1)

Prunedtree<-prune.rpart(fittree,cp= fittree$cptable[which.min(fittree$cptable[,"xerror"]),"CP"])

pred2<-predict(Prunedtree,newdata=test_data,type="class")

Accuracy2=sum(test_data$Churn==pred2)/length(pred2) 

#-----This is where the code ends,below are methods of verifying the results obtained above-------------------#

#Confusion Matrix for computing accuracy-cross-verifying the one obtained above

confusionMatrix(pred1,test_data$Churn)

confusionMatrix(pred2,test_data$Churn)

#Varible Importance-using first 6 variables for prediction
#VarImp computes the importance of variables in constructing the tree
#Ordering this as below would give the most important variables for tree construction which 
#then can be used to reduce the size of the tree-instead of by direct pruning

importanceorder=order(varImp(fittree,surrogates=FALSE,competes = TRUE),decreasing=TRUE)

names=rownames(varImp(fittree,surrogates=FALSE,competes = TRUE))[importanceorder]

PrunedTree2=rpart(Churn~TotalCharges+MonthlyCharges+tenure+OnlineSecurity+TechSupport+PaymentMethod,data=train_data)

pred3<-predict(PrunedTree2,newdata=test_data,type="class")


Accuracy3=sum(test_data$Churn==pred3)/(length(pred3))


#k fold Cross-Validation used for pruning and measurement of accuracy-using tree package

library(tree)

set.seed(31)

my.tree=tree(Churn~.,data=train_data)

set.seed(32)

my.tree.cv=cv.tree(my.tree) #k-fold cv used where number of cross validations is set to 10 by default

opt.trees = which(my.tree.cv$dev == min(my.tree.cv$dev))

PruneTree3=cv.tree(my.tree,best=min(my.tree.cv$size[opt.trees]))

pred4=predict(PruneTree3,newdata=test_data,type="class")

Accuracy4=sum(test_data$Churn==pred4)/(length(pred4)) #this is the accuracy of pruned tree

#Plot tree
par(xpd = TRUE)

plot(fittree, compress = TRUE)

text(fittree, use.n = TRUE)
#plot(Prunedtree, uniform=TRUE, 
     #main="Classification Tree for Churn prediction")
#text(fittree, use.n=TRUE, all=TRUE, cex=.8)
