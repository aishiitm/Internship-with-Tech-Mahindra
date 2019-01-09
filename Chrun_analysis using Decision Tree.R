library(rpart)

mydata = read.csv(file.choose())

View(mydata)

mydata=mydata[,2:length(mydata)]

if(is.factor(mydata$Churn)==FALSE)
{
  mydata$Churn =as.factor(mydata$Churn)
  print("MY ERROR")
}

View(mydata)

set.seed(12)
samp_size = sample(3,nrow(mydata),replace = T,prob = c(0.60,0.20,0.20))

set.seed(123)
trds=mydata[samp_size==1,]

set.seed(1234)
valid = mydata[samp_size==2,]

set.seed(12345)
teds = mydata[samp_size==3,]

#attach(trds)

fit = rpart(Churn~., data=trds,method = "class",control=rpart.control(cp=1e-5,minsplit=2))

print(fit)

plot(fit)

plotcp(fit)

printcp(fit)

opt<- which.min(fit$cptable[,"xerror"])

cp = fit$cptable[opt,"CP"]

cp

pr_tree = prune.rpart(fit,cp=cp)

printcp(pr_tree)

plotcp(pr_tree)

#attach(valid)

pred = predict(fit,valid,type="class")

tab=table(valid$Churn,pred)

acc_tab = prop.table(tab)

a = ((acc_tab[1,1]+acc_tab[2,2])*100)

a

#prediction with optimization

pred_pr = predict(pr_tree,valid,type="class")

tab_pr=table(valid$Churn,pred_pr)

acc_pr = prop.table(tab_pr)

a_pr = ((acc_pr[1,1]+acc_pr[2,2])*100)

a_pr

#Predcition on teds data set

training= rbind(trds,valid)

#attach(training)

fit1 = rpart(training$Churn~., data =training,method = "class")

print(fit1)

pred_fit1 = predict(fit1,training,type="class")

tab_fit1=table(training$Churn,pred_fit1)

acc_fit1 = prop.table(tab_fit1)

a_fit1 = ((acc_fit1[1,1]+acc_fit1[2,2])*100)

a_fit1

#With Prune_tree

fit2 = rpart(training$Churn~., data =training,method = "class",control = rpart.control(minsplit = 2,cp=cp))

print(fit2)

pred_fit2 = predict(fit2,training,type="class")

tab_fit2=table(training$Churn,pred_fit2)

acc_fit2 = prop.table(tab_fit2)

a_fit2 = ((acc_fit2[1,1]+acc_fit2[2,2])*100)

a_fit2

a_tab = cbind(a_fit1,a_fit2)

a_tab

