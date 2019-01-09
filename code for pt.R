library(caret)
september_data<-read.csv("september_pt.csv",header=TRUE)

#Removing insignificant columns from the data
september_data<-september_data[,c(16,22,23,24,27,28,29,30,31,38,39,44,45)]

##Predicting actualdelaytime using four variables mentioned in the document
for(i in 1:nrow(september_data)){
    if(september_data$location_origin_variation_status[i]=="LATE"||september_data$location_origin_variation_status[i]=="ON TIME"||september_data$location_origin_variation_status[i]=="OFF ROUTE"){
      september_data$location_origin_timetable_variation[i]=september_data$location_origin_timetable_variation[i]
    }else if(september_data$location_origin_variation_status[i]=="EARLY"){
      september_data$location_origin_timetable_variation[i]=-september_data$location_origin_timetable_variation[i]
    }else if(september_data$location_origin_variation_status[i]==""){
      september_data$location_origin_timetable_variation[i]=NA
    }
}
september_data$actualdelaytime<-NULL
for(j in 1:nrow(september_data)){
  if(september_data$location_terminus_variation_status[j]=="LATE"||september_data$location_terminus_variation_status[j]=="ON TIME"||september_data$location_terminus_variation_status[j]=="OFF ROUTE"){
    september_data$actualdelaytime[j]=(september_data$location_terminus_timetable_variation[j]-september_data$location_origin_timetable_variation[j])
  }else if(september_data$location_terminus_variation_status[j]=="EARLY"){
    september_data$actualdelaytime[j]=-(september_data$location_terminus_timetable_variation[j]+september_data$location_origin_timetable_variation[j])
  }else if(september_data$location_origin_variation_status[j]==""||september_data$location_terminus_variation_status[j]==""){
    september_data$actualdelaytime[j]=NA
  }
}

#Removing rows with NAs in the 14th column(actualdelaytime)
september_data2<-september_data[complete.cases(september_data[,14]),]
september_data3<-september_data2[,c(1:8,13,14)]

##Removing outliers from the data for a better prediction
pt_model_for_outliers<-lm(actualdelaytime~.,data=september_data3)
cooksd2<-cooks.distance(pt_model_for_outliers)
influential2 <- as.numeric(names(cooksd2)[(cooksd2 > 4*mean(cooksd2, na.rm=T))])
pt_data_without_outliers<-september_data3[-influential2,]

##Partitioning the data into train and test and determining the Mean squared error
set.seed(345)
indexpt <- sample(1:nrow(pt_data_without_outliers),round(0.75*nrow(pt_data_without_outliers)))
trainpt <- pt_data_without_outliers[indexpt,]
testpt <- pt_data_without_outliers[-indexpt,]

# Fitting linear model on the traindata omitting rows with NAs
lm.pt.fit <- lm(actualdelaytime~., data=trainpt,na.action=na.omit)

#Summary of the model
 summary(lm.pt.fit)
# Predicted data from lm.pt.fit
pr.lm.pt <- predict(lm.pt.fit,testpt)

# Mean squared error and Variable importance computed
MSE.lm.pt <- sum((pr.lm.pt-testpt$actualdelaytime)^2,na.rm=TRUE)/nrow(testpt)
variableimportancept<-varImp(lm.pt.fit)

##-----Another method-Scaling and using Cross Validation in linear model  for measuring RMSE-------------------#
stdpt<-apply(pt_data_without_outliers,2,sd)
data_scaled_with_stdpt<-as.data.frame(scale(pt_data_without_outliers),center=TRUE,scale=std)
set.seed(450)
cv.error.pt<-NULL
variableimportance.pt<-NULL
k <- 10
#Loop for calculating cross validation error for 10 iterations and then calculating the mean of it
for(i in 1:k){
  index.pt <- sample(1:nrow(pt_data_without_outliers),round(0.75*nrow(pt_data_without_outliers)))
  train.cv.pt <- data_scaled_with_stdpt[index.pt,]
  test.cv.pt <- data_scaled_with_stdpt[-index.pt,]
  
  modelpt <- lm(actualdelaytime~.,data=train.cv.pt)
  
  pr.lmpt <- predict(modelpt,test.cv.pt[,1:9])
  pr.lmpt <- pr.lmpt*(sd(pt_data_without_outliers$actualdelaytime))+mean(pt_data_without_outliers$actualdelaytime)
  test.cv.r.pt <- (test.cv.pt$actualdelaytime)*(sd(pt_data_without_outliers$actualdelaytime))+mean(pt_data_without_outliers$actualdelaytime)                                                                                         
  variableimportance.pt[i]<-varImp(modelpt)
  cv.error.pt[i] <- (sum((test.cv.r.pt-pr.lmpt)^2,na.rm=TRUE))/length(test.cv.r.pt)
}
cross_validation_error_in_linear_fit_pt<-mean(cv.error.pt)
df.pt<-data.frame(matrix(unlist(variableimportance.pt),nrow=10,byrow=T),stringsasFactors=FALSE)
df.pt<-df.pt[,-10]
colnames(df.pt)<-names(data_scaled_with_stdpt[,c(1:9)])

#The function gives the mean of the variable importance of each variable through all the 10 iterations
varimp_according_to_linear_model_pt<-apply(df.pt,2,mean)

#The correlation matrix of the data
correlation.pt<-as.data.frame(cor(data_scaled_with_stdpt,use="pairwise.complete.obs"))
