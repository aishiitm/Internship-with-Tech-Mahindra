
#ultiple Linear/Non-linear regression to determine train delay given 46 factors                
library(MASS)
library(caret)
library(xlsx)
library(neuralnet)
library(e1071)

a<-read.csv("TSC_430.csv",sep=",")
b<-read.csv("TSC_Meta_data",sheet="Sheet2",header=TRUE)


colnames(a)<-b[,2]
#Averaging factors of precipitation, visibility, etc at the origin and destination
anew2<- transform(anew2, journey_average_precipitation = rowMeans(anew2[,c(2,8)], na.rm = TRUE))
anew2<- transform(anew2, journey_average_temperature = rowMeans(anew2[,c(3,9)], na.rm = TRUE))
anew2<- transform(anew2, journey_average_cloud_cover = rowMeans(anew2[,c(4,10)], na.rm = TRUE))
anew2<- transform(anew2, journey_average_weather_visibility = rowMeans(anew2[,c(5,11)], na.rm = TRUE))
anew2<- transform(anew2, journey_average_weather_wind_gust_speed = rowMeans(anew2[,c(6,12)], na.rm = TRUE)) 
anew2<- transform(anew2, journey_average_weather_wind_speed = rowMeans(anew2[,c(7,13)], na.rm = TRUE))
anew8<-anew2[,c(19:24)]
anew8$journeystopcount<-anew2$journey_stopcount

#Computing the actual delay time(Delay at destination-delay at the origin)
ptm<-proc.time()
for(i in 1:nrow(a)){
  if(a$journey_origin_variation_status[i]=="LATE"){
    a$actualdelaytime[i]=((a$journey_destination_timetable_variation[i])-(a$journey_origin_timetable_variation[i]))
  }else if(a$journey_origin_variation_status[i]=="EARLY"){
    a$actualdelaytime[i]=((a$journey_destination_timetable_variation[i])+(a$journey_origin_timetable_variation[i]))}
  else if(a$journey_origin_variation_status[i]=="ON TIME"){
    a$actualdelaytime[i]=((a$journey_destination_timetable_variation[i])-(a$journey_origin_timetable_variation[i]))
  }else if(a$journey_origin_variation_status[i]=="OFF ROUTE"){
    a$actualdelaytime[i]=a$journey_destination_timetable_variation[i]
  }else if(a$journey_origin_variation_status[i]==""){
    a$actualdelaytime[i]=NA
  }
}
elapsed<-proc.time()-ptm

#Calculating mean of origin and destination for all the 12 other variables-reduces to Mean_of_variables_dataset
anew8$actualdelaytime<-a$actualdelaytime

#Removing outliers in the data-using cooks distance
model_for_outliers<-lm(actualdelaytime~.,data=anew8)
cooksd<-cooks.distance(model_for_outliers)
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])
data_without_outliers<-anew8[-influential,]

##Removing rows with NA-complete.cases function
final_data_for_prediction<-data_without_outliers[complete.cases(data_without_outliers),]
set.seed(345)
index <- sample(1:nrow(final_data_for_prediction),round(0.75*nrow(final_data_for_prediction)))
train <-final_data_for_prediction[index,]
test <- final_data_for_prediction[-index,]
# Fitting linear model-without scaling and centering
lm.fit <- glm(actualdelaytime~., data=train)
summary(lm.fit)

# Predicted data from lm
pr.lm2 <- predict(lm.fit,test)
# Test MSE
MSE.lm <- sum((pr.lm2 - test$actualdelaytime)^2)/nrow(test)


##-----Another method-Scaling and using Cross Validation in linear model  for measuring RMSE-------------------#
std<-apply(final_data_for_prediction,2,sd)
data_scaled_with_mean<-as.data.frame(scale(final_data_for_prediction),center=TRUE,scale=std)
set.seed(450)
cv.error<-NULL
variableimportance<-NULL
k <- 10
for(i in 1:k){
    index <- sample(1:nrow(final_data_for_prediction),round(0.75*nrow(final_data_for_prediction)))
    train.cv <- data_scaled_with_mean[index,]
    test.cv <- data_scaled_with_mean[-index,]
    
    model2 <- lm(actualdelaytime~.,data=train.cv)
    
    pr.lm <- predict(model2,test.cv[,1:8])
    pr.lm <- pr.lm*(sd(final_data_for_prediction$actualdelaytime))+mean(final_data_for_prediction$actualdelaytime)
    test.cv.r <- (test.cv$actualdelaytime)*(sd(final_data_for_prediction$actualdelaytime))+mean(final_data_for_prediction$actualdelaytime)                                                                                         
    variableimportance[i]<-varImp(model2)
    cv.error[i] <- (sum((test.cv.r-pr.lm)^2))/length(test.cv.r)
    
   
}
cross_validation_error_in_linear_fit<-mean(cv.error)
df<-data.frame(matrix(unlist(variableimportance),nrow=10,byrow=T),stringsasFactors=FALSE)
df<-df[,-8]
colnames(df)<-names(data_scaled_with_mean[,c(1:7)])
varimp_according_to_linear_model<-apply(df,2,mean)#This gives the mean of the variable importance of each variable through all the 10 iterations
correlation<-as.data.frame(cor(data_scaled_with_mean))



##--------------Using neural networks for regression-------------------------##
n <- names(train)
f <- as.formula(paste("actualdelaytime ~", paste(n[!n %in% "actualdelaytime"], collapse = " + ")))
set.seed(786)
m<-10
cv.error.nn<-NULL
variableimportance.nn<-NULL
for(j in 1:m){
  index <- sample(1:nrow(data_scaled_with_mean),round(0.75*nrow(data_scaled_with_mean)))
  train.cv.nn <- data_scaled_with_mean[index,]
  test.cv.nn <- data_scaled_with_mean[-index,]
  nn <- neuralnet(f,data=train.cv.nn,hidden=4,linear.output=TRUE)
  pr.nn <- compute(nn,test.cv.nn[,1:8])
  pr.nn <- pr.nn$net.result*(sd(final_data_for_prediction$actualdelaytime))+mean(final_data_for_prediction$actualdelaytime) 
  test.cv.r.nn <- (test.cv.nn$actualdelaytime)*(sd(final_data_for_prediction$actualdelaytime))+mean(final_data_for_prediction$actualdelaytime)
  variableimportance.nn[j]<-varImp(nn)
  cv.error.nn[j] <- (sum((test.cv.r.nn - pr.nn)^2))/nrow(test.cv.nn)
}
mean(cv.error.nn)
df.nn<-data.frame(matrix(unlist(variableimportance.nn),nrow=10,byrow=T),stringsasFactors=FALSE)
colnames(df.nn)<-names(data_scaled_with_mean[,c(1:7)])
varimp.nn<-apply(df.nn,2,mean)





