#Reading CSV
df=read.csv("housing.csv")
#Structure of Data
head(df)
summary(df)
str(df)

#To plot scatter plot
pairs(df[1:9])
#To identify Correlation
cor(df[1:9])

#To remove Null Values
table(is.na(df$total_bedrooms))
df$total_bedrooms[is.na(df$total_bedrooms)]=median(df$total_bedrooms,na.rm = TRUE)
table(is.na(df$total_bedrooms))

#TO handle Categorical Variables
df$ocean_proximity=as.numeric(factor(df$ocean_proximity,
                                     levels = c('<1H OCEAN','INLAND','ISLAND','NEAR BAY', 'NEAR OCEAN'),
                                    labels = c(1,2,3,4,5)))
head(df$ocean_proximity)

#Splitting Data into Train & Test
set.seed(42)
df=df[sample(nrow(df)),]
select.data= sample (1:nrow(df), 0.7*nrow(df))
train.data= df[select.data,]
test.data= df[-select.data,]


nrow(train.data)
nrow(test.data)

#Perform Feature Scaling
train.data[-9]=scale(train.data[-9])
head(train.data)
test.data[-9]=scale(test.data[-9])
head(test.data)


#To perform Random Forest Operation
install.packages("randomForest")
library(randomForest)
classifier= randomForest(x=train.data[-9],y=train.data$median_house_value,ntree = 10)

y_pred=predict(classifier, newdata = test.data[-9])
head(y_pred)

#Creating a confusion matrix
actuals_Preds= data.frame(cbind(actuals=test.data$median_house_value, predicteds=y_pred))
head(actuals_Preds)

test_mse = mean(((y_pred - test.data[,9])^2))
test_rmse = sqrt(test_mse)
test_rmse

#Appluing K fold Cross Validation
install.packages("caret")
library(caret)
folds= createFolds(train.data$median_house_value, k=10)
cv=lapply(folds,function(x){
  train_fold=train.data[-x,]
  test_fold=train.data[x,]
  classifier=randomForest(x=train.data[-9],y=train.data$median_house_value,ntree = 10)
  y_pred=predict(classifier, newdata = test_fold[-9])
  test_mse = mean(((y_pred - test_fold[,9])^2))
  test_rmse = sqrt(test_mse)
  test_rmse})
cv



