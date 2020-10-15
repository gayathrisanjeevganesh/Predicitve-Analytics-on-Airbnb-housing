library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
#library(gridExtra)
library(scales)
#library(Rmisc)
#library(ggrepel)
#install.packages("dplyr")
library("randomForest")
#install.packages("zoo")
library(zoo)
#library(psych)
#library(xgboost)
library(tidyr)
library(dplyr)
library(ggplot2)
library(corrplot)
#install.packages("imputeTS")
library("imputeTS")
#install.packages("gbm")
library("gbm")
library(e1071)
library(rpart)
library(randomForest)
library("ranger")
#install.packages('caTools') 
library(caTools)
### Read in training data
data = read.csv('C:/Users/ram/Desktop/Gayathri/SaiAdmissions/Columbia/Books/Sem1/Applied analytics frameworks and methods 1/Class PPT/KAGGLE/analysisData.csv')

##Read in scoring data 
scoringData = read.csv('C:/Users/ram/Desktop/Gayathri/SaiAdmissions/Columbia/Books/Sem1/Applied analytics frameworks and methods 1/Class PPT/KAGGLE/scoringData.csv')

#kittys correlation

corMatrix = as.data.frame(cor(all_numVar[,-12]))
corMatrix$var1 = rownames(corMatrix)
corMatrix %>%
  gather(key=var2,value=r,1:11)%>%
  ggplot(aes(x=var1,y=var2,fill=r))+
  geom_tile()+
  geom_text(aes(label=round(r,2)),size=3)+
  scale_fill_gradient2(low = 'red',high='green',mid = 'white')+
  theme(axis.text.x=element_text(angle=90))

#data cleaning

#finding na columns

apply(all_numVar, 2, function(x) any(is.na(x)))

data$beds <- ifelse(is.na(data$beds), mean(data$beds, 
                                           na.rm=TRUE), 
                    data$beds)
sum(all_numVar$beds)

data$square_feet <- ifelse(is.na(data$square_feet), 
                           mean(data$square_feet, na.rm=TRUE), 
                           data$square_feet)
sum(all_numVar$square_feet)

data$monthly_price <- ifelse(is.na(data$monthly_price),
                             mean(data$monthly_price, na.rm=TRUE),
                             data$monthly_price)
sum(all_numVar$monthly_price)

data$weekly_price <- ifelse(is.na(data$weekly_price), 
                            mean(data$weekly_price, na.rm=TRUE), 
                            data$weekly_price)

data$security_deposit <- ifelse(is.na(data$security_deposit), 
                                mean(data$security_deposit, na.rm=TRUE), 
                                data$security_deposit)

data$cleaning_fee <- ifelse(is.na(data$cleaning_fee), 
                            0, 
                            data$cleaning_fee)
sum(all_numVar$cleaning_fee)

#cleaning scoring data of NA's

scoringData$beds <- ifelse(is.na(scoringData$beds), mean(scoringData$beds, 
                                                         na.rm=TRUE), 
                           scoringData$beds)
sum(scoringData$beds)

scoringData$square_feet <- ifelse(is.na(scoringData$square_feet), 
                                  mean(scoringData$square_feet, na.rm=TRUE), 
                                  scoringData$square_feet)
sum(scoringData$square_feet)

scoringData$monthly_price <- ifelse(is.na(scoringData$monthly_price),
                                    mean(scoringData$monthly_price, na.rm=TRUE),
                                    scoringData$monthly_price)
sum(scoringData$monthly_price)

scoringData$weekly_price <- ifelse(is.na(scoringData$weekly_price), 
                                   mean(scoringData$weekly_price, na.rm=TRUE), 
                                   scoringData$weekly_price)

scoringData$security_deposit <- ifelse(is.na(scoringData$security_deposit), 
                                       mean(scoringData$security_deposit, na.rm=TRUE), 
                                       scoringData$security_deposit)

scoringData$cleaning_fee <- ifelse(is.na(scoringData$cleaning_fee), 
                                   0, 
                                   scoringData$cleaning_fee)
sum(scoringData$cleaning_fee)

#introducing new variables

data$description = as.character(data$description)

# Whether there is a description (0,1). 0 represents no description
data$noDescription = as.numeric(data$description!="")
data$noDescription = factor(data$noDescription,labels=c('no description','contains description'))
# Number of characters in the description
data$charCountDescription = nchar(data$description)

#UPPER CASE DESC

data$upperCaseDescription = sapply(gregexpr("[A-Z]",data$description),function(x) sum(x>0))

# description variable playing around scoringData

scoringData$description = as.character(scoringData$description)

# Whether there is a description (0,1). 0 represents no description
scoringData$noDescription = as.numeric(scoringData$description!="")
scoringData$noDescription = factor(scoringData$noDescription,labels=c('no description','contains description'))
# Number of characters in the description
scoringData$charCountDescription = nchar(scoringData$description)

#UPPER CASE DESC
scoringData$upperCaseDescription = sapply(gregexpr("[A-Z]",scoringData$description),function(x) sum(x>0))


library(caTools)
set.seed(2018)

train = data[split,]
test = data[!split,]

set.seed(2018)
#forest1 = randomForest(price~longitude+latitude+accommodates+bedrooms+availability_365+cleaning_fee+availability_30+availability_90+neighbourhood_group_cleansed+reviews_per_month++ noDescription+charCountDescription+bathrooms+minimum_nights+host_total_listings_count+availability_60+number_of_reviews+beds+maximum_nights+guests_included+host_listings_count, data = data, ntree=100)

split = sample.split(data$price,SplitRatio = 0.7)
train = data[split,]
test = data[!split,]

forest = randomForest(price~ weekly_price+cleaning_fee+accommodates+
                        beds+bedrooms+latitude+longitude+availability_30+
                        availability_90+availability_60+
                        square_feet+guests_included+security_deposit+bathrooms+
                        monthly_price+review_scores_location+extra_people+
                        availability_365+neighbourhood_group_cleansed+
                        host_total_listings_count
                      + noDescription+charCountDescription+
                        cancellation_policy+review_scores_rating
                      +room_type+upperCaseDescription,
                      data= train , ntree = 20)

predForest = predict(forest,newdata = data)
rmseForest = sqrt(mean((predForest-data$price)^2))
rmseForest

#model 2

split = sample.split(data$price,SplitRatio = 0.7)
train = data[split,]
test = data[!split,]

forest = randomForest(price~ weekly_price+cleaning_fee+accommodates+
                        beds+bedrooms+latitude+longitude+availability_30+
                        availability_90+availability_60+
                        square_feet+guests_included+security_deposit+bathrooms+
                        monthly_price+review_scores_location+extra_people+
                        availability_365+neighbourhood_group_cleansed+
                        host_total_listings_count,
                      data= train , ntree = 20)

predForest = predict(forest,newdata = test)
rmseForest = sqrt(mean((predForest-test$price)^2))
rmseForest

#linear regression

modellr = lm(price~ weekly_price+cleaning_fee+accommodates+beds+bedrooms+
               square_feet+guests_included+security_deposit+bathrooms+
               monthly_price+review_scores_location+extra_people+
               availability_365+neighbourhood_group_cleansed+
               noDescription+charCountDescription,train)
### In sample metrics
# Predict
predlr = predict(modellr,test)
# Print summary of regression results
summary(model)
summary(model)$r.squared
# Calculate RMSE
rmselr = sqrt(mean((predlr-test$price)^2))
rmselr

install.packages("caret")
library(caret)
varImpPlot(forest, type= 2)


#writing submission file
pred = predict(xgModel,newdata=scoringData)
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'C:/Users/ram/Desktop/Gayathri/SaiAdmissions/Columbia/Books/Sem1/Applied analytics frameworks and methods 1/Class PPT/KAGGLE/submission.csv',row.names = F)


