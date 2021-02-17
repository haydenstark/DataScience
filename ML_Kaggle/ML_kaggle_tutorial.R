
#columns; longitude, latitude, housing_median_age, total_rooms, total_bedrooms, population,
#  households, median_income, median_house_values, ocean_proximity

#each row is a group of houses in close proximity, hence medians

setwd('C:/Users/james/Documents/GitHub/DataScience/ML_Kaggle')
library(tidyverse)
library(reshape2)

housing = read.csv('housing.csv')
head(housing)

summary(housing)

par(mfrow=c(2,5))
colnames(housing)

ggplot(data = melt(housing), mapping = aes(x = value)) +
  geom_histogram(bins = 30) + facet_wrap(~variable, scales='free_x')

#imputing missing values in total_bedrooms
housing$total_bedrooms[is.na(housing$total_bedrooms)] = median(housing$total_bedrooms, na.rm=TRUE)


#turning total columns into means
housing$mean_bedrooms = housing$total_bedrooms/housing$households
housing$mean_rooms = housing$total_rooms/housing$households

drops = c('total_bedrooms','total_rooms')
housing = housing[ , !(names(housing) %in% drops)]  #dropping total bedrooms and total rooms
head(housing)


#turning ocean_proximity column into boolean table
categories = unique(housing$ocean_proximity)
categories
cat_housing = data.frame(ocean_proximity = housing$ocean_proximity)

for(cat in categories){
  cat_housing[, cat] = rep(0, times=nrow(cat_housing))
}
head(cat_housing)

for(i in 1:length(cat_housing$ocean_proximity)){
  cat = as.character(cat_housing$ocean_proximity[i])
  cat_housing[,cat][i] = 1
}
head(cat_housing)
tail(cat_housing)

cat_columns = names(cat_housing)
keep_columns = cat_columns[cat_columns != 'ocean_proximity']
cat_housing = select(cat_housing,one_of(keep_columns))
head(cat_housing)
tail(cat_housing)


#scaling and dropping ocean proximity and median house value
colnames(housing)
drops = c('ocean_proximity','median_house_value')
housing_num = housing[ , !(names(housing) %in% drops)]
head(housing_num)

scaled_housing_num = scale(housing_num)
head(scaled_housing_num)


#merging everything
cleaned_housing = cbind(cat_housing, scaled_housing_num, median_house_value=housing$median_house_value)
head(cleaned_housing)


#train test split
set.seed(42)
sample = sample.int(n = nrow(cleaned_housing), size=floor(.8*nrow(cleaned_housing)), replace=F)
train = cleaned_housing[sample, ]   #sample
test = cleaned_housing[-sample, ]   #everything but

head(train)
nrow(train) + nrow(test) == nrow(cleaned_housing)  #just double checking that train and test total the original data


#starting with 3 predictors; median income, total rooms, population
library('boot')
?cv.glm     #generalized linear model
glm_house = glm(median_house_value~median_income+mean_rooms+population, data=cleaned_housing)
k_fold_cv_error = cv.glm(cleaned_housing, glm_house, K=5)  #using 5 folds

k_fold_cv_error$delta   #first value; raw cross validation estimate of prediction error... second; adjusted cross-validation estimate (compensates for bias)... check ?cv.glm

glm_cv_rmse = sqrt(k_fold_cv_error$delta)[1]
glm_cv_rmse  #off by approx 83,000

names(glm_house)
glm_house$coefficients #median income appears to have biggest effect


#random forest model
library('randomForest')
?randomForest
names(train)

set.seed(42)
train_y = train[, 'median_house_value']
train_x = train[, names(train)!='median_house_value']

head(train_y)
head(train_x)

rf_model = randomForest(train_x, y=train_y, ntree=500, importance=T)

names(rf_model)
rf_model$importance  #%IncMSE - Percentage included mean squared error; the higher the number, the more important it is as a predictor

#oob error estimate (out of bag)
oob_prediction = predict(rf_model)  #leaving out a data source forces oob predictions
train_mse=mean(as.numeric((oob_prediction - train_y)^2))
oob_rmse = sqrt(train_mse)
oob_rmse  #so this shows our rf model is approx off by 49,000

test_y = test[, 'median_house_value']
test_x = test[, names(test)!='median_house_value']
names(test_x)
y_pred = predict(rf_model, test_x)
test_mse = mean(((y_pred - test_y)^2))
test_rmse = sqrt(test_mse)
test_rmse  #so model scored roughly same as training data, aka it's good at making predictions and not overfit




#MY TURN!
#my first question/idea is - would eliminating a couple of the columns result in more accurate house value prediction

#going off of their already cleaned data, removing the ocean proximity and bedrooms columns
my_housing = cleaned_housing[, c(6:11,13,14)]
head(my_housing)

#split
set.seed(42)
sample = sample.int(n = nrow(my_housing), size=floor(.8*nrow(my_housing)), replace=F)
my_train = my_housing[sample, ]   #sample
my_test = my_housing[-sample, ]   #everything but

head(my_train)
nrow(my_train) + nrow(my_test) == nrow(my_housing)  #double check

my_train_y = my_train[, 'median_house_value']
my_train_x = my_train[, names(my_train)!='median_house_value']
head(my_train_y)
head(my_train_x)

rf_model = randomForest(my_train_x, y=my_train_y, ntree=500, importance=T)
#I considered increasing ntree, but lets keep it for now to see if fewer parameters actually helps

names(rf_model)
rf_model$importance

#following their same course of action for consistency
oob_prediction = predict(rf_model)
train_mse=mean(as.numeric((oob_prediction - my_train_y)^2))
oob_rmse = sqrt(train_mse)
oob_rmse  #approx off by 50,000
#so theirs was slightly more accurate, and it doesn't seem like removing the columns helped

my_test_y = my_test[, 'median_house_value']
my_test_x = my_test[, names(my_test)!='median_house_value']
names(my_test_x)
y_pred = predict(rf_model, my_test_x)
test_mse = mean(((y_pred - my_test_y)^2))
test_rmse = sqrt(test_mse)
test_rmse
#and again 50,000
#so removing ocean proximity and bedrooms decreased the accuracy



#I also wanted to try was tuning the ntree parameter
rf_model = randomForest(my_train_x, y=my_train_y, ntree=1000, importance=T)

names(rf_model)
rf_model$importance

oob_prediction = predict(rf_model)
train_mse=mean(as.numeric((oob_prediction - my_train_y)^2))
oob_rmse = sqrt(train_mse)
oob_rmse  #still 50,000


my_test_y = my_test[, 'median_house_value']
my_test_x = my_test[, names(my_test)!='median_house_value']
names(my_test_x)
y_pred = predict(rf_model, my_test_x)
test_mse = mean(((y_pred - my_test_y)^2))
test_rmse = sqrt(test_mse)
test_rmse #again 50,000
#so increasing ntree to 1000 didn't affect results much at all




#last idea - will it be more accurate if only given latitude and longitude and income? (I doubt it)
names(cleaned_housing)
my_housing = cleaned_housing[, c(6,7, 11, 14)]
head(my_housing)

#split
set.seed(42)
sample = sample.int(n = nrow(my_housing), size=floor(.8*nrow(my_housing)), replace=F)
my_train = my_housing[sample, ]   #sample
my_test = my_housing[-sample, ]   #everything but

head(my_train)
nrow(my_train) + nrow(my_test) == nrow(my_housing)  #double check

my_train_y = my_train[, 'median_house_value']
my_train_x = my_train[, names(my_train)!='median_house_value']
head(my_train_y)
head(my_train_x)

rf_model = randomForest(my_train_x, y=my_train_y, ntree=500, importance=T)

names(rf_model)
rf_model$importance
#OH, it's now holding a lot more weight to latitude and longitude than income

oob_prediction = predict(rf_model)
train_mse=mean(as.numeric((oob_prediction - my_train_y)^2))
oob_rmse = sqrt(train_mse)
oob_rmse
#47,558  ... this is more accurate?

my_test_y = my_test[, 'median_house_value']
my_test_x = my_test[, names(my_test)!='median_house_value']
names(my_test_x)
y_pred = predict(rf_model, my_test_x)
test_mse = mean(((y_pred - my_test_y)^2))
test_rmse = sqrt(test_mse)
test_rmse
#and 48,417 ...
#so this slightly increased the accuracy?
#however, considering it's only by a thousand and we're referring to the price of houses
#it's not necessarily a huge leap forward



#wait wait, I want to try ntree again
rf_model = randomForest(my_train_x, y=my_train_y, ntree=1500, importance=T)

names(rf_model)
rf_model$importance

oob_prediction = predict(rf_model)
train_mse=mean(as.numeric((oob_prediction - my_train_y)^2))
oob_rmse = sqrt(train_mse)
oob_rmse
#47,337

my_test_y = my_test[, 'median_house_value']
my_test_x = my_test[, names(my_test)!='median_house_value']
names(my_test_x)
y_pred = predict(rf_model, my_test_x)
test_mse = mean(((y_pred - my_test_y)^2))
test_rmse = sqrt(test_mse)
test_rmse
#48,143
#so not much change when tuning ntree