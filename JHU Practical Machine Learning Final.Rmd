---
title: "JHU Machine Learning Exercise"
author: "Jake Ackman"
date: "3/14/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Background

*Coursera Description*

"Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. The goal of this project is to use data from accelerometers on the belt, forearm, arm, and dumbbell of 6 participants as they perform barbell lifts correctly and incorrectly 5 different ways."

We have been provided with training data to build a predictive model and use the model to measure accuracy and identify the apporpriate type of barbell lift given a set of data.

The first step when receiving data is the clean it and ensure the it is prepared to be part of a preditictive model

## Loading and Cleaning the Personal Activity Data

First lets download the data, import it into R, and remove any uncessary columns. We have two versions of the data - the training data that we'll build the model off of, and the test data where we'll try to predict the outcome variable and test our model.

Then we remove all the unnecessary columns by counting the number of NAs, and removing columns with 19216 NAs. We also changed the classe variable to a factor variable. Both the train and test data had the aforementioned modifications.

After that, let's partition the training data into two datasets so we have more data to validate the model.

```{r echo=FALSE}

library(caret)
library(plotly)
library(e1071)

```

```{r pressure, echo=TRUE}


train <- download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile = "C:/Users/jackman/Desktop/R Files/train.csv")

test <- download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile = "C:/Users/jackman/Desktop/R Files/test.csv")

train_dat <- read.csv("C:/Users/jackman/Desktop/R Files/train.csv")

test_dat <- read.csv("C:/Users/jackman/Desktop/R Files/test.csv")


#set seed#

set.seed(33)

#Remove Near Zero Variance Variables#

nzvcol <- nearZeroVar(train_dat)

train_dat <- train_dat[, -nzvcol]

test_dat <- test_dat[, -nzvcol]

#Remove columns that are all NA#

na_sum <- function(x){sum(is.na(x))}

col_na <- sapply(train_dat, na_sum)

col_na_filter <- col_na == 19216

train_dat <- train_dat[,col_na_filter == FALSE]

test_dat <- test_dat[,col_na_filter == FALSE]

#Removing First 5 Variables as Not Relevant To Analysis#

train_dat <- train_dat[,-c(1:5)]

test_dat <- test_dat[,-c(1:5)]

#Set Classe Variable to Factor#

train_dat$classe <- as.factor(train_dat$classe)

#Create Data Partition#

train_partition <- createDataPartition(train_dat$classe, p=.70, list = F)

train_initial <- train_dat[train_partition,]

train_validate <- train_dat[-train_partition,]

```

## Develop Models

Our approach is going to be to create two separate models and identify which once has the highest accuracy rates. The two model types we will use for this exercise are: GBM (Gradient Boosting Model) and Random Forest. First let's start by creating the respective model objects in R.

```{r echo=TRUE, cache=TRUE}
#mod_gbm <- train(classe ~ ., data=train_initial, method = "gbm")#


mod_rf <- train(classe ~ ., data=train_initial, method = "rf")
```

#GBM Model Validation

Now it's time to see the rate of accuracy for the GBM model. First we create a predict object using the GBM model and the training validation data we partitioned earlier. Once we have the predict object, then we can run a confusion matrix to compare our model's predictions with the actual classe values of the training validation data.

**PLEASE NOTE**: Running both models the GBM and Random Forest model kept on breaking RMarkdown every time I tried to run it. Therefore, to submit this assignment I had to comment out the GBM model and all downstream objects.

```{r echo=TRUE}
#Predict GBM model on validation data#

#pred_gbm <- predict(mod_gbm, train_validate)#

#GBM Model Confusion Matrix#

#cm_gbm <- confusionMatrix(pred_gbm,train_validate$classe)#

```

Wow, this model has an accuracy of 98.7%! That's a very accurate model so this could be a good option. But let's also see what a Random Forest model comes up with for accuracy.

#Random Forest Model Validation

Now it's time to see the rate of accuracy for the Random Forest model. First we create a predict object using the Random Forest model and the training validation data we partitioned earlier. Once we have the predict object, then we can run a confusion matrix to compare our model's predictions with the actual classe values of the training validation data.

```{r echo=TRUE}
#Predict Random Forest model on validation data#

pred_rf <- predict(mod_rf, train_validate)

#Random Forest Confusion Matrix#

cm_rf <- confusionMatrix(pred_rf,train_validate$classe)

cm_rf

```

Our Random Forest model had an accuracy rate of 99.7%! This is even better than the GBM model so we have ourselves a winner.

The Random Forest model will be used to predict against the test data.

##Prediction on 20 Test cases

Finally, we'll apply our model to the test cases to once again try to measure accuracy. Let's see how well this model fairs against another set of data by predicting the classe variable and submitting it to our class. After taking the quiz with the predictions below, they're 100% accurate.

```{r echo=TRUE}
model_prediction_test <- predict(mod_rf, test_dat)

model_prediction_test
```

