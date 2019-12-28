#Set Working Directory

setwd("Z:/MIS Project/census")

#install.packages("data.table")
#install.packages("corrplot")

library(data.table)
library(ggplot2)
library(plotly)
library(caret)
library(corrplot)
library(mlr)
library(FSelector)
library(DMwR)
library(e1071)

Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_221')

#train <- read.csv("train.csv")
train <- fread("train.csv",na.strings = c(""," ","?","NA",NA))
train <- data.table(train)

#test <- read.csv("test.csv")
test <- fread("test.csv",na.strings = c(""," ","?","NA",NA))

#na.strings removes Question marks from data and replaces it with NA


#Looking at the data

dim(train)
str(train)

dim(test)
str(test)

#Test first few rows of train and test

train[1:5]
test [1:5]


#check Target variable

#Checking Unique values in our target variable
unique(train$income_level)
unique(test$income_level)

#Checking The count of Unique values in our target variable
table(train$income_level)
table(test$income_level)


#Encoding Target Variables as 0s and 1s for Binary Classifier

train[,income_level := ifelse(income_level == "-50000",0,1)]
test[,income_level := ifelse(income_level == "-50000",0,1)]


#Check how imbalanced our data is

round(prop.table(table(train$income_level))*100)
(prop.table(table(test$income_level))*100)


#We Can see our training data has 94% 1s and 6% 0s
#And all of our test data has 1s in Income_Level

#set column classes (changed from int, chr to num, factor)
factcols <- c(2:5,7,8:16,20:29,31:38,40,41)
numcols <- setdiff(1:40,factcols)


train[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
train[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

test[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
test[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]



#Separating Categorical and Numerical Columns
#separate categorical variables
cat_train <- train[,factcols, with=FALSE]
cat_test <- test[,factcols,with=FALSE]


#separate numerical variables
num_train <- train[,numcols,with=FALSE]
num_test <- test[,numcols,with=FALSE] 
rm(train,test) # to save memory


#Exploratory Data Analysis

#plot variable age 
plt <- ggplot(data = num_train, aes(x= age, y=..density..)) + geom_histogram(fill="red",color="blue",alpha = 0.5,bins =100) + geom_density()
plt + ggtitle("Age Distribution") + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))


#variable capital_gains
plt <- ggplot(data = num_train, aes(x= capital_gains, y=..density..)) + geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) + geom_density()
plt + ggtitle("Capital Gains Distribution") + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))


#variable capital_losses
plt <- ggplot(data = num_train, aes(x= capital_losses, y=..density..)) + geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) + geom_density()
plt + ggtitle("Capital Losses Distribution") + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))


# wage per hour
plt <- ggplot(data = num_train, aes(x= wage_per_hour, y=..density..)) + geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) + geom_density()
plt + ggtitle("Hourly Wage Distribution") + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))


## Plot numerical columns vs. target variable
#add target variable
num_train[,income_level := cat_train$income_level]
#create a scatter plot
plt <- ggplot(data=num_train,aes(x = age, y=wage_per_hour))+geom_point(aes(colour=income_level))+scale_y_continuous("wage per hour", breaks = seq(0,10000,1000))
plt + ggtitle("Hourly Wage vs. Age") + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))



## Plot categorical columns vs. target variable
#dodged bar chart
#variable class_of_worker
plt <- ggplot(cat_train,aes(x=class_of_worker,fill=income_level))+geom_bar(position = "dodge",  color="black")+scale_fill_brewer(palette = "Pastel1")+theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10))
plt + ggtitle("Income Distribution by worker's class") + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))
# Only two category levels (Not In Universe & Private) seem to dominate. 
# In such situation, a good practice is to combine levels having less than 5% frequency of the total category frequency.

#variable education
plt <- ggplot(cat_train,aes(x=education,fill=income_level))+geom_bar(position = "dodge",  color="black")+scale_fill_brewer(palette = "Pastel1")+theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10))
plt + ggtitle("Income Distribution by education") + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))


# checking categories using 2 way tables
prop.table(table(cat_train$marital_status,cat_train$income_level),1)
prop.table(table(cat_train$class_of_worker,cat_train$income_level),1)




#Data Cleansing

#check missing values in numerical data and do imputation if needed
sum(is.na(num_train))
sum(is.na(num_test))
# We see that numeric variables has no missing values



#check correlations between numerical columns

#set threshold as 0.7
num_train[,income_level := NULL]
ax <-findCorrelation(x = cor(num_train), cutoff = 0.7)
num_train <- num_train[,-ax,with=FALSE] 
num_test[,weeks_worked_in_year := NULL]
# The variable weeks_worked_in_year gets removed. 
# For hygiene purpose, we've removed that variable from test data too. It's not necessary though!

num_train.cor = cor(num_train)
corrplot(num_train.cor)

palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = num_train.cor, col = palette, symm = TRUE)




# check for missing values in categorical data. We'll use base sapply() to find out percentage of missing values per column.
#check missing values per columns
mvtr <- sapply(cat_train, function(x){sum(is.na(x))/length(x)})*100
mvte <- sapply(cat_test, function(x){sum(is.na(x)/length(x))}*100)
mvtr
mvte



#Data Manipulation

#select columns with missing value less than 5%
cat_train <- subset(cat_train, select = mvtr < 5 )
cat_test <- subset(cat_test, select = mvte < 5)

#Leaves all the columns with more than 5% missing values ^

sum(is.na(cat_train))
sum(is.na(cat_test)) #We Still have NA values.



#set NA as Unavailable - train data
#convert to characters
cat_train <- cat_train[,names(cat_train) := lapply(.SD, as.character),.SDcols = names(cat_train)]
for (i in seq_along(cat_train)) set(cat_train, i=which(is.na(cat_train[[i]])), j=i, value="Unavailable")
#convert back to factors
cat_train <- cat_train[, names(cat_train) := lapply(.SD,factor), .SDcols = names(cat_train)]


#set NA as Unavailable - test data
cat_test <- cat_test[, (names(cat_test)) := lapply(.SD, as.character), .SDcols = names(cat_test)]
for (i in seq_along(cat_test)) set(cat_test, i=which(is.na(cat_test[[i]])), j=i, value="Unavailable")
#convert back to factors
cat_test <- cat_test[, (names(cat_test)) := lapply(.SD, factor), .SDcols = names(cat_test)]



#combine factor levels with less than 5% values
#train
for(i in names(cat_train)){
  p <- 5/100
  ld <- names(which(prop.table(table(cat_train[[i]])) < p))
  levels(cat_train[[i]])[levels(cat_train[[i]]) %in% ld] <- "Other"
}

#test
for(i in names(cat_test)){
  p <- 5/100
  ld <- names(which(prop.table(table(cat_test[[i]])) < p))
  levels(cat_test[[i]])[levels(cat_test[[i]]) %in% ld] <- "Other"
}


#check columns with unequal levels (unique values)
#install.packages("mlr") 

summarizeColumns(cat_train)[,"nlevs"]
summarizeColumns(cat_test)[,"nlevs"]




# let's look at numeric variables and reflect on possible ways for binning.
num_train[,.N,age][order(age)]
num_train[,.N,wage_per_hour][order(-N)]
num_train[,.N,capital_gains][order(-N)]
num_train[,.N,capital_losses][order(-N)]
num_train[,.N,dividend_from_Stocks][order(-N)]
num_train[,.N,num_person_Worked_employer][order(-N)]



#bin age variable 0-30 31-60 61 - 90
num_train[,age:= cut(x = age,breaks = c(0,30,60,90),include.lowest = TRUE,labels = c("young","adult","old"))]
num_train[,age := factor(age)]

num_test[,age:= cut(x = age,breaks = c(0,30,60,90),include.lowest = TRUE,labels = c("young","adult","old"))]
num_test[,age := factor(age)]




#Bin numeric variables with Zero and MoreThanZero (basically converting continuous to discrete)
num_train[,wage_per_hour := ifelse(wage_per_hour == 0,"Zero","MoreThanZero")][,wage_per_hour := as.factor(wage_per_hour)]
num_train[,capital_gains := ifelse(capital_gains == 0,"Zero","MoreThanZero")][,capital_gains := as.factor(capital_gains)]
num_train[,capital_losses := ifelse(capital_losses == 0,"Zero","MoreThanZero")][,capital_losses := as.factor(capital_losses)]
num_train[,dividend_from_Stocks := ifelse(dividend_from_Stocks == 0,"Zero","MoreThanZero")][,dividend_from_Stocks := as.factor(dividend_from_Stocks)]

num_test[,wage_per_hour := ifelse(wage_per_hour == 0,"Zero","MoreThanZero")][,wage_per_hour := as.factor(wage_per_hour)]
num_test[,capital_gains := ifelse(capital_gains == 0,"Zero","MoreThanZero")][,capital_gains := as.factor(capital_gains)]
num_test[,capital_losses := ifelse(capital_losses == 0,"Zero","MoreThanZero")][,capital_losses := as.factor(capital_losses)]
num_test[,dividend_from_Stocks := ifelse(dividend_from_Stocks == 0,"Zero","MoreThanZero")][,dividend_from_Stocks := as.factor(dividend_from_Stocks)]





#combine data and make test & train files
d_train <- cbind(num_train,cat_train)
d_test <- cbind(num_test,cat_test)


setDF(d_train)
setDF(d_test)

#create task
train.task <- makeClassifTask(data = d_train,target = "income_level")
test.task <- makeClassifTask(data=d_test,target = "income_level")



#remove zero variance features
train.task <- removeConstantFeatures(train.task)
test.task <- removeConstantFeatures(test.task)



#get variable importance chart
#install.packages("FSelector")
var_imp <- generateFilterValuesData(train.task, method = c("information.gain"))
plotFilterValues(var_imp,feat.type.cols = TRUE)


#install.packages("DMwR")


# original dataset
write.csv(d_train, "census_orig.csv")
write.csv(d_test, "test.csv")


TPR = TP / (TP+FN)


#undersampling 
train.under <- undersample(train.task,rate = 0.1) #keep only 10% of majority class
table(getTaskTargets(train.under))
write.csv(getTaskData(train.under, recode.target = "01"), "census_under.csv")



#oversampling
train.over <- oversample(train.task,rate=15) #make minority class 15 times
table(getTaskTargets(train.over))
write.csv(getTaskData(train.over, recode.target = "01"), "census_over.csv")



#SMOTE
#train.smote <- smote(train.task,rate = 15,nn = 5)
system.time(
  train.smote <- smote(train.task,rate = 10,nn = 3) 
)
table(getTaskTargets(train.smote))
write.csv(getTaskData(train.smote, recode.target = "01"), "census_smote.csv")

# ROSE
library(ROSE)
# use ROSE to synthetically generate data
data.rose <- ROSE(income_level ~ ., data = d_train, seed = 1)$data
table(data.rose$income_level)
write.csv(data.rose, "census_rose.csv")


dim(d_train)
dim(data.rose)
d_train










#lets see which algorithms are available
listLearners("classif","twoclass")[c("class","package")]


#naive Bayes
naive_learner <- makeLearner("classif.naiveBayes",predict.type = "response")
naive_learner$par.vals <- list(laplace = 1)


#10fold CV - stratified
folds <- makeResampleDesc("CV",iters=10,stratify = TRUE)
#cross validation function
fun_cv <- function(a){
  crv_val <- resample(naive_learner,a,folds,measures = list(acc,tpr,tnr,fpr,fp,fn))
  crv_val$aggr
}

fun_cv(train.task) 
fun_cv(train.under) 
fun_cv(train.over)
fun_cv(train.smote)



## SMOTE Data
#train and predict 
nB_model <- train(naive_learner, train.smote)
nB_predict <- predict(nB_model,test.task)
#evaluate
nB_prediction <- nB_predict$data$response
dCM <- confusionMatrix(d_test$income_level,nB_prediction)
#calculate metrics
dCM



## original Data
#train and predict 
nB_model <- train(naive_learner, train.task)
nB_predict <- predict(nB_model,test.task)
#evaluate
nB_prediction <- nB_predict$data$response
dCM <- confusionMatrix(d_test$income_level,nB_prediction)
#calculate metrics
dCM
