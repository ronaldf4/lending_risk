rm(list=ls())
getwd()
setwd("E:/STEVENS/study/FE-582/project/bank_loan_status")
train <- read.csv('credit_train.csv', na.strings = c("","NA",'n/a'))
#View(train)

# More than 50000 rows i.e more than 50% of data in months deliquent column is null
train <- train[,-13]
#nrow(train)
#ncol(train)
train <- train[complete.cases(train),]
#nrow(train)
#ncol(train)

#number of duplicate loan IDs
#nrow(train[duplicated(train$Loan.ID),])

train_no_dup <- unique(train, by='Loan.ID')
#nrow(train_no_dup)
#ncol(train_no_dup)
#View(train_no_dup)

hist(train_no_dup$Credit.Score)
train_no_dup[which(train_no_dup$Credit.Score > 1000),]$Credit.Score <- train_no_dup[which(train_no_dup$Credit.Score > 1000),]$Credit.Score/10
hist(train_no_dup$Credit.Score)

outlier_elim <- function(data_col){
  quant_25 <- as.numeric(quantile(data_col)[2])
  quant_75 <- as.numeric(quantile(data_col)[4])
  IQR <- quant_75-quant_25
  data_col[which((data_col < (quant_25 - 1.5*IQR)) | (data_col > (quant_75 + 1.5*IQR)))] <- NA
  return(data_col)
}


num_col_train <- colnames(train_no_dup)[c(4,6,7,11,13,15,16)]

for (col_name in num_col_train) {
  #print(col_name)
  boxplot(train_no_dup[[col_name]], main= paste(col_name,'_tran_uncleaned',sep = ''))
  hist(train_no_dup[[col_name]], main = paste(col_name,'_train_uncleaned',sep = ''))
  train_no_dup[[col_name]] <- outlier_elim(train_no_dup[[col_name]])
  sum(is.na(train_no_dup[[col_name]]))
  hist(train_no_dup[[col_name]], main = paste(col_name,'_train_cleaned',sep = ''))
}


train_no_dup <- train_no_dup[complete.cases(train_no_dup),]
#View(train_no_dup)
train_no_id <- train_no_dup[,-c(1,2)]
str(train_no_id)
#View(train_no_id)
#summary(train_no_id)
cat_col_train <- colnames(train_no_id)[c(3,6,7,8)]

library(caret)
library(dplyr)

#creating dummy data for categorucal variables
dmy_train <- dummyVars(" ~ .", data = train_no_id[,c(3,6,7,8)])
#View(dmy_train)
trsf_train <- data.frame(predict(dmy_train, newdata = train_no_id[,c(3,6,7,8)]))
#View(trsf)

#One hot encoded data
train_one_hot <- cbind(train_no_id[,-c(3,6,7,8)],trsf_train)
#View(train_one_hot)
#summary(train_one_hot)


##### Exploratory Data Analysis #####

# Current Loan Amount bins
summary(train_no_id$Current.Loan.Amount)
train_no_id$Current.Loan.Amount.bins <- cut(train_no_id$Current.Loan.Amount,
                                            c(15000,165000,315000,465000,
                                              615000, 789184))
# Credit Score bins
summary(train_no_id$Credit.Score)
train_no_id$Credit.Score.bins <- cut(train_no_id$Credit.Score, 
                                     c(585, 610, 635, 660, 685,
                                       710, 735, 760))

# Annual Income Bins
train_no_id$Annual.Income.bins <- cut(train_no_id$Annual.Income, c(75000,375000,675000,975000,
                                                                   1275000, 1575000, 1875000, 2893662))

# Current credit balance bins
summary(train_no_id$Current.Credit.Balance)
train_no_id$Current.Credit.Balance.bins <- cut(train_no_id$Current.Credit.Balance,
                                               c(0,100000,200000,300000, 400000,
                                                 500000, 600000, 700000, 800000))

# Remove any NAs after binning
train_no_id <- train_no_id[complete.cases(train_no_id),]

# Subset of people whose loan got charged off
subset_charged_off <- train_no_id[train_no_id$Loan.Status == 'Charged Off',]

### Basic Analysis ###

library(ggplot2)

# Counts per Loan status
ggplot(train_no_id, aes(x=Loan.Status, y=..count..)) +
  geom_bar(aes(fill=Loan.Status)) +
  geom_text(stat='count', aes(label=..count..), vjust=1)+
  xlab('Loan Status')

# Counts per Term 
ggplot(train_no_id, aes(x=Term, y=..count..)) +
  geom_bar(aes(fill=Term)) +
  geom_text(stat='count', aes(label=..count..), vjust=1)+
  xlab('Term')

# Counts per Annual Income range
ggplot(train_no_id, aes(x=Annual.Income.bins, y=..count..)) +
  geom_bar(aes(fill=Annual.Income.bins)) +
  geom_text(stat='count', aes(label=..count..), vjust=1)+
  xlab('Annual Income')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Counts per Credit Score range
# Obvious observation: The higher the credit score, 
# more the chances of getting a loan
ggplot(train_no_id, aes(x=Credit.Score.bins, y=..count..)) +
  geom_bar(aes(fill=Credit.Score.bins)) +
  geom_text(stat='count', aes(label=..count..), vjust=1)+
  xlab('Credit Score Range')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Counts per Years in current job
ggplot(train_no_id, aes(x=Years.in.current.job, y=..count..)) +
  geom_bar(aes(fill=Years.in.current.job)) +
  geom_text(stat='count', aes(label=..count..), vjust=1)+
  xlab('Years in current job')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Counts per Home ownership
ggplot(train_no_id, aes(x=Home.Ownership, y=..count..)) +
  geom_bar(aes(fill=Home.Ownership)) +
  geom_text(stat='count', aes(label=..count..), vjust=1)+
  xlab('Home Ownership')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Counts per purpose
ggplot(train_no_id, aes(x=Purpose, y=..count..)) +
  geom_bar(aes(fill=Purpose)) +
  geom_text(stat='count', aes(label=..count..), vjust=1)+
  xlab('Purpose')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Current Loan Amount

## Current loan amount Range vs count for loan defaulters
ggplot(subset_charged_off, aes(x=Current.Loan.Amount.bins, y=..count..)) +
  geom_bar(aes(fill=Loan.Status)) +
  geom_text(stat='count', aes(label=..count..), vjust=1)+
  xlab('Current Loan Amount range')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ~63%(~8200) loan defaulters are in current-loan-amount range of 165k - 465k
# Higher chance of loan default in this range


### Term Analysis
ggplot(subset_charged_off, aes(x=Term, y=..count..))+
  geom_bar(aes(fill=Loan.Status))+ 
  geom_text(stat='count', aes(label=..count..), vjust=1)+
  xlab('Term')
# This plot denotes that short term loan borrowers are more prone to default loan
# then long term loan borrowers

### Credit Score

## Credit Score Range vs count for loan defaulters
ggplot(subset_charged_off, aes(x=Credit.Score.bins, y=..count..)) +
  geom_bar(aes(fill=Loan.Status)) +
  geom_text(stat='count', aes(label=..count..), vjust=1)+
  xlab('Credit Score range')
# Surprisingly enough ~40% (5391) loan defaulters have credit score 
# between 710-735

### Annual Income analysis

## Annual Income Range vs count for loan defaulters
ggplot(subset_charged_off, aes(x=Annual.Income.bins, y=..count..)) +
  geom_bar(aes(fill=Loan.Status)) +
  geom_text(stat='count', aes(label=..count..), vjust=1)+
  xlab('Annual Income range')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# As we can see, there ~4000 defaulters whose income is between 675k - 975k
# ~3000 defaulters have income between 975k - 1275k
# Hence, almost ~54% (~7000) loan defaulters come in income range of 675k - 1275k

income_lower <- as.numeric(quantile(subset_charged_off$Annual.Income)[2])
income_upper <- as.numeric(quantile(subset_charged_off$Annual.Income)[4])
print(paste0("Income of most of the people, who defaulted, lies between: ",
             income_lower, "-", round(income_upper)))

## Number of Years in current job
ggplot(subset_charged_off, aes(x=Years.in.current.job, y=..count..)) +
  geom_bar(aes(fill=Loan.Status)) +
  geom_text(stat='count', aes(label=..count..), vjust=1)+
  xlab('Years in current job')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Home Ownership
ggplot(subset_charged_off, aes(x=Home.Ownership, y=..count..)) +
  geom_bar(aes(fill=Loan.Status)) +
  geom_text(stat='count', aes(label=..count..), vjust=1)+
  xlab('Home Ownership')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Purpose
ggplot(subset_charged_off, aes(x=Purpose, y=..count..)) +
  geom_bar(aes(fill=Loan.Status)) +
  geom_text(stat='count', aes(label=..count..), vjust=1)+
  xlab('Purpose of a loan') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# most of the loan defaulters have "debt consolidation" as common purpose

## Current credit balance

ggplot(subset_charged_off, aes(x=Current.Credit.Balance.bins, y=..count..)) +
  geom_bar(aes(fill=Loan.Status)) +
  geom_text(stat='count', aes(label=..count..), vjust=1)+
  xlab('Current credit balance range') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Other Analysis ###

# 1) Annual Income correlated with Current Loan Amount
ggplot(train_no_id, aes(x=Annual.Income.bins, y=Current.Loan.Amount, 
                        fill=Annual.Income.bins)) +
  geom_boxplot() + xlab("Annual Income")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2) find the range in which most of the income falls

ggplot(train_no_id, aes(x=Years.in.current.job, y=Annual.Income, 
                        fill=Years.in.current.job)) +
  geom_boxplot() + xlab("Years in current job")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3) Current loan amount vs Credit score
ggplot(train_no_id, aes(x=Credit.Score.bins, y=Current.Loan.Amount, 
                        fill=Credit.Score.bins)) +
  geom_boxplot() + xlab("Credit Score range")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4) More people with less income
ggplot(train_no_id, aes(x=Annual.Income.bins, color=Annual.Income.bins)) + 
  geom_density() + theme(axis.text.x = element_text(angle = 45, hjust = 1))




#Removig co-linear features
train_y = train_one_hot$Loan.Status
train_x = train_one_hot[,-c(1)]
reduced_Data = cor(train_x)
hc = findCorrelation(reduced_Data, cutoff=0.8) # putt any value as a "cutoff" 
hc = sort(hc)
train_one_hot_reduced = train_x[,-c(hc)]
train_one_hot_reduced$Loan.Status = train_y
#View(train_one_hot_reduced)

library(dplyr)
#removing perfectly correlated columns
df <- select(train_one_hot_reduced, -c(Years.in.current.job.9.years, Purpose.wedding))

levels(df$Loan.Status)[levels(df$Loan.Status)=="Fully Paid"] <- 1
levels(df$Loan.Status)[levels(df$Loan.Status)=="Charged Off"] <- 0
#View(df)

train_df <- df[1:37567,]
test_df <- df[37568:46958,]

#View(test_df)

#Logistic Regression
model <- glm(Loan.Status ~.,family=binomial(link='logit'),data=train_df)
print(model)
summary(model)

anova(model, test="Chisq")

fitted.results <- predict(model,newdata=test_df[,-c(41)],type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

#confusionMatrix(fitted.results != test_df$Loan.Status)

misClasificError <- mean(fitted.results != test_df$Loan.Status)
print(paste('Accuracy',1-misClasificError))
#"ACCURACY 0.672239378127995"

#kNN Algorithm............................
#install.packages('class')
library(class)

data_test_pred <- knn(train = train_df, test = test_df,cl = train_df$Loan.Status,k = 9)

table(data_test_pred,test_df$Loan.Status)
confusionMatrix(data_test_pred,test_df$Loan.Status)
# ACCURACY: 0.6561

#Naive Bayes Algorithm....................

library(e1071)
#Fitting the Naive Bayes model
Naive_Bayes_Model=naiveBayes(Loan.Status ~., data=train_df)
#What does the model say? Print the model summary
Naive_Bayes_Model

#Prediction on the dataset
NB_Predictions=predict(Naive_Bayes_Model,test_df)
#Confusion matrix to check accuracy
table(NB_Predictions, test_df$Loan.Status)
confusionMatrix(NB_Predictions, test_df$Loan.Status)
# ACCURACY : 0.6674

#Random Forest Algorithm
#install.packages("randomForest")
library(randomForest)

model1 <- randomForest(Loan.Status ~ ., data = train_df, importance = TRUE)
model1

model2 <- randomForest(Loan.Status ~ ., data = train_df, ntree = 500, mtry = 6, importance = TRUE)
model2

# Predicting on test set
predTest <- predict(model1, test_df, type = "class")
# Checking classification accuracy
confusionMatrix(predTest, test_df$Loan.Status)

#install.packages("gbm")
library(gbm)
set.seed(1)
boost.loan=gbm(Loan.Status~.,data=train_df,distribution="gaussian",n.trees=500,interaction.depth=4,cv.folds = 10)
summary(boost.loan)
