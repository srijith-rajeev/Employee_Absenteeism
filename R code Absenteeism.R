#remove all the objects stored
rm(list=ls())

#set current working directory
setwd("D:/Data Scientist/Edwisor/Project/Project 2")

#Current working directory
getwd()

##Load data in R
#reading csv
df = read.csv("Absenteeism_at_work_Project.csv",header = T)


#Getting the number of variables and obervation in the datasets
dim(df)

#Getting the column names of the dataset
colnames(df)

#Getting the structure of the dataset
str(df)

#Remove comma in column 10
library(stringr)
df[,10] <- as.numeric(str_replace(as.character(df[,10]),",",""))
####################################################################


#Getting first 5 rows of the dataset
head(df, 5)


#Summary of data
summary(df)
for (i in 1:21)
{
  print(summary(df[i]))
}
summary(df[1:4])
summary(df[5:8])
summary(df[9:12])
summary(df[13:16])
summary(df[17:20])
summary(df[21])
print('Data Distribution Column Wise')
for (i in 1:21)
{
  print(colnames(df[i]))
  print(table(df[i]))
  
}

table(df$ID)
table(df$`Reason for absence`)
table(df$`Month of absence`)

#missing value analysis
missing_val = data.frame(apply(df,2,function(x){sum(is.na(x))}))
unique(missing_val)
#missing values exist.

#employee specific data
df1=df[order(df$ID),][c(1,6:9,13:20)]
unique(df1)
class(unique(df1[df1$ID == 29,5]))

length(unique(df1[df1$ID == 29,5]))
#check for unique values with NA
max = 0
for(i in 1:36)
{
  for(j in 2:13)
  {
    #print(unique(df1[df1$ID == i,j]))
    if(length(unique(df1[df1$ID == i,j])) > max)
    {
      max = length(unique(df1[df1$ID == i,j]))
      
    }
  }

}
max
# max is 2 means all col values of df1 same with for each column

for(i in 1:36)
{
  for(j in c(6:9,13:20))
  {
    x = unlist(df[df$ID == i,j])
    x[which(is.na(x))] = median(x[which(!is.na(x))])
    df[df$ID == i,j] = x
  }
  
}

# SPLIT , LAPPLY code
# medimpute1 = function(x){
#   x = unlist(x)
#   x[which(is.na(x))] = median(x[which(!is.na(x))])
#   x
#   
# }
# 
# medimpute = function(df){
#   for(k in 1:ncol(df))
#     df[,k]=medimpute1(df[,k])
#   df
# }
# 
# df3=list.rbind(lapply(split(df1,df1$ID), medimpute))
#all ID based unique missing data imputed.

#chcek ID specific value columns are correct
max = 0
for(i in 1:36)
{
  for(j in c(6:9,13:20))
  {
    #print(unique(df1[df1$ID == i,j]))
    if(length(unique(df[df$ID == i,j]))>max)
    {
      max = length(unique(df[df$ID == i,j]))
      if(length(length(df[df$ID == i,j]))>1)
      {
        print(i)
      }
    }
  }
  
}
max

#One row of ID index 29 is typo, with ID 28.
length(unique(df[df$ID == 29,7]))
df$slno = 1:nrow(df) 
df[df$ID == 29, c(6:9,22)]
# got slno with 52 has data mismatch of ID
df[df$slno == 52,1] = 28

#removing col slno
df = df[-22]



str(df)
# hist(df$ID)
# hist(df$Reason.for.absence)
# hist(df$Month.of.absence)
# hist(df$Day.of.the.week)
# hist(df$Seasons)
hist(df$Transportation.expense)
hist(df$Distance.from.Residence.to.Work)
hist(df$Service.time)
hist(df$Age)
hist(df$Work.load.Average.day)
hist(df$Hit.target)
#hist(df$Disciplinary.failure)
#hist(df$Education)
hist(df$Son)
#hist(df$Social.drinker)
#hist(df$Social.smoker)
hist(df$Pet)
hist(df$Weight)
hist(df$Height)
hist(df$Body.mass.index)
hist(df$Absenteeism.time.in.hours)


#missing value analysis
missing_val = data.frame(apply(df,2,function(x){sum(is.na(x))}))
unique(missing_val)
#missing values exist for numeric data.
#impute or ignore missing value ?

#mode input missing values for each ID, if multiple mode, impute the first mode 
###########################################################
# functions
Mode <- function(x){
  t <- table(x)
  as.numeric(names(which(t == max(t))))[1]}

ModeImpute <- function(x){
  ModeImpute1 <- function(x){
    x[which(is.na(x))] <- Mode(na.omit(x))
    x}
  if(class(x) == "numeric")
    y <- ModeImpute1(x)
  if(class(x) == "data.frame"){
    for(j in 1:ncol(x))
      x[,j] <- ModeImpute1(x[,j])
    y <- x}
  y}
###################################################################
library(rlist)
#df <- list.rbind(lapply(split(df,df$ID),ModeImpute))
##knn impute Month of Absence
# df$Month.of.absence[which(df$Month.of.absence == 0)] = NA
# library(DMwR)
# df = knnImputation(df, k = 3)

#another way as did in python = mode impute all missing values
df <- ModeImpute(df)

df$Month.of.absence = as.integer(df$Month.of.absence)

#Convert categorical variables as factors

df$ID = factor(df$ID)
#Reason for absence as value 0 , which is not specified.
df$Reason.for.absence = factor(df$Reason.for.absence)
#Month of absence has zero month. NO meaning for that
df$Month.of.absence = factor(df$Month.of.absence, labels = c("Jan", "Feb","Mar", "Apr","May", "Jun",
                                     "Jul", "Aug","Sep", "Oct","Nov", "Dec"))
df$Day.of.the.week = factor(df$Day.of.the.week, labels = c("Mon", "Tue", "Wed", "Thu", "Fri"))
df$Seasons = factor(df$Seasons,labels = c('Summer','Autumn','Winter','Spring'))
df$Disciplinary.failure = factor(df$Disciplinary.failure, labels = c("No", "Yes"))
df$Education = factor(df$Education, labels = c("HighSchool", "Graduate", "PostGraduate", "Doctor"))
df$Social.drinker = factor(df$Social.drinker, labels = c("No", "Yes"))
df$Social.smoker = factor(df$Social.smoker, labels = c("No", "Yes"))
str(df)


#removing ID as other variables represienting ID is known
df = subset(df, select=-ID)


#outier analysis
# ## BoxPlots - Distribution and Outlier Check
cnames_numeric = c("Transportation.expense","Distance.from.Residence.to.Work","Service.time",
                   "Age","Work.load.Average.day","Hit.target","Son",
                   "Pet","Weight","Height","Body.mass.index","Absenteeism.time.in.hours")

library(ggplot2)
library(gridExtra)
for (i in 1:length(cnames_numeric))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames_numeric[i])), data = subset(df))+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames_numeric[i])+
           ggtitle(paste("Box plot of ",cnames_numeric[i])))
  
}
#outliers exist for ID specific variables. But ignored
print(gn1)
print(gn2)
print(gn3)
print(gn4)
print(gn5)
#outlier for Work load Avg day
print(gn6)
#outlier for Hit target
print(gn7)
print(gn8)
print(gn9)
print(gn10)
print(gn11)
print(gn12)



#outliers of employee specific data not removed.
#Remove outliers using boxplot method
# outliers to be removed for Work load Average/day ,Hit target, Absenteeism time in hours
val =df$Work.load.Average.day[df$Work.load.Average.day %in% boxplot.stats(df$Work.load.Average.day)$out]
cat('No of outliers is ' , length(val))
df$Work.load.Average.day[df$Work.load.Average.day %in% val] = NA

val =df$Hit.target[df$Hit.target %in% boxplot.stats(df$Hit.target)$out]
cat('No of outliers is ' , length(val))
df$Hit.target[df$Hit.target %in% val] = NA

val =df$Absenteeism.time.in.hours[df$Absenteeism.time.in.hours %in% boxplot.stats(df$Absenteeism.time.in.hours)$out]
cat('No of outliers is ' , length(val))
df$Absenteeism.time.in.hours[df$Absenteeism.time.in.hours %in% val] = NA
 
apply(df,2,function(x){sum(is.na(x))})

#removing outlier observations
df = na.omit(df)
rownames(df) = 1:nrow(df)


# coorelation between numeric variables
cor(df[cnames_numeric]) 
#install.packages('corrplot')
library(corrplot)

corrplot(cor(df[cnames_numeric]) , method="color", title = '')
library(usdm)
vif(df[,cnames_numeric])

vifcor(df[,cnames_numeric], th = 0.8)
# variable weight is is have collinearity with body mass index.
#so removing weight
df = subset(df, select=-Weight)


#Simple Random Sampling
set.seed(123)
train_index = sample(1:nrow(df), 0.6 * nrow(df))
train = df[train_index,]
testAndcv = df[-train_index,]
rownames(testAndcv) = 1:nrow(testAndcv)
test_index = sample(1:nrow(testAndcv), 0.5 * nrow(testAndcv))
test = testAndcv[test_index,]
cv = testAndcv[-test_index,]

# errors matrices  -  MAPE and MSE
#MAPE
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)*100/y))
}
#MSE
MSE = function(y, yhat){
  mean(((y - yhat)^2)/length(y))
}


#check consistency with python. linreg in whole datset df
lin_regressor = lm(formula = Absenteeism.time.in.hours ~ . , data = df)
summary(lin_regressor)

#fitting linear regression
#lin_regressor = step(lm(formula = Absenteeism.time.in.hours ~ . , data = train))
lin_regressor = lm(formula = Absenteeism.time.in.hours ~ . , data = train)
summary(lin_regressor)
#r squared is 0.502
anova(lin_regressor)
#some variables are significant
lin_regressor$coefficients

#applying step
lin_regressor = step(lm(formula = Absenteeism.time.in.hours ~ . , data = train))
summary(lin_regressor)
anova(lin_regressor)
lin_regressor$coefficients

#significant variables are Reason for absence, Age, Son, Social drinker, social smoker, Pet, Body mass index, Work load average per day.
y_linreg = predict(lin_regressor, newdata = train)

#MSE
MSE(train$Absenteeism.time.in.hours, y_linreg)
#MSE 0.014

# Predicting the cv and Test set results
y_linreg = predict(lin_regressor, newdata = cv)

#MSE
MSE(cv$Absenteeism.time.in.hours, y_linreg)
#MSE 0.084

y_linreg = predict(lin_regressor, newdata = test)

#MSE
MSE(test$Absenteeism.time.in.hours, y_linreg)
#MSE 0.059


library(rpart)

# decision tree
dectre = rpart(Absenteeism.time.in.hours ~ . , data = train, method = "anova")

y_dectre = predict(dectre, train[,1:18])
#MSE
MSE(train$Absenteeism.time.in.hours, y_dectre)
#MSE 0.013
plot(dectre)
text(dectre)

y_dectre = predict(dectre, cv[,1:18])
#MSE
MSE(cv$Absenteeism.time.in.hours, y_dectre)
#MSE 0.093

y_dectre = predict(dectre, test[,1:18])
#MSE
MSE(test$Absenteeism.time.in.hours, y_dectre)
#MSE 0.064

#random forest
set.seed(1234)
randfor = randomForest::randomForest(x = train[1:18],
                                     y = train$Absenteeism.time.in.hours,
                                     ntree = 19)
y_randfor = predict(randfor, train[,1:18])
plot(randfor)
#MSE
MSE(train$Absenteeism.time.in.hours, y_randfor)
#MSE 0.0048

y_randfor = predict(randfor, cv[,1:18])
plot(randfor)
#MSE
MSE(cv$Absenteeism.time.in.hours, y_randfor)
#MSE 0.076

y_randfor = predict(randfor, test[,1:18])
plot(randfor)
#MSE
MSE(test$Absenteeism.time.in.hours, y_randfor)
#MSE 0.0566


#month wise projection
#reading csv
df = read.csv("Absenteeism_at_work_Project.csv",header = T)
df = na.omit(df[which(df$Month.of.absence!=0),c(3,21)])

monthwise = with(df,tapply(Absenteeism.time.in.hours,Month.of.absence,sum))
barplot(monthwise,ylab = "Total Absenteeism Hours", xlab = "Month")



