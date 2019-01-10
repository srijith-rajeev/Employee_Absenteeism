#Load libraries
import os
import pandas as pd
import numpy as np
from sklearn.cross_validation import train_test_split

#Set working directory
os.chdir("D:/Data Scientist/Edwisor/Project/Project 2")
df = pd.read_csv("Absenteeism_at_work_Project.csv")

#Getting the number of variables and obervation in the datasets
df.shape

#Getting the column names of the dataset
df.columns

#Getting the describtion of the dataset
df.describe()

#Getting the structure of the dataset
df.info()

#Getting first 5 rows of the dataset
df.head(5)

#workload average per day variable is numeric
df.iloc[:,9]
df.iloc[:,9] = df.iloc[:,9].str.replace(',', '').astype(float)

#missing value analysis
missing_val = pd.DataFrame(df.isnull().sum())
missing_val
#missing values exist


#imputing employee specific data
col = [5,6,7,8,12,13,14,15,16,17,18,19]

df['Transportation expense'].fillna(df.groupby('ID')['Transportation expense'].transform('median'), inplace=True) 
df['Distance from Residence to Work'].fillna(df.groupby('ID')['Distance from Residence to Work'].transform('median'), inplace=True) 
df['Service time'].fillna(df.groupby('ID')['Service time'].transform('median'), inplace=True) 
df['Age'].fillna(df.groupby('ID')['Age'].transform('median'), inplace=True) 
df['Education'].fillna(df.groupby('ID')['Education'].transform('median'), inplace=True) 
df['Son'].fillna(df.groupby('ID')['Son'].transform('median'), inplace=True) 
df['Social drinker'].fillna(df.groupby('ID')['Social drinker'].transform('median'), inplace=True) 
df['Social smoker'].fillna(df.groupby('ID')['Social smoker'].transform('median'), inplace=True) 
df['Pet'].fillna(df.groupby('ID')['Pet'].transform('median'), inplace=True) 
df['Weight'].fillna(df.groupby('ID')['Weight'].transform('median'), inplace=True) 
df['Height'].fillna(df.groupby('ID')['Height'].transform('median'), inplace=True) 
df['Body mass index'].fillna(df.groupby('ID')['Body mass index'].transform('median'), inplace=True) 
     
#One row of ID index 29 is typo, with ID 28.
# got index with 51 has data mismatch of ID -- if possible add code to find the observation
df.iloc[51,0] = 28

#removing the variable ID as its data is already in other ID specific values
df = df.drop(['ID'], axis=1)

# histogram plot
df.hist(column = 'Transportation expense')
df.hist(column = 'Distance from Residence to Work')
df.hist(column = 'Service time')
df.hist(column = 'Age')
df.hist(column = 'Work load Average/day ')
df.hist(column = 'Hit target')
df.hist(column = 'Son')
df.hist(column = 'Pet')
df.hist(column = 'Weight')
df.hist(column = 'Height')
df.hist(column = 'Body mass index')
df.hist(column = 'Absenteeism time in hours')


#furhter missing value imputing done in R in different way. Check which is better
df['Month of absence'].replace([0], np.nan, inplace=True)
#impute missig data
df['Reason for absence'].fillna(df['Reason for absence'].mode()[0], inplace=True) 
df['Month of absence'].fillna(df['Month of absence'].mode()[0], inplace=True) 
df['Work load Average/day '].fillna(df['Work load Average/day '].mode()[0], inplace=True) 
df['Hit target'].fillna(df['Hit target'].mode()[0], inplace=True) 
df['Disciplinary failure'].fillna(df['Disciplinary failure'].mode()[0], inplace=True)
df['Absenteeism time in hours'].fillna(df['Absenteeism time in hours'].mode()[0], inplace=True) 


# creating dummy variables to convert categorical into numeric values
catvar = ['Reason for absence','Month of absence','Day of the week','Seasons','Disciplinary failure'
          ,'Education','Social drinker','Social smoker']
df[catvar] = df[catvar].astype('object')
dummies = pd.get_dummies(df[catvar], prefix= catvar, drop_first=True)
df.drop(catvar, axis=1, inplace = True)
df = pd.concat([df,dummies], axis =1 )


# is scaling required for this data?

#checking normality of numeric data


#oulier analysis
numvar = ['Transportation expense',"Distance from Residence to Work","Service time",
                   "Age","Work load Average/day ","Hit target","Son",
                   "Pet","Weight","Height","Body mass index","Absenteeism time in hours"]

# ## BoxPlots - Distribution and Outlier Check
%matplotlib inline  

import matplotlib.pyplot as plt
for var in numvar:
    plt.boxplot(df[var])
    plt.title(var)
    plt.show()
    
#outliers of employee specific data not removed.
outvar = ["Work load Average/day ","Hit target","Absenteeism time in hours"]    
for var in outvar:
    q75, q25 = np.percentile(df[var], [75 ,25])
    #Calculate IQR
    iqr = q75 - q25
    #Calculate inner and outer fence
    minimum = q25 - (iqr*1.5)
    maximum = q75 + (iqr*1.5)
    #Replace with NA
    df.loc[df[var] < minimum, [var]] = np.nan
    df.loc[df[var] > maximum, [var]] = np.nan
    
#Calculate missing value
missing_val = pd.DataFrame(df.isnull().sum())
missing_val
#removing outlier observations
df = df.dropna(axis = 0)
df = df.reset_index(drop=True)
 
# coorelation between numeric variables
plt.matshow(df[numvar].corr())
df[numvar].corr() 

df[numvar].corr().unstack().sort_values().drop_duplicates()
#body mass index and weight are highly correlated with corr 0.918. So dropping weight
df = df.drop(['Weight'], axis=1)

df.info()

# anova and further variable selection will be done after fitting the model

#Simple Random Sampling
train,testAndcv = train_test_split(df,test_size = 0.4, random_state = 0)
test, cv = train_test_split(testAndcv,test_size =0.5 )

#function MSE
def MSE(y, yhat):
    mse = np.mean(((y - yhat)**2)/len(yhat))
    return mse  

#adding intercept column for linear regression
df.insert(loc=0, column='intercept', value=1)
train.insert(loc=0, column='intercept', value=1)
test.insert(loc=0, column='intercept', value=1)
cv.insert(loc=0, column='intercept', value=1)

#independentVar = ['intercept','instant','yr','holiday','atemp','hum','windspeed','Fall','Summer','Winter','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','Mon','Tue','Wed','Thu','Fri','Sat','Mist','Light']
#train_independentVar = pd.DataFrame(train, columns= independentVar)
#test_independentVar = pd.DataFrame(test, columns= independentVar)
import statsmodels.api as sm

#check consistency with R. linreg in whole datset df
linreg = sm.OLS(df.loc[:,'Absenteeism time in hours'], df.loc[:, df.columns != 'Absenteeism time in hours']).fit()
linreg.summary()


#Linear Regression 
linreg = sm.OLS(train.loc[:,'Absenteeism time in hours'], train.loc[:, train.columns != 'Absenteeism time in hours']).fit()
linreg.summary()
#r squared value is 0.434
#not all variables are significant
y_linreg = linreg.predict(train.loc[:, train.columns != 'Absenteeism time in hours'])
#MSE 
MSE(train.loc[:,'Absenteeism time in hours'], y_linreg)
#MSE 0.017

# Predicting the Test set results  - dot it and find MSE
y_linreg = linreg.predict(cv.loc[:, cv.columns != 'Absenteeism time in hours'])
#MSE
MSE(cv.loc[:,'Absenteeism time in hours'], y_linreg)
#MSE 0.058

y_linreg = linreg.predict(test.loc[:, test.columns != 'Absenteeism time in hours'])
#MSE
MSE(test.loc[:,'Absenteeism time in hours'], y_linreg)
#MSE 0.074


#Decision Tree
from sklearn.tree import DecisionTreeRegressor
#intercept is not needed in dataset for decision tree and random forest
df = df.drop(['intercept'], axis=1)
train = train.drop(['intercept'], axis=1)
test = test.drop(['intercept'], axis=1)
cv = cv.drop(['intercept'], axis=1)
# decision tree 
dectre = DecisionTreeRegressor(max_depth=2,random_state=0).fit(train.loc[:, train.columns != 'Absenteeism time in hours'], train.loc[:,'Absenteeism time in hours'])
y_dectre = dectre.predict(train.loc[:, train.columns != 'Absenteeism time in hours'])
#MSE
MSE(train.loc[:,'Absenteeism time in hours'], y_dectre)
#MSE 0.02
y_dectre = dectre.predict(cv.loc[:, cv.columns != 'Absenteeism time in hours'])
#MSE
MSE(cv.loc[:,'Absenteeism time in hours'], y_dectre)
#MSE 0.07

y_dectre = dectre.predict(test.loc[:, test.columns != 'Absenteeism time in hours'])
#MSE
MSE(test.loc[:,'Absenteeism time in hours'], y_dectre)
#MSE 0.097

#random forest
from sklearn.ensemble import RandomForestRegressor
randfor = RandomForestRegressor(n_estimators = 10, random_state = 0)
randfor.fit(train.loc[:, train.columns != 'Absenteeism time in hours'], train.loc[:,'Absenteeism time in hours'])
y_randfor = randfor.predict(train.loc[:, train.columns != 'Absenteeism time in hours'])
#MSE
MSE(train.loc[:,'Absenteeism time in hours'], y_randfor)
#MSE 0.005
y_randfor = randfor.predict(cv.loc[:, cv.columns != 'Absenteeism time in hours'])
#MSE
MSE(cv.loc[:,'Absenteeism time in hours'], y_randfor)
#MSE 0.063

y_randfor = randfor.predict(test.loc[:, test.columns != 'Absenteeism time in hours'])
#MSE
MSE(test.loc[:,'Absenteeism time in hours'], y_randfor)
#MSE 0.087

#----applying regularization
#scaling
from sklearn.preprocessing import MinMaxScaler
scaler = MinMaxScaler()
df_scaled = scaler.fit_transform(df.loc[:, df.columns != 'Absenteeism time in hours'])
df_scaled = pd.DataFrame(df_scaled)
df_scaled.columns = df.loc[:, df.columns != 'Absenteeism time in hours'].columns
df_scaled.insert(loc=0, column='Absenteeism time in hours', value=df.loc[:, 'Absenteeism time in hours'])


#Simple Random Sampling for trian and test data
train_scaled,testAndcv_scaled = train_test_split(df_scaled,test_size = 0.4, random_state = 0)
test_scaled, cv_scaled = train_test_split(testAndcv_scaled,test_size =0.5 )
train_scaled.insert(loc=0, column='intercept', value=1)
test_scaled.insert(loc=0, column='intercept', value=1)
cv_scaled.insert(loc=0, column='intercept', value=1)


#regularization
from sklearn.linear_model import Lasso
from sklearn.linear_model import Ridge
from sklearn.linear_model import ElasticNet

lm_lasso = Lasso()
lm_lasso.fit(train_scaled.loc[:, train_scaled.columns != 'Absenteeism time in hours'], train_scaled.loc[:,'Absenteeism time in hours'])
#model evaluation graphical
plt.figure(figsize = (15,15))
ft_importances_lm = pd.Series(lm_lasso.coef_, index = df_scaled.columns)
ft_importances_lm.plot(kind = 'barh')
plt.show();

#Rsquared value
np.round(lm_lasso.score(train_scaled.loc[:, train_scaled.columns != 'Absenteeism time in hours'], train_scaled.loc[:,'Absenteeism time in hours'])*100,2)
#0.0

#find mse for all train, test and cv

lm_ridge = Ridge()
lm_ridge.fit(train_scaled.loc[:, train_scaled.columns != 'Absenteeism time in hours'], train_scaled.loc[:,'Absenteeism time in hours'])
#model evaluation graphical
plt.figure(figsize = (15,15))
ft_importances_lm = pd.Series(lm_ridge.coef_, index = df_scaled.columns)
ft_importances_lm.plot(kind = 'barh')
plt.show();

#Rsquared value
np.round(lm_ridge.score(train_scaled.loc[:, train_scaled.columns != 'Absenteeism time in hours'], train_scaled.loc[:,'Absenteeism time in hours']),2)
#0.42
y_linreg = lm_ridge.predict(train_scaled.loc[:, train_scaled.columns != 'Absenteeism time in hours'])
#MSE
MSE(train_scaled.loc[:,'Absenteeism time in hours'], y_linreg)
#MSE 0.016
y_linreg = lm_ridge.predict(cv_scaled.loc[:, cv_scaled.columns != 'Absenteeism time in hours'])
#MSE 
MSE(cv_scaled.loc[:,'Absenteeism time in hours'], y_linreg)
#MSE 0.063
y_linreg = lm_ridge.predict(test_scaled.loc[:, test_scaled.columns != 'Absenteeism time in hours'])
#MSE 
MSE(test_scaled.loc[:,'Absenteeism time in hours'], y_linreg)
#MSE 0.061

lm_elastic = ElasticNet()
lm_elastic.fit(train_scaled.loc[:, train_scaled.columns != 'Absenteeism time in hours'], train_scaled.loc[:,'Absenteeism time in hours'])
#model evaluation graphical
plt.figure(figsize = (15,15))
ft_importances_lm = pd.Series(lm_elastic.coef_, index = df_scaled.columns)
ft_importances_lm.plot(kind = 'barh')
plt.show();

#Rsquared value
np.round(lm_elastic.score(train_scaled.loc[:, train_scaled.columns != 'Absenteeism time in hours'], train_scaled.loc[:,'Absenteeism time in hours']),2)
#0.0

#find mse for all train, test and cv
