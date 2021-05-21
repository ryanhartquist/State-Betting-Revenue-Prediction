####Regression Models for Sports Betting State Tax Revenue

#############################################
#=============Read in Libraries=============#
#############################################

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os
import seaborn as sns
import statsmodels.api as sm
from scipy.stats.mstats import zscore
from sklearn.neural_network import MLPRegressor

#For QQ Plot
import scipy.stats as sts

#Correlation p-values
from scipy.stats.stats import pearsonr

#Regression output
from sklearn.linear_model import LinearRegression

#Split data
from sklearn.model_selection import train_test_split
from sklearn import metrics
from sklearn import preprocessing

#Variable dominance
#from dominance_analysis import Dominance

#####################################################
#============Setup the Working Directory============#
#####################################################

#os.chdir(r'C:\Users\Andrew\source\repos\msis5223-pds2-2021spring\project-team-repository-team_project_ma_jb_rh_ak')
# os.chdir(r'C:\Users\Jeff\Documents\MS BAnDS\MSIS 5223 - Data Science Programming II\Project')
os.chdir(r"C:\Users\admin\OneDrive - Oklahoma A and M System\Documents\MSIS 5223 - R-Python II\Group Project")

#############################################
#===============Read in data================#
#############################################

demo_data = pd.read_csv('demographic_data.csv')
state_data = pd.read_csv('state_betting_data.csv')

#############################################
#=====DataFrame Combining/Manipulating======#
#############################################

demo_data = demo_data.rename(columns={'StateName': 'State'})
merge_data_right = pd.merge(left=demo_data, right=state_data,how='right', left_on='State', right_on='State')

##States with adequate data - Arkansas, Colorado, Delaware, Illinois, Indiana, Iowa, Michigan, Mississippi, Nevada, 
## New Jersey, New Hampshire, New York, Oregon, Pennsylvania, Rhode Island, Tennessee, West Virginia

#States with adequate data used to create models
mod_states = ['Arkansas', 'Colorado', 'Delaware', 'Illinois', 'Indiana', 'Iowa', 'Michigan', 'Mississippi',
'New Jersey', 'New Hampshire', 'New York', 'Oregon', 'Pennsylvania', 'Rhode Island', 'Tennessee', 'West Virginia']

#Merging demographic data and state betting data
total_data = merge_data_right[merge_data_right.State.isin(mod_states)]

#Converting Month/Year to categorical
onehot_data = pd.get_dummies(total_data[['Month/Year']], drop_first=True)

#Merging categorical data with main dataframe
total_hot_data = total_data.join(onehot_data)

#Calculating the number of Months a state has had legalized betting at the given Month/Year
total_hot_data['Month/Year'] = pd.to_datetime(total_hot_data['Month/Year'], format='%b-%y')
total_hot_data['MonthsBetting'] = total_hot_data.groupby('State')['Month/Year'].rank(ascending=True)

#Categorical Column names
hot_data_cols = (['Month/Year_Apr-20', 'Month/Year_Aug-18', 'Month/Year_Aug-19',
       'Month/Year_Aug-20', 'Month/Year_Dec-18', 'Month/Year_Dec-19',
       'Month/Year_Dec-20', 'Month/Year_Feb-19', 'Month/Year_Feb-20',
       'Month/Year_Feb-21', 'Month/Year_Jan-19', 'Month/Year_Jan-20',
       'Month/Year_Jan-21', 'Month/Year_Jul-18', 'Month/Year_Jul-19',
       'Month/Year_Jul-20', 'Month/Year_Jun-18', 'Month/Year_Jun-19',
       'Month/Year_Jun-20', 'Month/Year_Mar-19', 'Month/Year_Mar-20',
       'Month/Year_May-19', 'Month/Year_May-20', 'Month/Year_Nov-18',
       'Month/Year_Nov-19', 'Month/Year_Nov-20', 'Month/Year_Oct-18',
       'Month/Year_Oct-19', 'Month/Year_Oct-20', 'Month/Year_Sep-18',
       'Month/Year_Sep-19', 'Month/Year_Sep-20'])

#Selecting predictor variables
xvars = (['p0_18', 'p19_25', 'p26_34', 'p35_54', 'p55_64', 'p65_plus',#1-6
       'Population', 'Median_Household_Income', 'Per_Capita_Income',  #7-9
       'Total_Taxes_2017', 'Per_Capita_Taxes_2017', 'TotalTaxes2018', #10-12
       'PerCapitaTaxes2018', 'TotalTaxes2019', 'PerCapitaTaxes2019', 'Number of casinos', #13-16
       'in-person betting','mobile', 'Month/Year_Apr-20', 'Month/Year_Aug-18', 'Month/Year_Aug-19', #17-21
       'Month/Year_Aug-20', 'Month/Year_Dec-18', 'Month/Year_Dec-19', #22-24
       'Month/Year_Dec-20', 'Month/Year_Feb-19', 'Month/Year_Feb-20', #25-27
       'Month/Year_Feb-21', 'Month/Year_Jan-19', 'Month/Year_Jan-20', #28-30
       'Month/Year_Jan-21', 'Month/Year_Jul-18', 'Month/Year_Jul-19', #31-33
       'Month/Year_Jul-20', 'Month/Year_Jun-18', 'Month/Year_Jun-19', #34-36
       'Month/Year_Jun-20', 'Month/Year_Mar-19', 'Month/Year_Mar-20', #37-39
       'Month/Year_May-19', 'Month/Year_May-20', 'Month/Year_Nov-18', #40-42
       'Month/Year_Nov-19', 'Month/Year_Nov-20', 'Month/Year_Oct-18', #43-45
       'Month/Year_Oct-19', 'Month/Year_Oct-20', 'Month/Year_Sep-18', #46-48
       'Month/Year_Sep-19', 'Month/Year_Sep-20', 'MonthsBetting'])    #49-51

#################################################
#================Data Splitting=================#
#################################################

##Set percents for T/V/T
train_ratio = 0.75
validation_ratio = 0.15
test_ratio = 0.10

#1st Split into Train/Test Data
data_train, data_test, y_train, y_test = train_test_split(total_hot_data[xvars], 
                                                                      total_hot_data['Bet Revenue'], 
                                                                      test_size=1-train_ratio,
                                                                      random_state= 1234)

#2nd Split into Validate/Test Data
data_val, data_test, y_val, y_test = train_test_split(data_test, y_test, 
                                                      test_size=test_ratio/(test_ratio + validation_ratio),
                                                      random_state = 1234)

#################################################
#=============Descriptive Analysis==============#
#################################################

#Train/Validate/Test descriptive statistics
data_train[xvars].mean()
y_train.mean()
data_test[xvars].mean()
y_test.mean()
data_val[xvars].mean()
y_val.mean()
data_train[xvars].median()
y_train.median()
data_test[xvars].median()
y_test.median()
data_val[xvars].median()
y_val.median()
data_train[xvars].std()
y_train.std()
data_test[xvars].std()
y_test.std()
data_val[xvars].std()
y_val.std()

##t-test on Bet Revenue
sts.ttest_ind(y_val, y_test, equal_var = False)

##Comparing age breakdown splits
popvar = (['p0_18', 'p19_25', 'p26_34', 'p35_54', 'p55_64', 'p65_plus'])

#Predictor Variable scatterplots with target
pp = sns.pairplot(data=total_hot_data,
                  y_vars=['Bet Revenue'],
                  x_vars= xvars)    

#################################################
#==============Regression Analysis==============#
#=====Building Multiple Regression Model========#
#################################################

#Build Regression model from train data
#Also used to assess homoscedasticity and independence
linreg = sm.OLS(y_train, data_train.astype(float)).fit()
linreg.summary()

#Create model with standardized coeffecients to compare each variables impact on predicting bet revenue
stdcoef = sm.OLS(zscore(y_train), zscore(data_train)).fit()
stdcoef.summary()

#Assess Regression Assumptions

#Homoscedasticity
plt.scatter(stdcoef.fittedvalues, stdcoef.resid)
plt.xlabel('Predicted/Fitted Values')
plt.ylabel('Residual Values')
plt.title('Assessing Homoscedasticity')
plt.show()

#Normality
sm.ProbPlot(linreg.resid).qqplot(line='s')

#Predictions for T/V/T
#Train
linpred_train = linreg.predict(data_train)
plt.scatter(linpred_train, y_train)
metrics.r2_score(y_train, linpred_train)

#Validate
linpred_validate = linreg.predict(data_val)
plt.scatter(linpred_validate, y_val)
metrics.r2_score(y_val, linpred_validate)

#Test
linpred_test = linreg.predict(data_test)
plt.scatter(linpred_test, y_test)
metrics.r2_score(y_test, linpred_test)

#################################################
#==============Neural Network===================#
#################################################

######Scaling/Standardizing all 3 data sets
scaler = preprocessing.StandardScaler()
scaler.fit(data_train[xvars])

#Perform the standardization process
data_train_std = scaler.transform(data_train[xvars])
data_test_std = scaler.transform(data_test[xvars])
data_val_std = scaler.transform(data_val[xvars])

#Build NN model from train data
nnreg1 = MLPRegressor(activation='relu', solver='adam', 
                      hidden_layer_sizes=(100, 100, 100), 
                      max_iter=5000)

#Fit training data to model
nnreg1.fit(data_train_std, y_train)

#Predictions for T/V/T
#Train
nnpred_train = nnreg1.predict(data_train_std)
metrics.mean_absolute_error(y_train, nnpred_train)
metrics.mean_squared_error(y_train, nnpred_train)
metrics.r2_score(y_train, nnpred_train)

#Validate
nnpred_validate = nnreg1.predict(data_val_std)
metrics.mean_absolute_error(y_val, nnpred_validate)
metrics.mean_squared_error(y_val, nnpred_validate)
metrics.r2_score(y_val, nnpred_validate)

#Test
nnpred_test = nnreg1.predict(data_test_std)
metrics.mean_absolute_error(y_test, nnpred_test)
metrics.mean_squared_error(y_test, nnpred_test)
metrics.r2_score(y_test, nnpred_test)

#################################################
#================Scoring Data===================#
#################################################

#Predicting Bet Revenue for Jan 21 for all states that have not legalized sports betting
#Assuming that states would allow in person and mobile betting and have been in operation for 3 months at this point

#Read in score data
score_data = pd.read_csv('score_data.csv')

#Create seperate datasets for the MR and NN models
score_data_mr = score_data[xvars]
data_score_std = scaler.transform(score_data[xvars])

nnpred_score = nnreg1.predict(data_score_std)
linpred_score = linreg.predict(score_data_mr)
score_data['NN Predicted'] = nnpred_score
score_data['MR Predicted'] = linpred_score

#Export Score Data
#score_data.to_csv(r'C:\Users\Andrew\source\repos\msis5223-pds2-2021spring\project-team-repository-team_project_ma_jb_rh_ak\predicted_values.csv')

#####################################
########## XGBOOST ##################
#####################################

from sklearn.preprocessing import LabelEncoder
import xgboost as xgb # XGBoost stuff
from sklearn.metrics import accuracy_score, mean_squared_error, explained_variance_score, r2_score
from sklearn.model_selection import cross_val_score


#### Need to encode slightly differently for xgb

cat_cols = total_hot_data.select_dtypes(include=['category','object'], exclude=None).columns
dict_transformations = {}
dummies = total_hot_data.copy(deep=True)
for col in cat_cols:
    le= LabelEncoder()
    le.fit(total_hot_data[col])
    dict_transformations[col] = le
    dummies[col] = le.transform(dummies[col])

## Train Test Split
X_train, X_test, y_train, y_test = train_test_split(total_hot_data[xvars], total_hot_data['Bet Revenue'],test_size=0.25, random_state=0)

#2nd Split into Validate/Test Data
X_val, X_test, y_val, y_test = train_test_split(X_test, y_test, 
                                                      test_size=test_ratio/(test_ratio + validation_ratio),
                                                      random_state = 1234)
## XGBoost model
xgb_reg = xgb.XGBRegressor(n_estimators = 500,
                           min_split_loss=10,
                           max_depth=15,
                           #n_estimators=100,
                           eta=0.1,
                           subsample=0.9,
                           min_child_weight=9,
                           gamma=0,
                           scale_pos_weight=1,
                           objective='reg:squarederror', 
                           seed=10,
                           eval_metric="error", 
                           use_label_encoder=False)


xgb_reg.fit(X_train, 
            y_train,
            verbose=True)

train_preds_xgb_reg = xgb_reg.predict(X_train)
test_preds_xgb_reg = xgb_reg.predict(X_test)
val_preds_xgb_reg = xgb_reg.predict(X_val)
### Evaluate Results

y_pred = xgb_reg.predict(X_train)
y_pred_test = xgb_reg.predict(X_test)
y_pred_val = xgb_reg.predict(X_val)

score = xgb_reg.score(X_train, y_train)
print("Accuracy - R2 (train):", score)
score_test = xgb_reg.score(X_test, y_test)
print("Accuracy - R2 (test):", score_test)
score_val = xgb_reg.score(X_val, y_val)
print("Accuracy - R2 (val):", score_val)
cv_score = cross_val_score(xgb_reg,X_train,y_train,scoring='explained_variance',cv=10)
print("Cross Validated Score:", cv_score.mean())
expl_var_test = explained_variance_score(y_test, y_pred_test)
print("Explained Variance (test):", expl_var_test)
expl_var_train = explained_variance_score(y_train, y_pred)
print("Explained Variance (train):", expl_var_train)
mse_test = mean_squared_error(y_test, y_pred_test)
mae_test = mean_absolute_error(y_test, y_pred_test)
mse_train = mean_squared_error(y_train, y_pred)
mae_train = mean_absolute_error(y_train, y_pred)
mse_val = mean_squared_error(y_val, y_pred_val)
mae_val = mean_absolute_error(y_val, y_pred_val)
print("MSE:", mse)
print("RMSE:", mse*(1/2.0))


## Predictions plot
x_ax = range(len(y_test))
plt.plot(x_ax, y_test, label="original")
plt.plot(x_ax, y_pred_test, label="predicted")
plt.title("Revenue Test and Prediction")
plt.legend()
plt.show()
