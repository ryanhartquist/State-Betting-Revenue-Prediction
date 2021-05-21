import pandas as pd 
from pandas.api.types import is_string_dtype, is_numeric_dtype
pd.set_option('display.max_rows', 500)
pd.set_option('display.max_columns', 500)
import numpy as np
import matplotlib.pyplot as plt
import os 
import scipy.stats as sts
import seaborn as sns 
import statsmodels.api as sm
from matplotlib import ticker as ticker
#import shap


os.chdir(r"C:\Users\admin\OneDrive - Oklahoma A and M System\Documents\MSIS 5223 - R-Python II\Group Project")

state_betting = pd.read_table('state_betting_data.txt', sep=',')
demographic =  pd.read_table('demographic_data.txt', sep=',')

state_betting.info()
demographic.info()

#get state betting info
state_betting.head()
state_betting.describe()
state_betting.info()

#get demographic info
demographic.head()
demographic.describe()
demographic.info()

#Add Months Betting
state_betting['Month/Year'] = pd.to_datetime(state_betting['Month/Year'], format='%b-%y')
state_betting['Months_Betting'] = state_betting.groupby('State')['Month/Year'].rank(ascending=True)

# correlation matrix - State Betting
sns.heatmap(state_betting.corr(),cmap = "GnBu", annot = True)

#variable distributions - state_betting
for column in state_betting:
    plt.figure(column)
    plt.title(column)
    if is_numeric_dtype(state_betting[column]):
        state_betting[column].plot(kind = "hist")
    elif is_string_dtype(state_betting[column]):
        #show only top 10 value count for cat variables
        state_betting[column].value_counts()[:10].plot(kind= "bar")


# pair plots - state_betting
sns.pairplot(state_betting, height = 2.5)



### Data prep for categorical vs numerical

num_list = []
cat_list = []

for column in state_betting:
    if is_numeric_dtype(state_betting[column]):
        num_list.append(column)
    elif is_string_dtype(state_betting[column]):
        cat_list.append(column)


# Categorical vs Categorical - State Betting
for i in range(0,len(cat_list)):
    primary_cat = cat_list[i]
    for j in range(0,len(cat_list)):
        secondary_cat = cat_list[j]
        if secondary_cat != primary_cat:
            plt.figure(figsize = (15,15))
            chart = sns.countplot(
                data = state_betting,
                x=primary_cat,
                hue=secondary_cat,
                palette = 'GnBu',
                order=state_betting[primary_cat].value_counts().iloc[:10].index #show only top 10
            )
        

#Categorical vs Numerical - State Betting

for i in range(0, len(cat_list)):
    cat = cat_list[i]
    for j in range(0,len(num_list)):
        num = num_list[j]
        plt.figure (figsize = (15,15))
        sns.boxplot(x = cat, y = num, data = state_betting, palette = "GnBu")

# correlation matrix  - Demographic Data
sns.heatmap(demographic.corr(),cmap = "GnBu", annot = True)


#variable distributions - demographic
for column in demographic:
    plt.figure(column)
    plt.title(column)
    if is_numeric_dtype(demographic[column]):
        demographic[column].plot(kind = "hist")
    elif is_string_dtype(demographic[column]):
        #show only top 10 value count for cat variables
        demographic[column].value_counts()[:10].plot(kind= "bar")
plt.close()

# pair plots - demographic
sns.pairplot(demographic, height = 2.5)


num_list2 = []
cat_list2 = []

for column in demographic:
    if is_numeric_dtype(demographic[column]):
        num_list2.append(column)
    elif is_string_dtype(demographic[column]):
        cat_list2.append(column)


# Categorical vs Categorical  - Demographic Data
for i in range(0,len(cat_list2)):
    primary_cat = cat_list2[i]
    for j in range(0,len(cat_list2)):
        secondary_cat = cat_list2[j]
        if secondary_cat != primary_cat:
            plt.figure(figsize = (15,15))
            chart = sns.countplot(
                data = demographic,
                x=primary_cat,
                hue=secondary_cat,
                palette = 'GnBu',
                order=demographic[primary_cat].value_counts().iloc[:10].index #show only top 10
            )
        
#Categorical vs Numerical - Demographic Data

for i in range(0, len(cat_list2)):
    cat = cat_list2[i]
    for j in range(0,len(num_list2)):
        num = num_list2[j]
        plt.figure (figsize = (15,15))
        sns.boxplot(x = cat, y = num, data = demographic, palette = "GnBu")

### Re-alias data sets. 

state_data = state_betting
demo_data =  demographic



demo_data = demo_data.rename(columns={'StateName': 'State'})
merge_data_right = pd.merge(left=demo_data, right=state_data,how='right', left_on='State', right_on='State')

##States with adequate data - Arkansas, Colorado, Delaware, Illinois, Indiana, Iowa, Michigan, Mississippi, Nevada, 
## New Jersey, New Hampshire, New York, Oregon, Pennsylvania, Rhode Island, Tennessee, West Virginia

mod_states = ['Arkansas', 'Colorado', 'Delaware', 'Illinois', 'Indiana', 'Iowa', 'Michigan', 'Mississippi',
'New Jersey', 'New Hampshire', 'New York', 'Oregon', 'Pennsylvania', 'Rhode Island', 'Tennessee', 'West Virginia']

total_data = merge_data_right[merge_data_right.State.isin(mod_states)]

model_data = demo_data[demo_data.State.isin(mod_states)]

#month/year one-hot data
onehot_data = pd.get_dummies(total_data[['Month/Year']], drop_first=True)

total_hot_data = total_data.join(onehot_data)

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


##Set split ratios
train_ratio = 0.75
validation_ratio = 0.15
test_ratio = 0.10


#Additional Visuals

#function for tick formatting
def reformat_large_tick_values(tick_val, pos):
    """
    Turns large tick values (in the billions, millions and thousands) such as 4500 into 4.5K and also appropriately turns 4000 into 4K (no zero after the decimal).
    """
    if tick_val >= 1000000000:
        val = round(tick_val/1000000000, 1)
        new_tick_format = '{:}B'.format(val)
    elif tick_val >= 1000000:
        val = round(tick_val/1000000, 1)
        new_tick_format = '{:}M'.format(val)
    elif tick_val >= 1000:
        val = round(tick_val/1000, 1)
        new_tick_format = '{:}K'.format(val)
    elif tick_val < 1000:
        new_tick_format = round(tick_val, 1)
    else:
        new_tick_format = tick_val

    # make new_tick_format into a string value
    new_tick_format = str(new_tick_format)
    
    # code below will keep 4.5M as is but change values such as 4.0M to 4M since that zero after the decimal isn't needed
    index_of_decimal = new_tick_format.find(".")
    
    if index_of_decimal != -1:
        value_after_decimal = new_tick_format[index_of_decimal+1]
        if value_after_decimal == "0":
            # remove the 0 after the decimal point since it's not needed
            new_tick_format = new_tick_format[0:index_of_decimal] + new_tick_format[index_of_decimal+2:]
            
    return new_tick_format

#convert Month/year to time
total_data['Month/Year'] = pd.to_datetime(total_data['Month/Year'], format='%b-%y')
total_hot_data['Month/Year'] = pd.to_datetime(total_hot_data['Month/Year'], format='%b-%y')


##### Bet Revenue Growth by State
plt.rcParams.update({'figure.figsize': (17, 3), 'figure.dpi':300})
fig, ax = plt.subplots()
sns.lineplot(data=total_hot_data, x='Month/Year', y='Bet Revenue', hue='State').set_title("Bet Revenue Growth By State - U.S.")
plt.grid(linestyle='-', linewidth=0.3)
ax.tick_params(axis='x', rotation=45)
plt.ticklabel_format(style='plain', axis='y')
ax.yaxis.set_major_formatter(ticker.FuncFormatter(reformat_large_tick_values))
plt.legend(loc='upper right')
plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)

##### Bet Revenue Growth Individual States

State = total_data.State.unique()

for i in State:

    plt.rcParams.update({'figure.figsize': (17, 3), 'figure.dpi':300})
    fig, ax = plt.subplots()
    sns.lineplot(data=total_data.loc[total_data.State.isin([i])], x='Month/Year', y='Bet Revenue').set_title(i)
    plt.grid(linestyle='-', linewidth=0.3)
    ax.tick_params(axis='x', rotation=45)
    ax.yaxis.set_major_formatter(ticker.FuncFormatter(reformat_large_tick_values))

###### Total Bet Revenue 
plt.rcParams.update({'figure.figsize': (17, 3), 'figure.dpi':300})
fig, ax = plt.subplots()
sns.lineplot(data=total_data, x='Month/Year', y='Bet Revenue', estimator=np.sum).set_title("Bet Revenue Growth - U.S. (total)")
plt.grid(linestyle='-', linewidth=0.3)
ax.tick_params(axis='x', rotation=45)
ax.yaxis.set_major_formatter(ticker.FuncFormatter(reformat_large_tick_values))

###### Median Bet Revenue 
plt.rcParams.update({'figure.figsize': (17, 3), 'figure.dpi':300})
fig, ax = plt.subplots()
sns.lineplot(data=total_data, x='Month/Year', y='Bet Revenue', estimator=np.median).set_title("Bet Revenue Growth - U.S. (median)")
plt.grid(linestyle='-', linewidth=0.3)
ax.tick_params(axis='x', rotation=45)
ax.yaxis.set_major_formatter(ticker.FuncFormatter(reformat_large_tick_values))

###### Special box plot for bet revenue vs month/year

plt.figure (figsize = (15,15))
sns.boxplot(x = state_betting['Month/Year'].dt.strftime('%m/%d/%Y'),
            y = "Bet Revenue", 
            data = state_betting, 
            palette = "GnBu",
            order = ['06/01/2018','07/01/2018', '08/01/2018', 
                     '09/01/2018', '10/01/2018','11/01/2018', 
                     '12/01/2018', '01/01/2019', '02/01/2019',
                     '03/01/2019', '04/01/2019', '05/01/2019', 
                     '06/01/2019', '07/01/2019', '08/01/2019', 
                     '09/01/2019', '10/01/2019', '11/01/2019', 
                     '12/01/2019', '01/01/2020', '02/01/2020',
                     '03/01/2020', '04/01/2020', '05/01/2020', 
                     '06/01/2020', '07/01/2020', '08/01/2020', 
                     '09/01/2020', '10/01/2020', '11/01/2020', 
                     '12/01/2020', '01/01/2021', '02/01/2021']
            )
plt.xticks(rotation = 45)
