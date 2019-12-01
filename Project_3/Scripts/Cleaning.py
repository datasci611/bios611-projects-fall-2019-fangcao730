#!/usr/bin/env python
# coding: utf-8

# In[55]:


import pandas as pd
import numpy as np
from datetime import date
#reading in data files 
base=pd.read_csv("https://raw.githubusercontent.com/datasci611/bios611-projects-fall-2019-fangcao730/master/Project_3/Data/CLIENT_191102.tsv", sep='\t')
insurance=pd.read_csv("https://raw.githubusercontent.com/datasci611/bios611-projects-fall-2019-fangcao730/master/Project_3/Data/HEALTH_INS_ENTRY_191102.tsv", sep='\t')
entry_exit=pd.read_csv("https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project2_2019/ENTRY_EXIT_191102.tsv", sep='\t')
disable=pd.read_csv("https://raw.githubusercontent.com/datasci611/bios611-projects-fall-2019-fangcao730/master/Project_3/Data/DISABILITY_ENTRY_191102.tsv", sep ='\t')
wellness=pd.read_csv("https://raw.githubusercontent.com/datasci611/bios611-projects-fall-2019-fangcao730/master/Project_3/Data/VI_SPDAT_V1_191102.tsv",sep='\t')
UDES=pd.read_csv("https://raw.githubusercontent.com/datasci611/bios611-projects-fall-2019-fangcao730/master/Project_3/Data/EE_UDES_191102.tsv", sep='\t')


# In[71]:


#regroup the race 
def race_def (series):
    if (series == 'American Indian or Alaska Native (HUD) '):
        return 'Native'
    if (series == "Black or African American (HUD)" ):
        return 'Black'
    if (series == "Asian (HUD)" ):
        return 'Asian'
    if (series == "White (HUD)" ):
        return 'White'
    if (series == "Native Hawaiian or Other Pacific Islander (HUD)"):
        return 'Pacific'
    else:
        return 'other'
base['Client Primary Race']=base['Client Primary Race'].apply(race_def)
#re-group the type of insurance by government, private or employer 
def typeinsurance (series):
    if (series == 'MEDICARE' or series == 'MEDICAID' or series =="State Children's Health Insurance Program" or series == "State Health Insurance for Adults"):
        return 'government'
    if (series == "Veteran's Administration (VA) Medical Services" or  series== "Indian Health Services Program" ):
        return 'government'
    if (series == "Private Pay Health Insurance" ):
        return 'private'
    if (series == "Health Insurance obtained through COBRA" or "Employer - Provided Health Insurance"):
        return 'employer'
    if (series == 'Other'):
        return 'other'
insurance['Type_insurance']=insurance['Health Insurance Type (Entry)'].apply(typeinsurance)
def coverage (series):
    if (series == 'Yes (HUD)'):
        return 'Yes'
    if (series == "No (HUD)" ):
        return 'No'
    else:
        return 'no data'
UDES['covered_entry']=UDES['Covered by Health Insurance(4376)'].apply(coverage)
baseline_insurance=UDES[['covered_entry', 'EE UID']]


# In[66]:


wellness=wellness.drop(columns=['Client ID', 'Start Date(4111)', 'Client Unique ID', 'EE Provider ID', 'Provider (4110-provider)', 'Date Added (4110-date_added)'])
wellness.groupby('GRAND TOTAL (ADJUSTED FOR v2.0)(4671)').size()


# In[72]:


#select the columns needed to be merged with demographic information 
base_m=pd.DataFrame(base.drop(columns=['EE Provider ID','Client Age at Exit'])).rename(columns={'Client ID': "ID", 'Client Primary Race': "Race", 'Client Ethnicity': "Ethnicity"})
#income_m=income[['Client Unique ID', 'Client ID', 'Income Source (Entry)']].rename(columns={'Client ID': "ID", 'Income source (Entry)': "Income_source"})
entry_exit_m=entry_exit[['Client Unique ID','EE UID', 'Entry Exit Date Added', 'Entry Exit Date Updated','Reason for Leaving', 'Destination']].rename(columns={'Entry Exit Date Added': 'Entry', 'Entry Exit Date Updated': 'Exit', 'Reason for Leaving': 'why_leave'})
disable_m=pd.DataFrame(disable[['EE UID', 'Client ID', 'Disability Type (Entry)']]).rename(columns={'Client ID': "ID",'Disability Type (Entry)': 'Disability'})
#calculate time interval between entry exit dates 
entry_exit_m['day1']= pd.to_datetime(entry_exit_m['Entry'], format='%m/%d/%Y')
entry_exit_m['daylast']= pd.to_datetime(entry_exit_m['Exit'], format='%m/%d/%Y')
entry_exit_m['Interval']=entry_exit_m['daylast']-entry_exit_m['day1']


# In[74]:


insurance=pd.merge(baseline_insurance, insurance, on='EE UID')
insurance.to_csv("insurance.csv")
insurance


# In[9]:


disable_t=pd.merge(disable_m,base_m, on='EE UID')


# In[10]:


#count the number of disabilities for each person
disable_sum=disable_t.groupby('EE UID').nunique('Disability')
new=disable_sum[['Disability']]


# In[59]:


#merge UDES, demographic info and entry exit and drop un-used variables
UDES_m=UDES.drop(columns=['EE Provider ID','Zip Code (of Last Permanent Address, if known)(1932)'])
total=pd.merge(base_m, entry_exit_m, on='EE UID')
df=pd.merge(UDES_m,total,on='EE UID').drop(columns=['Client Unique ID_y', 'day1','daylast'])
#correct the Interval variable format
df['Interval']=df['Interval']/np.timedelta64(1, 'D')
df['Interval_months']=df['Interval']/30
#merge the number of disabilities with the data frame 
df=pd.merge(df, new, on='EE UID')
df.groupby('why_leave').size()
pd.crosstab(df['why_leave'],df['Disability'] , dropna=True)
df


# In[60]:


df.to_csv("total.csv", index=False, header=True)


# In[53]:


#wellness dataset merge
dfw=pd.merge(wellness, df, on='EE UID', how ="left").rename(columns={'GRAND TOTAL (ADJUSTED FOR v2.0)(4671)': 'TotalVi'})
dfw


# In[61]:


def vi (colData):
    if (colData >= 8):
        return 'permanent'
    if (colData<= 7 & colData>=4 ):
        return 'rehousing'
    if (colData <= 3 & colData >=0 ):
        return 'nointervention'
dfw['vi_category']=dfw['TotalVi'].apply(vi)
destination=entry_exit_m[['Destination', 'EE UID']]
dfw=pd.merge(dfw, destination, on='EE UID' )
dfw.to_csv("vi_category.csv", index=False, header=True)


# In[9]:





# In[ ]:




