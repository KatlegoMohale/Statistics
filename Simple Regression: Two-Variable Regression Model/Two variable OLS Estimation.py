#!/usr/bin/env python
# coding: utf-8

# In[2]:


import pandas
import statsmodels.api as sm


# In[12]:


df = pandas.read_csv("C:/Users/User/Desktop/datasets/education-vs-wages.csv")
X = df['Years of Schooling']
Y = df['Hourly Wage']

X = sm.add_constant(X)
model = sm.OLS(Y, X).fit()
summary = model.summary()
print(summary)


# In[ ]:




