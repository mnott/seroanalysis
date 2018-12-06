# -*- coding: utf-8 -*-
"""

"""

# -*- coding: utf-8 -*-


import pandas as pd
import numpy as np
from datetime import datetime
import matplotlib.pyplot as plt
from pandasql import sqldf, load_meat, load_births
pysqldf = lambda q: sqldf(q, globals())

#
# Plot the Waste produced
#
# Input: Wastes_August_september.xlsx
#
# Column Date_Time is concatenation of Date and Time
# Column Quantity  is amount of waste discarded
#
wbook = pd.ExcelFile('data/Wastes_August_september_m.xlsx')
sheet = wbook.parse('9000_E06')
cols = ['Date', 'Quantity']
df = pd.DataFrame(data=sheet, columns = cols)
#print(df)
df['Date_Time'] = df['Date_Time'].map(lambda x: datetime.strptime(str(x), "%d.%m.%y %H:%M:%S"))
df.plot(x=cols[0], y=cols[1])


#
# Calculate Waste as sum by Day
#
wbook = pd.ExcelFile('data/Wastes_August_september_m.xlsx')
sheet = wbook.parse('9000_E06')
cols = ['Date', 'Quantity']
df = pd.DataFrame(data=sheet, columns = cols)
qry = "select Date, sum(Quantity)*5 from df group by Date"
dfout = pysqldf(qry)
print(df)
dfout['Date'] = dfout['Date'].map(lambda x: datetime.strptime(str(x), "%Y-%m-%d %H:%M:%S.%f"))
dfout.plot(x='Date', y='sum(Quantity)*5')


#
# Calculate defect count as sum by Day
#
wbook2 = pd.ExcelFile('data/inspection_results_and_defects_August_september.xlsx')
sheet2 = wbook2.parse('Defects')
cols2 = ['Date', 'NUM_DEFECTOS']
df2 = pd.DataFrame(data=sheet2, columns = cols2)
qry2 = "select Date, sum(NUM_DEFECTOS) from df2 group by Date"
dfout2 = pysqldf(qry2)
print(dfout2)
dfout2['Date'] = dfout2['Date'].map(lambda x: datetime.strptime(str(x), "%Y-%m-%d %H:%M:%S.%f"))
dfout2.plot(x='Date', y='sum(NUM_DEFECTOS)')


#
# Join the two data frames
#
df_merged = pd.merge(dfout, dfout2, on='Date', how='inner')
print(df_merged)

#
# Correlate betwen quantity and defects
#
df_merged['sum(Quantity)*5'].corr(df_merged['sum(NUM_DEFECTOS)'])
df_merged.plot(x='Date')

##
# No clear correlation (r=.47) between defect count and waste
##





#
# Calculate defect count as sum by Day
#
wbook3 = pd.ExcelFile('data/inspection_results_and_defects_August_september.xlsx')
sheet3 = wbook3.parse('Inspection results')
cols3 = ['Date', 'Val']
df3 = pd.DataFrame(data=sheet3, columns = cols3)

df4 = df3.copy()
df4 = df4[df4['Val'] == 'R']
df3 = df4.copy()

qry3 = "select Date, count(Val)*100 from df3 group by Date"
dfout3 = pysqldf(qry3)
print(dfout3)
dfout3['Date'] = dfout3['Date'].map(lambda x: datetime.strptime(str(x), "%Y-%m-%d %H:%M:%S.%f"))
dfout3.plot(x='Date', y='count(Val)*100')


#
# Join the two data frames
#
df_merged2 = pd.merge(dfout2, dfout3, on='Date', how='inner')
print(df_merged2)

#
# Correlate betwen quantity and defects
#
df_merged2['sum(NUM_DEFECTOS)'].corr(df_merged2['count(Val)*100'])
df_merged2.plot(x='Date')

##
# No clear correlation (r=.40) between defect count and R's
##


#
# Put in the Waste as well
#
df_merged3 = pd.merge(df_merged2, dfout, on='Date', how='inner')
print(df_merged3)

## See Waste vs. Defects => Low Correlation (44%)
df_merged3.plot(x='Date', y=['sum(Quantity)*5', 'sum(NUM_DEFECTOS)'])
df_merged3['sum(Quantity)*5'].corr(df_merged2['sum(NUM_DEFECTOS)'])


## See Waste vs. R => No Correlation (6%)
df_merged3.plot(x='Date', y=['sum(Quantity)*5', 'count(Val)*100'])
df_merged3['sum(Quantity)*5'].corr(df_merged2['count(Val)*100'])

## See R vs Defects => Low Correlation (40%f)
df_merged3.plot(x='Date', y=['sum(NUM_DEFECTOS)', 'count(Val)*100'])
df_merged3['sum(NUM_DEFECTOS)'].corr(df_merged2['count(Val)*100'])


































#####################################################################
################ Requirement - 1
#####################################################################
# Excel processing - Added Date_Time column in Excel by concating Date and Time
#xls_file_master_org = pd.ExcelFile('data/Wastes_August_september.xlsx')
#xls_file_master_org.sheet_names
#df_wastes = xls_file_master_org.parse('9000_E06')
#columns = ['Date_Time','Quantity']
#df_wastes = pd.DataFrame(data=df_wastes, columns=columns)
#### df_wastes.plot(x=['Date_Time'],y='Quantity')
#df_wastes.plot(x='Date_Time',y='Quantity')
#### GRAPH do not show Date_time values may be due to graphs' width and height

#####################################################################
################ Requirement - 2
#####################################################################
# Excel processing - Added Date_Time column in Excel by concating Date and Time
# Excel processing - deleting rows where Lower Acceptable value and Higher Acceptable value both are 0
#xls_file_master_org1 = pd.ExcelFile('data/output-11-30.xlsx')
#xls_file_master_org1.sheet_names
#df_output = xls_file_master_org1.parse('defects')
#df_output = pd.DataFrame(data=df_output, columns=columns1)
#columns1 = ['Date_Time','Inspection char. Description','Measured value']

#df_inspection_unique = pd.DataFrame(columns=['unique_inspection'])



#currentrownum = 1
#for unique_inspection in df_output['Inspection char. Description'].unique():
#    if str(unique_inspection) != "nan":
#        df_inspection_rows = df_output.loc[(df_output['Inspection char. Description'] == unique_inspection) | (df_output['Inspection char. Description'] == "nan")]
#        print("("+str(currentrownum)+") - Inspection Text --->" + str(unique_inspection))
#        ### df_inspection_rows.plot(x=['Date_Time'],y='Measured value')
#        df_inspection_rows.plot(x='Date_Time',y='Measured value')
#        currentrownum = currentrownum + 1


#####################################################################
################ Requirement - 3
#####################################################################