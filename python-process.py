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

########################################################################
##
## Look at Waste Produced, "R"-evisions, and Defect Counts
##
########################################################################


#
# Calculate Waste as sum by Day
#
wbook = pd.ExcelFile('data/Wastes_August_september_m.xlsx')
sheet = wbook.parse('9000_E06')
cols = ['Date', 'Quantity']
df = pd.DataFrame(data=sheet, columns = cols)
qry = "select Date, sum(Quantity)*5 as Waste from df group by Date"
dfout = pysqldf(qry)
print(df)
dfout['Date'] = dfout['Date'].map(lambda x: datetime.strptime(str(x), "%Y-%m-%d %H:%M:%S.%f"))
plot = dfout.plot(x='Date', y='Waste')
plot.get_figure().savefig('data/01-waste-time.pdf', format='pdf')



#
# Calculate defect count as sum by Day
#
wbook2 = pd.ExcelFile('data/inspection_results_and_defects_August_september.xlsx')
sheet2 = wbook2.parse('Defects')
cols2 = ['Date', 'NUM_DEFECTOS']
df2 = pd.DataFrame(data=sheet2, columns = cols2)
qry2 = "select Date, sum(NUM_DEFECTOS) as DefectCount from df2 group by Date"
dfout2 = pysqldf(qry2)
print(dfout2)
dfout2['Date'] = dfout2['Date'].map(lambda x: datetime.strptime(str(x), "%Y-%m-%d %H:%M:%S.%f"))
plot = dfout2.plot(x='Date', y='DefectCount')
plot.get_figure().savefig('data/02-defects-time.pdf', format='pdf')


#
# Join the two data frames
#
df_merged = pd.merge(dfout, dfout2, on='Date', how='inner')
print(df_merged)

#
# Correlate betwen quantity and defects: Moderate (47%)
#
#
# Correlation Coefficient          Descriptor
#
#   .90 - .99                      Near perfect
#   .70 - .89                      Very strong
#   .50 - .69                      Substantial
#   .30 - .49                      Moderate
#   .10 - .29                      Low
#   .01 - .09                      Trivial
#
df_merged['Waste'].corr(df_merged['DefectCount']) # 47%
plot = df_merged.plot(x='Date')
plot.get_figure().savefig('data/03-waste-defects.pdf', format='pdf')


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

qry3 = "select Date, count(Val)*100 as Revisions from df3 group by Date"
dfout3 = pysqldf(qry3)
print(dfout3)
dfout3['Date'] = dfout3['Date'].map(lambda x: datetime.strptime(str(x), "%Y-%m-%d %H:%M:%S.%f"))
plot = dfout3.plot(x='Date', y='Revisions')
plot.get_figure().savefig('data/04-revisions-time.pdf', format='pdf')



#
# Join the two data frames
#
df_merged2 = pd.merge(dfout2, dfout3, on='Date', how='inner')
print(df_merged2)

#
# Put in the Waste as well
#
df_merged3 = pd.merge(df_merged2, dfout, on='Date', how='inner')
print(df_merged3)

## See R vs Defects => Moderate Correlation (40%)
df_merged3['DefectCount'].corr(df_merged3['Revisions'])
plot = df_merged3.plot(x='Date', y=['DefectCount', 'Revisions'])
plot.get_figure().savefig('data/05-defects-revisions-time.pdf', format='pdf')

## See Waste vs. Defects => Moderate Correlation (44%)
df_merged3['Waste'].corr(df_merged3['DefectCount'])
plot = df_merged3.plot(x='Date', y=['Waste', 'DefectCount'])
plot.get_figure().savefig('data/06-defects-waste-time.pdf', format='pdf')


## See Waste vs. R => Trivial Correlation (6%)
df_merged3['Waste'].corr(df_merged3['Revisions'])
plot = df_merged3.plot(x='Date', y=['Waste', 'Revisions'])
plot.get_figure().savefig('data/07-waste-revisions-time.pdf', format='pdf')






########################################################################
##
## Look at Features
##
########################################################################

wbook4 = pd.ExcelFile('data/inspection_results_and_defects_August_september.xlsx')
sheet4 = wbook4.parse('Inspection results')
cols4 = ['Date', 'Feature', 'Media']
df4 = pd.DataFrame(data=sheet4, columns = cols4)

# Average over each day
qry4 = "select Date, avg(Media)*200 as 'Temp' from df4 where Feature == 'TEMPERATURA SALA' and Media < 100 group by Date"
dfout4 = pysqldf(qry4)
print(dfout4)
dfout4['Date'] = dfout4['Date'].map(lambda x: datetime.strptime(str(x), "%Y-%m-%d %H:%M:%S.%f"))
plot = dfout4.plot(x='Date', y='Temp')
plot.get_figure().savefig('data/10-temp-time.pdf', format='pdf')

# Don't Average over each day
cols5 = ['Date_Time', 'Feature', 'Media']
df5 = pd.DataFrame(data=sheet4, columns = cols5)

qry5 = "select Date_Time, Media as 'Temp' from df5 where Feature == 'TEMPERATURA SALA' and Media < 100"
dfout5 = pysqldf(qry5)
print(dfout5)
dfout5['Date_Time'] = dfout5['Date_Time'].map(lambda x: datetime.strptime(str(x), "%d.%m.%y %H:%M:%S"))
plot = dfout5.plot(x='Date_Time', y='Temp')
plot.get_figure().savefig('data/11-temp-time.pdf', format='pdf')


#
# Correlate Defects and Temperature
#
df_merged4 = pd.merge(dfout4, dfout2, on='Date', how='inner')
print(df_merged4)

## See Temp vs Defects => low (18%)
df_merged4['Temp'].corr(df_merged2['DefectCount'])
plot = df_merged4.plot(x='Date', y=['Temp', 'DefectCount'])

plot.get_figure().savefig('data/12-temp-defects-time.pdf', format='pdf')
































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