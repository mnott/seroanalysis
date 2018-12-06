# -*- coding: utf-8 -*-
"""

"""

# -*- coding: utf-8 -*-


import pandas as pd
import numpy as np
from datetime import datetime
import matplotlib.pyplot as plt

#
# Import our own helpers
#
from helpers.helpers import loadExcel, queryDataFrame, parseDateTime, asPDF


########################################################################
##
## Look at Waste Produced, "R"-evisions, and Defect Counts
##
########################################################################


########################################################################
#
# Load waste data frame
#
ff_waste = loadExcel('data/Wastes_August_september_m.xlsx', '9000_E06', ['Date', 'Quantity'])

#
# Calculate Waste as sum by Day
#
df = queryDataFrame(ff_waste, 'select Date, sum(Quantity)*5 as Waste from df group by Date')
df = parseDateTime(df, 'Date', "%Y-%m-%d %H:%M:%S.%f")
# print(df)

plot = df.plot(x='Date', y='Waste')
asPDF(plot, 'data/01-waste-time.pdf')


########################################################################
#
# Load Defects
#
ff_defects = loadExcel('data/inspection_results_and_defects_August_september.xlsx', 'Defects', ['Date', 'NUM_DEFECTOS'])
df2 = queryDataFrame(ff_defects, 'select Date, sum(NUM_DEFECTOS) as DefectCount from df group by Date')
df2 = parseDateTime(df2, 'Date', "%Y-%m-%d %H:%M:%S.%f")
# print(df2)

plot = df2.plot(x='Date', y='DefectCount')
asPDF(plot, 'data/02-defects-time.pdf')

#
# Correlate between Waste and Defect Count
#
df_waste_defects = pd.merge(df, df2, on='Date', how='inner')
print(df_waste_defects)

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
df_waste_defects['Waste'].corr(df_waste_defects['DefectCount']) # 47%
plot = df_waste_defects.plot(x='Date')
asPDF(plot, 'data/03-waste-defects.pdf')


########################################################################
#
# Load Inspection Results
#
ff_results = loadExcel('data/inspection_results_and_defects_August_september.xlsx', 'Inspection results', ['Date_Time', 'Date', 'Val', 'Feature', 'Media'])

df_revisions = queryDataFrame(ff_results, "select Date, count(Val)*100 as Revisions from df where Val = 'R' group by Date")
df_revisions = parseDateTime(df_revisions, 'Date', "%Y-%m-%d %H:%M:%S.%f")

plot = df_revisions.plot(x='Date', y='Revisions')
asPDF(plot, 'data/04-revisions-time.pdf')


########################################################################
#
# Create merged data frame for Correlations
#
df_merged = pd.merge(df_waste_defects, df_revisions, on='Date', how='inner')
# print (df_merged)

## See R vs Defects => Moderate Correlation (40%)
df_merged['DefectCount'].corr(df_merged['Revisions'])
plot = df_merged.plot(x='Date', y=['DefectCount', 'Revisions'])
asPDF(plot, 'data/05-defects-revisions-time.pdf')

## See Waste vs. Defects => Moderate Correlation (44%)
df_merged['Waste'].corr(df_merged['DefectCount'])
plot = df_merged.plot(x='Date', y=['Waste', 'DefectCount'])
asPDF(plot, 'data/06-defects-waste-time.pdf')

## See Waste vs. R => Trivial Correlation (6%)
df_merged['Waste'].corr(df_merged['Revisions'])
plot = df_merged.plot(x='Date', y=['Waste', 'Revisions'])
asPDF(plot, 'data/07-waste-revisions-time.pdf')


########################################################################
##
## Look at Features
##
########################################################################

# Average over each day
df_temp = queryDataFrame(ff_results, "select Date, avg(Media) as 'Temp' from df where Feature == 'TEMPERATURA SALA' and Media < 100 group by Date")
df_temp = parseDateTime(df_temp, 'Date', "%Y-%m-%d %H:%M:%S.%f")

plot = df_temp.plot(x='Date', y='Temp')
asPDF(plot, 'data/10-temp-time.pdf')

# Don't Average over each day
df_temp_all = queryDataFrame(ff_results, "select Date_Time, Media as 'Temp' from df where Feature == 'TEMPERATURA SALA' and Media < 100")
plot = df_temp_all.plot(x='Date_Time', y='Temp')
asPDF(plot, 'data/11-temp-time.pdf')

#
# Scale Average Temperature
#
df_temp_scaled = queryDataFrame(ff_results, "select Date, avg(Media) * 200 as 'Temp' from df where Feature == 'TEMPERATURA SALA' and Media < 100 group by Date")
df_temp_scaled = parseDateTime(df_temp_scaled, 'Date', "%Y-%m-%d %H:%M:%S.%f")


#
# Correlate Temperature vs. Defects, Waste, Revisions
#
df_merged = pd.merge(df_merged, df_temp_scaled, on='Date', how='inner')


## Correlate Temp vs Defects
df_merged['Temp'].corr(df_merged['DefectCount']) # 34%
plot = df_merged.plot(x='Date', y=['Temp', 'DefectCount'])
asPDF(plot, 'data/12-temp-defects-time.pdf')

## Correlate Temp vs Waste
df_merged['Temp'].corr(df_merged['Waste']) # 19%
plot = df_merged.plot(x='Date', y=['Temp', 'Waste'])
asPDF(plot, 'data/13-temp-waste-time.pdf')

## Correlate Temp vs Revisions
df_merged['Temp'].corr(df_merged['Revisions']) # 28%
plot = df_merged.plot(x='Date', y=['Temp', 'Revisions'])
asPDF(plot, 'data/14-temp-revisions-time.pdf')























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