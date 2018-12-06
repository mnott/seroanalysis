# -*- coding: utf-8 -*-
"""

"""

# -*- coding: utf-8 -*-


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

#####################################################################
################ Requirement - 1
#####################################################################
# Excel processing - Added Date_Time column in Excel by concating Date and Time
xls_file_master_org = pd.ExcelFile('data/Wastes_August_september.xlsx')
xls_file_master_org.sheet_names
df_wastes = xls_file_master_org.parse('9000_E06')
columns = ['Date_Time','Quantity']
df_wastes = pd.DataFrame(data=df_wastes, columns=columns)
#### df_wastes.plot(x=['Date_Time'],y='Quantity')
df_wastes.plot(x='Date_Time',y='Quantity')
#### GRAPH do not show Date_time values may be due to graphs' width and height

#####################################################################
################ Requirement - 2
#####################################################################
# Excel processing - Added Date_Time column in Excel by concating Date and Time
# Excel processing - deleting rows where Lower Acceptable value and Higher Acceptable value both are 0
xls_file_master_org1 = pd.ExcelFile('data/output-11-30.xlsx')
xls_file_master_org1.sheet_names
df_output = xls_file_master_org1.parse('defects')
columns1 = ['Date_Time','Inspection char. Description','Measured value']
df_output = pd.DataFrame(data=df_output, columns=columns1)

df_inspection_unique = pd.DataFrame(columns=['unique_inspection'])



currentrownum = 1
for unique_inspection in df_output['Inspection char. Description'].unique():
    if str(unique_inspection) != "nan":
        df_inspection_rows = df_output.loc[(df_output['Inspection char. Description'] == unique_inspection) | (df_output['Inspection char. Description'] == "nan")]
        print("("+str(currentrownum)+") - Inspection Text --->" + str(unique_inspection))
        ### df_inspection_rows.plot(x=['Date_Time'],y='Measured value')
        df_inspection_rows.plot(x='Date_Time',y='Measured value')
        currentrownum = currentrownum + 1


#####################################################################
################ Requirement - 3
#####################################################################