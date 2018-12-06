# -*- coding: utf-8 -*-
"""

"""

# -*- coding: utf-8 -*-

import pandas as pd
from datetime import datetime
from pandasql import sqldf


########################################################################
##
## Helper Functions
##
########################################################################

#
# Load a sheet from an excel file, given some columns, into a data frame
#
def loadExcel(file, sheet, cols):
    wbook = pd.ExcelFile(file)
    sheet = wbook.parse(sheet)
    df = pd.DataFrame(data = sheet, columns = cols)
    return df

#
# Query a Data Frame
#
def queryDataFrame(df, query):
    print (query)
    return sqldf(query, locals())


#
# Parse a DateTime field
#
def parseDateTime(df, field, format):
    df[field] = df[field].map(lambda x: datetime.strptime(str(x), format))
    return df

#
# Save a plot as pdf
#
def asPDF(plot, file):
    plot.get_figure().savefig(file, format='pdf')
