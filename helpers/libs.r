##############################################################
#
# Shared Libraries
#
# (c) 2018 Matthias Nott, SAP
#
###############################################################

#
# Load some Libraries
#
# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics) # For Correlation. Needs PerformanceAnalytics package

#
# Load some Libraries
#
# install.packages("readxl")
library(readxl)                # For reading Excel, needs readxl package

# install.packages("car")
library(car)     # CAR library. Needs car package

# install.packages("psych")
library(psych)
library(ggplot2)

# install.packages("Hmisc")
library(Hmisc)

# install.packages("sqldf")
library(sqldf)

#
# Load SPSS equivalent functions
#
source("helpers/spss.functions.r")

#
# Load our own functions
#
source("helpers/helpers.r")
source("helpers/assumptions.r")
