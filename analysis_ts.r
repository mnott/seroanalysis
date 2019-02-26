#
# Poor man's Time Series on Siro data
#

#
# Set Options and Working directory
#
Sys.setenv(LANG = "en")                # Set R language to English
Sys.setlocale("LC_ALL", 'en_US.UTF-8') # Set to UTF-8
options(digits=5)                      # Default to 5 digits
options(warn = -1)                     # Turn off warnings
rm(list=ls())

source("helpers/libs.r")

library(stats)
library(dplyr)
library(lubridate)


#
# Configure
#
datadir <- "data/input/20190225/"


##############################################################################
#
# Agua
#
##############################################################################

file <- paste(datadir, "Maval_Agua_Enero_offset3.csv", sep = "")
df <- read.csv(file, header = TRUE, sep = ";")
df <- df[ which(df$Fin_Dosificado_Agua=='true'), ]
df <- df[,c("X_time", "Process_Value_Agua")]
names(df) <- c("time", "value")

df$time <- as.POSIXct(df$time, format="%Y-%m-%dT%H:%M:%OS")
df$value <- as.numeric(df$value)

df <- df %>%
  group_by(yr = year(time), mn = month(time), dy = day(time), hr = hour(time)) %>%
  summarise(value = mean(value, na.rm = T))

df$time <- make_datetime(year = df$yr, month = df$mn, day = df$dy, hour = df$hr)

df <- df[c("time", "value")]

df_agua <- df
names(df_agua) <- c("time", "agua")


##############################################################################
#
# Sugar
#
##############################################################################

file <- paste(datadir, "Maval_Azucar_Enero.csv", sep = "")
df <- read.csv(file, header = TRUE, sep = ";")
df <- df[ which(df$Fin_Dosificado_Azucar=='true'), ]
df <- df[,c("X_time", "Process_Value_Azucar")]
names(df) <- c("time", "value")

df$time <- as.POSIXct(df$time, format="%Y-%m-%dT%H:%M:%OS")
df$value <- as.numeric(df$value)

df <- df %>%
  group_by(yr = year(time), mn = month(time), dy = day(time), hr = hour(time)) %>%
  summarise(value = mean(value, na.rm = T))

df$time <- make_datetime(year = df$yr, month = df$mn, day = df$dy, hour = df$hr)

df <- df[c("time", "value")]

df_azucar <- df
names(df_azucar) <- c("time", "azucar")


##############################################################################
#
# Temperatura
#
##############################################################################

file <- paste(datadir, "201901 InspectionResults_offset+3_temp.csv", sep = "")
df <- read.csv(file, header = TRUE, sep = ";")
df <- df[ which(df$characteristicText=='TEMPERATURA MASA'), ]
df <- df[,c("X_time", "meanValue")]
names(df) <- c("time", "value")

df$time <- as.POSIXct(df$time, format="%Y-%m-%dT%H:%M")
df$value <- as.numeric(sub(",", ".", df$value, fixed = TRUE))

df <- df %>%
  group_by(yr = year(time), mn = month(time), dy = day(time), hr = hour(time)) %>%
  summarise(value = mean(value, na.rm = T))

df$time <- make_datetime(year = df$yr, month = df$mn, day = df$dy, hour = df$hr)

df <- df[c("time", "value")]

df_temperatura <- df
names(df_temperatura) <- c("time", "temperatura")

#
# Remove values that are outside the sugar/water timeframe
#
df_temperatura <- df_temperatura[ which(df_temperatura$time >= min(df_azucar$time)), ]
df_temperatura <- df_temperatura[ which(df_temperatura$time <= max(df_azucar$time)), ]

#
# Remove outliers
#
# We see an outlier which is abouve 500°C, so we remove it
#
df_temperatura <- df_temperatura[ which(df_temperatura$temperatura <= 100), ]



##############################################################################
#
# Alto
#
##############################################################################

file <- paste(datadir, "201901 InspectionResults_offset+3_alto.csv", sep = "")
df <- read.csv(file, header = TRUE, sep = ";")
df <- df[ which(df$characteristicText=='ALTO'), ]
df <- df[,c("X_time", "meanValue")]
names(df) <- c("time", "value")

df$time <- as.POSIXct(df$time, format="%Y-%m-%dT%H:%M")
df$value <- as.numeric(sub(",", ".", df$value, fixed = TRUE))

df <- df %>%
  group_by(yr = year(time), mn = month(time), dy = day(time), hr = hour(time)) %>%
  summarise(value = mean(value, na.rm = T))

df$time <- make_datetime(year = df$yr, month = df$mn, day = df$dy, hour = df$hr)

df <- df[c("time", "value")]

df_alto <- df
names(df_alto) <- c("time", "alto")

#
# Remove values that are outside the sugar/water timeframe
#
df_alto <- df_alto[ which(df_alto$time >= min(df_azucar$time)), ]
df_alto <- df_alto[ which(df_alto$time <= max(df_azucar$time)), ]


##############################################################################
#
# Combine Data Frames
#
##############################################################################

df_combined <- merge(df_agua, df_azucar)
df_combined <- merge(df_combined, df_temperatura)
df_combined <- merge(df_combined, df_alto)

#
# Cleanup
#
rm(df, df_agua, df_azucar, df_temperatura, df_alto)


##############################################################################
#
# Exploration
#
##############################################################################

outfile <- paste(datadir, "output.pdf", sep = "")
pdf(outfile, width = 21, height = 15)


df <- df_combined
plot.ts(df)


##############################################################################
#
# Plot raw data
#
##############################################################################

par(mfrow=c(2,1))
df_raw <- df_combined
matplot(x = df_raw$time, y = as.matrix(df_raw[-1]), type='l', pch=1, col = 1:4, xlab='Date (January)', ylab = 'Values', xaxt="n")
legend("topright", inset=.05, legend=colnames(df_raw[2:5]), pch=1, col= 1:4, horiz=TRUE)
timelabels<-format(df_raw$time,"%d")
axis(1,at=df_raw$time,labels=timelabels)

r = acf(df_combined$agua,        lag.max = nrow(df_combined))
par(mfrow=c(1,1))



##############################################################################
#
# Auto Correlations and Cross Correlations
#
##############################################################################


# Convert Timestamp into hours
# df_combined$time <- as.numeric(df_combined$time - df_combined[1, "time"]) / 3600

#
# Auto Correlations
#
par(mfrow=c(2,2))

r = acf(df_combined$agua,        lag.max = nrow(df_combined))
r = acf(df_combined$azucar,      lag.max = nrow(df_combined))
r = acf(df_combined$temperatura, lag.max = nrow(df_combined))
r = acf(df_combined$alto,        lag.max = nrow(df_combined))

par(mfrow=c(1,1))


#
# Cross Correlations
#
par(mfrow=c(2,3))

r = ccf(df_combined$agua,        -df_combined$azucar,      lag.max = nrow(df_combined))
r = ccf(df_combined$agua,        -df_combined$temperatura, lag.max = nrow(df_combined))
r = ccf(df_combined$azucar,       df_combined$temperatura, lag.max = nrow(df_combined))
r = ccf(df_combined$agua,         df_combined$alto,        lag.max = nrow(df_combined))
r = ccf(df_combined$azucar,      -df_combined$alto,        lag.max = nrow(df_combined))
r = ccf(df_combined$temperatura, -df_combined$alto,        lag.max = nrow(df_combined))

par(mfrow=c(1,1))

dev.off()









