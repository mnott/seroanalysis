#
# Poor man's Time Series on Siro data
#
rm(list=ls())

library(jsonlite)
library(dplyr);

#
# Get data from data source
#
# TODO: Parameterize with time window and sensor id
# TODO: We should have a call to get a list of available sensor ids (perhaps by time window)
#
df <- fromJSON("https://siro-aguilar--s-l-u--qassiro-qas-sensor-js.cfapps.eu10.hana.ondemand.com/sensorData.xsodata/SensorSet")

# Select the columns we're intersted in
df <- df$d$results[,c("sendorId", "time", "value")]

# Correct a spelling error on a column name
names(df) <- c("sensorId", "time", "value")

# Convert from Strings to values
df$time <- as.POSIXct(df$time, format="%Y-%m-%dT%H:%M:%OS")
df$value <- as.numeric(df$value)

# Aggregate by splitting into the relevant time parts
# In the given example we go down to minutes
df <- df %>%
  group_by(sensorId, yr = year(time), mn = month(time), dy = day(time), hr = hour(time), min = minute(time)) %>%
  summarise(value = mean(value, na.rm = T))

# Re-add a dttm column
df$time <- make_datetime(year = df$yr, month = df$mn, day = df$dy, hour = df$hr, min = df$min)

# Remove the now superfluous time columns
df <- df[c("sensorId", "value", "time")]

# Show the result
df

