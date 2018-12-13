##############################################################
#
# Analysis - Medina
#
# Analyze Waste, DefectCount, Revisions, Oven Temperatures
#
# Use RStudio to run this code.
#
##############################################################

#
# Set Options and Working directory
#
Sys.setenv(LANG = "en")                # Set R language to English
Sys.setlocale("LC_ALL", 'en_US.UTF-8') # Set to UTF-8
options(digits=5)                      # Default to 5 digits
options(warn = -1)                     # Turn off warnings
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)
rm(list=ls())


source("helpers/libs.r")


##############################################################################
#
# Configuration
#
##############################################################################
#
# Set to true if we want to remove outliers
#
outliers <- TRUE

plant  <- "medina"
line   <- "L06"
data   <- "inspection_results_Defects_Waste_Sensors_December_L06_Medina.xlsx"
file   <- paste("data/input",  plant, line, data, sep="/")
outdir <- paste("data/output", plant, line, "r", sep="/")
outfile <- paste(outdir, "inspection_results_Defects_Waste_Sensors_December_L06_Medina_aggregated.xlsx", sep="/")


##############################################################################
#
# Read the Data File
#
##############################################################################

ff_waste   <- read_excel(file, sheet="Waste"   ) #,  cell_cols(c("H", "I", "J")), col_types=c("date", "date", "numeric"))
ff_defects <- read_excel(file, sheet="Defects" ) #, cell_cols(c("H:M")), col_types=c("date", "date", "skip", "skip", "skip", "numeric"))
ff_results <- read_excel(file, sheet="Inspection Results")
ff_s_oven  <- read_excel(file, sheet="Sensors_Oven")

##############################################################################
#
# Preprocess
#
##############################################################################

# Waste
names(ff_waste)[names(ff_waste) == "Date in Format YYYYMMDD"] <- "Date"
names(ff_waste)[names(ff_waste) == "Field of type TIMS"]      <- "Time"
ff_waste$DateTime <- as.POSIXct(paste(ff_waste$Date, format(as.POSIXct(ff_waste$Time), "%H:%M:%S")))

# Defects
names(ff_defects)[names(ff_defects) == "Natural Number"] <- "DefectCount"
names(ff_defects)[names(ff_defects) == "Work ctr__1"] <- "WorkCenter"
ff_defects$DateTime <- as.POSIXct(paste(ff_defects$Date, format(as.POSIXct(ff_defects$Time), "%H:%M:%S")))

# Results
names(ff_results)[names(ff_results) == "Short text for the characteristic"] <- "Feature"
names(ff_results)[names(ff_results) == "Mean value"] <- "Mean"
names(ff_results)[names(ff_results) == "Lower tol.limit"] <- "LowerLimit"
names(ff_results)[names(ff_results) == "Upper specLimit"] <- "UpperLimit"
names(ff_results)[names(ff_results) == "CrDate"] <- "Date"
names(ff_results)[names(ff_results) == "End"]    <- "Time"
ff_results$DateTime <- as.POSIXct(paste(ff_results$Date, format(as.POSIXct(ff_results$Time), "%H:%M:%S")))

# Sensors Oven
names(ff_s_oven)[names(ff_s_oven) == "_time"]    <- "Time"
names(ff_s_oven)[names(ff_s_oven) == "TemperaturaCamaraCombustion"]    <- "TCombustion"
names(ff_s_oven)[names(ff_s_oven) == "Temperatura_Zona_2"] <- "T1"
names(ff_s_oven)[names(ff_s_oven) == "Temperatura_Zona_1"] <- "T2"
names(ff_s_oven)[names(ff_s_oven) == "Temperatura_Zona_3"] <- "T3"
names(ff_s_oven)[names(ff_s_oven) == "Temperatura_Zona_4"] <- "T4"
ff_s_oven$DateTime <- as.POSIXct(ff_s_oven$Time, format = "%Y-%m-%dT%H:%M:%OSZ")
ff_s_oven$Tiempo_Homeado <- NULL



##############################################################################
#
# Analyze and Normalize Waste
#
##############################################################################

df_waste <- sqldf('select DateTime, Quantity as Waste from ff_waste')

#
# XTS on waste
#
xts_waste <- xts(df_waste, order.by = df_waste$DateTime)
xts_waste$DateTime <- NULL
storage.mode(xts_waste) <- "numeric"
ep <- endpoints(xts_waste, on = "hours")
xts_waste <- period.apply(xts_waste,INDEX=ep,FUN=sum)
xts_waste <- align.time(xts_waste, n = 3600)


#
# Normality and Outlier Check on Waste
#
df_waste <- data.frame(DateTime=index(xts_waste), coredata(xts_waste))

normality(df_waste$Waste, file = "", width = 16, height = 11, headervariable = "Waste")

if(outliers) {
  df_waste <- sqldf('select DateTime, Waste from df_waste where Waste < 100')
  normality(df_waste$Waste, file = "", width = 16, height = 11, headervariable = "Waste")
}

# Convert back to XTS for merging later
xts_waste <- xts(df_waste, order.by = df_waste$DateTime)
xts_waste$DateTime <- NULL


##############################################################################
#
# Analyze and Normalize Defects
#
##############################################################################

df_defects <- sqldf('select DateTime, DefectCount as DefectCount from ff_defects')

xts_defects <- xts(df_defects, order.by = df_defects$DateTime)
xts_defects$DateTime <- NULL
storage.mode(xts_defects) <- "numeric"
ep <- endpoints(xts_defects, on = "hours")
xts_defects <- period.apply(xts_defects,INDEX=ep,FUN=sum)
xts_defects <- align.time(xts_defects, n = 3600)

#
# Normality and Outlier Check on Defects
#
df_defects <- data.frame(DateTime=index(xts_defects), coredata(xts_defects))

normality(df_defects$DefectCount, file = "", width = 16, height = 11, headervariable = "Defect Count")

if(outliers) {
  df_defects <- sqldf('select DateTime, DefectCount from df_defects where DefectCount < 1424')
  normality(df_defects$DefectCount, file = "", width = 16, height = 11, headervariable = "Defect Count")
}

# Convert back to XTS for merging later
xts_defects <- xts(df_defects, order.by = df_defects$DateTime)
xts_defects$DateTime <- NULL


##############################################################################
#
# Analyze and Normalize Revisions
#
##############################################################################

df_revisions <- sqldf("select DateTime, Val as Revisions from ff_results where Val = 'R'")

# All "R" Revisions set to 1 so we can sum later
df_revisions$Revisions <- 1

xts_revisions <- xts(df_revisions, order.by = df_revisions$DateTime)
xts_revisions$DateTime <- NULL
storage.mode(xts_revisions) <- "numeric"
ep <- endpoints(xts_revisions, on = "hours")
xts_revisions <- period.apply(xts_revisions,INDEX=ep,FUN=sum)
xts_revisions <- align.time(xts_revisions, n = 3600)

#
# Normality and Outlier Check on Revisions
#
df_revisions <- data.frame(DateTime=index(xts_revisions), coredata(xts_revisions))

normality(df_revisions$Revisions, file = "", width = 16, height = 11, headervariable = "Revisions")

if(outliers) {
  df_revisions <- sqldf('select DateTime, Revisions from df_revisions where Revisions < 10')
  normality(df_revisions$Revisions, file = "", width = 16, height = 11, headervariable = "Revisions")
}

# Convert back to XTS for merging later
xts_revisions <- xts(df_revisions, order.by = df_revisions$DateTime)
xts_revisions$DateTime <- NULL


##############################################################################
#
# Analyze and Normalize Oven Temperatures
#
##############################################################################


df_s_oven <- sqldf('select DateTime, TCombustion, T1, T2, T3, T4 from ff_s_oven order by Time')

#
# XTS on s_oven
#
xts_s_oven <- xts(df_s_oven, order.by = df_s_oven$DateTime)

# Get Hourly Means
ep <- endpoints(xts_s_oven, on = "hours")
xts_s_oven <- period.apply(xts_s_oven,INDEX=ep,FUN=mean)

# Round observations to the next hour
xts_s_oven <- align.time(xts_s_oven, n = 3600)
xts_s_oven$DateTime <- NULL

#
# Normality and Outlier Check on Oven Temperatures
#
df_s_oven <- data.frame(DateTime=index(xts_s_oven), coredata(xts_s_oven))

normality(df_s_oven$TCombustion, file = "", width = 16, height = 11, headervariable = "TCombustion")
normality(df_s_oven$T1, file = "", width = 16, height = 11, headervariable = "T1")
normality(df_s_oven$T2, file = "", width = 16, height = 11, headervariable = "T2")
normality(df_s_oven$T3, file = "", width = 16, height = 11, headervariable = "T3")
normality(df_s_oven$T4, file = "", width = 16, height = 11, headervariable = "T4")

# NB: We leave these outliers for the moment, as they seem to be two data sets really.

# Convert back to XTS for merging later
xts_s_oven <- xts(df_s_oven, order.by = df_s_oven$DateTime)
xts_s_oven$DateTime <- NULL



##############################################################################
#
# Merge all Time Series
#
##############################################################################

xts_merged <- merge(xts_waste, xts_defects)
xts_merged <- merge(xts_merged, xts_revisions)
xts_merged <- merge(xts_merged, xts_s_oven)
df_merged <- data.frame(DateTime=index(xts_merged), coredata(xts_merged))


##############################################################################
#
# Write to Excel
#
##############################################################################


write.xlsx(df_merged, outfile, row.names = FALSE)













