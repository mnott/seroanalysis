##############################################################
#
# Analysis - Medina
#
# Use RStudio to run this code.
#
##############################################################

#
# Set Options and Working directory
#
Sys.setenv(LANG = "en")  # Set R language to English
options(digits=5)        # Default to 5 digits
options(warn = -1)       # Turn off warnings
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
data   <- "inspection_results_Defects_Waste_November_December_L06_MEDINA.xlsx"
file   <- paste("data/input",  plant, line, data, sep="/")
outdir <- paste("data/output", plant, line, "r", sep="/")



##############################################################################
#
# Read the Data File
#
##############################################################################

ff_waste   <- read_excel(file, sheet="Waste"   ) #,  cell_cols(c("H", "I", "J")), col_types=c("date", "date", "numeric"))
ff_defects <- read_excel(file, sheet="Defects" ) #, cell_cols(c("H:M")), col_types=c("date", "date", "skip", "skip", "skip", "numeric"))
ff_results <- read_excel(file, sheet="Inspection Results")

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
ff_defects$DateTime <- as.POSIXct(paste(ff_defects$Date, format(as.POSIXct(ff_defects$Time), "%H:%M:%S")))

# Results
names(ff_results)[names(ff_results) == "Short text for the characteristic"] <- "Feature"
names(ff_results)[names(ff_results) == "Mean value"] <- "Mean"
names(ff_results)[names(ff_results) == "Lower tol.limit"] <- "LowerLimit"
names(ff_results)[names(ff_results) == "Upper specLimit"] <- "UpperLimit"
names(ff_results)[names(ff_results) == "CrDate"] <- "Date"
names(ff_results)[names(ff_results) == "End"]    <- "Time"
ff_results$DateTime <- as.POSIXct(paste(ff_results$Date, format(as.POSIXct(ff_results$Time), "%H:%M:%S")))


limit <- 0


df_limits <- sqldf(sprintf('select * from ff_results where (mean >= (UpperLimit - %f * UpperLimit) or mean <= (LowerLimit + %f * LowerLimit)) and UpperLimit != 0 and LowerLimit != 0',limit,limit));

df_upper_limits <- sqldf(sprintf('select * from ff_results where mean >= (UpperLimit - %f * UpperLimit) and UpperLimit != 0 and LowerLimit != 0',limit,limit)); 
df_lower_limits <- sqldf(sprintf('select * from ff_results where mean <= (LowerLimit + %f * LowerLimit) and UpperLimit != 0 and LowerLimit != 0',limit,limit)); 

total_results <- sqldf('select count(*) from ff_results')


#
# Calculate the number of rows that are outside any of the limits
#
df_limits <- sqldf(sprintf('select * from ff_results where (mean >= (UpperLimit - %f * UpperLimit) or mean <= (LowerLimit + %f * LowerLimit)) and UpperLimit != 0 and LowerLimit != 0',limit,limit));




df_zero_limits <- sqldf('select * from ff_results where UpperLimit == 0 or LowerLimit == 0') #3465
results_zero_limits <- sqldf('select count(*) from df_zero_limits')

df_top_limits <- sqldf('select Feature, count(Feature) as lcount from df_limits group by Feature order by lcount desc')
df_top_upper_limits <- sqldf('select Feature, count(Feature) as lcount from df_upper_limits group by Feature order by lcount desc')
df_top_lower_limits <- sqldf('select Feature, count(Feature) as lcount from df_lower_limits group by Feature order by lcount desc')

results_top_limits <- sqldf('select sum(lcount) from df_top_limits');
results_upper_limits <- sqldf('select sum(lcount) from df_top_upper_limits');
results_lower_limits <- sqldf('select sum(lcount) from df_top_lower_limits');

results_lower_limits
results_upper_limits

df_under_lower_limits <- sqldf('select * from ff_results where mean <= (LowerLimit) and UpperLimit != 0 and LowerLimit != 0'); 
df_over_upper_limits  <- sqldf('select * from ff_results where mean >= (UpperLimit) and UpperLimit != 0 and LowerLimit != 0'); 

sqldf('select count(*) from df_under_lower_limits') #  451
sqldf('select count(*) from df_over_upper_limits')  # 1469


sqldf('select count(*) from ff_results where mean <= (LowerLimit) and UpperLimit != 0 and LowerLimit != 0 and Val = "R"'); # 119 out of  451
sqldf('select count(*) from ff_results where mean >= (UpperLimit) and UpperLimit != 0 and LowerLimit != 0 and Val = "R"'); # 134 out of 1469


sqldf('select count(*) from ff_results where UpperLimit != 0 and LowerLimit = 0')  #  74 have only Lower Limit set
sqldf('select count(*) from ff_results where UpperLimit  = 0 and LowerLimit != 0') # 396 have only Upper Limit set

sqldf('select distinct(Feature) from ff_results where UpperLimit != 0 and LowerLimit = 0')  #  74 have only Lower Limit set
sqldf('select distinct(Feature) from ff_results where UpperLimit  = 0 and LowerLimit != 0') # 396 have only Upper Limit set


# Which ones of the features have only one side set to 0, and how often?
#
# > sqldf('select distinct(Feature) from ff_results where UpperLimit != 0 and LowerLimit = 0')  #  74 have only Lower Limit set
# Feature
# 1 REVISAR DISPLAY TANQUE CHOCO  : always 0 Lower Limit
# > sqldf('select distinct(Feature) from ff_results where UpperLimit  = 0 and LowerLimit != 0') # 396 have only Upper Limit set
# Feature
# 1 TEMP INT PRODUCTO SALIDA HORNO: always 0 Upper Limit
# 2                     PESO MEDIO: 531 times Upper Limit is not 0; 626 total
# 3                        FORMULA  always 0 Upper Limit
# 4    PESO UNIDAD BOLLER<U+00CD>A: 539 times Upper Limit is not 0; 547 total
# sqldf('select distinct(Feature) from df_zero_limits') # 25




##############################################################################
#
# Analyze and Normalize Waste
#
##############################################################################

df_waste <- sqldf('select Date, sum(Quantity)*5 as Waste from ff_waste group by Date')

normality(df_waste$Waste, file = "", width = 16, height = 11, headervariable = "Waste")

if(outliers) {
  # df_tmp <- sqldf('select Date, Waste from df_waste where Waste >= 2100')
  # df_tmp
  # rm(df_tmp)
  
  df_waste <- sqldf('select Date, Waste from df_waste where Waste < 2100')
  normality(df_waste$Waste, file = "", width = 16, height = 11, headervariable = "Waste")
}


##############################################################################
#
# Analyze and Normalize Defects
#
##############################################################################

df_defects <- sqldf('select Date, sum(DefectCount) as DefectCount from ff_defects group by Date')

normality(df_defects$DefectCount, file = "", width = 16, height = 11, headervariable = "Defect Count")

if(outliers) {
  df_defects <- sqldf('select Date, DefectCount from df_defects where DefectCount < 8547')
  normality(df_defects$DefectCount, file = "", width = 16, height = 11, headervariable = "Defect Count")
}


##############################################################################
#
# Analyze and Normalize Revisions
#
##############################################################################

df_revisions <- sqldf("select Date, count(Val)*100 as Revisions from ff_results where Val = 'R' group by Date")
normality(df_revisions$Revisions, file = "", width = 16, height = 11, headervariable = "Revisions")

if(outliers) {
  df_revisions <- sqldf('select Date, Revisions from df_revisions where Revisions < 21400')
  normality(df_revisions$Revisions, file = "", width = 16, height = 11, headervariable = "Revisions")
}


##############################################################################
#
# Create a joint table of Waste, Defects and Revisions
#
##############################################################################

df_wdr <- merge(df_defects, df_waste,     by = "Date")
df_wdr <- merge(df_wdr,     df_revisions, by = "Date")


##############################################################################
#
# Analyze all given features
#
##############################################################################

df_features <- unique(ff_results$Feature)

df_correlations <- NULL

# i <- 0
for (feature in df_features) {
  print (feature)
  
  df_feature <- sqldf(sprintf("select Date, avg(Mean) as 'aFeature' from ff_results where Feature == '%s' group by Date", feature))
  
  nrows <- nrow(df_feature)
  
  if(nrows < 12) {
    next
  }
  
  df_analyze <- merge(df_wdr, df_feature, by = "Date")
  
  r_w <- cor.test(df_analyze$aFeature, df_analyze$Waste)
  r_d <- cor.test(df_analyze$aFeature, df_analyze$DefectCount)
  r_r <- cor.test(df_analyze$aFeature, df_analyze$Revisions)

  df_correlations = rbind(df_correlations, data.frame(feature, r_w$estimate, r_w$p.value, r_d$estimate, r_d$p.value, r_r$estimate, r_r$p.value, nrows))
  
  # i <- i+1  
  # if(i > 10) {
  #  break
  #}
}
colnames(df_correlations)<- c("feature", "rwr", "rwp", "rdr", "rdp", "rrr", "rrp", "n")

#
# Get the top features for waste in terms of correlation coefficient
#
df_top_waste     <- sqldf("select feature, n from df_correlations where rwr >= 0.25 order by rwr desc")
df_top_defects   <- sqldf("select feature, n from df_correlations where rdr >= 0.25 order by rdr desc")
df_top_revisions <- sqldf("select feature, n from df_correlations where rrr >= 0.35 order by rrr desc")

df_cor_waste     <-correlate(df_top_waste,     df_waste,     ff_results, sig = .1, file = paste(outdir, "10_cor_waste.pdf",     sep="/"))
df_cor_defects   <-correlate(df_top_defects,   df_defects,   ff_results, sig = .1, file = paste(outdir, "10_cor_defects.pdf",   sep="/"))
df_cor_revisions <-correlate(df_top_revisions, df_revisions, ff_results, sig = .1, file = paste(outdir, "10_cor_revisions.pdf", sep="/"))
 













