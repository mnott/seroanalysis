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


##############################################################################
#
# Read the Waste Table
#
##############################################################################

ff_waste <- read_excel("data/input/medina/L06/old/Wastes_August_september_m.xlsx", sheet="9000_E06")

# Group by Date for Waste
df <- sqldf('select Date, sum(Quantity)*5 as Waste from ff_waste group by Date')
normality(df$Waste, file = "", width = 16, height = 11, headervariable = "Waste")

# Shapiro-Wilk normality test
# 
# data:  x
# W = 0.587, p-value = 7.9e-13
# 
# 
# Anderson-Darling normality test
# 
# data:  x
# A = 6.06, p-value = 4.8e-15
# 
# estimate    se      z
# skew        4.826 0.285 16.946
# kurtosis   31.359 0.563 55.748

# Interpretation: This data is seriously not normally distributed as per
# numerical (SW and AD highly significant) as well as visual inspection.


# There are three outliers:
#         Date Waste
# 1 2018-08-01  3659
# 2 2018-08-14  8526
# 3 2018-10-03  3175
#
# Let's remove those outliers
if(outliers) {
  df <- sqldf('select Date, Waste from df where Waste < 3175')
  normality(df$Waste, file = "", width = 16, height = 11, headervariable = "Waste")
}

# Shapiro-Wilk normality test
# 
# data:  x
# W = 0.985, p-value = 0.58
# 
# 
# Anderson-Darling normality test
# 
# data:  x
# A = 0.253, p-value = 0.73
# 
# estimate    se      z
# skew        0.193 0.291  0.665
# kurtosis   -0.234 0.574 -0.408

# Interpretation: This data is pretty much normally distributed.


##############################################################################
#
# Waste vs. Defects
#
##############################################################################

ff_defects <- read_excel('data/input/medina/L06/old/inspection_results_and_defects_August_september.xlsx', 'Defects')
df2 <- sqldf('select Date, sum(NUM_DEFECTOS) as DefectCount from ff_defects group by Date')

normality(df2$DefectCount, file = "", width = 16, height = 11, headervariable = "Defect Count")
 
# Shapiro-Wilk normality test
# 
# data:  x
# W = 0.949, p-value = 0.036
# 
# 
# Anderson-Darling normality test
# 
# data:  x
# A = 0.449, p-value = 0.27
# 
# estimate    se     z
# skew        0.901 0.343 2.626
# kurtosis    2.155 0.674 3.196

# There's one outlier:
#
#         Date DefectCount
# 1 2018-09-27        6569
#
# Let's remove the one outlier

if(outliers) {
  df2 <- sqldf('select Date, DefectCount from df2 where DefectCount < 6569')
  normality(df2$DefectCount, file = "", width = 16, height = 11, headervariable = "Defect Count")
}

# Shapiro-Wilk normality test
# 
# data:  x
# W = 0.982, p-value = 0.66
# 
# 
# Anderson-Darling normality test
# 
# data:  x
# A = 0.273, p-value = 0.65
# 
# estimate    se      z
# skew        0.131 0.347  0.379
# kurtosis   -0.366 0.681 -0.538

# Interpretation: This data is pretty much normally distributed.


#-------------------------------------------------------------
#
# Do Correlation
#
#-------------------------------------------------------------
# If we do the correlation analysis, we see that with H0
# being no correlation, we all are below .000, which means
# we need to reject the null hypothesis.
#
# For the levels of the correlation (Pearsons r), we have
# the following rules of thumb [TODO: cit.]:
#
# Correlation Coefficient          Descriptor
#
#   .90 - .99                      Near perfect
#   .70 - .89                      Very strong
#   .50 - .69                      Substantial
#   .30 - .49                      Moderate
#   .10 - .29                      Low
#   .01 - .09                      Trivial
#-------------------------------------------------------------

#
# Correlation between Waste and Defects
#
df_waste_defects <- merge(df, df2, by = "Date")

heteroscedasticity(df_waste_defects$Waste, df_waste_defects$DefectCount, df_waste_defects, file = "")
linearity(df_waste_defects$Waste, df_waste_defects$DefectCount, data=df_waste_defects, file = "")
scatterplotMatrix(~ df_waste_defects$Waste + df_waste_defects$DefectCount, data=df_waste_defects)

options(digits=3)
zpearsont(cor.test(df_waste_defects$Waste, df_waste_defects$DefectCount,  use = "pairwise.complete.obs"))

frl<-sqldf('select Waste, DefectCount from df_waste_defects')
colnames(frl) <- c("Waste", "Defect Count")
pdf("data/output/medina/L06/r/01_waste_defect_corr.pdf", width = 7.9) # Open a new pdf file
chart.Correlation(frl)
dev.off()

options(digits=2)
# cor(fr, method="pearson", use = "pairwise.complete.obs")
rcorr(as.matrix(frl), type="pearson")


#
# Plot Waste and Defect Count in one Graph
#
df_waste_defects_melted <- reshape2::melt(df_waste_defects, id.var='Date')
pdf("data/output/medina/L06/r/01_waste_defect_plot.pdf", width = 7.9) # Open a new pdf file
ggplot(df_waste_defects_melted, aes(x=Date, y=value, col=variable)) + geom_line() + ggtitle("Waste vs. Defect Count")
dev.off()


##############################################################################
#
# Waste vs. Revisions
#
##############################################################################

ff_results <- read_excel('data/input/medina/L06/old/inspection_results_and_defects_August_september.xlsx', 'Inspection results')
df3 <- sqldf("select Date, count(Val)*100 as Revisions from ff_results where Val = 'R' group by Date")

normality(df3$Revisions, file = "", width = 16, height = 11, headervariable = "Revisions")

# Shapiro-Wilk normality test
# 
# data:  x
# W = 0.8, p-value = 1e-06
# 
# 
# Anderson-Darling normality test
# 
# data:  x
# A = 3, p-value = 1e-06
# 
# estimate   se   z
# skew          2.1 0.33 6.4
# kurtosis      6.3 0.66 9.6

# There are five outliers:
#
#         Date Revisions
# 1 2018-08-01      4900
# 2 2018-08-02      4500
# 3 2018-08-30      8400
# 4 2018-09-11      4700
# 5 2018-09-27      4600

if(outliers) {
  df3 <- sqldf('select Date, Revisions from df3 where Revisions < 4500')
  normality(df3$Revisions, file = "", width = 16, height = 11, headervariable = "Revisions")
}

# Shapiro-Wilk normality test
# 
# data:  x
# W = 0.9, p-value = 0.02
# 
# 
# Anderson-Darling normality test
# 
# data:  x
# A = 1, p-value = 0.01
# 
# estimate   se     z
# skew         0.57 0.35  1.64
# kurtosis    -0.64 0.69 -0.92


#
# Correlation between Waste and Revisions
#
df_waste_revisions <- merge(df, df3, by = "Date")

heteroscedasticity(df_waste_revisions$Waste, df_waste_revisions$Revisions, df_waste_revisions, file = "")
linearity(df_waste_revisions$Waste, df_waste_revisions$Revisions, data=df_waste_revisions, file = "")
scatterplotMatrix(~ df_waste_revisions$Waste + df_waste_revisions$Revisions, data=df_waste_revisions)

options(digits=3)
zpearsont(cor.test(df_waste_revisions$Waste, df_waste_revisions$Revisions,  use = "pairwise.complete.obs"))

frl<-sqldf('select Waste, Revisions from df_waste_revisions')
colnames(frl) <- c("Waste", "Revisions")
pdf("data/output/medina/L06/r/02_waste_revisions_corr.pdf", width = 7.9) # Open a new pdf file
chart.Correlation(frl)
dev.off()

options(digits=2)
# cor(fr, method="pearson", use = "pairwise.complete.obs")
rcorr(as.matrix(frl), type="pearson")


#
# Plot Waste and Defect Count in one Graph
#
df_waste_revisions_melted <- reshape2::melt(df_waste_revisions, id.var='Date')
pdf("data/output/medina/L06/r/02_waste_revisions_plot.pdf", width = 7.9) # Open a new pdf file
ggplot(df_waste_revisions_melted, aes(x=Date, y=value, col=variable)) + geom_line() + ggtitle("Waste vs. Revisions")
dev.off()


##############################################################################
#
# Defects vs. Revisions
#
##############################################################################


#
# Correlation between Waste and Revisions
#
df_defects_revisions <- merge(df2, df3, by = "Date")

heteroscedasticity(df_defects_revisions$DefectCount, df_defects_revisions$Revisions, df_defects_revisions, file = "")
linearity(df_defects_revisions$DefectCount, df_defects_revisions$Revisions, data=df_defects_revisions, file = "")
scatterplotMatrix(~ df_defects_revisions$DefectCount + df_defects_revisions$Revisions, data=df_defects_revisions)

options(digits=3)
zpearsont(cor.test(df_defects_revisions$DefectCount, df_defects_revisions$Revisions,  use = "pairwise.complete.obs"))

frl<-sqldf('select DefectCount, Revisions from df_defects_revisions')
colnames(frl) <- c("DefectCount", "Revisions")
pdf("data/output/medina/L06/r/03_defects_revisions_corr.pdf", width = 7.9) # Open a new pdf file
chart.Correlation(frl)
dev.off()

options(digits=2)
# cor(fr, method="pearson", use = "pairwise.complete.obs")
rcorr(as.matrix(frl), type="pearson")


#
# Plot Defect Count and Revisions in one Graph
#
df_defects_revisions_melted <- reshape2::melt(df_defects_revisions, id.var='Date')
pdf("data/output/medina/L06/r/03_defects_revisions_plot.pdf", width = 7.9) # Open a new pdf file
ggplot(df_defects_revisions_melted, aes(x=Date, y=value, col=variable)) + geom_line() + ggtitle("Revisions vs. Defect Count")
dev.off()


##############################################################################
#
# Combine Datasets into one single
#
##############################################################################

df_merged <- merge(df_waste_defects, df3, by = "Date")


##############################################################################
#
# Load Temperature Sala
#
##############################################################################

df_temp <- sqldf("select Date, avg(Media) as 'Temp' from ff_results where Feature == 'TEMPERATURA SALA' group by Date")
normality(df_temp$Temp, file = "", width = 16, height = 11, headervariable = "Temperatura Sala")

# Shapiro-Wilk normality test
# 
# data:  x
# W = 0.4, p-value = 2e-13
# 
# 
# Anderson-Darling normality test
# 
# data:  x
# A = 10, p-value <2e-16
# 
# estimate   se  z
# skew          4.6 0.34 14
# kurtosis     21.0 0.66 32


# There are two outliers:
#
#         Date Temp
# 1 2018-08-16   26
# 2 2018-08-22   27

if(outliers) {
  df_temp <- sqldf("select Date, Temp from df_temp where Temp < 25.65")
  normality(df_temp$Temp, file = "", width = 16, height = 11, headervariable = "Temperatura Sala")
}

# Shapiro-Wilk normality test
# 
# data:  x
# W = 1, p-value = 0.05
# 
# 
# Anderson-Darling normality test
# 
# data:  x
# A = 0.5, p-value = 0.2
# 
# estimate   se   z
# skew         0.87 0.34 2.5
# kurtosis     1.52 0.67 2.3



##############################################################################
#
# Defects vs. Temperature
#
##############################################################################


#
# Correlation between Temperatura Sala and Waste, Defects, Revisions
#
df_merged_test <- merge(df_merged, df_temp, by = "Date")


#
# Waste
#
heteroscedasticity(df_merged_test$Temp, df_merged_test$Waste, df_merged_test, file = "")
linearity(df_merged_test$Temp, df_merged_test$Waste, data=df_merged_test, file="")
scatterplotMatrix(~ df_merged_test$Temp + df_merged_test$Waste, data=df_merged_test)

options(digits=3)
zpearsont(cor.test(df_merged_test$Temp, df_merged_test$Waste,  use = "pairwise.complete.obs"))

frl<-sqldf('select Temp, Waste from df_merged_test')
colnames(frl) <- c("Temperatura Sala", "Waste")
pdf("data/output/medina/L06/r/04_temperature_waste_corr.pdf", width = 7.9) # Open a new pdf file
chart.Correlation(frl)
dev.off()

options(digits=2)
# cor(fr, method="pearson", use = "pairwise.complete.obs")
rcorr(as.matrix(frl), type="pearson")


#
# Defect Count
#
heteroscedasticity(df_merged_test$Temp, df_merged_test$DefectCount, df_merged_test, file = "")
linearity(df_merged_test$Temp, df_merged_test$DefectCount, data=df_merged_test, file="")
scatterplotMatrix(~ df_merged_test$Temp + df_merged_test$DefectCount, data=df_merged_test)

options(digits=3)
zpearsont(cor.test(df_merged_test$Temp, df_merged_test$DefectCount,  use = "pairwise.complete.obs"))

frl<-sqldf('select Temp, DefectCount from df_merged_test')
colnames(frl) <- c("Temperatura Sala", "DefectCount")
pdf("data/output/medina/L06/r/04_temperature_defects_corr.pdf", width = 7.9) # Open a new pdf file
chart.Correlation(frl)
dev.off()

options(digits=2)
# cor(fr, method="pearson", use = "pairwise.complete.obs")
rcorr(as.matrix(frl), type="pearson")


#
# Revisions
#
heteroscedasticity(df_merged_test$Temp, df_merged_test$Revisions, df_merged_test, file = "")
linearity(df_merged_test$Temp, df_merged_test$Revisions, data=df_merged_test, file="")
scatterplotMatrix(~ df_merged_test$Temp + df_merged_test$Revisions, data=df_merged_test)

options(digits=3)
zpearsont(cor.test(df_merged_test$Temp, df_merged_test$Revisions,  use = "pairwise.complete.obs"))

frl<-sqldf('select Temp, Revisions from df_merged_test')
colnames(frl) <- c("Temperatura Sala", "Revisions")
pdf("data/output/medina/L06/r/04_temperature_revisions_corr.pdf", width = 7.9) # Open a new pdf file
chart.Correlation(frl)
dev.off()

options(digits=2)
# cor(fr, method="pearson", use = "pairwise.complete.obs")
rcorr(as.matrix(frl), type="pearson")



#
# Plot all in one Graph
#
frl<-sqldf('select Waste, DefectCount, Revisions, Temp from df_merged_test')
colnames(frl) <- c("Waste", "Defect Count", "Revisions", "Temp")
pdf("data/output/medina/L06/r/05_temperature_wrd_corr.pdf", width = 7.9) # Open a new pdf file
chart.Correlation(frl)
dev.off()

options(digits=2)
# cor(fr, method="pearson", use = "pairwise.complete.obs")
rcorr(as.matrix(frl), type="pearson")


df_merged_test_melted <- reshape2::melt(df_merged_test, id.var='Date')
pdf("data/output/medina/L06/r/05_temperature_wrd_plot.pdf", width = 7.9) # Open a new pdf file
ggplot(df_merged_test_melted, aes(x=Date, y=value, col=variable)) + geom_line() + ggtitle("Temperatura Sala vs. Waste, Revisions, Defect Count")
dev.off()












