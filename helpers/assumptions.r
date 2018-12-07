#
# Testing Assumptions
#
# (c) 2017 Matthias Nott, SAP
#

#
# Dependencies
#

# install.packages("nortest")
library(nortest) # For Anderson-Darling Test. Needs nortest package

# install.packages("moments")
library(moments) # For Skewness/Kurtosis. Needs moments package

# install.packages("lmtest")
library(lmtest)  # For Heteroscedasticity tests. Needs lmtest package

# install.packages("gvlma")
library(gvlma)   # Check assumptions for regression/normality

# install.packages("ggplot2")
library(ggplot2) # For Plots. Needs ggplot2 package


#
# Load functions for labelled boxplots
#
source("helpers/boxplot.with.outlier.label.r")

source("helpers/spss.functions.r")

#
# Test for Normality
#
normality=function(x, header = "", file = "", width = 32, height = 20, headervariable = "") {
  require(qqplotr)
  x<-na.omit(x)
  if( NROW(x) <= 5000 ) {
    print(shapiro.test(x))  
  }
  if(file != "") {
    pdf(file, width = width, height = height) # Open a new pdf file
  }
  par(mfrow=c(2,2))
  #  hist(x)
  freqplot(x, "", "")
  
  p <- qqnorm_spss(x, standardize = FALSE, method = 1, ties.method = "average")
  plot.qqnorm.spss(p, plottype = 2, line = "TRUE", l.col="red", headervariable = headervariable)
  
  boxplot.with.outlier.label(x,x)
  
  plot.qqnorm.spss(p, plottype = 1, line = "TRUE", l.col="red", headervariable = headervariable)
  
  title(header, side = 3, line = -1, outer=TRUE)
  
  if(file != "") {
    dev.off()
  }
  print(ad.test(x))
  spssSkewKurtosis(x)
}


outlierplot=function(x, header) {
  par(mfrow=c(1,1))
  x<-na.omit(x)
  boxplot.with.outlier.label(x,x)
  title(header, side = 3, line = -1, outer=TRUE)
}


#
# Next, we are going to check for a bunch of assumptions that we
# find on http://r-statistics.co/Assumptions-of-Linear-Regression.html
#
#
heteroscedasticity=function(dep, indep, data_frame, header = "", file="", width = 7.85, height = 7.85){
  mod <- lm(dep ~ indep, data=data_frame)
  
  # Assumption: The mean of residuals is 0
  print(mean(mod$residuals))
  
  # Assumption: No autocorrelation of residuals
  # (Drop to near 0 after first line)
  par(mfrow=c(1,1))
  acf(mod$residuals)
  
  # Assumption: homoscedasticity or equal variance (lower left plot line orizontal)

  if(file != "") {
    pdf(file, width = width, height = height) # Open a new pdf file
  }
  par(mfrow=c(2,2))
  plot(mod)
  title(header, side = 3, line = -2, outer=TRUE)
  
  if(file != "") {
    dev.off()
  }
  
  # Use the gvlma assumptions test for the linear model
  gvlma(mod)
  
  # Print the outliers
  # print(influence.measures(mod))
}


#
# Linearity checks (bivariate)
#
linearity=function(dep, indep, data_frame, header = "", file="", width = 7.85, height = 7.85) {
  if(file != "") {
    pdf(file, width = width, height = height) # Open a new pdf file
  }
  par(mfrow=c(1,1))
  scatterplot(indep, dep,  data=data_frame)
  if(header != "") {
    title(header, side = 3, line = -2, outer=TRUE)
  }
  if(file != "") {
    dev.off()
  }
}


#
# Convert Pearson's r into a z-score
#
# Field, 2009, p. 171-172
#
# install.packages("SciViews")
library(SciViews)
zpearson=function(r, n) {
  zr = 1/2 * ln((1+r)/(1-r))
  se = 1/sqrt(n-3)
  z = zr / se
  p = 2*pnorm(-abs(z))
  #  t = r * sqrt(n-2)/sqrt(1-r**2)
  
  #
  # Calculate conf interval for r (Field, 2009, p. 173)
  #
  # This shows how to calculate them with SPSS; R's cortest
  # doest that for us for free.
  #
  # zl = zr - 1.96*se
  # zu = zr + 1.96*se
  # rl = (e**(2*zl)-1)/(e**(2*zl)+1)
  # ru = (e**(2*zu)-1)/(e**(2*zu)+1)
  
  mat=matrix(c(z, p, r*r, n), 1, dimnames=list(c("result"),
             c("z(r)","p(z)", "R^2", "n")))
  return(round(mat,3))
}


#
# Convert Pearson's r into a z-scrore
# based on the output of a cor.test
#
zpearsont=function(cortest) {
  r = cortest[["estimate"]]
  n = cortest[["parameter"]]
  print(cortest)
  return(zpearson(r, n))
}


