##############################################################
#
# General Helper Functions
#
# (c) 2017 Matthias Nott, SAP
#
###############################################################


#
# Import Data
#
# Imports a Data File.
#
# Args:
#   file: The location of the data file.
#   type: The file type. Default: rdata
#
# Returns:
#   A Data frame containing the data
#
import_data <- function(file, type="rdata") {
  df<-c()

  if (type == "spss") {
    ap <- read_spss(file)
    df <- data.frame(ap)
  } else if (type == "rdata") { # .RData
    load(file)
    df <- data.frame(data)
    # TODO: See if this is really always a "data" variable
    #       for now it works if we export from VIMGUI
    # TODO: Add CSV import
  } else if (type == "csv") { # .csv
    df <- read.csv(file, header = TRUE)
  } else {
    # TODO: others
  }
  return(df)
}


#
# Export Data
#
# Exports a Data File
#
# Args:
#   df  : The data frame to export
#   file: The target file
#   type: The file type. Default: csv
#
export_data <- function(df, file, type="csv") {
  if (type == "csv") {
    write.csv(df, file=file)
  }
}


#
# Print Missing Values
#
# Notice that this will not detect empty strings
# etc.
#
# Args:
#   df: The dataframe to look at
#
missing_values <- function(df) {
  for (Var in names(df)) {
    missing <- sum(is.na(df[,Var]))
    if (missing > 0) {
      print(c(Var,missing))
    }
  }
}


#
# Split data at a value
#
# Args:
#   df: The dataframe to split
#  var: The variable name to split
#  val: The value to split
# Returns: A list with two data frames.
#  Index 1 contains the part of the data
#  where the variable was != the value;
#  Index 2 contains the part of the data
#  where the variable was == the value.
#
split_data <- function(df, var, val) {
  return(split(df, df[var] == val))
}


#
# Center all Columns in Data Frame
#
center_all <- function(df) {
  dfx<-df
  for (i in names(dfx)) {
    i <- as.name(i)
    dfx[[i]]<-c(scale(dfx[[i]], center=TRUE, scale=FALSE))
  }
  return (dfx)
}




# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
# Source:
#
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#
# Helper function for the corrplot
#
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}


#
# Get a decent correlation plot
#
# Depends on: cor.mtest
#             library(corrplot)
#
# df_top_features: The list of features that we want to correlate
# df_reference   : The list of base values we want to correlate the features with (merged by "Date" column)
# df_base        : The base results table where we find the feature values as "Mean" and their name as "Feature"
# file           : The file name to dump the plot to as PDF (optional)
# sig            : The cutoff significance level (default: .1)
# size           : The size of the PDF, if any (default: 15)
# returns        : The data frame on which the correlations were calculated; can be used for e.g. chart.Correlation(df)
#
library(corrplot)
correlate <- function(df_top_features, df_reference, df_base, file = "", sig = .1, size = 15) {
  df_wdrf <- df_reference
  for (feature in df_top_features$feature) {
    df_feature <- sqldf(sprintf("select Date, avg(Mean) as '%s' from df_base where Feature == '%s' group by Date", feature, feature))
    df_wdrf <- merge(df_wdrf, df_feature, by = "Date")    
  }
  
  df_wdrf$Date <- NULL
  
  par(mfrow=c(1,1))

  if(file != "") {
    pdf(file, width = size, height = size)
  }
  
  # chart.Correlation(df_wdrf)
  # instead, we do something nicer
  
  p.mat <- cor.mtest(df_wdrf)#$p
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  corrplot(cor(df_wdrf), 
           method = "color", 
           col = col(200),
           type = "lower", 
           #order = "hclust", 
           number.cex = .7,
           addCoef.col = "black", # Add coefficient of correlation
           tl.col      = "black",
           tl.srt = 35,   # Text label color and rotation
           p.mat = p.mat, # Combine with significance
           sig.level = sig, insig = "blank", 
           # hide correlation coefficient on the principal diagonal
           diag = T)
  
  if(file != "") {
    dev.off()
  }
  
  return(df_wdrf)  
}
