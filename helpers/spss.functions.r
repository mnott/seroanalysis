#
# SPSS Compatibility Functions
#
# (c) 2017 Matthias Nott, SAP
#

#
# Use SPSS Kurtosis
#
# We want to have similar results to SPSS, hence we use this.
# We extend it to using z-Scores as well, as per Field (2009, p. 139)
# (significance values 1.96 / 2.58 / 3.29)
#
# http://www.stat.cmu.edu/~hseltman/files/spssSkewKurtosis.R
#
spssSkewKurtosis=function(x) {
  x <- na.omit(x)
  w=length(x)
  m1=mean(x)
  m2=sum((x-m1)^2)
  m3=sum((x-m1)^3)
  m4=sum((x-m1)^4)
  s1=sd(x)
  skew=w*m3/(w-1)/(w-2)/s1^3
  sdskew=sqrt( 6*w*(w-1) / ((w-2)*(w+1)*(w+3)) )
  zskew=skew/sdskew
  kurtosis=(w*(w+1)*m4 - 3*m2^2*(w-1)) / ((w-1)*(w-2)*(w-3)*s1^4)
  sdkurtosis=sqrt( 4*(w^2-1) * sdskew^2 / ((w-3)*(w+5)) )
  zkurtosis=kurtosis/sdkurtosis
  mat=matrix(c(skew,kurtosis, sdskew,sdkurtosis, zskew,zkurtosis ), 2,
             dimnames=list(c("skew","kurtosis"), c("estimate","se","z")))
  return(round(mat,3))
}

#
# Frequency Plot
#
freqplot=function(data, header, xtitle) {
  myhist <- hist(data, las=2, main=header, xlab=xtitle, plot=FALSE)
  multiplier <- myhist$counts / myhist$density
  mydensity <- density(data)
  myx <-seq(min(data), max(data), length.out = 100)
  mymean <- mean(data)
  mysd   <- sd(data)
  normal <- dnorm(x=myx, mean=mymean, sd=mysd)
  mymin = min(normal*multiplier[1], myhist$counts, 0)
  mymax = max(normal*multiplier[1], myhist$counts)
  myhist <- hist(data, las=2, main=header, xlab=xtitle, ylim = c(mymin,mymax), plot=TRUE)
  lines(myx, normal*multiplier[1], col="blue", lwd=2)
}

#
# General code to reshape two vectors into a long data.frame
#
twoVarWideToLong <- function(sample1,sample2) {
  res <- data.frame(
    GroupID=as.factor(c(rep(1, length(sample1)), rep(2, length(sample2)))),
    DV=c(sample1, sample2)
  )   
}  

#
# Independent Samples Test
#
# install.packages("car")
library(car)
indeptest = function(sample1, sample2) {
  long.data <- twoVarWideToLong(sample1, sample2)

  print(leveneTest(DV~GroupID,long.data))
  
  t.test(long.data$DV ~ long.data$GroupID, var.equal = T)
}

#
# ETA Squared
#
# install.packages("lsr")
library(lsr)

etasq = function(dep, indep, data_frame) {
  mod<-lm(dep ~ indep, data=data_frame)
  a<-etaSquared(mod, type=2, anova=FALSE)
  etasq=a[1,1]
  eta=sqrt(etasq)

  mat=matrix(c(eta, etasq), 1,
             dimnames=list(c("result"),c("eta","etasq")))
  return(round(mat,3))
}


qq_get_p <- function(x, method=1, ties.method = "average")
{
  if (method < 1 | method > 7) 
    stop("'method' must be a number between 1 and 7:", 
         "\n\t1 = Blom \n\t2 = Rankit / Hazen \n\t3 = Tukey",
         "\n\t4 = van de Waerden / Weibull \n\t5 = Benard and Bos-Levenbach",
         "\n\t6 = Gringorten\n\t7 = Yu and Huang", call. = FALSE) 
  n <- length(x) 
  #i <- order(order(x))   # to recreate original order from sorted data
  #xs <- sort(x)
  r <- rank(x, ties.method = ties.method)
  p <- switch(method,
              "1" = (r - 3/8) / (n + 1/4),      # Blom
              "2" = (r - 1/2) / n,              # Rankit / Hazen
              "3" = (r - 1/3) / (n + 1/3),      # Tukey   
              "4" = r / (n + 1),                # van de Waerden / Weibull
              "5" = (r - 3/10) / (n + 4/10),    # Benard and Bos-Levenbach
              "6" = (r - 0.44) / (n + 0.12),    # Gringorten
              "7" = (r - 0.326) / (n + 0.348))  # Yu and Huang 
  #p[i]
  p
}


#' SPSS like QQ-plot
#'
#' The QQ-plot in SPSS and R looks very different. The points
#' points and the QQ-line are positioned differently. 
#' \code{qqnorm_spss} implements a version of the QQ-plot that resembles
#' the SPSS version. The function returns an object containing the 
#' processed data. The output can be plotted using the function \code{plot} 
#' and \code{ggplot}. The parameters that can be passed to the 
#' plotting functions are documented in \code{\link{plot.qqnorm.spss}} and
#' \code{\link{ggplot.qqnorm.spss}}.
#' 
#' @param x A numeric vector.
#' @param standardize Whether the quantiles of the standardized values
#'  should be displayed. The default is to display the quantiles using the
#'  original data.
#' @param method The method used to assign probabilties for the
#'  ranks that are then converted into quantiles.   
#'  The following methods are implemented (see Castillo-Gutiérrez, Lozano-Aguilera, 
#'  & Estudillo-Martínez, 2012): 
#'  \code{1 =} Blom (default), \code{2 =} Rankit / Hazen, \code{3 =} Tukey,
#'  \code{4 =} Van der Waerden / Weibull, \code{5 =} Benard and Bos-Levenbach,
#'  \code{6 =} Gringorten and \code{7 =} Yu and Huang.
#' @param  ties.method Method to assign ranks to ties. One of 
#'  \code{"average", "first", "random", "max", "min"}. See \code{ties.method} 
#'  argument from \code{\link{rank}} for more details.
#' 
#' @return An list object of class \code{qqnorm.spss} with the 
#'  following elements:
#'  \item{x}{The orginal data}
#'  \item{y}{Corresponding quantiles in original scaling}
#'  \item{x.std}{Standardized values}
#'  \item{y.std}{Corresponding quantiles for standardized values}
#'  \item{method.name}{Name of the method to assign probabilities to ranks}
#'  \item{ties.method}{Method to treat ties}
#'  \item{xname}{Name of the variable used to produce the plot}
#' @references 
#'  Castillo-Gutiérrez, S., Lozano-Aguilera, E., & Estudillo-Martínez, M. D. (2012). 
#'    Selection of a Plotting Position for a Normal Q-Q Plot. R Script. 
#'    \emph{Journal of Communication and Computer, 9}(3), 243–250.
#' @export
#' @section TODO:
#' Check output against SPSS results. 
#' @example inst/examples/example-qq-plot.R
#' 
qqnorm_spss <- function(x, standardize=FALSE, method=1, 
                        ties.method="average") 
{ 
  xname <- deparse(substitute(x))
  x <- na.omit(x)
  methods <- c('Blom'=1, 'Rankit / Hazen'=2, 'Tukey'=3, 'Van der Waerden / Weibull'=4,
               'Benard and Bos-Levenbach'=5, 'Gringorten'=6, 'Yu and Huang'=7)
  method.name <- names(methods[method])
  p <- qq_get_p(x, method=method, ties.method=ties.method)
  y.std <- qnorm(p) 
  x.std <- as.vector(scale(x))
  y <- y.std * sd(x) + mean(x)
  l <- list(x=x, y=y, x.std=x.std, y.std=y.std, 
            method.name=method.name,
            standardize=standardize,
            ties.method=ties.method,
            xname=xname)
  class(l) <- "qqnorm.spss"
  l
} 


#' Plot the output from \code{qqplot.spss}
#' 
#' @param x An object as returned by \code{\link{qqnorm_spss}}
#' @param plottype The type of plot created. 
#'  \code{1 =} Standard QQ-plot, \code{2 =} Detrended QQ-plot.
#' @param line Whether to plot a QQ-line (defaul is \code{TRUE})
#' @param l.col Color of the QQ-line.
#' @param ... Passed to \code{plot} method.
#' @export
#' @keywords internal
#' 
plot.qqnorm.spss <- function(x, plottype=1, line=TRUE,
                             l.col="black", headervariable = "", ...) 
{
  qq <- x
  x <- qq$x
  y <- qq$y
  
  if(headervariable == "") {
    headervariable <- qq$xname  
  }
  
  main <- paste("Normal Q-Q plot of", headervariable) 
  xlab <- "Observed value"
  ylab <- "Expected normal value"
  if (qq$standardize) {
    x <- qq$x.std
    y <- qq$y.std
    xlab <- "Standardized observed value"
  }  
  if (plottype == 2) {        # convert to detrended data
    main <-  paste("Detrended normal Q-Q plot of", headervariable) 
    ylab <- "Deviation from normal"
    y <- x - y
  }
  plot(x, y, main=main,   # cex=.8, pch=16, col="black"
       xlab=xlab, ylab=ylab, ...)
  if (line) {
    if (plottype == 2)          # detrended plot
      abline(h=0, col=l.col)    # zero line is shown
    else                        # standard plot
      abline(0, 1, col=l.col)   # slope of 1
  }  
}


#' Plot the output from \code{qqplot.spss} using \code{ggplot2}
#' 
#' @param x An object as returned by \code{\link{qqnorm_spss}}
#' @param plottype The type of plot created. 
#'  \code{1 =} Standard QQ-plot, \code{2 =} Detrended QQ-plot.
#' @param line Whether to plot a QQ-line (defaul is \code{TRUE})
#' @param l.col Color of the QQ-line.
#' @param ... Not evaluated.
#' @return A ggplot object.
#' @export
#' @keywords internal
#' 
ggplot.qqnorm.spss <- function(x, plottype=1, line=TRUE,
                               l.col="black", headervariable = "", ...) 
{
  qq <- x
  x <- qq$x
  y <- qq$y
  
  if(headervariable == "") {
    headervariable <- qq$xname  
  }

  main <- paste("Normal Q-Q plot of", headervariable) 
  xlab <- "Observed value"
  ylab <- "Expected normal value"
  if (qq$standardize) {
    x <- qq$x.std
    y <- qq$y.std
    xlab <- "Standardized observed value"
  }  
  if (plottype == 2) {        # convert to detrended data
    main <-  paste("Detrended Normal Q-Q plot of", headervariable) 
    ylab <- "Deviation from normal"
    y <- x - y
  }
  d <- data.frame(x, y)
  g <- ggplot2::ggplot(data=d, aes(x,y)) + 
    ggplot2::geom_point() + 
    ggplot2::xlab(xlab) + ggplot2::ylab(ylab) + ggplot2::ggtitle(main)
  
  if (line) {
    if (plottype == 2)          # detrended plot
      gline <- ggplot2::geom_abline(intercept = 0, slope=0, colour=l.col)
    else                        # standard plot
      gline <- ggplot2::geom_abline(intercept=0, slope=1, colour=l.col)   # slope of 1
    g <- g + gline
  }  
  g
}

