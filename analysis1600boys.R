
library(Rmisc)
library(stargazer)
library(rdd)
library(StatMeasures)
library(rddensity)
library(tidyverse)
library(highlight)
library(easycsv)
library(data.table)
library(stringr)
library(haven)
library(foreign)
library(readr)
library(effsize)
library(ggplot2)
library(vctrs)
library(ggpubr)
library(plyr)
library(dplyr)
library(ggthemes)


#Clean 
data1600meters <- read_dta("C:/Users/rowse/Dropbox/Track_Times/Data/Mens_1600_Data/cleaned_1600boysdata.dta")

#Super Clean 
#data1600meters <- read_dta("C:/Users/rowse/Dropbox/Track_Times/Data/New Data/clean_1600boys.dta")





#Histogram of All Race Outcomes#
all_hist <- ggplot(data1600meters, aes(x=inttime)) + 
  geom_histogram(binwidth = 1, fill = "dodgerblue", color = "white", position = "dodge") + scale_x_continuous(name = "Time", limits = c(240,390), breaks = c(240, 270, 300, 330, 360, 390), 
                                                                                                              labels = c('4:00', '4:30', '5:00', '5:30', '6:00', '6:30')) + scale_y_continuous(name = "Count (in thousands)", breaks = c(5000, 10000, 15000, 20000, 25000, 30000), labels = c("5", "10", "15", "20", "25", "30")) + ggtitle('Boys 1,600: All Race Outcomes') + theme(legend.position = "none") 
all_hist

#Create an Integer for EndPr

data1600meters$int_seasonalbest <- as.integer(data1600meters$time_bt_season)
data1600meters$int_careerbest <- as.integer(data1600meters$time_bt_career)

seasonbest_data <- data1600meters[c("int_seasonalbest", "id")]
seasonbest_data <- unique(seasonbest_data)

careerbest_data <- data1600meters[c("int_careerbest", "AID")]
careerbest_data <- unique(careerbest_data)


#Histogram of Seasonal Records

seasonalrecord_hist <- ggplot(seasonbest_data, aes(x=int_seasonalbest)) + 
  geom_histogram(binwidth = 1, fill = "dodgerblue", color = "white", position = "dodge") + scale_x_continuous(name = "Time", limits = c(240,390), breaks = c(240, 270, 300, 330, 360, 390), 
                                                                                                              labels = c('4:00', '4:30', '5:00', '5:30', '6:00', '6:30')) + scale_y_continuous(name = "Count (in thousands)", breaks = c(2000, 4000, 6000,8000), labels = c("2", "4", "6", "8")) + ggtitle('Boys 1,600: Seasonal Records') + theme(legend.position = "none") 
seasonalrecord_hist

#Histogram of Career Records

careerrecord_hist <- ggplot(careerbest_data, aes(x=int_careerbest)) + 
  geom_histogram(binwidth = 1, fill = "dodgerblue", color = "white", position = "dodge") + scale_x_continuous(name = "Time", limits = c(240,390), breaks = c(240, 270, 300, 330, 360, 390), 
                                                                                                              labels = c('4:00', '4:30', '5:00', '5:30', '6:00', '6:30')) + scale_y_continuous(name = "Count (in thousands)", breaks = c(1000, 2000, 3000,4000), labels = c("1", "2", "3", "4")) + ggtitle('Boys 1,600: Career Records') + theme(legend.position = "none") 
careerrecord_hist

seasonbest_data <- NULL
careerbest_data <- NULL



#### McCrary Tests on the Density Functions ####

DCdensity2 <- function (runvar, cutpoint, bin = NULL, bw = NULL, verbose = FALSE, 
                        plot = TRUE, ext.out = FALSE, htest = FALSE, my_xlim = c(-0.5,0.5), my_title = "Default") # my_xlim param added
{
  runvar <- runvar[complete.cases(runvar)]
  rn <- length(runvar)
  rsd <- sd(runvar)
  rmin <- min(runvar)
  rmax <- max(runvar)
  if (missing(cutpoint)) {
    if (verbose) 
      cat("Assuming cutpoint of zero.\n")
    cutpoint <- 0
  }
  if (cutpoint <= rmin | cutpoint >= rmax) {
    stop("Cutpoint must lie within range of runvar")
  }
  if (is.null(bin)) {
    bin <- 2 * rsd * rn^(-1/2)
    if (verbose) 
      cat("Using calculated bin size: ", sprintf("%.3f", 
                                                 bin), "\n")
  }
  l <- floor((rmin - cutpoint)/bin) * bin + bin/2 + cutpoint
  r <- floor((rmax - cutpoint)/bin) * bin + bin/2 + cutpoint
  lc <- cutpoint - (bin/2)
  rc <- cutpoint + (bin/2)
  j <- floor((rmax - rmin)/bin) + 2
  binnum <- round((((floor((runvar - cutpoint)/bin) * bin + 
                       bin/2 + cutpoint) - l)/bin) + 1)
  cellval <- rep(0, j)
  for (i in seq(1, rn)) {
    cnum <- binnum[i]
    cellval[cnum] <- cellval[cnum] + 1
  }
  cellval <- (cellval/rn)/bin
  cellmp <- seq(from = 1, to = j, by = 1)
  cellmp <- floor(((l + (cellmp - 1) * bin) - cutpoint)/bin) * 
    bin + bin/2 + cutpoint
  if (is.null(bw)) {
    leftofc <- round((((floor((lc - cutpoint)/bin) * bin + 
                          bin/2 + cutpoint) - l)/bin) + 1)
    rightofc <- round((((floor((rc - cutpoint)/bin) * bin + 
                           bin/2 + cutpoint) - l)/bin) + 1)
    if (rightofc - leftofc != 1) {
      stop("Error occurred in bandwidth calculation")
    }
    cellmpleft <- cellmp[1:leftofc]
    cellmpright <- cellmp[rightofc:j]
    P.lm <- lm(cellval ~ poly(cellmp, degree = 4, raw = T), 
               subset = cellmp < cutpoint)
    mse4 <- summary(P.lm)$sigma^2
    lcoef <- coef(P.lm)
    fppleft <- 2 * lcoef[3] + 6 * lcoef[4] * cellmpleft + 
      12 * lcoef[5] * cellmpleft * cellmpleft
    hleft <- 3.348 * (mse4 * (cutpoint - l)/sum(fppleft * 
                                                  fppleft))^(1/5)
    P.lm <- lm(cellval ~ poly(cellmp, degree = 4, raw = T), 
               subset = cellmp >= cutpoint)
    mse4 <- summary(P.lm)$sigma^2
    rcoef <- coef(P.lm)
    fppright <- 2 * rcoef[3] + 6 * rcoef[4] * cellmpright + 
      12 * rcoef[5] * cellmpright * cellmpright
    hright <- 3.348 * (mse4 * (r - cutpoint)/sum(fppright * 
                                                   fppright))^(1/5)
    bw = 0.5 * (hleft + hright)
    if (verbose) 
      cat("Using calculated bandwidth: ", sprintf("%.3f", 
                                                  bw), "\n")
  }
  if (sum(runvar > cutpoint - bw & runvar < cutpoint) == 0 | 
      sum(runvar < cutpoint + bw & runvar >= cutpoint) == 0) 
    stop("Insufficient data within the bandwidth.")
  if (plot) {
    d.l <- data.frame(cellmp = cellmp[cellmp < cutpoint], 
                      cellval = cellval[cellmp < cutpoint], dist = NA, 
                      est = NA, lwr = NA, upr = NA)
    pmin <- cutpoint - 2 * rsd
    pmax <- cutpoint + 2 * rsd
    for (i in 1:nrow(d.l)) {
      d.l$dist <- d.l$cellmp - d.l[i, "cellmp"]
      w <- kernelwts(d.l$dist, 0, bw, kernel = "triangular")
      newd <- data.frame(dist = 0)
      pred <- predict(lm(cellval ~ dist, weights = w, data = d.l), 
                      interval = "confidence", newdata = newd)
      d.l$est[i] <- pred[1]
      d.l$lwr[i] <- pred[2]
      d.l$upr[i] <- pred[3]
    }
    d.r <- data.frame(cellmp = cellmp[cellmp >= cutpoint], 
                      cellval = cellval[cellmp >= cutpoint], dist = NA, 
                      est = NA, lwr = NA, upr = NA)
    for (i in 1:nrow(d.r)) {
      d.r$dist <- d.r$cellmp - d.r[i, "cellmp"]
      w <- kernelwts(d.r$dist, 0, bw, kernel = "triangular")
      newd <- data.frame(dist = 0)
      pred <- predict(lm(cellval ~ dist, weights = w, data = d.r), 
                      interval = "confidence", newdata = newd)
      d.r$est[i] <- pred[1]
      d.r$lwr[i] <- pred[2]
      d.r$upr[i] <- pred[3]
    }
    plot(d.l$cellmp, d.l$est, lty = 1, lwd = 2, col = "black", # xlim set here based on the parameter
         type = "l", xlim = my_xlim, ylim = c(min(cellval[cellmp <= 
                                                            
                                                            pmax & cellmp >= pmin]), max(cellval[cellmp <= 
                                                                                                   pmax & cellmp >= pmin])), xlab = NA, ylab = NA, 
         main = my_title)
    
    lines(d.l$cellmp, d.l$lwr, lty = 2, lwd = 1, col = "black", 
          type = "l")
    lines(d.l$cellmp, d.l$upr, lty = 2, lwd = 1, col = "black", 
          type = "l")
    lines(d.r$cellmp, d.r$est, lty = 1, lwd = 2, col = "black", 
          type = "l")
    lines(d.r$cellmp, d.r$lwr, lty = 2, lwd = 1, col = "black", 
          type = "l")
    lines(d.r$cellmp, d.r$upr, lty = 2, lwd = 1, col = "black", 
          type = "l")
    points(cellmp, cellval, type = "p", pch = 20)
  }
  cmp <- cellmp
  cval <- cellval
  padzeros <- ceiling(bw/bin)
  jp <- j + 2 * padzeros
  if (padzeros >= 1) {
    cval <- c(rep(0, padzeros), cellval, rep(0, padzeros))
    cmp <- c(seq(l - padzeros * bin, l - bin, bin), cellmp, 
             seq(r + bin, r + padzeros * bin, bin))
  }
  dist <- cmp - cutpoint
  w <- 1 - abs(dist/bw)
  w <- ifelse(w > 0, w * (cmp < cutpoint), 0)
  w <- (w/sum(w)) * jp
  fhatl <- predict(lm(cval ~ dist, weights = w), newdata = data.frame(dist = 0))[[1]]
  w <- 1 - abs(dist/bw)
  w <- ifelse(w > 0, w * (cmp >= cutpoint), 0)
  w <- (w/sum(w)) * jp
  fhatr <- predict(lm(cval ~ dist, weights = w), newdata = data.frame(dist = 0))[[1]]
  thetahat <- log(fhatr) - log(fhatl)
  sethetahat <- sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + 
                                                 (1/fhatl)))
  z <- thetahat/sethetahat
  p <- 2 * pnorm(abs(z), lower.tail = FALSE)
  if (verbose) {
    cat("Log difference in heights is ", sprintf("%.3f", 
                                                 thetahat), " with SE ", sprintf("%.3f", sethetahat), 
        "\n")
    cat("  this gives a z-stat of ", sprintf("%.3f", z), 
        "\n")
    cat("  and a p value of ", sprintf("%.3f", p), "\n")
  }
  if (ext.out) 
    return(list(theta = thetahat, se = sethetahat, z = z, 
                p = p, binsize = bin, bw = bw, cutpoint = cutpoint, 
                data = data.frame(cellmp, cellval)))
  else if (htest) {
    structure(list(statistic = c(z = z), p.value = p, method = "McCrary (2008) sorting test", 
                   parameter = c(binwidth = bin, bandwidth = bw, cutpoint = cutpoint), 
                   alternative = "no apparent sorting"), class = "htest")
  }
  else return(z)
}

###--------------------------------Men's 1600 McCrary-----------------------------------###

#We are going to run the McCrary Test From 270 to 480##


mccrary1600 <- NULL; 
for (i in 270:398)
{
  tmp <- DCdensity2(data1600meters$inttime, i, bin = 1)
  mccrary1600 <- data.frame(rbind(mccrary1600, tmp))
}


mccrary1600$zscores <- mccrary1600$rbind.mccrary1600..tmp.
mccrary1600$rbind.mccrary1600..tmp. <- NULL
mccrary1600$inttime <- c(270:398)
mccrary1600$sign <- ifelse(mccrary1600$zscores>0, "Positive", "Negative")



mens1600.mccraryplot <- ggplot(mccrary1600, aes(x = inttime, y = zscores, color = sign)) + 
  geom_point() + xlab ("Time") + ylab("McCrary Z-Scores") + ggtitle("McCrary Z-Score: Men's 1,600 Meters") + 
  scale_x_continuous(breaks=c(270, 280, 290, 300, 310, 320, 330, 340, 350, 360, 370, 380, 390, 400), labels = c('4:30', '4:40', '4:50',
                                                                                                                '5:00', '5:10', '5:20', '5:30', '5:40', '5:50', '6:00', '6:10', '6:20', 
                                                                                                                '6:30', '6:40')) + 
  theme(legend.position = "none")

mens1600.mccraryplot



## ----- Analyses ----- ##

#Create a Dummy above/under 300
data1600meters$min5 <- if_else(data1600meters$inttime >= 300, 1, 0)

#Graph the Probability of Improvement (all season) by Inttime

seasonalbest1600 <- ggplot(data1600meters, aes(inttime, improveprob)) + 
  geom_point() + geom_vline(xintercept = 300, color = "black", linetype = "longdash") + 
  ylab("Probability") + 
  xlab("Time") + ggtitle("Probability of Improvement (Rest-of-Season)") + scale_x_continuous(breaks = c(270, 285, 300, 315, 330, 345, 360), labels = c('4:30', '4:45', '5:00', '5:15', '5:30','5:45', '6:00'), limits = c(270,360)) + ylim(0.625,0.8) + theme_bw() + theme(plot.title = element_text(hjust = 0.5), panel.grid.minor = element_blank())

seasonalbest1600

minutes5rd <- ggplot(data1600meters, aes(inttime, improveprob, group = min5, fill = min5)) + 
  geom_point(shape = 1) + 
  stat_smooth(method=loess) + 
  geom_vline (xintercept = 300, color = "black", linetype = "longdash") + 
  scale_x_continuous(breaks = c(270, 285, 300, 315, 330), labels = c('4:30', '4:45', '5:00', '5:15', '5:30'), limits = c(270,330)) + ylab("Probability")+ ylim(0.625,0.775) +
  xlab("Time") + ggtitle("Probability of Improvement (Rest-of-Season)") + theme_bw() + theme(plot.title = element_text(hjust = 0.5),  panel.grid.minor = element_blank(), legend.position = "none")

minutes5rd


#Regression Discontinuity Tests

#Simple RD Model

data1600meters$centered_time <- data1600meters$time - 300

summary(rdrobust::rdrobust(data1600meters$improve, data1600meters$centered_time))

#RD w/ Grade

summary(rdrobust::rdrobust(data1600meters$improve, data1600meters$centered_time, 
                           covs = cbind(data1600meters$grade)))

#RD w/ Grade & EntryNum
summary(rdrobust::rdrobust(data1600meters$improve, data1600meters$centered_time, 
                           covs = cbind(data1600meters$grade, data1600meters$race_index)))

#RD w/ Grade, Entrynum, & Attempts Left
summary(rdrobust::rdrobust(data1600meters$improve, data1600meters$centered_time, 
                           covs = cbind(data1600meters$grade, data1600meters$race_index, 
                                        data1600meters$race_left)))


#RD w/ Grade, Entrynum, Attempts Left, & Progress 
summary(rdrobust::rdrobust(data1600meters$improve, data1600meters$centered_time, 
                           covs = cbind(data1600meters$grade, #FIGURE OUT PROGRESS , data1600meters$race_index,
                                        data1600meters$race_left)))

#Graph Probability of Improving (next race), by inttime

nextpr1600 <- ggplot(data1600meters, aes(inttime, data1600meters$'next', group = min5, fill = min5)) + 
  geom_point(shape = 1) + stat_smooth(method = "loess") + geom_vline(xintercept = 300, color = "black", linetype = "longdash") + 
  ylab("Probability") + 
  xlab("Time") + ggtitle("Probability of Improvement (Next Race)") + scale_x_continuous(breaks = c(285, 290, 295, 300, 305, 310, 315), labels = c( '4:45', '4:50', '4:55', '5:00','5:05', '5:10', '5:15'), limits = c(285,315)) + ylim(0.45,0.6) + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),panel.grid.minor = element_blank(), panel.grid.major = element_blank(), legend.position = "none")

nextpr1600

summary(rdrobust::rdrobust(data1600meters$improve_next, data1600meters$centered_time))


#Attempts Left Tests

factor(data1600meters$inttime)
racesleft_data <- summarySE(data1600meters, "race_left", c("inttime"), na.rm = TRUE)
racesleft_data

racesleft_plot <- ggplot(racesleft_data, aes(x = inttime, y = race_left)) + 
  geom_point() + ylab("Average Attempts Remaining") + geom_errorbar(aes(ymin=race_left-2*se, ymax=race_left+2*se), width=0.1) + xlab("Time") + ggtitle("Boys 1,600: Attempts Remaining in Season") + 
  scale_x_continuous(breaks = c(270, 280, 290, 300, 310, 320, 330), labels = c('4:30', '4:40', '4:50', '5:00', '5:10', '5:20', '5:30'), limits = c(270, 330)) + ylim(1, 2) + theme(legend.position = "none") 

racesleft_plot


#Finishing Position By Time
data1600meters$finish <- as.numeric(data1600meters$finish)
finish_data <- summarySE(data1600meters, measurevar = "finish", groupvars = c("inttime"), na.rm = TRUE)
finish_data

finishplot <- ggplot(finish_data, aes(x = inttime, y = finish)) + 
  geom_point() + geom_errorbar(aes(ymin=finish-2*se, ymax=finish+2*se), width=0.1) + ylab("Average Finishing Place") + xlab("Time") + 
  ggtitle("Boys 1,600: Finishing Place by Time") +scale_x_continuous(breaks = c(270, 280, 290, 300, 310, 320, 330), labels = c('4:30', '4:40', '4:50', '5:00', '5:10', '5:20', '5:30'), limits = c(270, 330)) + ylim(5,30)

finishplot


#Retention Tests

retention_data <- summarySE(data1600meters, measurevar = "retentiondummy", groupvars = c("inttime"), na.rm = TRUE)
retention_data

retention_plot <- ggplot(retention_data, aes(x = inttime, y = retentiondummy)) + 
  geom_point() + stat_smooth(method = "loess") + scale_x_continuous(breaks = c(270, 280, 290, 300, 310, 320, 330), labels = c('4:30', '4:40', '4:50', '5:00', '5:10', '5:20', '5:30'), limits = c(270, 330)) + xlab("Time") + 
  ylab("Probability of Returning Next Year") + ggtitle("Boys 1,600: Retention Probability by Seasonal Records") + ylim(0.1,0.2)

retention_plot


#Year-by-Year Analysis

year2009 <- subset(data1600meters, year == 2009)
improve_2009 <- summarySE(year2009, measure = "improve", groupvars = c("inttime"), na.rm = TRUE)
improve_2009

graph_2009 <- ggplot(improve_2009, aes(inttime, improve)) + 
  geom_point() + geom_vline(xintercept = 300, color = "black", linetype = "longdash") + 
  ylab("Improvement Probability") + 
  xlab("Time") + ggtitle("H.S. Boys 1,600: Probability of Improvement") + scale_x_continuous(breaks = c(270, 285, 300, 315, 330, 345, 360), labels = c('4:30', '4:45', '5:00', '5:15', '5:30','5:45', '6:00'), limits = c(270,360))

graph_2009

year2010 <- subset(data1600meters, year == 2010)
improve_2010 <- summarySE(year2010, measure = "improve", groupvars = c("inttime"), na.rm = TRUE)
improve_2010

graph_2010 <- ggplot(improve_2010, aes(inttime, improve)) + 
  geom_point() + geom_vline(xintercept = 300, color = "black", linetype = "longdash") + 
  ylab("Improvement Probability") + 
  xlab("Time") + ggtitle("H.S. Boys 1,600: Probability of Improvement") + scale_x_continuous(breaks = c(270, 285, 300, 315, 330, 345, 360), labels = c('4:30', '4:45', '5:00', '5:15', '5:30','5:45', '6:00'), limits = c(270,360))

graph_2010

year2011 <- subset(data1600meters, year == 2011)
improve_2011 <- summarySE(year2011, measure = "improve", groupvars = c("inttime"), na.rm = TRUE)
improve_2011

graph_2011 <- ggplot(improve_2011, aes(inttime, improve)) + 
  geom_point() + geom_vline(xintercept = 300, color = "black", linetype = "longdash") + 
  ylab("Improvement Probability") + 
  xlab("Time") + ggtitle("H.S. Boys 1,600: Probability of Improvement") + scale_x_continuous(breaks = c(270, 285, 300, 315, 330, 345, 360), labels = c('4:30', '4:45', '5:00', '5:15', '5:30','5:45', '6:00'), limits = c(270,360))

graph_2011

year2012 <- subset(data1600meters, year == 2012)
improve_2012 <- summarySE(year2012, measure = "improve", groupvars = c("inttime"), na.rm = TRUE)
improve_2012

graph_2012 <- ggplot(improve_2012, aes(inttime, improve)) + 
  geom_point() + geom_vline(xintercept = 300, color = "black", linetype = "longdash") + 
  ylab("Improvement Probability") + 
  xlab("Time") + ggtitle("H.S. Boys 1,600: Probability of Improvement") + scale_x_continuous(breaks = c(270, 285, 300, 315, 330, 345, 360), labels = c('4:30', '4:45', '5:00', '5:15', '5:30','5:45', '6:00'), limits = c(270,360))

graph_2012

year2013 <- subset(data1600meters, year == 2013)
improve_2013 <- summarySE(year2013, measure = "improve", groupvars = c("inttime"), na.rm = TRUE)
improve_2013

graph_2013 <- ggplot(improve_2013, aes(inttime, improve)) + 
  geom_point() + geom_vline(xintercept = 300, color = "black", linetype = "longdash") + 
  ylab("Improvement Probability") + 
  xlab("Time") + ggtitle("H.S. Boys 1,600: Probability of Improvement") + scale_x_continuous(breaks = c(270, 285, 300, 315, 330, 345, 360), labels = c('4:30', '4:45', '5:00', '5:15', '5:30','5:45', '6:00'), limits = c(270,360))

graph_2013

year2014 <- subset(data1600meters, year == 2014)
improve_2014 <- summarySE(year2014, measure = "improve", groupvars = c("inttime"), na.rm = TRUE)
improve_2014

graph_2014 <- ggplot(improve_2014, aes(inttime, improve)) + 
  geom_point() + geom_vline(xintercept = 300, color = "black", linetype = "longdash") + 
  ylab("Improvement Probability") + 
  xlab("Time") + ggtitle("H.S. Boys 1,600: Probability of Improvement") + scale_x_continuous(breaks = c(270, 285, 300, 315, 330, 345, 360), labels = c('4:30', '4:45', '5:00', '5:15', '5:30','5:45', '6:00'), limits = c(270,360))

graph_2014

year2015 <- subset(data1600meters, year == 2015)
improve_2015 <- summarySE(year2015, measure = "improve", groupvars = c("inttime"), na.rm = TRUE)
improve_2015

graph_2015 <- ggplot(improve_2015, aes(inttime, improve)) + 
  geom_point() + geom_vline(xintercept = 300, color = "black", linetype = "longdash") + 
  ylab("Improvement Probability") + 
  xlab("Time") + ggtitle("H.S. Boys 1,600: Probability of Improvement") + scale_x_continuous(breaks = c(270, 285, 300, 315, 330, 345, 360), labels = c('4:30', '4:45', '5:00', '5:15', '5:30','5:45', '6:00'), limits = c(270,360))

graph_2015

year2016 <- subset(data1600meters, year == 2016)
improve_2016 <- summarySE(year2016, measure = "improve", groupvars = c("inttime"), na.rm = TRUE)
improve_2016

graph_2016 <- ggplot(improve_2016, aes(inttime, improve)) + 
  geom_point() + geom_vline(xintercept = 300, color = "black", linetype = "longdash") + 
  ylab("Improvement Probability") + 
  xlab("Time") + ggtitle("H.S. Boys 1,600: Probability of Improvement") + scale_x_continuous(breaks = c(270, 285, 300, 315, 330, 345, 360), labels = c('4:30', '4:45', '5:00', '5:15', '5:30','5:45', '6:00'), limits = c(270,360)) + ylim(0.55,0.76)
                                                                                                                                                                                                                                           

graph_2016

year2017 <- subset(data1600meters, year == 2017)
improve_2017 <- summarySE(year2017, measure = "improve", groupvars = c("inttime"), na.rm = TRUE)
improve_2017

graph_2017 <- ggplot(improve_2017, aes(inttime, improve)) + 
  geom_point() + geom_vline(xintercept = 300, color = "black", linetype = "longdash") + 
  ylab("Improvement Probability") + 
  xlab("Time") + ggtitle("H.S. Boys 1,600: Probability of Improvement") + scale_x_continuous(breaks = c(270, 285, 300, 315, 330, 345, 360), labels = c('4:30', '4:45', '5:00', '5:15', '5:30','5:45', '6:00'), limits = c(270,360)) + ylim(0.5,0.775)

graph_2017

year2018 <- subset(data1600meters, year == 2018)
improve_2018 <- summarySE(year2018, measure = "improve", groupvars = c("inttime"), na.rm = TRUE)
improve_2018

graph_2018 <- ggplot(improve_2018, aes(inttime, improve)) + 
  geom_point() + geom_vline(xintercept = 300, color = "black", linetype = "longdash") + 
  ylab("Improvement Probability") + 
  xlab("Time") + ggtitle("H.S. Boys 1,600: Probability of Improvement") + scale_x_continuous(breaks = c(270, 285, 300, 315, 330, 345, 360), labels = c('4:30', '4:45', '5:00', '5:15', '5:30','5:45', '6:00'), limits = c(270,360)) + ylim(0.55,0.76)

graph_2018

year2019 <- subset(data1600meters, year == 2019)
improve_2019 <- summarySE(year2019, measure = "improve", groupvars = c("inttime"), na.rm = TRUE)
improve_2019

graph_2019 <- ggplot(improve_2019, aes(inttime, improve)) + 
  geom_point() + geom_vline(xintercept = 300, color = "black", linetype = "longdash") + 
  ylab("Improvement Probability") + 
  xlab("Time") + ggtitle("H.S. Boys 1,600: Probability of Improvement") + scale_x_continuous(breaks = c(270, 285, 300, 315, 330, 345, 360), labels = c('4:30', '4:45', '5:00', '5:15', '5:30','5:45', '6:00'), limits = c(270,360)) + ylim(0.5,0.76)

graph_2019

