# Keegan's clam data - assessing distribution of elevations at which clams were found

# elevtest is Keegan's tidal elevation data for each Nuttallia hole (PCPP, fall 2019)
# elevtest is a single column of the elevations at which clams were found (each hole has a unique elevation...I think)
# each elevation is repeated as many times as there were clams in that hole (if no clams, elevation is not listed)
# thus, can generate probability distribution function of the elevations

# distribution fitting code from http://www.di.fc.ul.pt/~jpn/r/distributions/fitting.html
# distribution descriptors from http://civil.colorado.edu/~balajir/CVEN5454/R-sessions/sess2/intro2fitdistrplus.pdf

# note that plotting FREQUENCY uses raw numbers;
# plotting DENSITY generates pdf such that sum(density x bin size) = 1
# neither of these plots the PROBABILITY/PROPORTION of clams in each elevation bin

# install.packages("fitdistrplus")
# install.packages("readxl")

library(fitdistrplus)
library(readxl)

# 1. read in data
ele_Apres <- df.Ancova.Size$Elevation[(df.Ancova.Size$APresence==1)]


# 2. make exploratory plot of histogram and cdf to visualize data
par(mfrow=c(1,1))
plotdist(ele_Apres, histo = TRUE, demp = TRUE)

# 3. adjust histogram parameters to establish desired bin size and axis ranges
# forces to min and max of data (good)
# specifies number of breaks (good)
# seems to generate equal but unspecified bin size (can determine manually but there must be a better way)
h<-hist(ele_Apres, breaks=seq(min(ele_Apres), max(ele_Apres), length.out=6), freq=FALSE, axes=FALSE) # length.out = # of bins
# custom axes labeling - needs cleaning up
axis(side=1, at=seq(min(ele_Apres), max(ele_Apres), length.out=6)) 
axis(side=2, at=seq(0, 1, by=0.2))

# 4. fit data with 4 candidate distributions
fit_w  <- fitdist(ele_Apres, "weibull")
fit_g  <- fitdist(ele_Apres, "gamma")
fit_ln <- fitdist(ele_Apres, "lnorm")
fit_n  <- fitdist(ele_Apres, "norm")

# 5. make exploratory default plot with automatic bins + cdf, qq, pp
par(mfrow=c(2,2))
plot.legend <- c("Weibull", "lognormal", "gamma", "normal")
denscomp(list(fit_w, fit_g, fit_ln, fit_n), legendtext = plot.legend)
cdfcomp (list(fit_w, fit_g, fit_ln, fit_n), legendtext = plot.legend)
qqcomp  (list(fit_w, fit_g, fit_ln, fit_n), legendtext = plot.legend)
ppcomp  (list(fit_w, fit_g, fit_ln, fit_n), legendtext = plot.legend)

# 6. make pretty plot with custom bins for final figure

# generate distributions to plot
x=seq(0, 4, length=100)
ele_Apres_w = dweibull(x, shape=fit_w$estimate["shape"], scale=fit_w$estimate["scale"])
ele_Apres_g = dgamma(x,shape=fit_g$estimate["shape"], rate=fit_g$estimate["rate"])
ele_Apres_ln = dlnorm(x,meanlog=fit_ln$estimate["meanlog"], sdlog=fit_ln$estimate["sdlog"])
ele_Apres_n = dnorm(x,mean=fit_n$estimate["mean"], sd=fit_n$estimate["sd"])

# make custom histogram & add distributions (need to clean up axis labeling still)
par(mfrow=c(1,1), mar=c(4,4,1,6))
a <- seq(1, 3.5, .5)
hist(ele_Apres, breaks=a, freq=FALSE, 
    xlab="Elevation (m)", ylab="Probability Density", main = "",
    ylim=c(0,max(c(ele_Apres_w, density(ele_Apres)$y))))
par(xpd=FALSE)
lines(x=x, y=ele_Apres_w, lty=2, lwd=2)
lines(x=x, y=ele_Apres_g,lty=4, lwd=2)
lines(x=x, y=ele_Apres_ln, lty=3, lwd=2)
lines(x=x, y=ele_Apres_n, lty=5, lwd=2)
par(xpd=TRUE)
legend(3, 1, c("Weibull", "lognormal", "gamma", "normal"), col = "black",
       text.col = "black", lty = c(2, 3, 4, 5), lwd = c(2,2,2,2),
       merge = TRUE)

# 7. calculate AIC (and assorted other goodness of fit criteria)
gofstat(list(fit_w, fit_g, fit_ln, fit_n), fitnames = c("weibull", "gamma", "lognormal", "normal"))
