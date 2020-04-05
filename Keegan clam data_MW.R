# Keegan's clam data - assessing distribution of elevations at which clams were found

# Depth.Hole.Data_MW2 is Keegan's tidal elevation data for each Nuttallia hole (PCPP, fall 2019)
# rows with N/A for sediment are removed (so this dataset would be incomplete for elevation)
# each row is repeated as many times as there were clams in that hole (if no clams, elevation is not listed)
# thus, can generate probability distribution function of the gravel and PC1 values

# column pc1new is pc1 data shifted to the right by adding the abs value of the minimum pc1 value (bringing min value to 0)
# 	and adding 0.01 (bringing min value to 0.01) so that the weibull, gamma, and lognormal distributions could be fit
# rationale is that the *distribution* is still the same even though values are shifted
# would not use these transformed values for x-y plots or for stats - just to look at distribution
# could even plot it with original histogram, and distributions shifted back to the left (not done here)

# distribution fitting code from http://www.di.fc.ul.pt/~jpn/r/distributions/fitting.html
# distribution descriptors from http://civil.colorado.edu/~balajir/CVEN5454/R-sessions/sess2/intro2fitdistrplus.pdf

# note that plotting FREQUENCY uses raw numbers;
# plotting DENSITY generates pdf such that sum(density x bin size) = 1
# neither of these plots the PROBABILITY/PROPORTION of clams in each elevation bin

# install.packages("fitdistrplus")
# install.packages("readxl")

library(fitdistrplus)
library(readxl)

Depth.Hole.Data_MW2 <- read.csv(paste(wd,"/Depth.Hole.Data_MW.csv", sep=""))
View(Depth.Hole.Data_MW2)

### GRAVEL ANALYSIS
my_data<-Depth.Hole.Data_MW2$gravel
my_data <- df.Ancova.Size$gravel[!is.na(df.Ancova.Size$gravel)]
# check min & max values to see where histogram should sit
min(my_data)
max(my_data)

# 3. adjust histogram parameters to establish desired bin size and axis ranges
# forces to min and max of data (good)
# specifies number of breaks (good)
# seems to generate equal but unspecified bin size (can determine manually but there must be a better way)
h<-hist(my_data, breaks=seq(min(my_data), max(my_data), length.out=6), freq=FALSE, axes=FALSE) # length.out = # of bins

#axis(side=1, at=seq(min(my_data), max(my_data), length.out=6))
axis(side=1, at=seq(0.3, 0.9, length.out=6))
axis(side=2, at=seq(0, 3.5, by=0.5))

# 4. fit data with 4 candidate distributions
fit_w  <- fitdist(my_data, "weibull")
fit_g  <- fitdist(my_data, "gamma")
fit_ln <- fitdist(my_data, "lnorm")
fit_n  <- fitdist(my_data, "norm")

# 5. make plot with custom bins for final figure

# generate distributions to plot
x=seq(0, 1, length=100)
my_data_w = dweibull(x, shape=fit_w$estimate["shape"], scale=fit_w$estimate["scale"])
my_data_g = dgamma(x,shape=fit_g$estimate["shape"], rate=fit_g$estimate["rate"])
my_data_ln = dlnorm(x,meanlog=fit_ln$estimate["meanlog"], sdlog=fit_ln$estimate["sdlog"])
my_data_n = dnorm(x,mean=fit_n$estimate["mean"], sd=fit_n$estimate["sd"])

# check max values to determine ylim
max(my_data_w)
max(h$density)

# make custom histogram & add distributions (need to clean up axis labeling still)
hist(my_data, breaks=seq(0.3, 1, by=0.1), freq=F, axes=T, xlab="proportion gravel")
#axis(side=1, at=seq(0.3, 0.9, length.out=6)) #x-axis
axis(side=1, at=seq(0.3, 1, by=0.1)) #x-axis
axis(side=2, at=seq(0, 8, by=0.5)) #y-axis
lines(x=x, y=my_data_w, col="red")
lines(x=x, y=my_data_g, col="blue")
lines(x=x, y=my_data_ln, col="green")
lines(x=x, y=my_data_n, col="purple")

# 6. calculate AIC (and assorted other goodness of fit criteria)
gofstat(list(fit_w, fit_g, fit_ln, fit_n), fitnames = c("weibull", "gamma", "lognormal", "normal"))

### PC1 ANALYSIS

my_data<-Depth.Hole.Data_MW2$pc1new
my_data <- df.Ancova.Holes$PC1[!(is.na(df.Ancova.Holes$PC1))]
my_data <- my_data+4.777406+0.0001
# check min & max values to see where histogram should sit
min(my_data)
max(my_data)

# 3. adjust histogram parameters to establish desired bin size and axis ranges
# forces to min and max of data (good)
# specifies number of breaks (good)
# seems to generate equal but unspecified bin size (can determine manually but there must be a better way)
h<-hist(my_data, breaks=seq(min(my_data), max(my_data), length.out=6), freq=FALSE, axes=FALSE) # length.out = # of bins
max(h$density)
axis(side=1, at=seq(-3, 9, by=1.5)) # x-axs
axis(side=2, at=seq(0, 0.3, by=0.1)) # y-axis

# 4. fit data with 4 candidate distributions
fit_w  <- fitdist(my_data, "weibull")
fit_g  <- fitdist(my_data, "gamma")
fit_ln <- fitdist(my_data, "lnorm")
fit_n  <- fitdist(my_data, "norm")

# 5. make plot with custom bins for final figure

# generate distributions to plot
x=seq(0, 12, length=100)
my_data_w = dweibull(x, shape=fit_w$estimate["shape"], scale=fit_w$estimate["scale"])
my_data_g = dgamma(x,shape=fit_g$estimate["shape"], rate=fit_g$estimate["rate"])
my_data_ln = dlnorm(x,meanlog=fit_ln$estimate["meanlog"], sdlog=fit_ln$estimate["sdlog"])
my_data_n = dnorm(x,mean=fit_n$estimate["mean"], sd=fit_n$estimate["sd"])

# check max values to determine ylim
max(my_data_w)
max(h$density)

# make custom histogram & add distributions (need to clean up axis labeling still)
hist(my_data, breaks=seq(0, 12, length.out=6), freq=FALSE, axes=FALSE,xlab="PC1", ylim= c(0,.5))
axis(side=1, at=seq(0, 12, by=1.5)) # x-axs
axis(side=2, at=seq(0, 0.5, by=0.1)) # y-axis
lines(x=x, y=my_data_w, col="red")
lines(x=x, y=my_data_g, col="blue")
lines(x=x, y=my_data_ln, col="green")
lines(x=x, y=my_data_n, col="purple")

# 6. calculate AIC (and assorted other goodness of fit criteria)
gofstat(list(fit_w, fit_g, fit_ln, fit_n), fitnames = c("weibull", "gamma", "lognormal", "normal"))
gofstat(fit_n, fitnames ="normal")

