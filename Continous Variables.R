
#### DENSITIES VS CONTINOUS VARIABLES ##########################################
# Alive Density ~ Elevation Distribution
par(mfrow=c(1,1), mar=c(4,4,1,1))
plot((df.Ancova.Holes$APresence*m2conversion)~df.Ancova.Holes$Elevation, ylab="Alive Clam Density per m^2", xlab="Elevation (m)")
ele_Apres <- df.Ancova.Holes$Elevation[!is.na(df.Ancova.Holes$Elevation)]

plotdist(ele_Apres, histo = TRUE, demp = TRUE)

min(ele_Apres)
max(ele_Apres)

# fit data with 4 candidate distributions
fit_w.ele_Apres <- fitdist(ele_Apres, "weibull")
fit_g.ele_Apres  <- fitdist(ele_Apres, "gamma")
fit_ln.ele_Apres <- fitdist(ele_Apres, "lnorm")
fit_n.ele_Apres  <- fitdist(ele_Apres, "norm")

# a. generate distributions to plot
x=seq(0, 5, length=100)
ele_Apres_w = dweibull(x, shape=fit_w.ele_Apres$estimate["shape"], scale=fit_w.ele_Apres$estimate["scale"])
ele_Apres_g = dgamma(x,shape=fit_g.ele_Apres$estimate["shape"], rate=fit_g.ele_Apres$estimate["rate"])
ele_Apres_ln = dlnorm(x,meanlog=fit_ln.ele_Apres$estimate["meanlog"], sdlog=fit_ln.ele_Apres$estimate["sdlog"])
ele_Apres_n = dnorm(x,mean=fit_n.ele_Apres$estimate["mean"], sd=fit_n.ele_Apres$estimate["sd"])

# b. make custom histogram & add distributions (need to clean up axis labeling still)
par(mfrow=c(1,1), mar=c(4,4,1,6))
breaks.ele_Apres <- seq(1,5, .5)
hist(ele_Apres, breaks= breaks.ele_Apres, freq=FALSE, 
     xlab="Elevation (m)", ylab="Probability Density", main = "",
     ylim=c(0,.6))
par(xpd=FALSE)
lines(x=x, y=ele_Apres_w, lty=2, lwd=2)
lines(x=x, y=ele_Apres_g,lty=4, lwd=2)
lines(x=x, y=ele_Apres_ln, lty=3, lwd=2)
lines(x=x, y=ele_Apres_n, lty=5, lwd=2)
par(xpd=TRUE)
legend(3.8, .6, c("Weibull", "lognormal", "gamma", "normal"), col = "black",
       text.col = "black", lty = c(2, 3, 4, 5), lwd = c(2,2,2,2),
       merge = TRUE)

# c. calculate AIC (and assorted other goodness of fit criteria)
gofstat(list(fit_w.ele_Apres, fit_g.ele_Apres, fit_ln.ele_Apres, fit_n.ele_Apres), fitnames = c("weibull", "gamma", "lognormal", "normal"))


# Alive Density ~ PC1
par(mfrow=c(1,1), mar=c(4,4,1,1))
plot(df.Ancova.Holes$APresence~df.Ancova.Holes$PC1, ylab="Alive Clam Density per 5 cm depth interval", xlab= "PC1")
PC1_APres <- df.Ancova.Holes$PC1[!is.na(df.Ancova.Holes$PC1)]

min(PC1_APres)
max(PC1_APres)

#Rescale PC1_APres by adding 5 to propoperly fit distributions 
PC1_APres <- PC1_APres+4.777506


# fit data with 4 candidate distributions
fit_w.PC1_APres <- fitdist(PC1_APres, "weibull")
fit_g.PC1_APres  <- fitdist(PC1_APres, "gamma")
fit_ln.PC1_APres <- fitdist(PC1_APres, "lnorm")
fit_n.PC1_APres  <- fitdist(PC1_APres, "norm")
# a. generate distributions to plot
x=seq(0, 12, length=100)
PC1_APres_w = dweibull(x, shape=fit_w.PC1_APres$estimate["shape"], scale=fit_w.PC1_APres$estimate["scale"])
PC1_APres_g = dgamma(x,shape=fit_g.PC1_APres$estimate["shape"], rate=fit_g.PC1_APres$estimate["rate"])
PC1_APres_ln = dlnorm(x,meanlog=fit_ln.PC1_APres$estimate["meanlog"], sdlog=fit_ln.PC1_APres$estimate["sdlog"])
PC1_APres_n = dnorm(x,mean=fit_n.PC1_APres$estimate["mean"], sd=fit_n.PC1_APres$estimate["sd"])

# b. make custom histogram & add distributions (need to clean up axis labeling still)
par(mfrow=c(1,1), mar=c(4,4,1,6))
breaks.PC1_Apres <- seq(-3, 6, 1)
hist(PC1_APres, freq = F)
hist(PC1_APres, breaks=7, freq=FALSE, 
     xlab="PC1-(min(PC1)", ylab="Probability Density", main = "",
     ylim=c(0,.4))
par(xpd=FALSE)
lines(x=x, y=PC1_APres_w, lty=2, lwd=2)
lines(x=x, y=PC1_APres_g,lty=4, lwd=2)
lines(x=x, y=PC1_APres_ln, lty=3, lwd=2)
lines(x=x, y=PC1_APres_n, lty=5, lwd=2)
par(xpd=TRUE)
legend(9, .4, c("Weibull", "lognormal", "gamma", "normal"), col = "black",
       text.col = "black", lty = c(2, 3, 4, 5), lwd = c(2,2,2,2),
       merge = TRUE)

# c. calculate AIC (and assorted other goodness of fit criteria)
gofstat(list(fit_w.PC1_APres, fit_g.PC1_APres, fit_ln.PC1_APres, fit_n.PC1_APres), fitnames = c("weibull", "gamma", "lognormal", "normal"))
gofstat(fit_n.PC1_APres)

# Alive Density ~ Gravel (%)
par(mfrow=c(1,1), mar=c(4,4,1,1))
plot(df.Ancova.Density$APresence~df.Ancova.Density$gravel, ylab="Alive Clam Density per 5 cm depth interval", xlab= "Gravel Percentage from Sample")
gravel_APres <- df.Ancova.Size$gravel[(df.Ancova.Size$APresence==1)]
gravel_APres <- gravel_APres[!is.na(gravel_APres)]
# fit data with 4 candidate distributions
fit_w.gravel_APres <- fitdist(gravel_APres, "weibull")
fit_g.gravel_APres  <- fitdist(gravel_APres, "gamma")
fit_ln.gravel_APres <- fitdist(gravel_APres, "lnorm")
fit_n.gravel_APres  <- fitdist(gravel_APres, "norm")
# a. generate distributions to plot
x=seq(0, 4, length=100)
gravel_APres_w = dweibull(x, shape=fit_w.gravel_APres$estimate["shape"], scale=fit_w.gravel_APres$estimate["scale"])
gravel_APres_g = dgamma(x,shape=fit_g.gravel_APres$estimate["shape"], rate=fit_g.gravel_APres$estimate["rate"])
gravel_APres_ln = dlnorm(x,meanlog=fit_ln.gravel_APres$estimate["meanlog"], sdlog=fit_ln.gravel_APres$estimate["sdlog"])
gravel_APres_n = dnorm(x,mean=fit_n.gravel_APres$estimate["mean"], sd=fit_n.gravel_APres$estimate["sd"])

# b. make custom histogram & add distributions (need to clean up axis labeling still)
par(mfrow=c(1,1), mar=c(4,4,1,6))
breaks.gravel_APres <- seq(.3, .9, .1)
hist(gravel_APres, freq = F)
hist(gravel_APres, breaks=breaks.gravel_APres, freq=FALSE, 
     xlab="Gravel Percentage from Sample", ylab="Probability Density", main = "",
     ylim=c(0,max(c(gravel_APres_w, density(gravel_APres)$y))))
par(xpd=FALSE)
lines(x=x, y=gravel_APres_w, lty=2, lwd=2)
lines(x=x, y=gravel_APres_g,lty=4, lwd=2)
lines(x=x, y=gravel_APres_ln, lty=3, lwd=2)
lines(x=x, y=gravel_APres_n, lty=5, lwd=2)
par(xpd=TRUE)
legend(.3, 5, c("Weibull", "lognormal", "gamma", "normal"), col = "black",
       text.col = "black", lty = c(2, 3, 4, 5), lwd = c(2,2,2,2),
       merge = TRUE)

# c. calculate AIC (and assorted other goodness of fit criteria)
gofstat(list(fit_w.gravel_APres, fit_g.gravel_APres, fit_ln.gravel_APres, fit_n.gravel_APres), fitnames = c("weibull", "gamma", "lognormal", "normal"))




# Alive Density ~ Gravel (Normalized)
par(mfrow=c(1,1), mar=c(4,4,1,1))
plot(df.Ancova.Density$APresence~df.Ancova.Density$normGravel, ylab="Alive Clam Density per 5 cm depth interval", xlab="Normalized Gravel Proportions")
gravel_APresnorm <- df.Ancova.Size$normGravel[(df.Ancova.Size$APresence==1)]
gravel_APresnorm <- gravel_APresnorm[!is.na(gravel_APresnorm)]
# fit data with 4 candidate distributions
fit_w.gravel_APresnorm <- fitdist(gravel_APresnorm, "weibull")
fit_g.gravel_APresnorm  <- fitdist(gravel_APresnorm, "gamma")
fit_ln.gravel_APresnorm <- fitdist(gravel_APresnorm, "lnorm")
fit_n.gravel_APresnorm  <- fitdist(gravel_APresnorm, "norm")
# a. generate distributions to plot
x=seq(0, 4, length=100)
gravel_APresnorm_w = dweibull(x, shape=fit_w$estimate["shape"], scale=fit_w$estimate["scale"])
gravel_APresnorm_g = dgamma(x,shape=fit_g$estimate["shape"], rate=fit_g$estimate["rate"])
gravel_APresnorm_ln = dlnorm(x,meanlog=fit_ln$estimate["meanlog"], sdlog=fit_ln$estimate["sdlog"])
gravel_APresnorm_n = dnorm(x,mean=fit_n$estimate["mean"], sd=fit_n$estimate["sd"])

# b. make custom histogram & add distributions (need to clean up axis labeling still)
par(mfrow=c(1,1), mar=c(4,4,1,6))
breaks.gravel_APresnorm <- seq(-.8, .2, .2)
hist(gravel_APresnorm, freq = F)
hist(gravel_APresnorm, breaks=breaks.gravel_APresnorm, freq=F, 
     xlab="Normalized Gravel Proportions", ylab="Probability Density", main = "",
     ylim=c(0,max(c(gravel_APresnorm_w, density(gravel_APresnorm)$y))))
par(xpd=FALSE)
lines(x=x, y=gravel_APresnorm_w, lty=2, lwd=2)
lines(x=x, y=gravel_APresnorm_g,lty=4, lwd=2)
lines(x=x, y=gravel_APresnorm_ln, lty=3, lwd=2)
lines(x=x, y=gravel_APresnorm_n, lty=5, lwd=2)
par(xpd=TRUE)
legend(-.7, 3.4, c("Weibull", "lognormal", "gamma", "normal"), col = "black",
       text.col = "black", lty = c(2, 3, 4, 5), lwd = c(2,2,2,2),
       merge = TRUE)

# c. calculate AIC (and assorted other goodness of fit criteria)
gofstat(list(fit_w.gravel_APresnorm, fit_g.gravel_APresnorm, fit_ln.gravel_APresnorm, fit_n.gravel_APresnorm), fitnames = c("weibull", "gamma", "lognormal", "normal"))
gofstat(fit_n.gravel_APresnorm)

#### Size VS CONTINOUS VARIABLES ##########################################

par(mfrow=c(2,2))
plot(df.Ancova.Size$Sizemm~df.Ancova.Size$Elevation)
abline(lm(df.Ancova.Size$Sizemm~df.Ancova.Size$Elevation))
plot(df.Ancova.Size$Sizemm~df.Ancova.Size$PC1)
plot(df.Ancova.Size$Sizemm~df.Ancova.Size$gravel)
plot(df.Ancova.Size$Sizemm~df.Ancova.Size$normGravel)






