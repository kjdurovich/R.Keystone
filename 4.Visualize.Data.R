
#======== Visualize Live Clam Density Data ====================================

#live clam~stream side
boxplot(df.Ancova.Density$APresence~df.Ancova.Density$StreamSide, ylab= "Number of Live Clams", xlab = "Side of Stream")
#live clam~distance from stream
boxplot(df.Ancova.Density$APresence~df.Ancova.Density$StreamDistance, ylab= "Number of Live Clams", xlab = "Distance from Stream (m)" )
#live clam~ depth bin
boxplot(df.Ancova.Density$APresence~df.Ancova.Density$DepthBin,ylab= "Number of Live Clams", xlab = "Clam Burial Depth")
#live clam~stream side
plot(df.Ancova.Density$APresence~df.Ancova.Density$PC1, add=TRUE, ylab= "Number of Live Clams", xlab="Sediment PC1")
abline(lm(df.Ancova.Density$APresence~df.Ancova.Density$PC1), lty=9)
#live clam~elevation
plot(df.Ancova.Density$APresence~df.Ancova.Density$Elevation, ylab= "Number of Live Clams", xlab= "Tidal Elevation")
abline(lm(df.Ancova.Density$APresence~df.Ancova.Density$Elevation),lty = 9)


#======== Visualize Dead Density Data ====================================
#dead clam~stream side
boxplot(df.Ancova.Density$DPresence~df.Ancova.Density$StreamSide,ylab= "Number of Dead Valves", xlab = "Side of Stream")
#dead clam~distance from stream
boxplot(df.Ancova.Density$DPresence~df.Ancova.Density$StreamDistance,ylab= "Number of Dead Valves", xlab = "Distance from Stream (m)")
abline(lm(df.Ancova.Density$DPresence~df.Ancova.Density$StreamDistance))
#dead clam~elevation
plot(df.Ancova.Density$DPresence~df.Ancova.Density$Elevation, ylab= "Number of Dead Valves", xlab = "Tidal Elevation")
abline(lm(df.Ancova.Density$DPresence~df.Ancova.Density$Elevation))
#dead clam~ depth bin
boxplot(df.Ancova.Density$DPresence~df.Ancova.Density$DepthBin, ylab= "Number of Dead Valves", xlab = "Clam Burial Depth")
abline(lm(df.Ancova.Density$DPresence~df.Ancova.Density$DepthBin))
#dead clam~stream side
plot(df.Ancova.Density$DPresence~df.Ancova.Density$PC1, ylab= "Number of Dead Valves", xlab = "Sediment PC1")
abline(lm(df.Ancova.Density$DPresence~df.Ancova.Density$PC1))




#boxplots to show mean and range of clam sizes in relation to different variables
  #Clam Size~Stream Side
boxplot(df.Ancova.Size$Sizemm~df.Ancova.Size$StreamSide, ylab="Clam Size (mm)", xlab="Side of Stream") 
  #Clam Size~Tidal Elevation
plot(df.Ancova.Size$Sizemm~df.Ancova.Size$Elevation, ylab="Clam Size (mm)", xlab="Tidal Elevation")
abline(lm(df.Ancova.Size$Sizemm~df.Ancova.Size$Elevation))
  #Clam Size~Depth Bin
boxplot(df.Ancova.Size$Sizemm~df.Ancova.Size$DepthBin, ylab="Clam Size (mm)", xlab="Clam Burial Depth")
abline(lm(df.Ancova.Size$Sizemm~df.Ancova.Size$DepthBin))
  #Clam Size~ Distance from stream
boxplot(df.Ancova.Size$Sizemm~df.Ancova.Size$StreamDistance, ylab="Clam Size (mm)", xlab="Distance from Stream")
abline(lm(df.Ancova.Size$Sizemm~df.Ancova.Size$StreamDistance))
  #Clam Size ~PC1
plot(df.Ancova.Size$Sizemm~df.Ancova.Size$PC1, ylab="Clam Size (mm)", xlab= "Sediment PC1")

#======== Find best fitting distributions ======================================
#make histogram of Alive Presence
hist(df.Ancova.Density$APresence)
#add normal curve
mean.APrsence <- mean(df.Ancova.Density$APresence)
sd.APresence <- sd(df.Ancova.Density$APresence)
curve(dnorm(x, mean=mean.APrsence, sd=sd.APresence), add=TRUE, col="red")
#add poisson distribution line
pois.a <-  fitdist(df.Ancova.Density$APresence, 'pois', method = 'mle')
print(pois.a)
dist.a <-  dpois(0:100, lambda = pois.a$estimate)
dist.a <-  dist.a * sum(df.Ancova.Density$APresence)
dist.a <-  as.data.frame(dist.a)
lines(dist.a, lwd = 1, col="darkblue", )

#make histogram of Dead Presence
hist(df.Ancova.Density$DPresence)
#add normal curve
mean.DPrsence <- mean(df.Ancova.Density$DPresence)
sd.DPresence <- sd(df.Ancova.Density$DPresence)
n.DPresence <- nrow(df.Ancova.Density)
curve(dnorm(x, mean=mean.DPrsence, sd=sd.DPresence), add=TRUE, col="red")
#add poisson distribution line
pois.d <-  fitdist(df.Ancova.Density$APresence, 'pois', method = 'mle')
print(pois.d)
dist.d <-  dpois(0:100, lambda = pois.d$estimate)
dist.d <-  dist.d * sum(df.Ancova.Density$DPresence)
dist.d <-  as.data.frame(dist.d)
lines(dist.d, lwd = 1, col="darkblue")

#make histogram of Size
  #making a dataset that will work
df.test.size <- df.Ancova.Size
head(df.test.size)
df.test.size[1:2] <- NULL
df.test.size[4] <- NULL
df.test.size[5:8] <- NULL
df.test.size <- df.test.size[complete.cases(df.test.size),]
 #plot hist
hist(df.test.size$Sizemm, xlim = c(10,70))
#add normal curve
mean.Size <- mean(df.test.size$Sizemm)
sd.Size <- sd(df.Ancova.Size$Sizemm)
curve(dnorm(x, mean=mean.Size, sd=sd.Size), add=TRUE, col="red")
#add poisson distribution line
pois.s <-  fitdist(df.test.size$Sizemm,"pois", method = 'mme')
print(pois.d)
dist.d <-  dpois(0:100, lambda = pois.d$estimate)
dist.d <-  dist.d * sum(df.Ancova.Density$DPresence)
dist.d <-  as.data.frame(dist.d)
lines(dist.d, lwd = 1, col="darkblue")
norm.s <-  fitdist(df.test.size$Sizemm, 'norm', method = 'mme')
summary(norm.s)


#find AIC values for each model
  #alive
norm.a <-  fitdist(df.Ancova.Density$APresence, 'norm', method = 'ml')
summary(norm.a)
summary(pois.a)
  #dead
norm.d <-  fitdist(df.Ancova.Density$DPresence, 'norm', method = 'mle')
summary(norm.d)
summary(pois.d)
  #size
norm.s <-  fitdist(df.test.size$Sizemm, 'norm', method = 'mle')
summary(norm.s)
summary(pois.s)



#Loop to make histograms within each catagory with distributions################
df.density.sd <- c(0,2,5,10)
df.density.db <- c(0,5,10,15)

  #make dataframes for output
     #names for columns
AICnames <- c("name", "aic.norm","aic.pois")
DepthBinAIC <- data.frame(matrix(ncol = 3, nrow = 12))
colnames(DepthBinAIC) <- AICnames
StreamDistanceAIC <- data.frame(matrix(ncol = 3, nrow = 12))
colnames(StreamDistanceAIC) <- AICnames


#Live Distance from stream
for(i in 1:length(df.density.sd)){
  #Set plot area
  par(mfrow=c(2,2))
  hist(df.Ancova.Density$APresence[df.Ancova.Density$StreamDistance==df.density.sd[i]],
       xlab= paste(df.density.sd[i],"m from stream", sep=""), ylab = "Number of Clams", main = "", right=TRUE)
  #add normal curve
  mean.x <- mean(df.Ancova.Density$APresence[df.Ancova.Density$StreamDistance==df.density.sd[i]])
  sd.x <- sd(df.Ancova.Density$APresence[df.Ancova.Density$StreamDistance==df.density.sd[i]])
  curve(dnorm(x, mean=mean.x, sd=sd.x), add=TRUE, col="red")
  norm.x <-  fitdist(df.Ancova.Density$APresence[df.Ancova.Density$StreamDistance==df.density.sd[i]], 'norm', method = 'mme')
  StreamDistanceAIC$name[i] <- paste("alive- ",df.density.sd[i]," stream distance", sep="")
  StreamDistanceAIC$aic.norm[i] <-  norm.x$aic
  #add poisson distribution line
  pois.x <-  fitdist(df.Ancova.Density$APresence[df.Ancova.Density$StreamDistance==df.density.sd[i]], 'pois', method = 'mle')
  print(pois.x)
  dist.x <-  dpois(0:100, lambda = pois.x$estimate)
  dist.x <-  dist.x * sum(df.Ancova.Density$APresence[df.Ancova.Density$StreamDistance==df.density.sd[i]])
  dist.x <-  as.data.frame(dist.x)
  lines(dist.x, lwd = 1, col="darkblue", )
  StreamDistanceAIC$aic.pois[i] <-  pois.x$aic
}



legend("topright",c("True Density","Estimate"),lty=1,col=1:2)


length(df.Ancova.Density$StreamDistance[df.Ancova.Density$StreamDistance==10])

#Live Bin Depth
for(i in 1:length(df.density.db)){
  #Set plot area
  par(mfrow=c(2,2))
  hist(df.Ancova.Density$APresence[df.Ancova.Density$DepthBin==df.density.db[i]],
       xlab= paste(df.density.db[i]," deep", sep=""), main = "", ylab = "Number of Clams")
  
  #add normal curve
  mean.x <- mean(df.Ancova.Density$APresence[df.Ancova.Density$DepthBin==df.density.db[i]])
  sd.x <- sd(df.Ancova.Density$APresence[df.Ancova.Density$DepthBin==df.density.db[i]])
  curve(dnorm(x, mean=mean.x, sd=sd.x), add=TRUE, col="red")
  norm.x <-  fitdist(df.Ancova.Density$DPresence[df.Ancova.Density$DepthBin==df.density.db[i]], 'norm', method = 'mme')
  DepthBinAIC$name[i] <- paste("alive- ",df.density.db[i]," deep", sep="")
  DepthBinAIC$aic.norm[i] <-  norm.x$aic
  #add poisson distribution line
  pois.x <-  fitdist(df.Ancova.Density$APresence[df.Ancova.Density$DepthBin==df.density.db[i]], 'pois', method = 'mme')
  print(pois.x)
  dist.x <-  dpois(0:100, lambda = pois.x$estimate)
  dist.x <-  dist.x * sum(df.Ancova.Density$DPresence[df.Ancova.Density$DepthBin==df.density.db[i]])
  dist.x <-  as.data.frame(dist.x)
  lines(dist.x, lwd = 1, col="darkblue", )
  DepthBinAIC$aic.pois[i] <-  pois.x$aic
}

#Dead Distance from stream
for(i in 1:length(df.density.sd)){
  #Set plot area
  par(mfrow=c(2,2))
  hist(df.Ancova.Density$DPresence[df.Ancova.Density$StreamDistance==df.density.sd[i]],
       xlab= paste(df.density.sd[i],"m from stream", sep=""), main = "", ylab="Number of Valves")
  
  #add normal curve
  mean.x <- mean(df.Ancova.Density$APresence[df.Ancova.Density$StreamDistance==df.density.sd[i]])
  sd.x <- sd(df.Ancova.Density$APresence[df.Ancova.Density$StreamDistance==df.density.sd[i]])
  curve(dnorm(x, mean=mean.x, sd=sd.x), add=TRUE, col="red")
  norm.x <-  fitdist(df.Ancova.Density$DPresence[df.Ancova.Density$StreamDistance==df.density.sd[i]], 'norm', method = 'mme')
  StreamDistanceAIC$name[i+4] <- paste("dead- ",df.density.sd[i]," stream distance", sep="")
  StreamDistanceAIC$aic.norm[i+4] <-  norm.x$aic
  #add poisson distribution line
  pois.x <-  fitdist(df.Ancova.Density$DPresence[df.Ancova.Density$StreamDistance==df.density.sd[i]], 'pois', method = 'mle')
  print(pois.x)
  dist.x <-  dpois(0:100, lambda = pois.x$estimate)
  dist.x <-  dist.x * sum(df.Ancova.Density$DPresence[df.Ancova.Density$StreamDistance==df.density.sd[i]])
  dist.x <-  as.data.frame(dist.x)
  lines(dist.x, lwd = 1, col="darkblue", )
  StreamDistanceAIC$aic.pois[i+4] <-  pois.x$aic
}

#Dead Bin Depth
for(i in 1:length(df.density.db)){
  #Set plot area
  par(mfrow=c(2,2))
  hist(df.Ancova.Density$DPresence[df.Ancova.Density$DepthBin==df.density.db[i]],
       xlab= paste(df.density.db[i]," deep", sep=""), main = "", ylab="Number of Valves")
  
  #add normal curve
  mean.x <- mean(df.Ancova.Density$APresence[df.Ancova.Density$DepthBin==df.density.db[i]])
  sd.x <- sd(df.Ancova.Density$APresence[df.Ancova.Density$DepthBin==df.density.db[i]])
  curve(dnorm(x, mean=mean.x, sd=sd.x), add=TRUE, col="red")
  norm.x <-  fitdist(df.Ancova.Density$DPresence[df.Ancova.Density$DepthBin==df.density.db[i]], 'norm', method = 'mme')
  DepthBinAIC$name[i+4] <- paste("dead- ",df.density.db[i]," deep", sep="")
  DepthBinAIC$aic.norm[i+4] <-  norm.x$aic
  #add poisson distribution line
  pois.x <-  fitdist(df.Ancova.Density$DPresence[df.Ancova.Density$DepthBin==df.density.db[i]], 'pois', method = 'mme')
  print(pois.x)
  dist.x <-  dpois(0:100, lambda = pois.x$estimate)
  dist.x <-  dist.x * sum(df.Ancova.Density$DPresence[df.Ancova.Density$DepthBin==df.density.db[i]])
  dist.x <-  as.data.frame(dist.x)
  lines(dist.x, lwd = 1, col="darkblue", )
  DepthBinAIC$name[i+4] <- paste("dead- ",df.density.db[i]," deep", sep="")
  DepthBinAIC$aic.pois[i+4] <-  pois.x$aic
}

#Size Distance from stream
for(i in 2:length(df.density.sd)){
  #Set plot area
  par(mfrow=c(2,2))
  hist(df.test.size$Sizemm[df.test.size$StreamDistance==df.density.sd[i]],
       xlab= paste(df.density.sd[i],"m from stream", sep=""), main = "", ylab = "Number of Clams")
  
  #add normal curve
  mean.x <- mean(df.test.size$Sizemm[df.test.size$StreamDistance==df.density.sd[i]])
  sd.x <- sd(df.test.size$Sizemm[df.test.size$StreamDistance==df.density.sd[i]])
  curve(dnorm(x, mean=mean.x, sd=sd.x), add=TRUE, col="red")
  norm.x <-  fitdist(df.test.size$Sizemm[df.test.size$StreamDistance==df.density.sd[i]], 'norm', method = 'mme')
  StreamDistanceAIC$name[i+8] <- paste("dead- ",df.density.sd[i]," stream distance", sep="")
  StreamDistanceAIC$aic.norm[i+8] <-  norm.x$aic
  #add poisson distribution line
  pois.x <-  fitdist(df.test.size$Sizemm[df.test.size$StreamDistance==df.density.sd[i]], 'pois', method = 'mme')
  print(pois.x)
  dist.x <-  dpois(0:100, lambda = pois.x$estimate)
  dist.x <-  dist.x * sum(df.test.size$Sizemm[df.test.size$StreamDistance==df.density.sd[i]])
  dist.x <-  as.data.frame(dist.x)
  lines(dist.x, lwd = 1, col="darkblue", )
  StreamDistanceAIC$aic.pois[i+8] <-  pois.x$aic
}

#Size Bin Depth
for(i in 1:length(df.density.db)){
  #Set plot area
  par(mfrow=c(2,2))
  hist(df.test.size$Sizemm[df.test.size$DepthBin==df.density.db[i]],
       xlab= paste(df.density.db[i]," deep", sep=""), main = "",ylab = "Number of Clams" )
  
  #add normal curve
  mean.x <- mean(df.test.size$Sizemm[df.test.size$DepthBin==df.density.db[i]])
  sd.x <- sd(df.test.size$Sizemm[df.test.size$DepthBin==df.density.db[i]])
  curve(dnorm(x, mean=mean.x, sd=sd.x), add=TRUE, col="red")
  norm.x <-  fitdist(df.test.size$Sizemm[df.test.size$DepthBin==df.density.db[i]], 'norm', method = 'mme')
  DepthBinAIC$name[i+8] <- paste("dead- ",df.density.db[i]," deep", sep="")
  DepthBinAIC$aic.norm[i+8] <-  norm.x$aic
  #add poisson distribution line
  pois.x <-  fitdist(df.test.size$Sizemm[df.test.size$DepthBin==df.density.db[i]], 'pois', method = 'mme')
  print(pois.x)
  dist.x <-  dpois(0:100, lambda = pois.x$estimate)
  dist.x <-  dist.x * sum(df.test.size$Sizemm[df.test.size$DepthBin==df.density.db[i]])
  dist.x <-  as.data.frame(dist.x)
  lines(dist.x, lwd = 1, col="darkblue", )
  DepthBinAIC$name[i+8] <- paste("size- ",df.density.db[i]," deep", sep="")
  DepthBinAIC$aic.pois[i+8] <-  pois.x$aic
}

#### Check distribution of continous variables
#Set plot area
par(mfrow=c(1,1))


##Check distribution of Live clams by Elevation
fit.ele.ln <- gamlss(df.Ancova.Density$APresence[!(df.Ancova.Density$APresence==0)]~df.Ancova.Density$Elevation[!(df.Ancova.Density$APresence==0)], family = NO())
fit.ele.gamma <- gamlss(df.Ancova.Density$APresence[!(df.Ancova.Density$APresence==0)]~df.Ancova.Density$Elevation[!(df.Ancova.Density$APresence==0)], family = GA())
fit.ele.lognorm <- gamlss(df.Ancova.Density$APresence[!(df.Ancova.Density$APresence==0)]~df.Ancova.Density$Elevation[!(df.Ancova.Density$APresence==0)], family = LOGNO())
fit.ele.pois <- gamlss(df.Ancova.Density$APresence[!(df.Ancova.Density$APresence==0)]~df.Ancova.Density$Elevation[!(df.Ancova.Density$APresence==0)], family = PO())

##Check distribution of live clams by PC1
fit.PC1.ln <- gamlss(df.Ancova.Density$APresence[!(df.Ancova.Density$APresence==0)]~df.Ancova.Density$PC1[!(df.Ancova.Density$APresence==0)], family = NO())
fit.PC1.gamma <- gamlss(df.Ancova.Density$APresence[!(df.Ancova.Density$APresence==0)]~df.Ancova.Density$PC1[!(df.Ancova.Density$APresence==0)], family = GA())
fit.PC1.lognorm <- gamlss(df.Ancova.Density$APresence[!(df.Ancova.Density$APresence==0)]~df.Ancova.Density$PC1[!(df.Ancova.Density$APresence==0)], family = LOGNO())
fit.PC1.pois <- gamlss(df.Ancova.Density$APresence[!(df.Ancova.Density$APresence==0)]~df.Ancova.Density$PC1[!(df.Ancova.Density$APresence==0)], family = PO())

library("gamlss.dist")

plot((df.Ancova.Density$APresence/30)~df.Ancova.Density$Elevation)

pdf.plot(obj=fit.ele.lognorm,1, min=0, max=5, no.points = 300, no.title = FALSE, add=TRUE)
points((df.Ancova.Density$APresence/30)~df.Ancova.Density$Elevation)
plot(fit.ele.lognorm)
mean(df.Ancova.Density$Elevation)
scattersmooth(df.)


#Check AICs
fit.ln$aic
fit.ele.gamma$aic
fit.ele.lognorm$aic
fit.ele.pois$aic
fit.PC1.ln$aic
fit.PC1.gamma$aic
fit.PC1.lognorm$aic
fit.PC1.pois$aic

#save outputs
Ele.Tide.AIC <- data.frame(matrix(ncol = 3, nrow = 8))
Ele.Tide.AIC.colnames <- c("Explanatory", "Distribution", "AIC")
colnames(Ele.Tide.AIC) <- Ele.Tide.AIC.colnames

Ele.Tide.AIC$Explanatory[1:4] <- "Elevation"
Ele.Tide.AIC$Explanatory[5:8] <- "PC1"

Ele.Tide.AIC$AIC[1] <- fit.ele.ln$aic
Ele.Tide.AIC$AIC[2] <- fit.ele.gamma$aic
Ele.Tide.AIC$AIC[3] <- fit.ele.lognorm$aic
Ele.Tide.AIC$AIC[4] <- fit.ele.pois$aic
Ele.Tide.AIC$AIC[5] <- fit.PC1.ln$aic
Ele.Tide.AIC$AIC[6] <- fit.PC1.gamma$aic
Ele.Tide.AIC$AIC[7] <- fit.PC1.lognorm$aic
Ele.Tide.AIC$AIC[8] <- fit.PC1.pois$aic

Ele.Tide.AIC$Distribution[1] <- fit.ele.ln$family$family
Ele.Tide.AIC$Distribution[2] <- fit.ele.gamma$family[2]
Ele.Tide.AIC$Distribution[3] <- fit.ele.lognorm$family[2]
Ele.Tide.AIC$Distribution[4] <- fit.ele.pois$family[2]
Ele.Tide.AIC$Distribution[5] <- fit.ele.ln$family$family
Ele.Tide.AIC$Distribution[6] <- fit.ele.gamma$family[2]
Ele.Tide.AIC$Distribution[7] <- fit.ele.lognorm$family[2]
Ele.Tide.AIC$Distribution[8] <- fit.ele.pois$family[2]







fitln.el <- fitdist(df.Ancova.Density$Elevation, "lnorm")
fitn.el <- fitdist(df.Ancova.Density$Elevation, "norm")
fitgamma.el <- fitdist(df.Ancova.Density$Elevation, "gamma")
fitlogis.el <- fitdist(df.Ancova.Density$Elevation, "logis")
   #Visualize
plot(fitln.el)
plot(fitn.el)
plot(fitgamma.el)
plot(fitlogis.el)
  #Find AIC Values
fitn.el$aic
fitln.el$aic
fitgamma.el$aic
fitlogis.el$aic


##Check distribution of PC1
plot(df.Ancova.Density$APresence~df.Ancova.Density$PC1)
fitlogis.PC1 <- fitdist(df.Ancova.Density$PC1, "logis")
fitn.PC1 <- fitdist(df.Ancova.Density$PC1, "norm")

#Visualize
plot(fitlogis.PC1)
plot(fitn.PC1)
#Find AIC Values
fitlogis.PC1$aic
fitn.PC1$aic

hist(df.Ancova.Density$Elevation, main="", xlab = "Elevation" )
plot(df.Ancova.Density$APresence~df.Ancova.Density$Elevation, xlab="Elevation (m)", ylab="# of live clams")
abline(lm(df.Ancova.Density$APresence~df.Ancova.Density$Elevation), col= "blue", add=TRUE)
fit.ln <- lm(df.Ancova.Density$APresence~df.Ancova.Density$Elevation)
curve(dnorm(x, mean=mean(df.Ancova.Density$Elevation), sd=sd(df.Ancova.Density$Elevation)), add=TRUE, col="red")


fit.ln$effects



sd(df.Ancova.Density$APresence~df.Ancova.Density$Elevation)

pois.x <-  fitdist(df.test.size$Sizemm[df.test.size$DepthBin==df.density.db[i]], 'pois', method = 'mme')
print(pois.x)
dist.x <-  dpois(0:100, lambda = pois.x$estimate)
dist.x <-  dist.x * sum(df.test.size$Sizemm[df.test.size$DepthBin==df.density.db[i]])
dist.x <-  as.data.frame(dist.x)
lines(dist.x, lwd = 1, col="darkblue", )

AIC(fit.ln)







par(mfrow=c(1,1))

