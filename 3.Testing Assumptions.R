#======== Create Dataset to test variance and homogeneity for ANCOVA============
# Create datasetes with just 2 variables in the pressence absence column
#Turn mortality (A,D,Z) into binary (Alive or not alive)

# create dataset with just one row for each bin depth
  #create new dataset to work with
  df.Ancova.Density <- df.Ancova.Size
    #remove size row (as it is no longer relavent for this dataset)
    df.Ancova.Density$Sizemm <- NULL

#Turn mortality (A,D,Z) into binary (dead or not dead)
  #make column containing a 1 if that row is for a live clam
for(i in 1:nrow(df.Ancova.Size)){
  if(df.Ancova.Size$Mortality[i] == "A"){df.Ancova.Size$APresence[i] <- 1}else
  {df.Ancova.Size$APresence[i] <- 0}}

  #make column containing a 1 if that row is for an empty valve
for(i in 1:nrow(df.Ancova.Size)){
  if(df.Ancova.Size$Mortality[i] == "D"){df.Ancova.Size$DPresence[i] <- 1}else
  {df.Ancova.Size$DPresence[i] <- 0}}
     #Make alive Ancova dataframe
     df.Ancova.Size.Dead <- df.Ancova.Size
     #remove mortalitly columns from new dataframe
     df.Ancova.Size.Dead$Mortality <- NULL
     df.Ancova.Size.Dead$APresence <- NULL

#Depth Bin Dataframe
#combine rows and sum presence columns for depth bins sums
df.Ancova.Density <- aggregate(data=df.Ancova.Density,cbind(APresence,DPresence)
                               ~UniqueID + StreamSide + StreamDistance + DownStreamTotal+
                               DepthBin, FUN=sum)
intermediate.depthbin <- df.Ancova.Size
intermediate.depthbin$StreamSide <- NULL
intermediate.depthbin$StreamDistance <- NULL
intermediate.depthbin$DownStreamTotal <- NULL
intermediate.depthbin$Mortality <- NULL
intermediate.depthbin$Sizemm <- NULL
intermediate.depthbin$APresence <- NULL
intermediate.depthbin$DPresence <- NULL
intermediate.depthbin <- unique(intermediate.depthbin)
intermediate.depthbin$DepthBin <- NULL

df.Ancova.Density<- left_join(intermediate.depthbin,df.Ancova.Density, by = "UniqueID")
df.Ancova.Density <-  unique(df.Ancova.Density)

#Hole Dataframe
#combine rows and sum presence columns for holes sums
df.Ancova.Holes <- aggregate(data=df.Ancova.Density,cbind(APresence,DPresence)
                             ~UniqueID + StreamSide + StreamDistance + DownStreamTotal,
                             FUN=sum)

intermediate.holes <- df.Ancova.Size
intermediate.holes$StreamSide <- NULL
intermediate.holes$StreamDistance <- NULL
intermediate.holes$DownStreamTotal <- NULL
intermediate.holes$Mortality <- NULL
intermediate.holes$Sizemm <- NULL
intermediate.holes$APresence <- NULL
intermediate.holes$DPresence <- NULL
intermediate.holes$DepthBin <- NULL
intermediate.holes <- unique(intermediate.holes)

df.Ancova.Holes<- left_join(intermediate.holes,df.Ancova.Holes, by = "UniqueID")
df.Ancova.Holes <-  unique(df.Ancova.Holes)

#======== Test for correlation between abiotic (explanatory variables) =========
#Sediment correlation with other explanatory variables
summary(lm(df.Ancova.Size$PC1~df.Ancova.Size$StreamSide))            #Side of stream
summary(lm(df.Ancova.Size$PC1~df.Ancova.Size$StreamDistance))        #Distance from stream
summary(lm(df.Ancova.Size$PC1~df.Ancova.Size$DownStreamTotal))       #Distance downstream
summary(lm(df.Ancova.Size$PC1~df.Ancova.Size$Elevation))             #Tidal Elevation
summary(lm(df.Ancova.Size$PC1~df.Ancova.Size$DepthBin))              #Bin Depth
summary(lm(df.Ancova.Size$PC1~df.Ancova.Size$gravel))                #Gravel

plot(df.Ancova.Size$PC1~df.Ancova.Size$gravel)


#Bin Depth correlation with other explanatory variables
summary(lm(df.Ancova.Size$DepthBin~df.Ancova.Size$StreamSide))       #Side of Stream
summary(lm(df.Ancova.Size$DepthBin~df.Ancova.Size$StreamDistance))   #Distance from stream
summary(lm(df.Ancova.Size$DepthBin~df.Ancova.Size$DownStreamTotal))  #Distance downstream
summary(lm(df.Ancova.Size$DepthBin~df.Ancova.Size$Elevation))        #Tidal Elevation

#Tidal Elevation correlation with other explanatory variables
summary(lm(df.Ancova.Size$Elevation~df.Ancova.Size$StreamSide))      #Side of Stream
summary(lm(df.Ancova.Size$Elevation~df.Ancova.Size$StreamDistance))  #Distance from stream
summary(lm(df.Ancova.Size$Elevation~df.Ancova.Size$DownStreamTotal)) #Distance downstream

#There is a strong correlation between tidal elevation and distance down 
#stream (r= 0.95) which is visualized in the plot below.
par(mfrow=c(1,1))
plot((df.Ancova.Holes$Elevation~df.Ancova.Holes$DownStreamTotal), ylab ="Tidal Elevation", 
     xlab = "Distance Downstream") 

par(xpd=FALSE)
abline(coef = coef(lm(df.Ancova.Holes$Elevation~df.Ancova.Holes$DownStreamTotal)))
summary(lm(df.Ancova.Holes$Elevation~df.Ancova.Holes$DownStreamTotal))
#Therefore downstream could be removed from the dataset for the ANCOVAs
  #df.Ancova.Size$DownStreamTotal <- NULL

#Distance from Stream correlation with other explanatory variable
summary(lm(df.Ancova.Size$StreamDistance~df.Ancova.Size$StreamSide)) #Side of Stream 




#======== Test for normality and homogeneity of variances between response and 
  #======== and explanatory variables ==========================================
#check that the resisduals for normality of catagorical variables
#Side of stream
#Isolate data for each side of the stream
df.streamside.C <- subset(df.Ancova.Density, df.Ancova.Density$StreamSide=="C")
df.streamside.N <- subset(df.Ancova.Density, df.Ancova.Density$StreamSide=="N")
df.streamside.S <- subset(df.Ancova.Density, df.Ancova.Density$StreamSide=="S")


#plot histograms of frequencies
breaks.streamside <- seq(0,15,2)
#break plot area into 3 columns
par(mfrow=c(1,3))
#Breaks for each histogram
breaks.streamside <- seq(0,15,2)
#Histogram for the Center of the stream 
hist(df.streamside.C$APresence, breaks=breaks.streamside, xlab = "Center", main="")
#Histogram for the North side of the stream 
hist(df.streamside.N$APresence, breaks= breaks.streamside, xlab = "North", main="")
#Histogram for the South side of the stream
hist(df.streamside.S$APresence, breaks= breaks.streamside, xlab = "South", main ="")
#All of the graphs are significantly skewed to the left

#test the homogeniety of variance within the catagorical variable
leveneTest(df.Ancova.Density$APresence~df.Ancova.Density$StreamSide)


#Determining if the resisiduals are normaly distributed for the continous variables
breaks.asf <- seq(-3,15,1)
#Histogram of the risiduals of Presence~Tidal Elevations
PresenceElevat <- (lm(df.Ancova.Density$APresence~df.Ancova.Density$Elevation))
hist(resid(PresenceElevat), breaks=breaks.asf)
#Histogram of the risiduals of Presence~Distance from stream
PresenceDistance <- (lm(df.Ancova.Density$APresence~df.Ancova.Density$StreamDistance))
hist(resid(PresenceDistance), breaks = breaks.asf)
#Histogram of the risiduals of Presence~Bin Depth
PresenceBinDepth <- (lm(df.Ancova.Density$APresence~df.Ancova.Density$DepthBin))
hist(resid(PresenceBinDepth), breaks = breaks.asf)
#Histogram of the risiduals of Presence~Sediment type
PresenceSedi <- (lm(df.Ancova.Density$APresence~df.Ancova.Density$PC1))
hist(resid(PresenceSedi), breaks = breaks.asf)


#check that the resisduals for homogeneity of catagorical variables
#Side of Stream
leveneTest(df.Ancova.Size.Alive$APresence~df.Ancova.Size.Alive$StreamSide, center=mean)
#Distance from Stream
#turn Stream Distance into catagorical data
df.Ancova.Size.Alive$StreamDistance <- as.factor(df.Ancova.Size.Alive$StreamDistance)
leveneTest(df.Ancova.Size.Alive$APresence~df.Ancova.Size.Alive$StreamDistance, center=mean)
#Each depth 
#turn Hole depth into catagorical data
df.Ancova.Size.Alive$DepthBin <- as.factor(df.Ancova.Size.Alive$DepthBin)
leveneTest(df.Ancova.Size.Alive$APresence~df.Ancova.Size.Alive$StreamDistance, center=mean)

### AT THIS POINT I REMOVED SIDE, and thus my only categorical variable, AND ###
            ###MOVED TO LINEAR REGRESSION AND MULTIPLE REGRESSION###

