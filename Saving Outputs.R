
#Save Plots
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#Save plots
#Save PCA LinePlot
pdf(file = paste(path.figures,"PCAlineplot.pdf"),   # Path for saving file
    width = 5,    # The width of the plot in inches
    height = 6)   # The height of the plot in inches
plot(sediment.pca, type ="l")    # Code for plot
dev.off()         #Save plot
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#Save PCA Biplot
pdf(file = paste(path.figures,"PCAbiplot.pdf"),     # Path for saving file
    width = 5,    # The width of the plot in inches
    height = 6)   # The height of the plot in inches
biplot(sediment.pca, center = TRUE, scale. = TRUE)  # Code for plot
dev.off()         #Save plot





#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#Save Downstream~Tidal Elevation Regression Plot
pdf(file = paste(path.figures,"Regresion.pdf"),
    width = 5,    # The width of the plot in inches
    height = 6)   # The height of the plot in inches
plot((df.Ancova.Size$Elevation~df.Ancova.Size$DownStreamTotal), ylab ="Tidal Elevation", 
     xlab = "Distance Downstream") 
abline(coef = coef(lm(df.Ancova.Size$Elevation~df.Ancova.Size$DownStreamTotal)))
dev.off()         #Save plot

#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#Plot showing Density Correlations
pdf(file = paste(path.figures,"Density.Correlations.pdf"),
    width = 5,    # The width of the plot in inches
    height = 5) 
plot(df.Ancova.Density[3:8])
dev.off()         #Save plot

#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#Save histograms of clam frequencies for each side of the stream
#break plot area into 3 columns
pdf(file = paste(path.figures,"Stream.Side.Frequencies.pdf"),
    width = 5,    # The width of the plot in inches
    height = 5)   # The height of the plot in inches
par(mfrow=c(1,3))
#Breaks for each histogram
breaks.streamside <- seq(0,15,2)
#Histogram for the Center of the stream 
hist(df.streamside.C$APresence, breaks=breaks.streamside, xlab = "Center", main="")
#Histogram for the North side of the stream 
hist(df.streamside.N$APresence, breaks= breaks.streamside, xlab = "North", main="")
#Histogram for the South side of the stream
hist(df.streamside.S$APresence, breaks= breaks.streamside, xlab = "South", main ="")
dev.off()         #Save plot


#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#Save Clam Size~Tidal Elevation Boxplot 

pdf(file = paste(path.figures,"Size.Elevation.pdf"),
    width = 5,    # The width of the plot in inches
    height = 5)   # The height of the plot in inches
#Clam Size~Tidal Elevation Boxplot
boxplot(df$Sizemm~df$Elevation)
dev.off() 
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#Save Clam Size~Depth Bin boxplot

pdf(file = paste(path.figures,"Size.DepthBin.pdf"),
    width = 5,    # The width of the plot in inches
    height = 5)   # The height of the plot in inches
#Clam Size~Depth Bin
boxplot(df$Sizemm~df$DepthBin)
dev.off() 
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#Save Clam Size~ Distance Down Stream (Binned in 10m intervals) boxplot

pdf(file = paste(path.figures,"Size.DistanceDownStream10.pdf"),
    width = 5,    # The width of the plot in inches
    height = 5)   # The height of the plot in inches
#Clam Size~ Distance Down Stream (Binned in 10m intervals)
boxplot(df$Sizemm~df$DownStream)
dev.off() 
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#Save Clam Size~Total Distance Down Stream boxplot

pdf(file = paste(path.figures,"Size.TotalDownStream.pdf"),
    width = 5,    # The width of the plot in inches
    height = 5)   # The height of the plot in inches
#Clam Size~Total Distance Down Stream 
boxplot(df$Sizemm~df$DownStreamTotal)
dev.off() 


#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#Save correlation plots for each explanatory variable
pdf(file = paste(path.figures,"Correlation.explanatory.Live.pdf"),
    width = 10,    # The width of the plot in inches
    height = 3)   # The height of the plot in inches
par(mfrow=c(1,4))

#Alive Presence~Stream Distance
plot(df.Ancova.Density$APresence~df.Ancova.Density$StreamDistance, 
     ylab = "Live Clams", xlab= "Distance from Stream")
abline(coef=coef(lm(df.Ancova.Density$APresence~df.Ancova.Density$StreamDistance)))
#Alive Presence~Tidal Elevation
plot(df.Ancova.Density$APresence~df.Ancova.Density$Elevation,
     ylab = "Live Clams", xlab= "Tidal Elevation")
abline(coef=coef(lm(df.Ancova.Density$APresence~df.Ancova.Density$StreamDistance)))
#Alive Presence~Depth Bin
plot(df.Ancova.Density$APresence~df.Ancova.Density$DepthBin,
     ylab = "Live Clams", xlab= "Depth Bin")
abline(coef=coef(lm(df.Ancova.Density$APresence~df.Ancova.Density$DepthBin)))
#Alive Presence~PC1 of Sediment Data
plot(df.Ancova.Density$APresence~df.Ancova.Density$PC1,
     ylab = "Live Clams", xlab= "Sediment PC1")
abline(coef=coef(lm(df.Ancova.Density$APresence~df.Ancova.Density$PC1)))
dev.off() 


#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

#Save correlation plots for each explanatory variable
pdf(file = paste(path.figures,"Correlation.explanatory.Size.pdf"),
    width = 10,    # The width of the plot in inches
    height = 3)   # The height of the plot in inches
par(mfrow=c(1,4))

#size~Stream Distance
plot(df.Ancova.Size$Sizemm~df.Ancova.Size$StreamDistance, 
     ylab = "Clam Size (mm)", xlab= "Distance from Stream")
abline(coef=coef(lm(df.Ancova.Size$Sizemm~df.Ancova.Size$StreamDistance)))
#Size~Tidal Elevation
plot(df.Ancova.Size$Sizemm~df.Ancova.Size$Elevation,
     ylab = "Clam Size (mm)", xlab= "Tidal Elevation")
abline(coef=coef(lm(df.Ancova.Size$Sizemm~df.Ancova.Size$StreamDistance)))
#Size~Depth Bin
plot(df.Ancova.Size$Sizemm~df.Ancova.Size$DepthBin,
     ylab = "Clam Size (mm)", xlab= "Depth Bin")
abline(coef=coef(lm(df.Ancova.Size$Sizemm~df.Ancova.Size$DepthBin)))
#Size~PC1 of Sediment Data
plot(df.Ancova.Size$Sizemm~df.Ancova.Size$PC1,
     ylab = "Clam Size (mm)", xlab= "Sediment PC1")
abline(coef=coef(lm(df.Ancova.Size$Sizemm~df.Ancova.Size$PC1)))
dev.off() 


#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


