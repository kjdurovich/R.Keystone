
#======== Linear Regression for Live Density====================================
 #Visualize the data
plot(df.Ancova.Density[3:8])
   #Note no correlations between the explanitory variables
   #Also not a lack of correlations between Alive and Dead pressence and other 
     #variables

#Alive Presence~Stream Distance
summary(lm(df.Ancova.Density$APresence~df.Ancova.Density$StreamDistance))
     #Adjusted R^2 value of 0.058

 #Alive Presence~Tidal Elevation
summary(lm(df.Ancova.Density$APresence~df.Ancova.Density$Elevation))
     #Adjusted R^2 value of -0.001

 #Alive Presence~Depth Bin
summary(lm(df.Ancova.Density$APresence~df.Ancova.Density$DepthBin))
     #Adjusted R^2 value of -0.003

 #Alive Presence~PC1 of Sediment Data
summary(lm(df.Ancova.Density$APresence~df.Ancova.Density$PC1))
     #Adjusted R^2 value of 0.336

 #Dead Presence~Alive Presence
summary(lm(df.Ancova.Density$DPresence~df.Ancova.Density$APresence))
     #Adjusted R^2 value of 0.0245

#======== Linear Regression for Clam Size====================================
  #Size~Stream Distance
summary(lm(df.Ancova.Size$Sizemm~df.Ancova.Size$StreamDistance))
  #Size~Tidal Elevation
summary(lm(df.Ancova.Size$Sizemm~df.Ancova.Size$Elevation))
  #Size~Depth Bin
summary(lm(df.Ancova.Size$Sizemm~df.Ancova.Size$DepthBin))
  #Size~Sediment PC1
summary(lm(df.Ancova.Size$Sizemm~df.Ancova.Size$PC1))

#======== Save files ===========================================================
#Save datframes
#save df.sediment 
write.csv(df.sediment, paste(path.data.output, "Sediment.Data.csv", sep = ""),
          row.names = FALSE)
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#save df.Ancova
write.csv(df.Ancova.Size, paste(path.data.output, "ANCOVA.Size.data.csv", sep = ""),
          row.names = FALSE)

#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

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
#break plot area into 3 columns
pdf(file = paste(path.figures,"Size.Elevation.pdf"),
    width = 5,    # The width of the plot in inches
    height = 5)   # The height of the plot in inches
#Clam Size~Tidal Elevation Boxplot
boxplot(df$Sizemm~df$Elevation)
dev.off() 
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#Save Clam Size~Depth Bin boxplot
#break plot area into 3 columns
pdf(file = paste(path.figures,"Size.DepthBin.pdf"),
    width = 5,    # The width of the plot in inches
    height = 5)   # The height of the plot in inches
#Clam Size~Depth Bin
boxplot(df$Sizemm~df$DepthBin)
dev.off() 
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#Save Clam Size~ Distance Down Stream (Binned in 10m intervals) boxplot
#break plot area into 3 columns
pdf(file = paste(path.figures,"Size.DistanceDownStream10.pdf"),
    width = 5,    # The width of the plot in inches
    height = 5)   # The height of the plot in inches
#Clam Size~ Distance Down Stream (Binned in 10m intervals)
boxplot(df$Sizemm~df$DownStream)
dev.off() 
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#Save Clam Size~Total Distance Down Stream boxplot
#break plot area into 3 columns
pdf(file = paste(path.figures,"Size.TotalDownStream.pdf"),
    width = 5,    # The width of the plot in inches
    height = 5)   # The height of the plot in inches
#Clam Size~Total Distance Down Stream 
boxplot(df$Sizemm~df$DownStreamTotal)
dev.off() 
