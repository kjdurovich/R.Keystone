library(lme4)
#======== Linear Regression for Live Density====================================
 #Visualize the data
plot(df.Ancova.Density[3:8])
   #Note no correlations between the explanatory variables
   #Also not a lack of correlations between Alive and Dead pressence and other 
     #variables

#Alive Presence~Distance from stream 
AD.SD <- summary(lm(df.Ancova.Density$APresence~df.Ancova.Density$StreamDistance))
     #Adjusted R^2 value of 0.058

#Alive Presence~Tidal Elevation
AD.E <- summary(lm(df.Ancova.Density$APresence~df.Ancova.Density$Elevation))
     #Adjusted R^2 value of -0.001

 #Alive Presence~Depth Bin
AD.DB <- summary(lm(df.Ancova.Density$APresence~df.Ancova.Density$DepthBin))
     #Adjusted R^2 value of -0.003

 #Alive Presence~PC1 of Sediment Data
AD.PC1 <- summary(lm(df.Ancova.Density$APresence~df.Ancova.Density$PC1))
     #Adjusted R^2 value of 0.036

 #Dead Presence~Alive Presence
summary(lm(df.Ancova.Density$DPresence~df.Ancova.Density$APresence))
     #Adjusted R^2 value of 0.0245



summary(lm(df.Ancova.Density$APresence~df.Ancova.Density$StreamDistance*df.Ancova.Density$PC1*df.Ancova.Density$Elevation*df.Ancova.Density$DepthBin))


#======== Linear Regression for Clam Size====================================
  #Size~Distance from Stream
AS.SD <- summary(glm(df.Ancova.Size$Sizemm~df.Ancova.Size$StreamDistance))
     #Adjusted R^2 value of 0.016
  #Size~Tidal Elevation
summary(glm(df.Ancova.Size$Sizemm~df.Ancova.Size$Elevation))
     #Adjusted R^2 value of 0.005
  #Size~Depth Bin
summary(glm(df.Ancova.Size$Sizemm~df.Ancova.Size$DepthBin))
     #Adjusted R^2 value of 0.023
  #Size~Sediment PC1
summary(glm(df.Ancova.Size$Sizemm~df.Ancova.Size$PC1))
     #Adjusted R^2 value of -0.003


summary(lm(df.Ancova.Size$Sizemm~df.Ancova.Size$StreamDistance+df.Ancova.Size$Elevation+df.Ancova.Size$DepthBin+df.Ancova.Size$PC1))

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
#save Ele.Live.AIC
write.csv(Ele.Tide.AIC, paste(path.data.output,"Ele.Tide.AIC.csv", sep = ""))
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#save df.ANCOVA.Holes
write.csv(df.Ancova.Holes, paste(path.data.output,"Hole.Data.csv", sep = ""))
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#save df.ANCOVA.Density
write.csv(df.Ancova.Density, paste(path.data.output,"BurialDepth.Hole.Data.csv", sep = ""))
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
