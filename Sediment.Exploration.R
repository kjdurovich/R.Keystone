#         Sediment Analysis
# library(robCompositions)     # no longer used
# library(EnvStats)            # no longer used
library(stats)
library(dplyr)
#======== Set up variables =====================================================
#adding a list of intergers to the raw dataframe
df$observations <- 1:nrow(df)
#make dataframe with just sediment data
df.sediment <- df[10:18]
#remove comment column
df.sediment$comments <- NULL

#make dataframe only with complete data to avoid errors
df.sediment <- df.sediment[complete.cases(df.sediment),]
names.sediment <- c("Sediment4000", "Sediment2000", "Sediment500", "Sediment250", 
                    "Sediment125", "Sediment63", "Sediment0")


#Add up sediment columns -- to be turned into a loop later
microns4000 <- sum(df$Sediment4000[!is.na(df$Sediment4000)])
microns2000 <- sum(df$Sediment2000[!is.na(df$Sediment2000)])
microns500 <- sum(df$Sediment500[!is.na(df$Sediment500)])
microns250 <- sum(df$Sediment250[!is.na(df$Sediment250)])
microns125 <- sum(df$Sediment125[!is.na(df$Sediment125)])
microns63 <- sum(df$Sediment63[!is.na(df$Sediment63)])
micron.pan <- sum(df$Sediment0[!is.na(df$Sediment0)])

#Number of live clams
clams.A <- sum(df$Mortality == "A")

#Number of dead halves
clams.D <- sum(df$Mortality == "D")

#make dataframe with all the sediment sums
sediment.sums <- c(micron.pan, microns63, 
                   microns125, microns250,microns500, 
                   microns2000, microns4000)
  #name the variable of sediment.sums
  names(sediment.sums) <- c("micron.pan", "microns63", 
                          "microns125", "microns250","microns500", 
                          "microns2000",  "microns4000")

  
#======== Visualize ============================================================

##Visualize distribution of sediment
#visualize sediment sums
barplot(sediment.sums, las=2,cex.names=1)
# noting much higher amounts in the larger grain sizes



#======== Working with principle component analysis of compositional data to ===
  #====== Noramalize and Reduce Sediment Data to one varaiable =================

# to normalize the sediment data I used the  geometric mean.
# this allows me to use a standard PCA 

  #remove rows with 0 from the dataset
  df.sediment <- df.sediment[!(df.sediment$Sediment0==0),]
  df.sediment <- df.sediment[!(df.sediment$Sediment125==0),]

#create a geometric mean function 
  gm_mean = function(x, na.rm=TRUE, zero.propagate = FALSE){
    if(any(x < 0, na.rm = TRUE)){
      return(NaN)
    }
    if(zero.propagate){
      if(any(x == 0, na.rm = TRUE)){
        return(0)
      }
      exp(mean(log(x), na.rm = na.rm))
    } else {
      exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
    }
}

#find the geometric means for each sediment type
    geo.means <- NA    #create the object geo.means
geo.means[1] <- gm_mean(df.sediment$Sediment4000)
geo.means[2] <- gm_mean(df.sediment$Sediment2000)
geo.means[3] <- gm_mean(df.sediment$Sediment500)
geo.means[4] <- gm_mean(df.sediment$Sediment250)
geo.means[5] <- gm_mean(df.sediment$Sediment125)
geo.means[6] <- gm_mean(df.sediment$Sediment63)
geo.means[7] <- gm_mean(df.sediment$Sediment0)

#normalize the data using the geometric mean
df.sediment$normSediment4000 <- log(df.sediment$Sediment4000/geo.means[1])
df.sediment$normSediment2000 <- log(df.sediment$Sediment2000/geo.means[2])
df.sediment$normSediment500 <- log(df.sediment$Sediment500/geo.means[3])
df.sediment$normSediment250 <- log(df.sediment$Sediment250/geo.means[4])
df.sediment$normSediment125 <- log(df.sediment$Sediment125/geo.means[5])
df.sediment$normSediment63 <- log(df.sediment$Sediment63/geo.means[6])
df.sediment$normSediment0 <- log(df.sediment$Sediment0/geo.means[7])


##Running the PCA

#Run a PCA on the normalized sediment data
sediment.pca <- prcomp(df.sediment[9:15], center = TRUE, scale. = TRUE)
plot(sediment.pca, type ="l")
#summarize PCA data
summary(sediment.pca)
#visualize PCA data
biplot(sediment.pca, center = TRUE, scale. = TRUE)


#make sure I am using correlation matrix 

#analyzing PCA data
sediment.pca.var <- sediment.pca$sdev^2

#look at what influence variables have on principle components
sediment.pca$rotation
abs(sediment.pca$rotation)


#values for each value for each PCA axis
sediment.pca$x
#write first column to df.sediment in a new column as sediment characteristic data
df.sediment$PC1 <- sediment.pca$x[,1]

#make a new dataset for ANCOVA Tests
df.Ancova <- left_join(df, df.sediment, by = "observations")

#remove unneccesary rows
df.Ancova[10:32] <- NULL

#make a column with the total distance downstream
df.Ancova$DownStreamTotal <- df.Ancova$DownStream+df.Ancova$SubDownStream
#save file

#remove Downstream 
df.Ancova$DownStream <- NULL
#remove SubDownStream
df.Ancova$SubDownStream <- NULL

#======== Test for correlation between abiotic (explanatory variables) =========
  #Sediment correlation with other explanitory variables
summary(lm(df.Ancova$PC1~df.Ancova$StreamSide))            #Side of stream
summary(lm(df.Ancova$PC1~df.Ancova$StreamDistance))        #Distance from stream
summary(lm(df.Ancova$PC1~df.Ancova$DownStreamTotal))       #Distance downstream
summary(lm(df.Ancova$PC1~df.Ancova$Elevation))             #Tidal Elevation
summary(lm(df.Ancova$PC1~df.Ancova$DepthBin))              #Bin Depth

  #Bin Depth correlation with other explanitory variables
summary(lm(df.Ancova$DepthBin~df.Ancova$StreamSide))       #Side of Stream
summary(lm(df.Ancova$DepthBin~df.Ancova$StreamDistance))   #Distance from stream
summary(lm(df.Ancova$DepthBin~df.Ancova$DownStreamTotal))  #Distance downstream
summary(lm(df.Ancova$DepthBin~df.Ancova$Elevation))        #Tidal Elevation
  
  #Tidal Elevation correlation with other explanitory variables
summary(lm(df.Ancova$Elevation~df.Ancova$StreamSide))      #Side of Stream
summary(lm(df.Ancova$Elevation~df.Ancova$StreamDistance))  #Distance from stream
summary(lm(df.Ancova$Elevation~df.Ancova$DownStreamTotal)) #Distance downstream
     
    #There is a strong correlation between tidal elevation and distance down 
          #stream (r= 0.95) which is visualized in the plot below.
plot((df.Ancova$Elevation~df.Ancova$DownStreamTotal), ylab ="Tidal Elevation", 
         xlab = "Distance Downstream") 
         abline(coef = coef(lm(df.Ancova$Elevation~df.Ancova$DownStreamTotal)))
       #Therefore downstream is removed from the dataset for the ANCOVAs
    df.Ancova$DownStreamTotal <- NULL
  
    #Distance from Stream correlation with other explanitory variable
summary(lm(df.Ancova$StreamDistance~df.Ancova$StreamSide)) #Side of Stream 

#======== Test for normality and homogeneity of variance =======================
plot((df.Ancova$Elevation~df.Ancova$DownStreamTotal))

#======== Save files =======================================================
#Save datframes
  #save df.sediment 
write.csv(df.sediment, paste(path.data.output, "Sediment.Data.csv", sep = ""),
          row.names = FALSE)
  #save df.Ancova
write.csv(df.Ancova, paste(path.data.output, "ANCOVA.data.csv", sep = ""),
          row.names = FALSE)

#Save plots
  #Save PCA LinePlot
pdf(file = paste(path.figures,"PCAlineplot.pdf"),   # Path for saving file
    width = 5,    # The width of the plot in inches
    height = 6)   # The height of the plot in inches
plot(sediment.pca, type ="l")    # Code for plot
dev.off()         #Save plot

  #Save PCA Biplot
pdf(file = paste(path.figures,"PCAbiplot.pdf"),     # Path for saving file
    width = 5,    # The width of the plot in inches
    height = 6)   # The height of the plot in inches
biplot(sediment.pca, center = TRUE, scale. = TRUE)  # Code for plot
dev.off()         #Save plot

  #Save Downstream~Tidal Elevation Regression Plot
pdf(file = paste(path.figures,"Regresion.pdf"),
    width = 5,    # The width of the plot in inches
    height = 6)   # The height of the plot in inches
plot((df.Ancova$Elevation~df.Ancova$DownStreamTotal), ylab ="Tidal Elevation", 
     xlab = "Distance Downstream", 
     abline(coef = coef(lm(df.Ancova$Elevation~df.Ancova$DownStreamTotal))))
dev.off()         #Save plot


 #Need to trouble shoot and make this all work. One option is to resolve the 
   #error, the other is to join both datasets in their entirety and then delete
   #unnneeded columns. This data set will then be exported to the dataset folder




