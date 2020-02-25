#         Sediment Analysis
# library(robCompositions)     # no longer used
# library(EnvStats)            # no longer used
library(stats)
library(dplyr)
library(car)

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
df.Ancova.Size <- left_join(df, df.sediment, by = "observations")

#remove unneccesary rows
df.Ancova.Size[10:18] <- NULL
df.Ancova.Size[11:24] <- NULL

#Add column for total downstream distance
df.Ancova.Size$DownStreamTotal <- df.Ancova.Size$DownStream+df.Ancova.Size$SubDownStream
#remove Downstream 
df.Ancova.Size$DownStream <- NULL
#remove SubDownStream
df.Ancova.Size$SubDownStream <- NULL
