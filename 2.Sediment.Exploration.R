#         Sediment Analysis
library(stats)
library(dplyr)
library(car)
#======== Set up variables =====================================================
#adding a list of intergers to the raw dataframe
df$observations <- 1:nrow(df)
#make dataframe with just sediment data
df.sediment <- df[10:18]
#remove comment column
df.sediment$comments <- NULL

#make dataframe only with complete data to avoid errors
df.sediment <- df.sediment[complete.cases(df.sediment),]
names.sediment <- c("Sediment4000", "Sediment2000", "Sediment500", 
                    "Sediment250", "Sediment125", "Sediment63", "Sediment0")

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
names(sediment.sums) <- c("pan", "63µ","125µ", "250µ","500µ", 
                          "2000µ",  "4000µ")


#======== Visualize Sediment Data ==============================================
#Visualize distribution of sediment
#visualize sediment sums
barplot(sediment.sums, las=2,cex.names=1)
# note much higher amounts in the larger grain sizes
#======== Working with principle component analysis of compositional data to ===
  #====== Noramalize and Reduce Sediment Data to one varaiable =================

# to normalize the sediment data I used the  geometric mean.
# this allows me to use a standard PCA 

  #remove rows with 0 from the dataset
  # df.sediment <- df.sediment[!(df.sediment$Sediment0==0),]
  # df.sediment <- df.sediment[!(df.sediment$Sediment125==0),]

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
geo.means[8] <- gm_mean(df.sediment$Sediment4000+df.sediment$Sediment2000)
geo.means[9] <- gm_mean(df.sediment$Sediment500+df.sediment$Sediment250+df.sediment$Sediment125+df.sediment$Sediment63)
geo.means[10] <- gm_mean(df.sediment$Sediment0)




#normalize the data using the geometric mean
df.sediment$normSediment4000 <- log(df.sediment$Sediment4000/geo.means[1])
df.sediment$normSediment2000 <- log(df.sediment$Sediment2000/geo.means[2])
df.sediment$normSediment500 <- log(df.sediment$Sediment500/geo.means[3])
df.sediment$normSediment250 <- log(df.sediment$Sediment250/geo.means[4])
df.sediment$normSediment125 <- log(df.sediment$Sediment125/geo.means[5])
df.sediment$normSediment63 <- log(df.sediment$Sediment63/geo.means[6])
df.sediment$normSediment0 <- log(df.sediment$Sediment0/geo.means[7])
df.sediment$normGravel <- log((df.sediment$Sediment4000+df.sediment$Sediment2000)/geo.means[8])
df.sediment$normSand <- log((df.sediment$Sediment500+df.sediment$Sediment250+df.sediment$Sediment125+df.sediment$Sediment63)/geo.means[9])
df.sediment$normSilt <- log((df.sediment$Sediment0)/geo.means[10])




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


#values for each value for each PCA axis
sediment.pca$x
#write first column to df.sediment in a new column as sediment characteristic data
df.sediment$PC1 <- sediment.pca$x[,1]
#write column of gravel
df.sediment$gravel <- df.sediment$Sediment4000+df.sediment$Sediment2000


#make a new dataset for ANCOVA Tests
df.Ancova.Size <- left_join(df, df.sediment, by = "observations")


#remove unneccesary rows
df.Ancova.Size[10:32] <- NULL


#Add column for total downstream distance
df.Ancova.Size$DownStreamTotal <- df.Ancova.Size$DownStream+df.Ancova.Size$SubDownStream
#rem
df.Ancova.Size$DownStream <- NULL
#remove SubDownStream
df.Ancova.Size$SubDownStream <- NULL

df.sediclass <- df

df.sediclass$gravel <- (df.sediclass$Sediment4000+df.sediclass$Sediment2000)
df.sediclass$sand <- (df.sediclass$Sediment500+df.sediclass$Sediment250+df.sediclass$Sediment125+df.sediclass$Sediment63)
df.sediclass$silt <- df.sediclass$Sediment0

df.sedistream.C <- subset(df.sediclass, df.sediclass$StreamSide=="C")
df.sedistream.NS <- df.sediclass[!(df.sediclass$StreamSide=="C"),]
df.sedistream.NS$comments <- NULL
df.sedistream.NS$Sizemm <- NULL
df.sedistream.NS$Elevation <- NULL
df.sedistream.NS <- df.sedistream.NS[complete.cases(df.sedistream.NS),]


#find the mean and ±SD for each sediment type for the north stream center
  #gravel proportion mean
mean(df.sedistream.C$gravel)
  #gavel proportion standard deviation
sd(df.sedistream.C$gravel)
  #sand proportion mean
mean(df.sedistream.C$sand)
  #sand proportion standard deviation
sd(df.sedistream.C$sand)
  #silt proportion mean
mean(df.sedistream.C$silt)
  #silt proportion standard deviation
sd(df.sedistream.C$silt)
  #number of depth bins on the center of the stream
nrow(df.sedistream.C)

#find the mean and ±SD for each sediment type for the north stream sides
mean(df.sedistream.NS$gravel)
sd(df.sedistream.NS$gravel)
mean(df.sedistream.NS$sand)
sd(df.sedistream.NS$sand)
mean(df.sedistream.NS$silt)
sd(df.sedistream.NS$silt)
nrow(df.sedistream.NS)


#Calculating the proportions sediment types in PC1
eigensum <- (0.40464282+0.09641047+0.40257305+0.42782278+ 0.46538140+0.38994063+ 0.33650894)
0.40464282/eigensum
0.09641047/eigensum
0.40257305/eigensum
0.42782278/eigensum
0.46538140/eigensum
0.38994063/eigensum
0.33650894/eigensum

