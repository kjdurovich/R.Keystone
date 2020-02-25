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
#========  ==============================================
#make a column with the total distance downstream
df$DownStreamTotal <- df$DownStream+df$SubDownStream

#boxplots to show mean and range of clam sizes in relationto different variables
  #Clam Size~Tidal Elevation
boxplot(df$Sizemm~df$Elevation)
  #Clam Size~Depth Bin
boxplot(df$Sizemm~df$DepthBin)
#Clam Size~ Distance Down Stream (Binned in 10m intervals)
boxplot(df$Sizemm~df$DownStream)
  #Clam Size~Total Distance Down Stream 
boxplot(df$Sizemm~df$DownStreamTotal)


