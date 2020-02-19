


#======== 1. File Management ===================================================

# working directory
wd <- getwd()

# folders for storing data outputs and figures
# store names of the folders in an object
output.folder.names <- c("data.output", "figures")
# and make the folders if they don't exist yet. 
for(i in 1:length(output.folder.names)){ 
  if(file.exists(output.folder.names[i]) == FALSE) 
    dir.create(output.folder.names[i])
}

# path to data output folder
path.data.output <- paste(wd,"/",output.folder.names[1],"/", sep = "")

# path to figures folder
path.figures <- paste(wd,"/",output.folder.names[2],"/", sep = "")

#Import necessary datasets
  #import hole data
df <- read.csv(paste(wd,"/PCPP.RAW.DATA.csv", 
                     sep = ""),
               stringsAsFactors = FALSE, 
               strip.white = TRUE, 
               na.strings = c(NA, ""))

#======== 2. Sediment Analysis ==================================================
#Add up sediment columns
microns4000 <- sum(df$Sediment4000[!is.na(df$Sediment4000)])
microns2000 <- sum(df$Sediment2000[!is.na(df$Sediment2000)])
microns500 <- sum(df$Sediment500[!is.na(df$Sediment500)])
microns250 <- sum(df$Sediment250[!is.na(df$Sediment250)])
microns125 <- sum(df$Sediment125[!is.na(df$Sediment125)])
microns63 <- sum(df$Sediment63[!is.na(df$Sediment63)])
micron.pan <- sum(df$Sediment0[!is.na(df$Sediment0)])

#make dataframe with all the sediment sums
sediment.sums <- c(micron.pan, microns125, microns63, microns125, microns250,
                   microns500, microns2000, microns4000)
names(sediment.sums) <- c("micron.pan", "microns125", "microns63", "microns125", 
                          "microns250", "microns500", "microns2000", 
                          "microns4000")
#visualize sediment sums
barplot(sediment.sums, las=2,cex.names=1)
# noting much higher amounts in the larger grain sizes


