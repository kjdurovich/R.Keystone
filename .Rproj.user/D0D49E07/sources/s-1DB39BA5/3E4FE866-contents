


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
   #rename column through duplication and remove old name
df$comments <- df$X
df$X <- NULL
#======== 2. Notes =============================================================

#Normalizingsediment data using the geometric mean
  #rescaling the proportion of each sediment type in each sample to the geometric 
  #mean of the sediment types proportion across all samples



#NOTES FROM MARJORIE
  #1. multivariante index of sediment compostition from PCA
  #2. distribution paramater of sediment composition (skewness, kurtosis)
  #3. univavarniate index e.g. modal sediment type


