#Keegan Durovich
#Keystone Data Anaysis

################################################################################
#NOTES
#Normalizingsediment data using the geometric mean
#rescaling the proportion of each sediment type in each sample to the geometric 
#mean of the sediment types proportion across all samples

#Many of the dataframe names contain "ANCOVA" this is because I had initially 
#planned on using an ANCOVA in this study, I ended up only using linear regression

#Simply running the Main (this script) wil runn the entire analysis and populate
  #the folders

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
  
  #import burial data
data.burial <- read.csv(paste(wd,"/BurialData.csv", 
                             sep = ""),
                       stringsAsFactors = FALSE, 
                       strip.white = TRUE, 
                       na.strings = c(NA, ""))

#======== 2. RUN CODE =============================================================

source("2.Visualize.Data.R")
source("3.Sediment.Exploration.R")
source("4.Testing Assumptions.R")
source("5.Linear Regression.R")

