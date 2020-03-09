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


data.vsur <- read.csv(paste(wd,"/VisualSurveys.csv", 
                            sep = ""),
                      stringsAsFactors = FALSE, 
                      strip.white = TRUE, 
                      na.strings = c(NA, ""))

if(!require(car)){install.packages("car")}
library(car)
if(!require(multcomp)){install.packages("multcomp")}
library("multcomp")
if(!require(lsmeans)){install.packages("lsmeans")}
library(lsmeans)
if(!require(rcompanion)){install.packages("rcompanion")}
library(rcompanion)
if(!require(multcompView)){install.packages("multcompView")}
library(multcompView)
if(!require(agricolae)){install.packages("agricolae")}
library(agricolae)
if(!require(gamlss)){install.packages("gamlss")}
library(gamlss)
if(!require(gamlss.dist)){install.packages("gamlss.dist")}
library(gamlss.dist)
if(!require(gamlss.demo)){install.packages("gamlss.demo")}
library(gamlss.demo)
if(!require(fitdistrplus)){install.packages("fitdistrplus")}
library("fitdistrplus")
#======== 2. RUN CODE =============================================================


source("2.Sediment.Exploration.R")
source("Maps.R")
source("3.Testing Assumptions.R")
source("4.Visualize.Data.R")
source("5.Linear Regression.R")

