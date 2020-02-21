#         Sediment Analysis
library(robCompositions)
library(EnvStats)
#======== Set up variables =====================================================

#make dataframe with just sediment data
df.sediment <- df[10:16]
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
sediment.sums <- c(micron.pan, microns125, microns63, 
                   microns125, microns250,microns500, 
                   microns2000, microns4000)
  #name the variable of sediment.sums
  names(sediment.sums) <- c("micron.pan", "microns125", "microns63", 
                          "microns125", "microns250","microns500", 
                          "microns2000",  "microns4000")

#======== Analyze and Visualize ================================================

##Visualize distribution of sediment
#visualize sediment sums
barplot(sediment.sums, las=2,cex.names=1)
# noting much higher amounts in the larger grain sizes


# ##Look at Correlations between clams and sediment characteristics
# summary(lm(df$Sediment500 ~ df$Mortality-1))
# summary(lm(df$Sediment500 ~ df$Mortality))
# #Try to get the Correlations for live clams
# lm(micron.pan~clams.A)
# summary(lm(micron.pan~clams.A))
# summary(lm(microns63~clams.A))
# 
# #Isolate live clams
# test <- lm(df$Sediment0[df$Mortality=="A"]~df$Mortality[!(df$Mortality=="Z")(df$Mortality=="D")])
# summary(test)
# #test length of vectors
# length(df$Mortality[!(df$Mortality=="Z"),(df$Mortality=="D")])
# length(df$Mortality =="A")
# length(df$Sediment0[!df$Mortality=="A"])
# #another attmept
# cleaned <- df$Mortality[!(df$Mortality=="Z")]
# cleaned1 <- cleaned[!(cleaned=="D")]
# length(cleaned1)
# lm(df$Sediment0[df$Mortality=="A"]~cleaned1)
# #nope
# 
# #regression with just live clams
#   #make a dataset just with live clams
# df1 <- df[grepl("A",df$Mortality),]
#   #make regression with just live clams
# lm(df1$Sediment0~df1$Mortality)
# 
# #talked with Thor and we came to the conclusion that the approach that I was 
#   #taking was not effective and that I should use principle component analysis 
#   #of compositional data and to aim for a variable that describes atleast 60-70% 
#   #of the variance.
# 

#======== Working with principle component analysis of compositional data.1 ====
 ####this was my original attempt that I later moved away from 

# 
# # robust estimation:
# res.rob <- pcaCoDa(df.sediment)
# res.rob
# summary(res.rob)
# plot(res.rob)
# 
# 
# ## classical estimation:
# res.cla <- pcaCoDa(df.sedimentno0, method="classical")
#    #running into errors, removing 0's might work?
# biplot(res.cla)
# 
# #test which varables are infitite to understand what is happening, why the error
# is.infinite(df.sediment$Sediment4000)

#======== Working with principle component analysis of compositional data.2 ====

# to normalize the sediment data I used the  geometric mean.
# this allows me to use a standard PCA 
  

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

