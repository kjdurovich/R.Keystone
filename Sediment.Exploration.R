#========Sediment Analysis =====================================================
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


