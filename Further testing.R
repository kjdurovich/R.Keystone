


#### Alive CLams ###############################################################
###Poisson Anova for A Presence   https://rcompanion.org/handbook/J_01.html
model.a <- glm(APresence~StreamDistance*DepthBin, data=df.Ancova.Density, family="poisson")
Anova(model.a, type="3", test="F")
summary(model.a)
marginal.a <-  emmeans(model.a,
                   ~ StreamDistance*DepthBin)
pairs(marginal.a,
      adjust="tukey")

cld(marginal.a,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")   ### Tukey adjustment for multiple comparisons


#One way Anova for significant variable (StreamDistance)
model.asd <- glm(APresence~StreamDistance, data=df.Ancova.Density, family="poisson")
Anova(model.asd, type="2", test="F")
summary(model.asd)

marginal.asd <-  emmeans(model.asd,
                       ~ StreamDistance)

pairs(marginal.sd,
      adjust="tukey")

#One way ANOVAs for significant interaction (StreamDistance*BinDepth)
#sd into db0
model.asd0 <- glm(df.Ancova.Density$StreamDistance[df.Ancova.Density$DepthBin==0]~
                   df.Ancova.Density$DepthBin[df.Ancova.Density$DepthBin==0], 
                 data=df.Ancova.Density, family="poisson")
Anova(model.asd0, type="2", test="F")
summary(model.asd0)
   #Tukey test
marginal.asd0 <-  emmeans(model.asd0,
                       ~ StreamDistance*DepthBin)

pairs(marginal.asd0,
      adjust="tukey")

#sd into db5
model.asd5 <- glm(df.Ancova.Density$StreamDistance[df.Ancova.Density$DepthBin==5]~
                  df.Ancova.Density$DepthBin[df.Ancova.Density$DepthBin==5], 
                 data=df.Ancova.Density, family="poisson")
Anova(model.asd5, type="2", test="F")
summary(model.asd5)


#sd into db10
model.asd10 <- glm(df.Ancova.Density$StreamDistance[df.Ancova.Density$DepthBin==10]~
                   df.Ancova.Density$DepthBin[df.Ancova.Density$DepthBin==10], 
                  data=df.Ancova.Density, family="poisson")
Anova(model.asd10, type="2", test="F")
summary(model.asd10)

#sd into db15
model.asd15 <- glm(df.Ancova.Density$StreamDistance[df.Ancova.Density$DepthBin==15]~
                    df.Ancova.Density$DepthBin[df.Ancova.Density$DepthBin==15], 
                  data=df.Ancova.Density, family="poisson")
Anova(model.asd15, type="2", test="F")
summary(model.asd15)

#db into sd0
model.adb0 <- glm(df.Ancova.Density$DepthBin[df.Ancova.Density$StreamDistance==0]~
                   df.Ancova.Density$StreamDistance[df.Ancova.Density$StreamDistance==0],
                 family = "poisson")

Anova(model.adb0, type = "2", test="F")
summary(model.adb0)

#db into sd2
model.adb2 <- glm(df.Ancova.Density$DepthBin[df.Ancova.Density$StreamDistance==2]~
                   df.Ancova.Density$StreamDistance[df.Ancova.Density$StreamDistance==2],
                 family = "poisson")

Anova(model.adb2, type = "2", test="F")
summary(model.adb2)

#db into sd5
model.adb5 <- glm(df.Ancova.Density$DepthBin[df.Ancova.Density$StreamDistance==5]~
                   df.Ancova.Density$StreamDistance[df.Ancova.Density$StreamDistance==5],
                 family = "poisson")

Anova(model.adb5, type = "2", test="F")
summary(model.adb5)

#db into sd10
model.adb10 <- glm(df.Ancova.Density$DepthBin[df.Ancova.Density$StreamDistance==10]~
                   df.Ancova.Density$StreamDistance[df.Ancova.Density$StreamDistance==10],
                 family = "poisson")

Anova(model.adb10, type = "2", test="F")
summary(model.adb10)

#### Dead CLams ################################################################
###Poisson Anova for D Presence   https://rcompanion.org/handbook/J_01.html
model.d <- glm(DPresence~StreamDistance*DepthBin, data=df.Ancova.Density, family="poisson")
Anova(model.d, type="III", test="F")
summary(model.d)
marginal.d <-  emmeans(model.d,
                       ~ StreamDistance*DepthBin)
pairs(marginal.d,
      adjust="tukey")
cld(marginal.d,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")   ### Tukey adjustment for multiple comparisons

#One way Anova for significant variable (Depth Bin)
model.ddb <- glm(DPresence~DepthBin, data=df.Ancova.Density, family="poisson")
Anova(model.ddb, type="3", test="F")
summary(model.ddb)

marginal.ddb <-  emmeans(model.ddb,
                        ~ DepthBin)

pairs(marginal.ddb,
      adjust="tukey")


#### Clam Size #################################################################

model.d <- glm(Sizemm~StreamDistance*DepthBin, data=df.Ancova.Size, family="poisson")
Anova(model.d, type="III", test="F")
summary(model.d)
marginal.d <-  emmeans(model.d,
                       ~ StreamDistance*DepthBin)
pairs(marginal.d,
      adjust="tukey")
cld(marginal.d,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")   ### Tukey adjustment for multiple comparisons


# create dataset with just one row for each bin depth
#create new dataset to work with
df.Ancova.Hole <- df.Ancova.Size
#combine rows and sum presence columns for combined rows

df.Ancova.Hole <- aggregate(data=df.Ancova.Hole, cbind(APresence,DPresence)~UniqueID, FUN=sum)

df.Ancova.Density <- aggregate(data=df.Ancova.Density,cbind(APresence,DPresence)
                               ~UniqueID + StreamSide + StreamDistance + 
                                 Elevation + DepthBin + PC1,FUN=sum)

mean(df.Ancova.Hole$APresence)
sd(df.Ancova.Hole$APresence)

mean(df.Ancova.Hole$DPresence)
sd(df.Ancova.Hole$DPresence)

mean(data.vsur$Alive)
sd(data.vsur$Alive)

mean(data.vsur$Total)
sd(data.vsur$Total)




