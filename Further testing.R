#### Stream Side for Density ###################################################
Anova(glm(df.Ancova.Density$APresence~df.Ancova.Density$StreamSide, family ="poisson"), type="2", test="F")
Anova(glm((df.Ancova.Density$DPresence/2)~df.Ancova.Density$StreamSide, family ="poisson"), type="2", test="F")
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


#### Dead CLams ################################################################
###Poisson Anova for D Presence   https://rcompanion.org/handbook/J_01.html
model.d <- glm((DPresence/2)~StreamDistance*DepthBin, data=df.Ancova.Density, family="poisson")
Anova(model.d, type="3", test="F")
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
Anova(model.ddb, type="2", test="F")
summary(model.ddb)

marginal.ddb <-  emmeans(model.ddb,
                        ~ DepthBin)

pairs(marginal.ddb,
      adjust="tukey")


#### Clam Size #################################################################
summary(lm(df.Ancova.Size$PC1~df.Ancova.Size$gravel))
df.Ancova.Size$
a <- (glm(Sizemm~StreamDistance*DepthBin*Elevation*DepthBin*gravel, data=df.Ancova.Size))
Anova((glm(Sizemm~StreamDistance*DepthBin*Elevation*DepthBin*gravel, data=df.Ancova.Size)), type="III", test="F")
Anova((glm(Sizemm~StreamDistance*DepthBin*Elevation*DepthBin*PC1, data=df.Ancova.Size)), type="III", test="F")
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



#Check the relation between StreamDistance and DepthBin


model.ssd0 <- glm(df.Ancova.Size$StreamDistance[df.Ancova.Size$DepthBin==0]~
                    df.Ancova.Size$DepthBin[df.Ancova.Size$DepthBin==0], family = "poisson")
Anova(model.ssd0, type = "2", test="F")


model.ssd5 <- glm(df.Ancova.Size$StreamDistance[df.Ancova.Size$DepthBin==5]~
                    df.Ancova.Size$DepthBin[df.Ancova.Size$DepthBin==5], family = "poisson")
Anova(model.ssd5, type = "2", test="F")



model.ssd10 <- glm(df.Ancova.Size$StreamDistance[df.Ancova.Size$DepthBin==10]~
                    df.Ancova.Size$DepthBin[df.Ancova.Size$DepthBin==10], family = "poisson")
Anova(model.ssd0, type = "2", test="F")


model.ssd15 <- glm(df.Ancova.Size$StreamDistance[df.Ancova.Size$DepthBin==15]~
                    df.Ancova.Size$DepthBin[df.Ancova.Size$DepthBin==15], family = "poisson")
Anova(model.ssd15, type = "2", test="F")


par(mfrow=c(1,1))
boxplot(df.Ancova.Size$DepthBin~df.Ancova.Size$StreamDistance)

mean(df.Ancova.Size$DepthBin[df.Ancova.Size$StreamDistance==0])
mean(df.Ancova.Size$DepthBin[df.Ancova.Size$StreamDistance==2])
mean(df.Ancova.Size$DepthBin[df.Ancova.Size$StreamDistance==5])
mean(df.Ancova.Size$DepthBin[df.Ancova.Size$StreamDistance==10])
AIC(model.ssd0)
AIC(model.ssd5)
AIC(model.ssd10)
AIC(model.ssd15)

model.ssd0$



mean(df.Ancova.Hole$APresence)
sd(df.Ancova.Hole$APresence)

mean(df.Ancova.Hole$DPresence)
sd(df.Ancova.Hole$DPresence)

mean(data.vsur$Alive)
sd(data.vsur$Alive)

mean(data.vsur$Total)
sd(data.vsur$Total)

#### Clam Size #################################################################
  #Stream Distance and Depth
model.sd <- glm(Sizemm~StreamDistance*DepthBin, data=df.Ancova.Size, family=gaussian (link = "log"))
Anova(model.sd, type="III", test="F")
summary(model.sd)
marginal.d <-  emmeans(model.d,
                       ~ StreamDistance*DepthBin)
pairs(marginal.d,
      adjust="tukey")
cld(marginal.d,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")   ### Tukey adjustment for multiple comparisons

boxplot(df.Ancova.Size$Sizemm~df.Ancova.Size$StreamDistance)
boxplot(df.Ancova.Size$Sizemm~df.Ancova.Size$DepthBin)
 #Tidal Elevation and PC1
model.sd <- glm(Sizemm~PC1*Elevation, data=df.Ancova.Size, family=gaussian (link = "log"))
Anova(model.sd, type="III", test="F")
summary(model.sd)
marginal.d <-  emmeans(model.d,
                       ~ StreamDistance*DepthBin)
pairs(marginal.d,
      adjust="tukey")
cld(marginal.d,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")   ### Tukey adjustment for multiple comparisons


### Further Sediment Analysis_Focusing on Gravel ###############################
df.sediment$gravel <- df.sediment$Sediment4000+df.sediment$Sediment2000



