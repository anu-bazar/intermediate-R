###################################TASK 1#################################
#load libraries
library(glmulti)
#ANOVA car
library(car)
#import data manually OR
#setwd("")
setwd("C:/Users/anuba/Downloads")
BM_A2_AFODDC_Task1$year=factor(BM_A2_AFODDC_Task1$year)#Converts year into discrete factor
#All independent variables except year are ornaments

#Making subsets for manipulation groups
BM_A2_AFODDC_Task1red=subset(BM_A2_AFODDC_Task1, year==2018)
BM_A2_AFODDC_Task1con=subset(BM_A2_AFODDC_Task1, year==2019)

#Needed predictor variables year + logdat + fwingp + fbripc + year:fwingp + year:fbripc, data = BM_A2_AFODDC_Task1

#First with the default AICc criterion
obj1<-glmulti(eggs~year + logdat + fwingp + fbripc + year:fwingp + year:fbripc, data = BM_A2_AFODDC_Task1, name = "AFODDC.glmulti1", crit = aicc, marginality = TRUE, method = "h", fitfunction = "glm")
#What we see on the resulting plot is that there are seven models that are within delta=2. The red line is at delta=2.

#Model-averaged parameter estimates for all parameters, from the best ranked 100 models (but only those including the parameter).
#It mixes unconditional and conditional estimates.
coef(obj1)
#Writes the rank order ad IC values of the first 100 models into a txt file named as stated in the field "name".
write(obj1)

#Initial full model
Anova(lm(eggs~year + logdat + fwingp + fbripc + year:fwingp + year:fbripc, data = BM_A2_AFODDC_Task1), Type = "III")

#Plotting
plot(obj1, type = "p") #Plot of IC ranks
plot(obj1, type = "w") #Plot of model weights
plot(obj1, type = "s") #Plot of parameter weights

#Summary
summary(obj1)
print(obj1)

#Compiling the top 10 
Anova(lm(eggs ~ 1 + year + logdat + fwingp + fbripc + year:fbripc, data = BM_A2_AFODDC_Task1), Type = "III")
Anova(lm(eggs ~ 1 + year + logdat + fbripc + year:fbripc, data = BM_A2_AFODDC_Task1), Type = "III")
Anova(lm(eggs ~ 1 + year + logdat + fwingp + fbripc + year:fwingp + year:fbripc, data = BM_A2_AFODDC_Task1), Type = "III")
Anova(lm(eggs ~ 1 + year + logdat + fwingp, data = BM_A2_AFODDC_Task1), Type = "III")
Anova(lm(eggs ~ 1 + year + logdat, data = BM_A2_AFODDC_Task1), Type = "III")
Anova(lm(eggs ~ 1 + year + logdat + fwingp + fbripc, data = BM_A2_AFODDC_Task1), Type = "III")
Anova(lm(eggs ~ 1 + year + logdat + fbripc, data = BM_A2_AFODDC_Task1), Type = "III")
Anova(lm(eggs ~ 1 + logdat, data = BM_A2_AFODDC_Task1), Type = "III")
Anova(lm(eggs ~ 1 + logdat + fwingp, data = BM_A2_AFODDC_Task1), Type = "III")
Anova(lm(eggs ~ 1 + year + logdat + fwingp + year:fwingp, data = BM_A2_AFODDC_Task1), Type = "III")

#The compiled text file is highlighted in the report.Thank you!
################################################TASK 2###########################################################
#1) Call data from the .csv file

library(car); library(lme4); library(stats); library(optimx);
library(Rmisc); library(ggplot2); library(emmeans)

xdata<-read.csv("C:/Users/anuba/Downloads/BM_A2_AFODDC_Task2.csv") #please change this variable when running

# 2) Change the variables ’population’, ’sex’ and ’light.regime’ to factors
View(xdata)
xdata$sex<-as.factor(xdata$sex)
xdata$pop<-as.factor(xdata$population)
xdata$liregim<-as.factor(xdata$light.regime)

# 3) Plot the distribution of the dependent variable (’disp.speed’)
hist(xdata$disp.speed)

qqPlot(xdata$disp.speed)

hist(log(xdata$disp.speed+1))

hist(sqrt(xdata$disp.speed))

qqPlot(sqrt(xdata$disp.speed))

xdata$sqrt.disp.speed<-sqrt(xdata$disp.speed)

# 4) Taking into account that the ’disp.speed’ is continous, apply log-transformation to 
# normalise it 

#and fit a lm (lme4 library) model with this behaviour as the dependent variable 
# and ’population’, ’sex’ and ’light.regime’ as independent variables. Do not use interactions in 
# the model term.

model<-lm(sqrt.disp.speed~pop + sex + liregim, data=xdata)

# 5) Run the model with the Anova command (car library), keeping in mind that we need type 
# III anova, check and interpret the summary
hist(resid(model))

qqPlot(resid(model))

plot(model)

anova(model)

Anova(model, type="III")

# 6) Check and interpret the estimated marginal means of the significant effects (emmeans 
#library), backtransformed to the original (response) scale and also provide a post-hoc test, 
# using the false discovery rate (fdr) method.
emmeans(model,list(pairwise~pop:liregim),adjust="fdr")
# 7) You should submit the code, results and interpretations copied to a single txt file. Please 
# provide the code as a .R document as well

###############################################TASK 3 ###################################################
#wd
setwd("C:/Users/anuba/Downloads")

# Install/load packages. 
library(vegan)
library(scales) 
library(ggforce) 
library(ggrepel)
library(ggplot2)
library(ggfortify)
library(dplyr)
library(dendextend)
library(devtools)

# Read and explore data----
fungal1=read.table("BM_A2_AFODDC_Task3.txt",row.names=1, header = T,sep = "\t")
fungal1[,1:3]=lapply(fungal1[,1:3],as.factor)
str(fungal1)
summary(fungal1)

par(mfrow=c(3,1))
#Factor 1 (C and X) and Factor 2 (W1, W2, W3 and W4)
boxplot(Factor1~Factor2,data = fungal1)
boxplot(Parallels~Factor2,data = fungal1)
boxplot(Parallels~Factor1,data = fungal1)
par(mfrow=c(1,1))

# PCA: Principal Component Analysis----
fungal1_r_P=rda(fungal1[,4:503])
# autoplot(fungal1_r_P)
summary(fungal1_r_P)

# Check details of PCA

fungal1_r_P$CA$v
barplot(fungal1_r_P$CA$v[,1],main="PC1 scores")
barplot(fungal1_r_P$CA$v[,2],main="PC2 scores")
screeplot(fungal1_r_P,bstick = T,type = c("barplot", "lines"))

(PC1_O=round(summary(fungal1_r_P)$cont$importance[2,1]*100,digits=1))
(PC2_O=round(summary(fungal1_r_P)$cont$importance[2,2]*100,digits=1))


biplot(fungal1_r_P)
ordipointlabel(fungal1_r_P)
# Extract scores from PCA

fungal1_r_P_xy <- scores(fungal1_r_P, display = c("sites","species"), scaling = 2)
fungal1_r_P_xy_biplot=as.data.frame(fungal1_r_P_xy$species)
fungal1_r_P_xy_biplot=cbind(row.names(fungal1_r_P_xy_biplot),fungal1_r_P_xy_biplot)
colnames(fungal1_r_P_xy_biplot)[1]="Variables"
fungal1_r_P_xy=cbind(fungal1[,1:3],fungal1_r_P_xy$sites)

# PCA plot with ggplot

Plot_fungal1_r_P=ggplot(fungal1_r_P_xy,aes(x=PC1, y=PC2)) + 
  geom_point(aes(x=PC1, y=PC2,shape=Factor1,colour=Factor2),size=4) + # add the point markers
  scale_shape_manual(values = c(15,16)) + # shape of points
  scale_colour_manual(values = c("blue","black","yellow","red"),
                      breaks=c("W1","W2","W3","W4")) + # colour of points
  labs(title="PCA for range standardized data",
       x=paste("PC1 (",round(PC1_O,digits = 1),"%)",sep=""),
       y=paste("PC2 (",round(PC2_O,digits = 1),"%)",sep=""))
Plot_fungal1_r_P
# ellipses
Plot_fungal1_r_P+geom_mark_ellipse(aes(color = Factor2,label = Factor2))
Plot_fungal1_r_P+geom_mark_ellipse(aes(shape = Factor1,label = Factor1))


# NMDS: Non-metric Multidimensional Scaling----

# Read and explore data
fungal1ITS=read.table("BM_A2_AFODDC_Task3.txt",header = T,row.names = 1)

# See the structure of the data
apply(fungal1ITS[,1:6],2,table)

# Calculate distance matrix
fungal1Dist=vegdist(fungal1ITS[,7:503],method = "bray")
View(fungal1ITS)
str(fungal1Dist)
View(as.matrix(fungal1Dist))
heatmap(as.matrix(fungal1Dist),col = rainbow(20))

fungal1_NMDS=metaMDS(fungal1ITS[,7:503],distance = "bray",autotransform = F)
plot(fungal1_NMDS)
plot(fungal1_NMDS,display = "sites")
ordipointlabel(fungal1_NMDS,display = "sites")

fungal1_NMDS_xy=as.data.frame(fungal1_NMDS$points)
fungal1_NMDS_xy=cbind(fungal1ITS[,1:3],fungal1_NMDS_xy)
fungal1_NMDS_xy[,1:3]=lapply(fungal1_NMDS_xy[,1:3],as.factor)
fungal1_NMDS_xy_biplot=as.data.frame(fungal1_NMDS$species)
fungal1_NMDS_xy_biplot[,3]=row.names(fungal1_NMDS_xy_biplot)
colnames(fungal1_NMDS_xy_biplot)[3]="Variables"

# plot NMDS
Plot_fungal1_NMDS=ggplot(fungal1_NMDS_xy,aes(x=MDS1, y=MDS2)) + 
  geom_point(aes(x=MDS1, y=MDS2, color = Factor2, shape=Factor1),size=4,stroke=2) + # add the point markers
  scale_shape_manual(values = c(21,22)) + # shape of points
  scale_colour_manual(values = c("black","green","red","blue")) +
  labs(title="Similarity of fungal community composition amplicon sequencing using NMDS with Bray-Curtis distances",
       x=paste("NMDS1",round(fungal1_NMDS$stress,digits = 3),sep="      stress= "),
       y="NMDS2")
Plot_fungal1_NMDS
# ellipses
Plot_fungal1_NMDS+geom_mark_ellipse(aes(fill = Factor2,label = Factor2))

# Check goodness of NMDS
stressplot(fungal1_NMDS)
fungal1_NMDS$stress
#[1] 0.2470173

# 9. Defend use of PCA or NMDS depending on sample -- should we use permanova here?
adonis2(fungal1ITS[,4:503]~Factor1,method = "bray",data=fungal1ITS)
adonis2(fungal1ITS[,4:503]~Factor2,method = "bray",data=fungal1ITS)
# 10. Test for significant difference among the samples based on Factor 1, factor 2 and parallels
fungal1_PCA=rda(fungal1ITS[,4:503])
plot(fungal1_PCA,display="sites",type="p")
plot(fungal1_NMDS,display="sites",type="p")

# Procrustes analysis
plot(procrustes(fungal1_NMDS,fungal1_PCA),main = "PCA vs NMDS for soil samples",xlab = "",ylab = "")
