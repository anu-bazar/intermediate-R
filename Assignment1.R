#TASK 1 STARTS HERE

library (dplyr)
library (ggplot2)

datamushroom <- BM_1A_AFODDC_Task1
box_plot <- boxplot(Mushroom_yield ~ Mushroom_strain, data = datamushroom,
                    varwidth = TRUE, log = "y", las = 1)
#posthoc test
anovasummary <- aov(Mushroom_yield ~ Mushroom_strain, data = datamushroom)
summary(anovasummary)
tukey.anovasummary<-TukeyHSD(anovasummary)
tukey.anovasummary


#TASK 2 STARTS HERE
library (dplyr)
library (ggplot2)
df <- BM_1A_AFODDC_Task2
model<-lm(Stature~Femur, data = df)
summary(model)
summary(model)$r.squared
plot(df$Stature, df$Femur, xlab="Stature", ylab="Femur")
plot(df$Stature, df$Femur, xlab="Stature", ylab="Femur", 
     xlim = c(165,max(df$Stature, na.rm=TRUE)),
     ylim = c(392,max(df$Femur, na.rm=TRUE)))
abline(coef = c(0,2.34
))

#TASK 3 STARTS HERE

library(dplyr)
library(rstatix)
a <- c(176.5,174.6,179.3,173.1,175.3,180.3,175.6,173.4,176.7,174)
b <- c(181.2,180,183.6,183.2,184,180.3,182.6,183.2,182.5,183)
C <- list(a,b)
names(C) <- c(paste("Group 1\n n=" , length(a) , sep=""), paste("Group 2\n n=" , length(b) , sep=""))
par(mgp=c(3,2,0))
boxplot(C , col="#FFFF00" , ylab="value" )
dataset3 <- as.numeric(BM_1A_AFODDC_Task3[-c(1),])

wilcox.test(a , b, paired=T, exact=F)
