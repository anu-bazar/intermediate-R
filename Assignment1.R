#TASK 1 


library (dplyr)
library (ggplot2)
#UPLOAD DATA
df <- BM_1A_AFODDC_Task1
box_plot <- boxplot(Mushroom_yield ~ Mushroom_strain, data = df,
                    varwidth = TRUE, log = "y", las = 1)
model<-lm(Mushroom_yield ~ Mushroom_strain, data = df)
shapiro.test(model$residuals)
#posthoc test
anovasummary <- aov(Mushroom_yield ~ Mushroom_strain, data = df)
summary(anovasummary)
tukey.anovasummary<-TukeyHSD(anovasummary)
tukey.anovasummary


#TASK 2 

library (dplyr)
library (ggplot2)
df <- BM_1A_AFODDC_Task2

#Analysis

model<-lm(Stature~Femur, data = df)
summary(model)

#Plot
plot(df$Stature, df$Femur, xlab="Stature", ylab="Femur")
abline(lm(Stature~Femur, data = df), col = "blue")


#Residual analysis
#Linearity and homoscedasticity
plot(model, pch=16, col="blue",lwd=2, which=1)
plot(model, pch=16, col="blue",lwd=2, which=3)
#Normality
plot(model, pch=16, col="blue",lwd=2, which=2)
shapiro.test(model$residuals)

#TASK 3

library(dplyr)
library(rstatix)

#Uploading data
a <- c(176.5,174.6,179.3,173.1,175.3,180.3,175.6,173.4,176.7,174)
b <- c(181.2,180,183.6,183.2,184,180.3,182.6,183.2,182.5,183)
C <- list(a,b)
names(C) <- c(paste("Group 1\n n=" , length(a) , sep=""), paste("Group 2\n n=" , length(b) , sep=""))

#Set graphical parameters
par(mgp=c(3,2,0))

#Boxplot
boxplot(C , col="#808080" , ylab="value" )

#Test statistic Mann-Whitney test
wilcox.test(a , b, paired=F, exact=F)
