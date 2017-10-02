#Load Data
setwd(...\HEV)
attach(Integrated_Spreadsheet)

#Load Packages; use the function instal.package() if a first instal is needed
library(psych)
library(car)
library(ggplot2)
library(Hmisc)
library(corrplot)

#Histograms of HEVs per '000 cars across the UK 

hist(HEVp1000, breaks = 50, xlab = "Hybrid Electric Vehicles per thousand cars", main = NULL, font.lab = 2, col = "gray80")
hist(log(HEVln), breaks = 50, xlab = "Hybrid Electric Vehicles per thousand cars (ln)", main = NULL, font.lab = 2, col = "gray80")

#Descriptive Statistics

describe(Integrated_Spreadsheet)

#Bivariate Analaysis with Socioeconomics 

pairs.panels(Integrated_Spreadsheet[c(3,41,48:61)], method = "spearman", hist.col = "gray80", ellipses = "FALSE", breaks = 25)
cor1 <- rcorr(as.matrix(Integrated_Spreadsheet[c(3,41,48:61)]), type = "spearman")
cor1
cor1$r
cor1$P
corrplot(cor1$r, type="upper", order="original", 
         p.mat = cor1$P, sig.level = 0.01, insig = "blank", tl.srt = 45, tl.col = "black")

#Bivariate Analaysis with Travel 

pairs.panels(Integrated_Spreadsheet[c(3,17:34)], method = "spearman", hist.col = "gray80", ellipses = "FALSE", breaks = 25)
cor2 <- rcorr(as.matrix(Integrated_Spreadsheet[c(3,17:34)]), type = "spearman")
cor2
cor2$r
cor2$P
corrplot(cor2$r, type="upper", order="original", 
         p.mat = cor2$P, sig.level = 0.01, insig = "blank", tl.srt = 45, tl.col = "black")

#Bivariate Analaysis with Household

pairs.panels(Integrated_Spreadsheet[c(3, 35:43)], method = "spearman", hist.col = "gray80", ellipses = "FALSE", breaks = 25)
cor3 <- rcorr(as.matrix(Integrated_Spreadsheet[c(3,35:43)]), type = "spearman")
cor3
cor3$r
cor3$P
corrplot(cor3$r, type="upper", order="original", 
         p.mat = cor3$P, sig.level = 0.01, insig = "blank", tl.srt = 45, tl.col = "black")

#Scatterplot between HEVs per '000 cars and proximity to the London Congestion Charge

ggplot(Integrated_Spreadsheet[which(Integrated_Spreadsheet$DistLCCln>0),], aes(x=DistLCCln, y=HEVln)) +
  geom_point(shape=1) + 
  xlab("Euclidean Distance to Closest Border Crossing (ln)") +
  ylab("Hybrid Electric Vehicles per '000 cars (ln)") +
  theme(axis.title.y=element_text(color = "black", face="bold")) +
  theme(axis.title.x=element_text(color = "black", face="bold")) +
  theme(axis.text.x=element_text(color = "black", size=12)) +
  theme(axis.text.y=element_text(color = "black", size=12)) +
  geom_smooth(method=lm,
              se=FALSE)

#Scatterplot between HEVs per '000 cars and interaction with the London Congestion Charge

ggplot(Integrated_Spreadsheet, aes(x=PropDriveLCC1kln, y=HEVln)) +
    geom_point(shape=1) + 
    xlab("Network Distance to Closest Fuel Station in the Republic (km)") +
    ylab("Hybrid Electric Vehicles per '000 cars (ln)") +
    theme(axis.title.y=element_text(color = "black", face="bold")) +
    theme(axis.title.x=element_text(color = "black", face="bold")) +
    theme(axis.text.x=element_text(color = "black", size=12)) +
    theme(axis.text.y=element_text(color = "black", size=12)) +
  geom_smooth(method=lm,
              se=FALSE)

#Boxplots of Area Categories against Hybrid Electric Vehicles per '000 cars and Kruskal Wallis Test

  Integrated_Spreadsheet$AreaCat <- factor(Integrated_Spreadsheet$AreaCat)

  ggplot(Integrated_Spreadsheet, aes(x=AreaCat, y=HEVp1000)) + geom_boxplot() theme(axis.text.x=element_text(angle=30, vjust=0.8, hjust=1, face="bold"))
  ggbox <- ggplot(Integrated_Spreadsheet, aes(x=AreaCat, y=HEVp1000)) + geom_boxplot(outlier.shape = NA)
  ggbox <- ggbox + theme(axis.text.x=element_text(color = "black", size=12)) 
  ggbox <- ggbox + theme(axis.text.y=element_text(color = "black", size=12))
  ggbox <- ggbox + theme(axis.title.x=element_text(color = "black", face="bold"))
  ggbox <- ggbox + theme(axis.title.y=element_text(color = "black", face="bold"))
  ggbox <- ggbox + ylab("Hybrid Electric Vehicles per '000 cars") + xlab("Local Authority Categories")
  ggbox
  
  kruskal.test(HEVp1000 ~ AreaCat, data = Integrated_Spreadsheet)
  
# Benchmark OLS log-log Regression Models

mod1 <- lm(HEVln ~ MeanAgeln + Level4ln + MeanY1kln + OneCarln + DriveWorkln + PopDln + MeanHSizeln + CCars1kln)
summary(mod1)
par(mfrow=c(2,2))
plot(mod1, main = "Demographic Model")
vif(mod1)
anova(mod1)

mod2 <- lm(HEVln ~ MeanAgeln + Level4ln + MeanY1kln + OneCarln + DriveWorkln + PopDln + MeanHSizeln + CCars1kln + LondonB + FirstNei + SecNei)
summary(mod2)
par(mfrow=c(2,2))
plot(mod2, main = "Contiguity Model")
vif(mod2)
anova(mod2)

mod3 <- lm(HEVln ~ MeanAgeln + Level4ln + MeanY1kln + OneCarln + DriveWorkln + PopDln + MeanHSizeln + CCars1kln + DistLCCln)
summary(mod3)
par(mfrow=c(2,2))
plot(mod3, main = "Proximity Model")
vif(mod3)
anova(mod3)

mod4 <- lm(HEVln ~ MeanAgeln + Level4ln + MeanY1kln + OneCarln + DriveWorkln + PopDln + MeanHSizeln + CCars1kln + PropDriveLCC1kln)
summary(mod4)
par(mfrow=c(2,2))
plot(mod4, main = "Interaction Model")
vif(mod4)
anova(mod4)