# Kristopher C. Toll
# Regression

RMasterData  <- read.csv(file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/MasterData_regression.csv", header = TRUE)

Outliers1 <- subset(RMasterData, ObsNumber == "719")
Outliers2 <- subset(RMasterData, ObsNumber == "722")
Outliers3 <- subset(RMasterData, ObsNumber == "706")
Outliers <- rbind(Outliers1, Outliers2, Outliers3)

RMasterData <- subset(RMasterData, ObsNumber != "719" & ObsNumber != "722" & ObsNumber != "706")

meanbymonth <- mean(RMasterData$InflationAdjustedPricePerAnnualAcreFoot ~ RMasterData$Month)

meanbystate <- mean(RMasterData$InflationAdjustedPricePerAnnualAcreFoot ~ RMasterData$State)

reg1 <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~ CommitedAverageAcreFeet + AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoEnviro + UrbantoUrban + PDSI + Jan + Feb + Mar + Apr + May + Jun + Jul.Aug + Oct + Nov + Dec + AZ + CA + ID + MT + NM + NV + OR + TX + UT + WA + WY, data = RMasterData)
summary(reg1)

library(mosaic)
library(car)
library(MASS)
library(gvlma)
library(sandwich)
library(lmtest)
# hetero
bptest(reg1)
# collinear
vif(reg1)
#
resettest(reg1)
# 
bgtest()

# robust model
reg2 <- coeftest(reg1, vcov. = vcovHC(reg1, "HC1"))
reg2
