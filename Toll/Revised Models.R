## Revised Models ##

# Import Data
library(stargazer)
library(readr)
library(lmtest)
MasterData <- read_csv("C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/MasterData.csv")
MasterData$IDCode <- NULL
options(scipen=999)

library(plm)
#panel <- pdata.frame(MasterData, index = c("State", "Year"), ) 
MasterData$Month <- factor(MasterData$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul/Aug", "Sep", "Oct", "Nov", "Dec"))

pooled <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + EnvirotoAg + EnvirotoUrban + EnvirotoEnviro + PDSI + AverageAnnualAcreFeet, data = MasterData)
pooled_robust <- coeftest(pooled, vcov=vcov(pooled, type = "HC0"))


pooled_states <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + EnvirotoAg + EnvirotoUrban + EnvirotoEnviro + PDSI + AverageAnnualAcreFeet + as.factor(State), data = MasterData)
pooled_states_robust <- coeftest(pooled_states, vcov=vcov(pooled_states, type = "HC0"))


pooled_months <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + EnvirotoAg + EnvirotoUrban + EnvirotoEnviro + PDSI + AverageAnnualAcreFeet + as.factor(Month), data = MasterData)
pooled_months_robust <- coeftest(pooled_months, vcov=vcov(pooled_months, type = "HC0"))

MasterData$season <- ifelse(MasterData$Month == "Jan" | MasterData$Month =="Feb" | MasterData$Month == "Mar", "Qrt1", ifelse(MasterData$Month == "Apr" | MasterData$Month == "May"| MasterData$Month == "Jun", "Qrt2",  ifelse(MasterData$Month == "Jul/Aug"| MasterData$Month == "Sep", "Qrt3", "Qtr4")))
pooled_seasons <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + EnvirotoAg + EnvirotoUrban + EnvirotoEnviro + PDSI + AverageAnnualAcreFeet + as.factor(season), data = MasterData)
pooled_seasons_robust <- coeftest(pooled_seasons, vcov=vcov(pooled_seasons, type = "HC0"))


stargazer(pooled, pooled_robust, pooled_states, pooled_states_robust, pooled_months, pooled_months_robust, pooled_seasons, pooled_seasons_robust, title = "Water Transfers", dep.var.labels = "Price per Acre Foot", column.labels = c("Pooled", "Pooled Robust", "State Effects", "Robust State Effects", "Month Effects", "Robust Month Effects", "Season Effects", "Robust Season Effects"), type = "html", out = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/transfer_models.htm")

## Import Leases Data ##

library(readr)
MasterData_Leases <- read_csv("C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/MasterData_Leases.csv")
MasterData_Leases$Month <- factor(MasterData_Leases$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul.Aug", "Sep", "Oct", "Nov", "Dec"))
MasterData_Leases <- subset(MasterData_Leases, MasterData_Leases$InflationAdjustedPricePerAnnualAcreFoot != 0)

pooled <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + EnvirotoAg + EnvirotoUrban + EnvirotoEnviro + PDSI + AverageAnnualAcreFeet + LeaseDuration, data = MasterData_Leases)
pooled_robust <- coeftest(pooled, vcov=vcov(pooled, type = "HC0"))


pooled_states <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + EnvirotoAg + EnvirotoUrban + EnvirotoEnviro + PDSI + AverageAnnualAcreFeet + LeaseDuration + as.factor(State), data = MasterData_Leases)
pooled_states_robust <- coeftest(pooled_states, vcov=vcov(pooled_states, type = "HC0"))


pooled_months <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + EnvirotoAg + EnvirotoUrban + EnvirotoEnviro + PDSI + AverageAnnualAcreFeet + LeaseDuration + as.factor(Month), data = MasterData_Leases)
pooled_months_robust <- coeftest(pooled_months, vcov=vcov(pooled_months, type = "HC0"))

MasterData_Leases$season <- ifelse(MasterData_Leases$Month == "Jan" | MasterData_Leases$Month =="Feb" | MasterData_Leases$Month == "Mar", "Qrt1", ifelse(MasterData_Leases$Month == "Apr" | MasterData_Leases$Month == "May"| MasterData_Leases$Month == "Jun", "Qrt2",  ifelse(MasterData_Leases$Month == "Jul/Aug"| MasterData_Leases$Month == "Sep", "Qrt3", "Qtr4")))
pooled_seasons <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + EnvirotoAg + EnvirotoUrban + EnvirotoEnviro + PDSI + AverageAnnualAcreFeet + LeaseDuration + as.factor(season), data = MasterData_Leases)
pooled_seasons_robust <- coeftest(pooled_seasons, vcov=vcov(pooled_seasons, type = "HC0"))


stargazer(pooled, pooled_robust, pooled_states, pooled_states_robust, pooled_months, pooled_months_robust, pooled_seasons, pooled_seasons_robust, title = "Leases", dep.var.labels = "Price per Acre Foot", column.labels = c("Pooled", "Pooled Robust", "State Effects", "Robust State Effects", "Month Effects", "Robust Month Effects", "Season Effects", "Robust Season Effects"), type = "html", out = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/Leases_models.htm")
