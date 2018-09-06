## Revised Models ##

# Import Data
library(stargazer)
library(readr)
library(lmtest)

library(readxl)
rawdata_s <- read_excel("C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Raw_Data/Water_Transfer_Data_Feb_10.xls")
rawdata_s <- subset(rawdata_s, rawdata_s$Lease != "1" & rawdata_s$Sale != "0")
rawdata_s <- subset(rawdata_s, rawdata_s$Combination != "1" & rawdata_s$EnvirotoEnviro != "1" & rawdata_s$Combination != "1" & rawdata_s$Exchange != "1" & rawdata_s$Unknown != "1")

MasterData <- read_csv("C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/MasterData.csv")
options(scipen=999)

library(plm)
#panel <- pdata.frame(MasterData, index = c("State", "Year"), ) 
MasterData$Month <- factor(MasterData$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul/Aug", "Sep", "Oct", "Nov", "Dec"))
MasterData$season <- ifelse(MasterData$Month == "Jan" | MasterData$Month =="Feb" | MasterData$Month == "Mar", "Qrt1", ifelse(MasterData$Month == "Apr" | MasterData$Month == "May"| MasterData$Month == "Jun", "Qrt2",  ifelse(MasterData$Month == "Jul/Aug"| MasterData$Month == "Sep", "Qrt3", "Qtr4")))
MasterData$Log_Price <- log(MasterData$InflationAdjustedPricePerAnnualAcreFoot)

write.csv(MasterData, file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/MasterData_sales.csv")

pooled <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + AverageAnnualAcreFeet, data = MasterData)
pooled_robust <- coeftest(pooled, vcov=vcovHC, type = "HC0")

pooled_full <- lm(log(InflationAdjustedPriceperAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + AverageAnnualAcreFeet, data = rawdata_s)

#pooled_months <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + AverageAnnualAcreFeet + as.factor(Month), data = MasterData)
#pooled_months_robust <- coeftest(pooled_months, vcov=vcov(pooled_months, type = "HC0"))

# Time Effects Models

pooled_seasons <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + AverageAnnualAcreFeet + as.factor(season), data = MasterData)
pooled_seasons_robust <- coeftest(pooled_seasons, vcov=vcovHC, type = "HC0")

colinearmodel <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + AverageAnnualAcreFeet + as.factor(Year), data = MasterData)

pooled_year <- lm(log(InflationAdjustedPriceperAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + AverageAnnualAcreFeet + as.factor(Year), data = subset(rawdata_s))
pooled_year_robust <- coeftest(pooled_year, vcov=vcovHC, type = "HC0")

stargazer(pooled, pooled_seasons, pooled_seasons_robust, pooled_full, pooled_year, pooled_year_robust, colinearmodel, title = "Water Transfers With Time Effects", dep.var.labels = c("Log Price per Acre Foot", "Log Price per Acre Foot", "Log Price per Acre Foot", "Log Price per Acre Foot"), column.labels = c("Subsample", "Subsample with Season Effects", "Subsample with Robust Season Effect", "Full Sample", "Full Sample with Year Effects", "Full Sample with Robust Year Effects", "Subsample with Year Effects (Colinear Model)"), type = "html", out = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/transfer_TimeModels.htm")

# state Effects with seasons

pooled_states <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + AverageAnnualAcreFeet +  relevel(as.factor(State), "CO"), data = subset(MasterData, MasterData$State != "MT" & MasterData$State != "WY"))
pooled_states_robust <- coeftest(pooled_states, vcov=vcovHC, type = "HC0")

pooled_season_states <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + AverageAnnualAcreFeet + relevel(as.factor(State), "CO")  + as.factor(season), subset(MasterData, MasterData$State != "MT" & MasterData$State != "WY"))
pooled_season_states_robust <- coeftest(pooled_season_states, vcov=vcovHC, type = "HC0")

pooled_full_states <- lm(log(InflationAdjustedPriceperAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + AverageAnnualAcreFeet + relevel(as.factor(State), "CO"), data = subset(rawdata_s, rawdata_s$State != "MT" & rawdata_s$State != "WY"))
pooled_full_state_robust <- coeftest(pooled_full_states, vcov=vcovHC, type = "HC0")

pooled_full_year_states <- lm(log(InflationAdjustedPriceperAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + AverageAnnualAcreFeet + relevel(as.factor(State), "CO") + as.factor(Year), data = subset(rawdata_s, rawdata_s$State != "MT" & rawdata_s$State != "WY"))
pooled_full_year_states_robust <- coeftest(pooled_full_year_states, vcov=vcovHC, type = "HC0")

stargazer(pooled_states, pooled_season_states, pooled_season_states_robust, pooled_full_states, pooled_full_year_states, pooled_full_year_states_robust, title = "Water Transfers With State and Time Effects", dep.var.labels = c("Log Price per Acre Foot", "Log Price per Acre Foot"), column.labels = c("Subsample with State Effects", "Subsample with State and Season Effects", "Subsample with Robust State and Season Effects", "Full Sample with State Effects", "Full Sample with State and Year Effects", "Full Sample with Robust State and Year Effects"), type = "html", out = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/transfer_models.htm")

# Extra Models

# Use the Partial F-test to compare extra models to pooled model  



pooled_season_states_interact <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + AverageAnnualAcreFeet + as.factor(season)*relevel(as.factor(State), "CO") + as.factor(season) + relevel(as.factor(State),  "CO"), subset(MasterData, MasterData$State != "MT" & MasterData$State != "WY"))
pooled_season_states_interact_robust <- coeftest(pooled_season_states_interact, vcov=vcovHC, type = "HC0")

anova(pooled_season_states, pooled_season_states_interact)


pooled_year_states_interact <- lm(log(InflationAdjustedPriceperAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + AverageAnnualAcreFeet + as.factor(Year)*relevel(as.factor(State), "CO") + as.factor(Year) + relevel(as.factor(State), "CO"), subset(rawdata_s, rawdata_s$State != "MT" & rawdata_s$State != "WY"))
pooled_year_states_interact_robust <- coeftest(pooled_year_states_interact, vcov=vcovHC, type = "HC0")

anova(pooled_full_year_states, pooled_year_states_interact)


stargazer(pooled_season_states_interact,  pooled_season_states_interact_robust, pooled_year_states_interact,  pooled_year_states_interact_robust, title = "Water Transfers with Interecepts for each State-Year", dep.var.labels = c("Log Price per Acre Foot", "Log Price per Acre Foot", "Log Price per Acre Foot", "Log Price per Acre Foot"), column.labels = c("Subsample State Interacted With Season", "Subsample State Interacted With Season Robust","Full Sample State Interacted With Year", "Full Sample State Interacted With Year Robust"), type = "html", out = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/extra_transfer_models.htm")

## Import Leases Data ##

library(readr)
MasterData_Leases <- read_csv("C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/MasterData_Leases.csv")
MasterData_Leases$Month <- factor(MasterData_Leases$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul/Aug", "Sep", "Oct", "Nov", "Dec"))
MasterData_Leases <- subset(MasterData_Leases, MasterData_Leases$InflationAdjustedPricePerAnnualAcreFoot != 0)
MasterData_Leases <- subset(MasterData_Leases, MasterData_Leases$EnvirotoEnviro != "1")

rawdata_l <- read_excel("C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Raw_Data/Water_Transfer_Data_Feb_10.xls")

rawdata_l <- subset(rawdata_l, rawdata_l$Lease != "0" & rawdata_l$Sale != "1" & rawdata_l$InflationAdjustedPriceperAnnualAcreFoot > 0)
rawdata_l <- subset(rawdata_l, rawdata_l$Combination != "1" & rawdata_l$EnvirotoEnviro != "1" & rawdata_l$Combination != "1" & rawdata_l$Exchange != "1" & rawdata_l$Unknown != "1")


# Creating Lease Duration as a dummy
#MasterData_Leases$season <- ifelse(MasterData_Leases$Month == "Jan" | MasterData_Leases$Month =="Feb" | MasterData_Leases$Month == "Mar", "Qrt1", ifelse(MasterData_Leases$Month == "Apr" | MasterData_Leases$Month == "May"| MasterData_Leases$Month == "Jun", "Qrt2",  ifelse(MasterData_Leases$Month == "Jul.Aug"| MasterData_Leases$Month == "Sep", "Qrt3", "Qtr4")))

#MasterData_Leases$LeaseDuration_a <- ifelse(MasterData_Leases$LeaseDuration == "1", "1", ifelse(MasterData_Leases$LeaseDuration == "2", "2", ifelse(MasterData_Leases$LeaseDuration == "3", "3", ifelse(MasterData_Leases$LeaseDuration == "4", "4", ifelse(MasterData_Leases$LeaseDuration >= 5 & MasterData_Leases$LeaseDuration <= 10, "5-10 Years", ifelse(MasterData_Leases$LeaseDuration >= 11 & MasterData_Leases$LeaseDuration <= 20, "11-20 years", "21-100 years"))))))
#MasterData_Leases$LeaseDuration_a <- factor(as.factor(MasterData_Leases$LeaseDuration_a), c("1", "2", "3", "4", "5-10 Years", "11-20 years", "21-100 years"))
#MasterData_Leases$log_price <- log(MasterData_Leases$InflationAdjustedPricePerAnnualAcreFoot)
#MasterData_Leases <- MasterData_Leases[1:48, 72:74]
#write.csv(MasterData_Leases, file ="C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/MasterData_Leases.csv")

rawdata_l$LeaseDuration_a <- ifelse(rawdata_l$LeaseDuration == "1", "1", ifelse(rawdata_l$LeaseDuration == "2", "2", ifelse(rawdata_l$LeaseDuration == "3", "3", ifelse(rawdata_l$LeaseDuration == "4", "4", ifelse(rawdata_l$LeaseDuration >= 5 & rawdata_l$LeaseDuration <= 10, "5-10 Years", ifelse(rawdata_l$LeaseDuration >= 11 & rawdata_l$LeaseDuration <= 20, "11-20 years", "21-100 years"))))))
rawdata_l$LeaseDuration_a <- factor(as.factor(rawdata_l$LeaseDuration_a), c("1", "2", "3", "4", "5-10 Years", "11-20 years", "21-100 years"))

# Creating Season



# Duration Comparison

subsamp_durt <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + AverageAnnualAcreFeet + LeaseDuration, data = subset(MasterData_Leases))
subsamp_durt_robust <- coeftest(subsamp_durt, vcov=vcovHC, type = "HC0")

subsamp_ddurt <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + AverageAnnualAcreFeet + as.factor(LeaseDuration_a), data = subset(MasterData_Leases))
subsamp_ddurt_robust <- coeftest(subsamp_ddurt, vcov=vcovHC, type = "HC0")

subsamp_ddurt_season <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + AverageAnnualAcreFeet + as.factor(LeaseDuration_a) + as.factor(season), data = subset(MasterData_Leases))
subsamp_ddurt_season_robust <- coeftest(subsamp_ddurt_season, vcov=vcovHC, type = "HC0")

subsamp_ddurt_year <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + AverageAnnualAcreFeet + as.factor(LeaseDuration_a) + as.factor(Year), data = subset(MasterData_Leases))
subsamp_ddurt_year_robust <- coeftest(subsamp_ddurt_year, vcov=vcovHC, type = "HC0")

stargazer(subsamp_durt, subsamp_ddurt, subsamp_ddurt_season, subsamp_ddurt_season_robust,subsamp_ddurt_year, subsamp_ddurt_year_robust, title = "Subsample on leases", dep.var.labels = c("Log Price per Acre Foot", "Log Price per Acre Foot", "Log Price per Acre Foot", "Log Price per Acre Foot"), column.labels = c("Lease Duration as an Integer", "Lease Duration as interecpts", "Season Effects", "Robust Season Effects", "Year Effects", "Robust Year Effects"), type = "html", out = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/LeaseDuration_subsample_models.htm")

expir <- lm(log(InflationAdjustedPriceperAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + AverageAnnualAcreFeet + LeaseDuration, data = subset(rawdata_l))
expir2 <- lm(log(InflationAdjustedPriceperAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + AverageAnnualAcreFeet + as.factor(LeaseDuration_a), data = subset(rawdata_l))
expir3 <- lm(log(InflationAdjustedPriceperAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + AverageAnnualAcreFeet + as.factor(LeaseDuration_a) + as.factor(Year), data = subset(rawdata_l))
expir4 <- lm(log(InflationAdjustedPriceperAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + AverageAnnualAcreFeet + as.factor(LeaseDuration_a) + as.factor(Year)+ relevel(as.factor(State), "CA"), data = subset(rawdata_l, rawdata_l$State != "NV"))
expir4_robust <- coeftest(expir4, vcov=vcovHC, type = "HC0")

stargazer(expir, expir2, expir3, expir4, expir4_robust, title = "Full Sample on leases", dep.var.labels = c("Log Price per Acre Foot", "Log Price per Acre Foot", "Log Price per Acre Foot", "Log Price per Acre Foot"), column.labels = c("Lease Duration as an Integer", "Lease Duration as interecpts", "Year Effects"), type = "html", out = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/LeaseDuration_Fullsample_models.htm")

# State and Season intercepts on subsample

subsamp_ddurt_state <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + AverageAnnualAcreFeet + as.factor(LeaseDuration_a) + relevel(as.factor(State), "CA"), data = subset(MasterData_Leases, MasterData_Leases$State != "NV"))

subsamp_ddurt_state_season <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + AverageAnnualAcreFeet + as.factor(LeaseDuration_a) + relevel(as.factor(State), "CA") + as.factor(season), data = subset(MasterData_Leases, MasterData_Leases$State != "NV"))
subsamp_ddurt_state_season_robust <- coeftest(subsamp_ddurt_state_season, vcov=vcovHC, type = "HC0")

subsamp_ddurt_state_year <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + AverageAnnualAcreFeet + as.factor(LeaseDuration_a) + relevel(as.factor(State), "CA") + as.factor(Year), data = subset(MasterData_Leases, MasterData_Leases$State != "NV"))
subsamp_ddurt_state_year_robust <- coeftest(subsamp_ddurt_state_year, vcov=vcovHC, type = "HC0")

stargazer(subsamp_ddurt_state, subsamp_ddurt_state_season, subsamp_ddurt_state_season_robust, subsamp_ddurt_state_year, subsamp_ddurt_state_year_robust, title = "Subsample Leases With State and Time Effects", dep.var.labels = c("Log Price per Acre Foot", "Log Price per Acre Foot", "Log Price per Acre Foot", "Log Price per Acre Foot"), column.labels = c("State Effects", "State and Season Effects", "Robust State and Season Effects", "State and Year Effects", "Robust State and Year Effects"), type = "html", out = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/Lease_state_time_effects_models.htm")

# Intereactions with State and Season

subsamp_State_season_interact <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + AverageAnnualAcreFeet + as.factor(LeaseDuration_a) + relevel(as.factor(State), "CA") + as.factor(season) + relevel(as.factor(State), "CA")*as.factor(season), data = subset(MasterData_Leases, MasterData_Leases$State != "NV"))
subsamp_State_season_interact_robust <- coeftest(subsamp_State_season_interact, vcov=vcovHC, type = "HC0")

subsamp_state_year_interact <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + AverageAnnualAcreFeet + as.factor(LeaseDuration_a) + relevel(as.factor(State), "CA") + as.factor(Year) + relevel(as.factor(State), "CA")*as.factor(Year), data = subset(MasterData_Leases, MasterData_Leases$State != "NV"))
subsamp_state_year_interact_robust <- coeftest(subsamp_state_year_interact, vcov=vcovHC, type = "HC0")

stargazer(subsamp_State_season_interact, subsamp_State_season_interact_robust, subsamp_state_year_interact, subsamp_state_year_interact_robust, title = "Subsample Lease with State-Time Intercepts", dep.var.labels = c("Log Price per Acre Foot", "Log Price per Acre Foot", "Log Price per Acre Foot", "Log Price per Acre Foot"), column.labels = c("State-Season Intercepts", "Robust State-Season Intercepts", "State-Year Intercepts", "Robust State-Year Intercepts"), type = "html", out = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/Lease_state_time_intercepts_models.htm")
