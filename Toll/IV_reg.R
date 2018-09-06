# Testing for Endogeneity

library(AER)

z <- lm(AverageAnnualAcreFeet ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI, subset(MasterData_Sales, MasterData_Sales$State != "MT" & MasterData_Sales$State != "WY"))

x <- lm(LogPrice ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + z$residuals, subset(MasterData_Sales, MasterData_Sales$State != "MT" & MasterData_Sales$State != "WY"))

# Endogeneity is a Problem

ivreg_sales <- ivreg(LogPrice ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + AverageAnnualAcreFeet |  AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + PCP, data = subset(MasterData_Sales, MasterData_Sales$State != "MT" & MasterData_Sales$State != "WY"))

summary(ivreg_sales, diagnostics = TRUE)

## Third Edition Revised Models ##

# Sales Models 

library(stargazer)
library(lmtest)
library(sandwich)
library(car)
library(plm)
options(scipen=99999)


library(readr)
MasterData_Sales <- read_csv("C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/MasterData_Sales.csv")
options(scipen=99999)

#MasterData_Sales$X1_1 <- NULL
#MasterData_Sales$LogPrice <- log(MasterData_Sales$InflationAdjustedPricePerAnnualAcreFoot)

#MasterData_Sales$Month <- factor(MasterData_Sales$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul/Aug", "Sep", "Oct", "Nov", "Dec"))
#MasterData_Sales$season <- ifelse(MasterData_Sales$Month == "Jan" | MasterData_Sales$Month =="Feb" | MasterData_Sales$Month == "Mar", "Qrt1", ifelse(MasterData_Sales$Month == "Apr" | MasterData_Sales$Month == "May"| MasterData_Sales$Month == "Jun", "Qrt2",  ifelse(MasterData_Sales$Month == "Jul/Aug"| MasterData_Sales$Month == "Sep", "Qrt3", "Qtr4")))

#write.csv(MasterData_Sales, file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/MasterData_sales.csv")


S_IV <- ivreg(LogPrice ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + AverageAnnualAcreFeet | AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + PCP, data = subset(MasterData_Sales, MasterData_Sales$State != "MT" & MasterData_Sales$State != "WY"))
Scov1 <- sqrt(diag(vcovHC(S_IV, type = "HC0")))

S_IV_state <- ivreg(LogPrice ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + relevel(as.factor(State), "CO") + AverageAnnualAcreFeet | AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + relevel(as.factor(State), "CO") + PDSI + PCP, data = subset(MasterData_Sales, MasterData_Sales$State != "MT" & MasterData_Sales$State != "WY"))
Scov2 <- sqrt(diag(vcovHC(S_IV_state, type = "HC0")))

S_IV_Year <- ivreg(LogPrice ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + as.factor(Year) + AverageAnnualAcreFeet | AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + as.factor(Year) + PDSI + PCP, data = subset(MasterData_Sales, MasterData_Sales$State != "MT" & MasterData_Sales$State != "WY"))
Scov3 <- sqrt(diag(vcovHC(S_IV_Year, type = "HC0")))

S_IV_State_Year <- ivreg(LogPrice ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + as.factor(Year) + relevel(as.factor(State), "CO") + AverageAnnualAcreFeet | AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + as.factor(Year) + relevel(as.factor(State), "CO") + PDSI + PCP, data = subset(MasterData_Sales, MasterData_Sales$State != "MT" & MasterData_Sales$State != "WY"))
Scov4 <- sqrt(diag(vcovHC(S_IV_State_Year, type = "HC0")))

S_IV_NoAgents <-  ivreg(LogPrice ~ as.factor(Year) + relevel(as.factor(State), "CO") + AverageAnnualAcreFeet | as.factor(Year) + relevel(as.factor(State), "CO") + PDSI + PCP, data = subset(MasterData_Sales, MasterData_Sales$State != "MT" & MasterData_Sales$State != "WY"))
Scov5 <- sqrt(diag(vcovHC(S_IV_NoAgents, type = "HC0")))

S_IV_Season <- ivreg(LogPrice ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + as.factor(season) + AverageAnnualAcreFeet | AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + as.factor(season) + PDSI + PCP, data = subset(MasterData_Sales, MasterData_Sales$State != "MT" & MasterData_Sales$State != "WY"))
Scov6 <- sqrt(diag(vcovHC(S_IV_Season, type = "HC0")))

S_IV_Season_state <- ivreg(LogPrice ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + as.factor(season) + relevel(as.factor(State), "CO") + AverageAnnualAcreFeet | AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + as.factor(season) + relevel(as.factor(State), "CO") + PDSI + PCP, data = subset(MasterData_Sales, MasterData_Sales$State != "MT" & MasterData_Sales$State != "WY"))
Scov7 <- sqrt(diag(vcovHC(S_IV_Season_state, type = "HC0")))


stargazer(S_IV, S_IV_state, S_IV_Year, S_IV_State_Year, S_IV_NoAgents, S_IV_Season, S_IV_Season_state, se = list(Scov1, Scov2, Scov3, Scov4, Scov5, Scov6, Scov7) ,title = "Permanent Transfers", dep.var.labels = c("Log Price per Acre Foot"), column.labels = c("IV"), type = "html", out = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/PermanentTransfersDraft3_IV.htm")


## Lease Models


MasterData_Leases <- read_csv("C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/MasterData_Leases.csv")

MasterData_Leases$LeaseDuration_a <- factor(as.factor(MasterData_Leases$LeaseDuration_a), c("1", "2", "3", "4", "5-10 Years", "11-20 years", "21-100 years"))


# Lease Models

L_OLS_DurCont <- lm(LogPrice ~ PDSI + LeaseDuration, data = subset(MasterData_Leases, MasterData_Leases$State != "NV"))
Lcov1 <- sqrt(diag(vcovHC(L_OLS_DurCont, type = "HC0")))
i <- vif(L_OLS_DurCont)
ncvTest(L_OLS_DurCont)

L_OLS_DurDis <- lm(LogPrice ~ PDSI + LeaseDuration_a, data = subset(MasterData_Leases, MasterData_Leases$State != "NV"))
Lcov2 <- sqrt(diag(vcovHC(L_OLS_DurDis, type = "HC0")))
j <- vif(L_OLS_DurDis)
ncvTest(L_OLS_DurDis)

L_OLS_agents <- lm(LogPrice ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + LeaseDuration_a, data = subset(MasterData_Leases, MasterData_Leases$State != "NV"))
Lcov3 <- sqrt(diag(vcovHC(L_OLS_agents, type = "HC0")))
k <- vif(L_OLS_agents)
ncvTest(L_OLS_agents)

L_OLS_state <- lm(LogPrice ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + LeaseDuration_a + relevel(as.factor(State), "CA"), data = subset(MasterData_Leases, MasterData_Leases$State != "NV"))
Lcov4 <- sqrt(diag(vcovHC(L_OLS_state, type = "HC0")))
l <- vif(L_OLS_state)
ncvTest(L_OLS_state)

L_OLS_year <- lm(LogPrice ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + LeaseDuration_a + as.factor(Year), data = subset(MasterData_Leases, MasterData_Leases$State != "NV"))
Lcov5 <- sqrt(diag(vcovHC(L_OLS_year, type = "HC0")))
m <- vif(L_OLS_year)
ncvTest(L_OLS_year)

L_OLS_year_state <- lm(LogPrice ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + LeaseDuration_a + relevel(as.factor(State), "CA") + as.factor(Year), data = subset(MasterData_Leases, MasterData_Leases$State != "NV"))
Lcov6 <- sqrt(diag(vcovHC(L_OLS_year_state, type = "HC0")))
n <- vif(L_OLS_year_state)
ncvTest(L_OLS_year_state)

L_OLS_Season <- lm(LogPrice ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + LeaseDuration_a + as.factor(season), data = subset(MasterData_Leases, MasterData_Leases$State != "NV"))
Lcov7 <- sqrt(diag(vcovHC(L_OLS_Season, type = "HC0")))
o <- vif(L_OLS_Season)
ncvTest(L_OLS_Season)

L_OLS_Season_state <- lm(LogPrice ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + LeaseDuration_a + relevel(as.factor(State), "CA") + as.factor(season), data = subset(MasterData_Leases, MasterData_Leases$State != "NV"))
Lcov8 <- sqrt(diag(vcovHC(L_OLS_Season_state, type = "HC0")))
p <- vif(L_OLS_Season_state)
ncvTest(L_OLS_Season_state)

stargazer(L_OLS_DurCont, L_OLS_DurDis, L_OLS_agents, L_OLS_state, L_OLS_year, L_OLS_year_state, L_OLS_Season, L_OLS_Season_state, se = list(Lcov1, Lcov2, Lcov3, Lcov4, Lcov5, Lcov6, Lcov7, Lcov8), title = "Lease Transfers", dep.var.labels = c("Log Price per Acre Foot"), column.labels = c("OLS"), type = "html", out = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/LeaseTransfersDraft3NoQ.htm")

