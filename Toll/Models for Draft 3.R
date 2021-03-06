## Third Edition Revised Models ##

# Sales Models 

library(stargazer)
library(lmtest)
library(sandwich)
library(car)
library(plm)

library(readr)
MasterData_Sales <- read_csv("C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/MasterData_Sales.csv")
options(scipen=99999)

#MasterData_Sales$X1_1 <- NULL
#MasterData_Sales$LogPrice <- log(MasterData_Sales$InflationAdjustedPricePerAnnualAcreFoot)

#MasterData_Sales$Month <- factor(MasterData_Sales$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul/Aug", "Sep", "Oct", "Nov", "Dec"))
#MasterData_Sales$season <- ifelse(MasterData_Sales$Month == "Jan" | MasterData_Sales$Month =="Feb" | MasterData_Sales$Month == "Mar", "Qrt1", ifelse(MasterData_Sales$Month == "Apr" | MasterData_Sales$Month == "May"| MasterData_Sales$Month == "Jun", "Qrt2",  ifelse(MasterData_Sales$Month == "Jul/Aug"| MasterData_Sales$Month == "Sep", "Qrt3", "Qtr4")))

#write.csv(MasterData_Sales, file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/MasterData_sales.csv")


S_OLS <- lm(LogPrice ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI, subset(MasterData_Sales, MasterData_Sales$State != "MT" & MasterData_Sales$State != "WY"))
S_OLS_robust1 <- coeftest(S_OLS, vcov=vcovHC, type = "HC0")
S_OLS_robust1.1 <- coeftest(S_OLS, vcov=vcovHC, type = "HC0")

cov1 <- sqrt(diag(vcovHC(S_OLS, type = "HC0")))
a <- vif(S_OLS)
ncvTest(S_OLS)

S_OLS_nq <- lm(LogPrice ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI, subset(MasterData_Sales, MasterData_Sales$State != "MT" & MasterData_Sales$State != "WY"))
S_OLS_robust <- coeftest(S_OLS_nq, vcov=vcovHC, type = "HC1")
cov_nq <- sqrt(diag(vcovHC(S_OLS_nq, type = "HC0")))
b <- vif(S_OLS_nq)
ncvTest(S_OLS_nq)

S_OLS_state <- lm(LogPrice ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + relevel(as.factor(State), "CO"), subset(MasterData_Sales, MasterData_Sales$State != "MT" & MasterData_Sales$State != "WY"))
cov2 <- sqrt(diag(vcovHC(S_OLS_state, type = "HC0")))
c <- vif(S_OLS_state)
ncvTest(S_OLS_state)
S_OLS_state_robust <- coeftest(S_OLS_state, vcov=vcovHC, type = "HC0")


S_OLS_Year <- lm(LogPrice ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + as.factor(Year), subset(MasterData_Sales, MasterData_Sales$State != "MT" & MasterData_Sales$State != "WY"))
cov3 <- sqrt(diag(vcovHC(S_OLS_Year, type = "HC0")))
d <- vif(S_OLS_Year)
ncvTest(S_OLS_Year)

S_OLS_State_Year <- lm(LogPrice ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + as.factor(Year) + relevel(as.factor(State), "CO"), subset(MasterData_Sales, MasterData_Sales$State != "MT" & MasterData_Sales$State != "WY"))
cov4 <- sqrt(diag(vcovHC(S_OLS_State_Year, type = "HC0")))
e <- vif(S_OLS_State_Year)
ncvTest(S_OLS_State_Year)

S_OLS_NoAgents <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~ PDSI + as.factor(Year) + relevel(as.factor(State), "CO"), subset(MasterData_Sales, MasterData_Sales$State != "MT" & MasterData_Sales$State != "WY"))
cov5 <- sqrt(diag(vcovHC(S_OLS_NoAgents, type = "HC0")))
f <- vif(S_OLS_NoAgents)
ncvTest(S_OLS_NoAgents)

S_OLS_Season <- lm(LogPrice ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + season, subset(MasterData_Sales, MasterData_Sales$State != "MT" & MasterData_Sales$State != "WY"))
cov6 <- sqrt(diag(vcovHC(S_OLS_Season, type = "HC0")))
g <- vif(S_OLS_Season)
ncvTest(S_OLS_Season)

S_OLS_Season_state <- lm(LogPrice ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoUrban + UrbantoEnviro + PDSI + season + relevel(as.factor(State), "CO"), subset(MasterData_Sales, MasterData_Sales$State != "MT" & MasterData_Sales$State != "WY"))
cov7 <- sqrt(diag(vcovHC(S_OLS_Season_state, type = "HC0")))
h <- vif(S_OLS_Season_state)
ncvTest(S_OLS_Season_state)

stargazer(S_OLS_NoAgents, S_OLS_nq, S_OLS_state, S_OLS_Year, S_OLS_State_Year, S_OLS_Season, S_OLS_Season_state, se = list(cov5, cov_nq, cov2, cov3, cov4, cov6, cov7) ,title = "Permanent Transfers", dep.var.labels = c("Log Price per Acre Foot"), column.labels = c("OLS"), type = "html", out = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/PermanentTransfersDraft3_1NoQ.htm")


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
