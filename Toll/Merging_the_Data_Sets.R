# Kristopher C. Toll
# Merging the Data


library(data.table)
# Read in the Data
ClimateData <- readRDS(file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/Climate_data.RDS")

Water_Sales <- readRDS(file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/Water_Sales.RDS")


# Merge
library(dplyr)
MasterData <- merge(Water_Sales, ClimateData, c("Month", "Year", "State"))

saveRDS(MasterData, file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/MasterData.RDS")

# Verify that MasterData has the same number of observations as Water_Sales

table(ClimateData$State)
table(Water_Sales$State)
table(MasterData$State)

table(ClimateData$Year)
table(Water_Sales$Year)
table(MasterData$Year)

table(ClimateData$Month)
table(Water_Sales$Month)
table(MasterData$Month)


# save as an csv and dta

library(foreign)

write.csv(MasterData, file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/MasterData.csv")
write.dta(MasterData, file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/MasterData.dta")
