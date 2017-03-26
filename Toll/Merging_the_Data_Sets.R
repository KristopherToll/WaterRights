# Kristopher C. Toll
# Merging the Data

# Read in the Data
ClimateData <- readRDS(file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/Climate_data.RDS")

Water_Sales <- readRDS(file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/Water_Sales.RDS")


# Merge
library(dplyr)
MasterData <- merge(Water_Sales, ClimateData, by = c("Month", "Year", "State"))

# Verify that MasterData has the same number of observations as Water_Sales

table(ClimateData$Month)
table(Water_Sales$Month)
table(MasterData$Month)
