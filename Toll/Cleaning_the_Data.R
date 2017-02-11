#Importing the DATA!
library(readxl)
bob <- read_excel("C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Raw_Data/Water_Transfer_Data_Feb_10.xls", na = "null")
View(bob)
# Removing "na" from bob
gerald <- subset(bob, TotalPrice != "NA" 
                 & MinimumAnnualAcreFeet != "NA" 
                 & AverageAnnualAcreFeet != "NA"
                 & MaximumAnnualAcreFeet != "NA"
                 & bob$CommittedMinimumAcreFeet != "NA"
                 & bob$CommitedAverageAcreFeet != "NA"
                 & bob$CommitedMaximumAcreFeet != "NA"
                 & bob$`InflationAdjustedTotal Price`)
