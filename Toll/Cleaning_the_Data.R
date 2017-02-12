
# challeng only use nemo names!
# Importing the DATA!
library(readxl)
nemo <- read_excel("C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Raw_Data/Water_Transfer_Data_Feb_10.xls", na = "null")
View(nemo)

# Removing "na" from nemo
dory <- subset(nemo, TotalPrice != "NA" 
                 & MinimumAnnualAcreFeet != "NA" 
                 & AverageAnnualAcreFeet != "NA"
                 & MaximumAnnualAcreFeet != "NA"
                 & nemo$CommittedMinimumAcreFeet != "NA"
                 & nemo$CommitedAverageAcreFeet != "NA"
                 & nemo$CommitedMaximumAcreFeet != "NA"
                 & nemo$InflationAdjustedTotalPrice != "NA"
                 & nemo$InflationAdjustedPricePerAnnualAcreFoot != "NA"
                 & nemo$InflationAdjustedPricePerCommittedAcreFoot != "NA")

# Removing Lease Observations
# To remove Sales use a one instead
bruce <- subset(dory, Lease == "0")
View(bruce)

# Remove the recycled vectors and the Lease Duration Vector and others
merlin <- bruce
merlin$Unknown <- NULL
merlin$Recycled <- NULL
merlin$LeaseDuration <- NULL
View(merlin)

# Now to fix time!
# Some observations are recored in two months such as July/Aug 
# abreviations are not always used
table(merlin$WaterStrategistIssue)


