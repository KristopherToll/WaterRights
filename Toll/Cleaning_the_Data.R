# Kristopher C. Toll
# Cleaning Up data from the water strategist and merging with population stats and rainfall
# challenge only use finding nemo names!

# Importing the DATA
library(readxl)
nemo <- read_excel("C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Raw_Data/Water_Transfer_Data_Feb_10.xls", 
                   na = "null")

# Removing "na" from nemo
dory <- subset(nemo, TotalPrice != "NA" 
                 & nemo$MinimumAnnualAcreFeet != "NA" 
                 & nemo$AverageAnnualAcreFeet != "NA"
                 & nemo$MaximumAnnualAcreFeet != "NA"
                 & nemo$CommittedMinimumAcreFeet != "NA"
                 & nemo$CommitedAverageAcreFeet != "NA"
                 & nemo$CommitedMaximumAcreFeet != "NA"
                 & nemo$InflationAdjustedTotalPrice != "NA"
                 & nemo$InflationAdjustedPricePerAnnualAcreFoot != "NA"
                 & nemo$InflationAdjustedPricePerCommittedAcreFoot != "NA")

# Removing Lease Observations and Combinations
# To remove Sales use a one instead
bruce <- subset(dory, Lease == "0"
                & Combination =="0")

# Only sales and exhanges are in bruce
table(bruce$Sale)

# Remove the recycled vectors and the Lease Duration Vector and others
marlin <- bruce
marlin$Unknown <- NULL
marlin$Recycled <- NULL
marlin$LeaseDuration <- NULL

# Now to fix time!
# Some observations are recored in two months such as July/Aug 
# abreviations are not always used for example january may be used insead of jan
gill <- marlin
table(gill$WaterStrategistIssue)

# Remove the year from month vector along with ' and uneeded spaces
gill$WaterStrategistIssue <- gsub("[0-9]", "", gill$WaterStrategistIssue)
gill$WaterStrategistIssue <- gsub("'", "", gill$WaterStrategistIssue)
gill$WaterStrategistIssue <- gsub(" ", "", gill$WaterStrategistIssue)
gill$WaterStrategistIssue <- gsub("  ", "", gill$WaterStrategistIssue)

# observations such as summer summer/fall will not be usefull to determine price by month
bloat <- subset(gill, gill$WaterStrategistIssue != "Summer/fall"
                                    & gill$WaterStrategistIssue != "Summer/Fall"
                                    & gill$WaterStrategistIssue != "Summer"
                                    & gill$WaterStrategistIssue != "summer"
                                    & gill$WaterStrategistIssue != "Spring"
                                    & gill$WaterStrategistIssue != "spring"
                                    & gill$WaterStrategistIssue != "Fall/winter/"
                                    & gill$WaterStrategistIssue != "Fall/Winter"
                                    & gill$WaterStrategistIssue != "Fall/win"
                                    & gill$WaterStrategistIssue != "Fall-Winter"
                                    & gill$WaterStrategistIssue != "Fall"
                                    & gill$WaterStrategistIssue != "fall"
                                    & gill$WaterStrategistIssue != "."
                                    & gill$WaterStrategistIssue != "Winter"
                                    & gill$WaterStrategistIssue != "winter")
# Make names for months match
# For months that were entered in as jul-aug or jul/aug. 
# For this study, the fist month was used to record when the sale took place
pearl <- bloat
pearl$WaterStrategistIssue <- gsub("April", "Apr", pearl$WaterStrategistIssue)
pearl$WaterStrategistIssue <- gsub("December", "Dec", pearl$WaterStrategistIssue)
pearl$WaterStrategistIssue <- gsub("February", "Feb", pearl$WaterStrategistIssue)
pearl$WaterStrategistIssue <- gsub("January", "Jan", pearl$WaterStrategistIssue)
pearl$WaterStrategistIssue <- gsub("March", "Mar", pearl$WaterStrategistIssue)
pearl$WaterStrategistIssue <- gsub("November", "Nov", pearl$WaterStrategistIssue)
pearl$WaterStrategistIssue <- gsub("October", "Oct", pearl$WaterStrategistIssue)
pearl$WaterStrategistIssue <- gsub("September", "Sep", pearl$WaterStrategistIssue)
pearl$WaterStrategistIssue <- gsub("Sept", "Sep", pearl$WaterStrategistIssue)
pearl$WaterStrategistIssue <- gsub("July", "Jul", pearl$WaterStrategistIssue)
pearl$WaterStrategistIssue <- gsub("June", "Jun", pearl$WaterStrategistIssue)

deb <- pearl
deb$WaterStrategistIssue <- gsub("Ju-Augl", "Jul/Aug", deb$WaterStrategistIssue)
deb$WaterStrategistIssue <- gsub("^Jul-Aug", "Jul/Aug", deb$WaterStrategistIssue)
deb$WaterStrategistIssue <- gsub("^July-Aug", "Jul/Aug", deb$WaterStrategistIssue)
deb$WaterStrategistIssue <- gsub("^July/Aug", "Jul/Aug", deb$WaterStrategistIssue)
deb$WaterStrategistIssue <- gsub("July/August", "Jul/Aug", deb$WaterStrategistIssue, fixed = TRUE)
deb$WaterStrategistIssue <- gsub("Jul/August", "Jul/Aug", deb$WaterStrategistIssue, fixed = TRUE)
deb$WaterStrategistIssue <- gsub("(^Jul$)", "Jul/Aug", deb$WaterStrategistIssue, perl = TRUE)
deb$WaterStrategistIssue <- gsub("(^Aug$)", "Jul/Aug", deb$WaterStrategistIssue, perl = TRUE)

#rename Columnn names to match ClimateData
library(plyr)
deb <- rename(deb, c("WaterStrategistIssue" = "Month"))

saveRDS(deb, file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/Water_Sales.RDS")
