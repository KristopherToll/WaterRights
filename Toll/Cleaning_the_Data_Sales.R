# Kristopher C. Toll
# Cleaning Up data from the water strategist and merging with population stats and rainfall
# challenge only use finding nemo names!

# Importing the DATA
library(readxl)
nemo <- read_excel("C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Raw_Data/Water_Transfer_Data_Feb_10.xls", 
                   na = "null")

nemo <- subset(nemo, IDCode != "CO1857"
               & IDCode != "CO1875"
               & IDCode != "NV155"
               & IDCode != "OR109"
               & IDCode != "OR111"
               & IDCode != "OR113"
               & IDCode != "WA55"
               & IDCode !="AZ227"
               & IDCode != "AZ228"
               & IDCode != "CA626"
               & IDCode != "CA625")


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
                & Combination =="0"
                & Exchange == "0")

# Only sales and exhanges are in bruce
table(bruce$Sale)

# Remove the recycled vectors and the Lease Duration Vector and others
marlin <- bruce
marlin$Unknown <- NULL
marlin$Recycled <- NULL
marlin$LeaseDuration <- NULL
marlin$Exchange <- NULL

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
names(deb)[names(deb)=="WaterStrategistIssue"] <- "Month"

library(data.table)
# Read in the Data
ClimateData <- readRDS(file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/Climate_data.RDS")

# Merge
library(dplyr)
MasterData_Sales <- merge(deb, ClimateData, c("Month", "Year", "State"))

#Create Dummy Variables for State and Month
MasterData_Sales$AZ <- ifelse(MasterData_Sales$State=="AZ", 1, 0)
MasterData_Sales$CA <- ifelse(MasterData_Sales$State=="CA", 1, 0)
MasterData_Sales$CO <- ifelse(MasterData_Sales$State=="CO", 1, 0)
MasterData_Sales$ID <- ifelse(MasterData_Sales$State=="ID", 1, 0)
MasterData_Sales$MT <- ifelse(MasterData_Sales$State=="MT", 1, 0)
MasterData_Sales$NM <- ifelse(MasterData_Sales$State=="NM", 1, 0)
MasterData_Sales$NV <- ifelse(MasterData_Sales$State=="NV", 1, 0)
MasterData_Sales$OR <- ifelse(MasterData_Sales$State=="OR", 1, 0)
MasterData_Sales$TX <- ifelse(MasterData_Sales$State=="TX", 1, 0)
MasterData_Sales$UT <- ifelse(MasterData_Sales$State=="UT", 1, 0)
MasterData_Sales$WA <- ifelse(MasterData_Sales$State=="WA", 1, 0)
MasterData_Sales$WY <- ifelse(MasterData_Sales$State=="WY", 1, 0)

MasterData_Sales$Apr <- ifelse(MasterData_Sales$Month=="Apr", 1, 0)
MasterData_Sales$Dec <- ifelse(MasterData_Sales$Month=="Dec", 1, 0)
MasterData_Sales$Feb <- ifelse(MasterData_Sales$Month=="Feb", 1, 0)
MasterData_Sales$Jan <- ifelse(MasterData_Sales$Month=="Jan", 1, 0)
MasterData_Sales$Jul.Aug <- ifelse(MasterData_Sales$Month=="Jul/Aug", 1, 0)
MasterData_Sales$Jun <- ifelse(MasterData_Sales$Month=="Jun", 1, 0)
MasterData_Sales$Mar <- ifelse(MasterData_Sales$Month=="Mar", 1, 0)
MasterData_Sales$May <- ifelse(MasterData_Sales$Month=="May", 1, 0)
MasterData_Sales$Nov <- ifelse(MasterData_Sales$Month=="Nov", 1, 0)
MasterData_Sales$Oct <- ifelse(MasterData_Sales$Month=="Oct", 1, 0)
MasterData_Sales$Sep <- ifelse(MasterData_Sales$Month=="Sep", 1, 0)

# Create a List that shows combination of buyer and seller
MasterData_Sales$Type <- ifelse(MasterData_Sales$AgtoAg == "1", "AgtoAg", ifelse(MasterData_Sales$AgtoUrban == "1", "AgtoUrban", ifelse(MasterData_Sales$AgtoEnivo =="1", "AgtoEnvio", ifelse(MasterData_Sales$UrbantoAg=="1", "UrbantoAg", ifelse(MasterData_Sales$UrbantoUrban=="1", "UrbantoUrban", ifelse(MasterData_Sales$UrbantoEnviro=="1", "UrbantoEnviro", ifelse(MasterData_Sales$EnvirotoAg=="1","EnvirotoAg", ifelse(MasterData_Sales$EnvirotoUrban=="1", "EnvirotoUrban", "EnvirotoEnviro"))))))))


saveRDS(MasterData_Sales, file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/MasterData_Sales.RDS")

# Verify that MasterData_Sales has the same number of observations as Water_Sales

table(ClimateData$State)
table(Water_Sales$State)
table(MasterData_Sales$State)

table(ClimateData$Year)
table(Water_Sales$Year)
table(MasterData_Sales$Year)

table(ClimateData$Month)
table(Water_Sales$Month)
table(MasterData_Sales$Month)


# save as an csv and dta

library(foreign)

write.csv(MasterData_Sales, file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/MasterData_Sales.csv")
write.dta(MasterData_Sales, file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/MasterData_Sales.dta")


saveRDS(deb, file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/Water_Sales.RDS")

# Making Plots

library(ggplot2)
options(scipen=999999)
MasterData_Sales <- subset(MasterData_Sales, ObsNumber != "719" & ObsNumber != "722" & ObsNumber != "706")

MasterData_Sales$Type <- ifelse(MasterData_Sales$AgtoAg == "1", "AgtoAg", ifelse(MasterData_Sales$AgtoUrban == "1", "AgtoUrban", ifelse(MasterData_Sales$AgtoEnivo =="1", "AgtoEnvio", ifelse(MasterData_Sales$UrbantoAg=="1", "UrbantoAg", ifelse(MasterData_Sales$UrbantoUrban=="1", "UrbantoUrban", ifelse(MasterData_Sales$UrbantoEnviro=="1", "UrbantoEnviro", ifelse(MasterData_Sales$EnvirotoAg=="1","EnvirotoAg", ifelse(MasterData_Sales$EnvirotoUrban=="1", "EnvirotoUrban", "EnvirotoEnviro"))))))))
MasterData_Sales$Month <- factor(MasterData_Sales$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul/Aug", "Sep", "Oct", "Nov", "Dec"))

ggplot(data = MasterData_Sales, aes(x=Month, y=InflationAdjustedPricePerAnnualAcreFoot)) + geom_jitter()  + aes(colour=Type) + facet_wrap(~State, ncol=4, scales = "free_y") + theme(legend.position="right", axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title="Sale Prices per Acre-Foot by State") 

