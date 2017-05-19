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
bruce <- subset(dory, Lease == "1"
                & Combination =="0"
                & Exchange == "0")

# Only sales and exhanges are in bruce
table(bruce$Sale)

# Remove the uneeded columns others
marlin <- bruce
marlin$Unknown <- NULL
marlin$Recycled <- NULL
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
                & gill$WaterStrategistIssue != "winter"
                & gill$WaterStrategistIssue != "summer/fall"
                & gill$WaterStrategistIssue != ""
                & gill$WaterStrategistIssue != "Jan,Feb,Mar,Sep")
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

Leases <- deb

saveRDS(Leases, file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/Water_Leases.RDS")

ClimateData <- readRDS(file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/Climate_data.RDS")

library(dplyr)
MasterData_Leases <- merge(Leases, ClimateData, c("Month", "Year", "State"))

saveRDS(MasterData_Leases, file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/MasterData_Leases_Leases.RDS")

library(foreign)
write.dta(MasterData_Leases, file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/MasterData_Leases_Leases.dta")
# Useful Plots for Leases and climate data

library(ggplot2)
# Get ride of sicentific notation
options(scipen=999999)


MasterData_Leases$Month <- factor(MasterData_Leases$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul/Aug", "Sep", "Oct", "Nov", "Dec"))

# Inflation Adjusted Price per acre Foot

ggplot(data = MasterData_Leases, aes(x=Month, y=InflationAdjustedPricePerAnnualAcreFoot)) + geom_jitter()  + aes(colour=Month) + theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title="Lease Price per Acre-Foot") 

ggplot(data = MasterData_Leases, aes(x=Month, y=InflationAdjustedPricePerAnnualAcreFoot)) + geom_jitter()  + aes(colour=Month) + facet_wrap(~State, ncol=4, scales = "free_y") + theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title="Lease Prices per Acre-Foot by State") 

ggplot(data = MasterData_Leases, aes(x = InflationAdjustedPricePerAnnualAcreFoot)) + geom_histogram(binwidth=500) + aes(colour=State) + theme_bw() + facet_wrap(~State, ncol=4, scales = "free_y") + labs(title="Histogram of Lease Price by State") + theme(legend.position="none") 





# Climate Data
ClimateData <- readRDS(file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/Climate_data.RDS")
ClimateData$Month <- factor(ClimateData$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul/Aug", "Sep", "Oct", "Nov", "Dec"))


# PDSI by State
ggplot(data = ClimateData, aes(x=Month, y=PDSI)) + geom_jitter()  + aes(colour=Month)+ facet_wrap(~State, ncol=4, scales = "free_y") + theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title="PDSI by State") 

ggplot(data = ClimateData, aes(x=Year, y=PDSI)) + geom_jitter()  + aes(colour=State) + facet_wrap(~State, ncol=4, scales = "free_y") + theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title="PDSI by Year") 

ggplot(data = ClimateData, aes(x=Month, y=PHDI)) + geom_boxplot()  + aes(colour=Month) + facet_wrap(~State, ncol=4) + theme(legend.position="none") + labs(title="") 

