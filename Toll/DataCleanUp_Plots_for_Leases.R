# Kristopher C. Toll
# Cleaning Up data from the water strategist and merging with population stats and rainfall
# challenge only use finding nemo names!

# Importing the DATA
library(readxl)
nemo <- read_excel("C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Raw_Data/Water_Transfer_Data_Feb_10.xls")

# Remove Observations that fit under no classification
nemo$ObsNumber <- 1:nrow(nemo)

nemo <- subset(nemo, ObsNumber != "719" & ObsNumber != "722" & ObsNumber != "706")

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
               & LeaseDuration != "NA")

# Removing Lease Observations and Combinations
# To remove Sales use a one instead
bruce <- subset(dory, Lease != "0")
bruce <- subset(bruce, Sale != "1")
bruce <- subset(bruce, Exchange != "1")
bruce <- subset(bruce, Unknown != "1" )
bruce <- subset(bruce, Combination != "1")
# Only Leases
table(bruce$Sale)
table(bruce$Lease)
# Remove the uneeded columns others
marlin <- bruce
marlin$Unknown <- NULL
marlin$Recycled <- NULL
marlin$Exchange <- NULL

# Get rid of observations that do not make sense
marlin <- subset(marlin, WaterStrategistIssue != "39753")

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

#saveRDS(Leases, file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/Water_Leases.RDS")

ClimateData <- readRDS(file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/Climate_data.RDS")

library(dplyr)
MasterData_Leases <- merge(Leases, ClimateData, c("Month", "Year", "State"))


#Create Dummy Variables for State and Month
#MasterData_Leases$AZ <- ifelse(MasterData_Leases$State=="AZ", 1, 0)
#MasterData_Leases$CA <- ifelse(MasterData_Leases$State=="CA", 1, 0)
#MasterData_Leases$CO <- ifelse(MasterData_Leases$State=="CO", 1, 0)
#MasterData_Leases$ID <- ifelse(MasterData_Leases$State=="ID", 1, 0)
#MasterData_Leases$MT <- ifelse(MasterData_Leases$State=="MT", 1, 0)
#MasterData_Leases$NM <- ifelse(MasterData_Leases$State=="NM", 1, 0)
#MasterData_Leases$NV <- ifelse(MasterData_Leases$State=="NV", 1, 0)
#MasterData_Leases$OR <- ifelse(MasterData_Leases$State=="OR", 1, 0)
#MasterData_Leases$TX <- ifelse(MasterData_Leases$State=="TX", 1, 0)
#MasterData_Leases$UT <- ifelse(MasterData_Leases$State=="UT", 1, 0)
#MasterData_Leases$WA <- ifelse(MasterData_Leases$State=="WA", 1, 0)
#MasterData_Leases$WY <- ifelse(MasterData_Leases$State=="WY", 1, 0)

#MasterData_Leases$Apr <- ifelse(MasterData_Leases$Month=="Apr", 1, 0)
#MasterData_Leases$Dec <- ifelse(MasterData_Leases$Month=="Dec", 1, 0)
#MasterData_Leases$Feb <- ifelse(MasterData_Leases$Month=="Feb", 1, 0)
#MasterData_Leases$Jan <- ifelse(MasterData_Leases$Month=="Jan", 1, 0)
#MasterData_Leases$Jul.Aug <- ifelse(MasterData_Leases$Month=="Jul/Aug", 1, 0)
#MasterData_Leases$Jun <- ifelse(MasterData_Leases$Month=="Jun", 1, 0)
#MasterData_Leases$Mar <- ifelse(MasterData_Leases$Month=="Mar", 1, 0)
#MasterData_Leases$May <- ifelse(MasterData_Leases$Month=="May", 1, 0)
#MasterData_Leases$Nov <- ifelse(MasterData_Leases$Month=="Nov", 1, 0)
#MasterData_Leases$Oct <- ifelse(MasterData_Leases$Month=="Oct", 1, 0)
#MasterData_Leases$Sep <- ifelse(MasterData_Leases$Month=="Sep", 1, 0)

MasterData_Leases <- subset(MasterData_Leases, ObsNumber != 3828 & ObsNumber != 3830 & ObsNumber != 3832)

MasterData_Leases$Type <- ifelse(MasterData_Leases$AgtoAg == "1", "AgtoAg", ifelse(MasterData_Leases$AgtoEnivo == "1", "AgtoEnivo", ifelse(MasterData_Leases$AgtoUrban == "1", "AgtoUrban", ifelse(MasterData_Leases$UrbantoAg == "1", "UrbantoAg", ifelse(MasterData_Leases$UrbantoEnviro == "1", "UrbantoEnviro", ifelse(MasterData_Leases$UrbantoUrban == "1", "UrbantoUrban", ifelse(MasterData_Leases$EnvirotoAg == "1", "EnvirotoAg", ifelse(MasterData_Leases$EnvirotoEnviro == "1", "EnvirotoEnviro", "EnvirotoUrban"))))))))

MasterData_Leases$Month <- factor(MasterData_Leases$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul/Aug", "Sep", "Oct", "Nov", "Dec"))
MasterData_Leases <- subset(MasterData_Leases, MasterData_Leases$InflationAdjustedPriceperAnnualAcreFoot != "0")
MasterData_Leases <- subset(MasterData_Leases, MasterData_Leases$EnvirotoEnviro != "1")


# Creating Lease Duration as a dummy

MasterData_Leases$LeaseDuration_a <- ifelse(MasterData_Leases$LeaseDuration == "1", "1", ifelse(MasterData_Leases$LeaseDuration == "2", "2", ifelse(MasterData_Leases$LeaseDuration == "3", "3", ifelse(MasterData_Leases$LeaseDuration == "4", "4", ifelse(MasterData_Leases$LeaseDuration >= 5 & MasterData_Leases$LeaseDuration <= 10, "5-10 Years", ifelse(MasterData_Leases$LeaseDuration >= 11 & MasterData_Leases$LeaseDuration <= 20, "11-20 years", "21-100 years"))))))
MasterData_Leases$LeaseDuration_a <- factor(as.factor(MasterData_Leases$LeaseDuration_a), c("1", "2", "3", "4", "5-10 Years", "11-20 years", "21-100 years"))

MasterData_Leases$season <- ifelse(MasterData_Leases$Month == "Jan" | MasterData_Leases$Month =="Feb" | MasterData_Leases$Month == "Mar", "Qrt1", ifelse(MasterData_Leases$Month == "Apr" | MasterData_Leases$Month == "May"| MasterData_Leases$Month == "Jun", "Qrt2",  ifelse(MasterData_Leases$Month == "Jul.Aug"| MasterData_Leases$Month == "Sep", "Qrt3", "Qtr4")))

MasterData_Leases$LogPrice <- log(MasterData_Leases$InflationAdjustedPriceperAnnualAcreFoot)

# Saving MasterData Leases
saveRDS(MasterData_Leases, file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/MasterData_Leases.RDS")


library(foreign)
write.dta(MasterData_Leases, file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/MasterData_Leases.dta")
write.csv(MasterData_Leases, file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/MasterData_Leases.csv")


library(sandwich)
library(lmtest)


options(scipen=999) # This is get rid of scientific notation

# Run regression 
library(lmtest)
library(sandwich)

# Regression with 1 add to the dependent
leasereg1 <- lm(log(InflationAdjustedPricePerAnnualAcreFoot + 1) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoEnviro + UrbantoUrban + EnvirotoEnviro + CommitedAverageAcreFeet + LeaseDuration + PDSI + Jan + Feb + Mar + Apr + May + Jun + Jul.Aug + Oct + Nov + Dec + AZ + CA + ID + MT + NM + NV + OR + TX + UT + WA + WY, data = MasterData_Leases)
leasereg2 <- coeftest(leasereg1, vcov. = vcovHC(leasereg1, "HC1"))


# Regression without the zero price listed in the dataset
leasereg1.1 <- lm(log(InflationAdjustedPricePerAnnualAcreFoot) ~ AgtoUrban + AgtoEnivo + UrbantoAg + UrbantoEnviro + UrbantoUrban + EnvirotoEnviro + CommitedAverageAcreFeet + LeaseDuration + PDSI + Jan + Feb + Mar + Apr + May + Jun + Jul.Aug + Oct + Nov + Dec + AZ + CA + ID + MT + NM + NV + OR + TX + UT + WA + WY, data = subset(MasterData_Leases, InflationAdjustedPricePerAnnualAcreFoot!= 0))
leasereg2.1 <- coeftest(leasereg1.1, vcov. = vcovHC(leasereg1.1, "HC1"))

# Leases price by state

MasterData_Leases$Month <- factor(MasterData_Leases$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul/Aug", "Sep", "Oct", "Nov", "Dec"))

MasterData_Leases$AgtoUrban <- as.numeric(MasterData_Leases$AgtoUrban)
MasterData_Leases$UrbantoUrban <- as.character(MasterData_Leases$UrbantoUrban)

MasterData_Leases$Type <- ifelse(MasterData_Leases$AgtoAg == "1", "AgtoAg", ifelse(MasterData_Leases$AgtoUrban == "1", "AgtoUrban", ifelse(MasterData_Leases$AgtoEnivo =="1", "AgtoEnvio", ifelse(MasterData_Leases$UrbantoAg=="1", "UrbantoAg", ifelse(MasterData_Leases$UrbantoUrban=="1", "UrbantoUrban", ifelse(MasterData_Leases$UrbantoEnviro=="1", "UrbantoEnviro", ifelse(MasterData_Leases$EnvirotoAg=="1","EnvirotoAg", ifelse(MasterData_Leases$EnvirotoUrban=="1", "EnvirotoUrban", "EnvirotoEnviro"))))))))

ggplot(data = MasterData_Leases, aes(x=Month, y=InflationAdjustedPricePerAnnualAcreFoot)) + geom_jitter()  + aes(colour=Type) + facet_wrap(~State, ncol=4, scales = "free_y") + theme(legend.position="right", axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title="Lease Prices per Acre-Foot by State") 

