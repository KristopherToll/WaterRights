# Kristopher C. Toll
# Cleaning state data on rainfall

AZ <- read.table(file ="C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Raw_Data/AZ.txt",
           header = TRUE)
AZ$StateCode <- "AZ"

CA <- read.table(file ="C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Raw_Data/CA.txt",
                 header = TRUE)
CA$StateCode <- "CA"

CO <- read.table(file ="C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Raw_Data/CO.txt",
                header = TRUE)
CO$StateCode <- "CO"

ID <- read.table(file ="C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Raw_Data/ID.txt",
                header = TRUE)
ID$StateCode <- "ID"

MT <- read.table(file ="C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Raw_Data/MT.txt",
                 header = TRUE)
MT$StateCode <- "MT"

NM <- read.table(file ="C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Raw_Data/NM.txt",
                header = TRUE)
NM$StateCode <- "NM"

NV <- read.table(file ="C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Raw_Data/NV.txt",
                header = TRUE)
NV$StateCode <- "NV"

OR <- read.table(file ="C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Raw_Data/OR.txt",
                header = TRUE)
OR$StateCode <- "OR"

TX <- read.table(file ="C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Raw_Data/TX.txt",
                header = TRUE)
TX$StateCode <- "TX"

UT <- read.table(file ="C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Raw_Data/UT.txt",
                header = TRUE)
UT$StateCode <- "UT"

WA <- read.table(file ="C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Raw_Data/WA.txt",
                header = TRUE)
WA$StateCode <- "WA"

WY <- read.table(file ="C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Raw_Data/WY.txt",
                header = TRUE)
WY$StateCode <- "WY"

ClimateData <- rbind(AZ, CA, CO, ID, MT, NM, NV, OR, TX, UT, WA, WY)

# Split Dates
library(tidyr)

df <- data.frame(date = ClimateData$YearMonth)

dates <- df %>% separate(date, into = c('year', 'month'), sep = 4)

ClimateData$Year <- dates$year
ClimateData$Month <- dates$month

# rewrite month numbers as abr. months
# Use Jul/Aug to match the Water_Sales data
ClimateData$Month <- gsub("^01$", "Jan", ClimateData$Month, perl = TRUE)
ClimateData$Month <- gsub("^02$", "Feb", ClimateData$Month, perl = TRUE)
ClimateData$Month <- gsub("^03$", "Mar", ClimateData$Month, perl = TRUE)
ClimateData$Month <- gsub("^04$", "Apr", ClimateData$Month, perl = TRUE)
ClimateData$Month <- gsub("^05$", "May", ClimateData$Month, perl = TRUE)
ClimateData$Month <- gsub("^06$", "Jun", ClimateData$Month, perl = TRUE)
ClimateData$Month <- gsub("^07$", "Jul/Aug", ClimateData$Month, perl = TRUE)
ClimateData$Month <- gsub("^08$", "Jul/Aug", ClimateData$Month, perl = TRUE)
ClimateData$Month <- gsub("^09$", "Sep", ClimateData$Month, perl = TRUE)
ClimateData$Month <- gsub("^10$", "Oct", ClimateData$Month, perl = TRUE)
ClimateData$Month <- gsub("^11$", "Nov", ClimateData$Month, perl = TRUE)
ClimateData$Month <- gsub("^12$", "Dec", ClimateData$Month, perl = TRUE)


# Rename StateCode to State
library(plyr)
ClimateData <- rename(ClimateData, c("StateCode" = "State"))

# Drop YearMonth 
ClimateData$YearMonth <- NULL


# Use average of July and Auguest for Jul/Aug


saveRDS(ClimateData, file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/climate_data.RDS")
