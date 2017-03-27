# Kristopher C. Toll
# Use Harry Potter references when conveient
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

Potter <- rbind(AZ, CA, CO, ID, MT, NM, NV, OR, TX, UT, WA, WY)

# Split Dates
library(tidyr)

df <- data.frame(date = Potter$YearMonth)

dates <- df %>% separate(date, into = c('year', 'month'), sep = 4)

Potter$Year <- dates$year
Potter$Month <- dates$month

# rewrite month numbers as abr. months
# Use Jul/Aug to match the Water_Sales data
Potter$Month <- gsub("^01$", "Jan", Potter$Month, perl = TRUE)
Potter$Month <- gsub("^02$", "Feb", Potter$Month, perl = TRUE)
Potter$Month <- gsub("^03$", "Mar", Potter$Month, perl = TRUE)
Potter$Month <- gsub("^04$", "Apr", Potter$Month, perl = TRUE)
Potter$Month <- gsub("^05$", "May", Potter$Month, perl = TRUE)
Potter$Month <- gsub("^06$", "Jun", Potter$Month, perl = TRUE)
Potter$Month <- gsub("^07$", "Jul/Aug", Potter$Month, perl = TRUE)
Potter$Month <- gsub("^08$", "Jul/Aug", Potter$Month, perl = TRUE)
Potter$Month <- gsub("^09$", "Sep", Potter$Month, perl = TRUE)
Potter$Month <- gsub("^10$", "Oct", Potter$Month, perl = TRUE)
Potter$Month <- gsub("^11$", "Nov", Potter$Month, perl = TRUE)
Potter$Month <- gsub("^12$", "Dec", Potter$Month, perl = TRUE)


# Rename StateCode to State
library(plyr)
Potter <- rename(Potter, c("StateCode" = "State"))

# Drop YearMonth 
Potter$YearMonth <- NULL
Potter$Division <- NULL

# Use average of July and Auguest for Jul/Aug
# Subset things out to take averages and then bind back in
library(mosaic)

Malfoy <- subset(Potter, Month != "Jul/Aug")

library(data.table)
Granger <- subset(Potter, Month == "Jul/Aug")
Granger$indx <- seq(1, nrow(Granger))

PCP = aggregate(PCP ~ State + Month + Year, transform(Granger, indx = cumsum(substr(indx, 551, 552) == '00')), mean)
TAVG = aggregate(TAVG ~ State + Month + Year, transform(Granger, indx = cumsum(substr(indx, 551, 552) == '00')), mean)
PDSI = aggregate(PDSI ~ State + Month + Year, transform(Granger, indx = cumsum(substr(indx, 551, 552) == '00')), mean)
PHDI = aggregate(PHDI ~ State + Month + Year, transform(Granger, indx = cumsum(substr(indx, 551, 552) == '00')), mean)
ZNDX = aggregate(ZNDX ~ State + Month + Year, transform(Granger, indx = cumsum(substr(indx, 551, 552) == '00')), mean)
PMDI = aggregate(PMDI ~ State + Month + Year, transform(Granger, indx = cumsum(substr(indx, 551, 552) == '00')), mean)
CDD = aggregate(CDD ~ State + Month + Year, transform(Granger, indx = cumsum(substr(indx, 551, 552) == '00')), mean)
HDD = aggregate(HDD ~ State + Month + Year, transform(Granger, indx = cumsum(substr(indx, 551, 552) == '00')), mean)
SP01 = aggregate(SP01 ~ State + Month + Year, transform(Granger, indx = cumsum(substr(indx, 551, 552) == '00')), mean)
SP02 = aggregate(SP02 ~ State + Month + Year, transform(Granger, indx = cumsum(substr(indx, 551, 552) == '00')), mean)
SP03 = aggregate(SP03 ~ State + Month + Year, transform(Granger, indx = cumsum(substr(indx, 551, 552) == '00')), mean)
SP06 = aggregate(SP06 ~ State + Month + Year, transform(Granger, indx = cumsum(substr(indx, 551, 552) == '00')), mean)
SP09 = aggregate(SP09 ~ State + Month + Year, transform(Granger, indx = cumsum(substr(indx, 551, 552) == '00')), mean)
SP12 = aggregate(SP12 ~ State + Month + Year, transform(Granger, indx = cumsum(substr(indx, 551, 552) == '00')), mean)
SP24 = aggregate(SP24 ~ State + Month + Year, transform(Granger, indx = cumsum(substr(indx, 551, 552) == '00')), mean)
TMIN = aggregate(TMIN ~ State + Month + Year, transform(Granger, indx = cumsum(substr(indx, 551, 552) == '00')), mean)
TMAX = aggregate(TMAX ~ State + Month + Year, transform(Granger, indx = cumsum(substr(indx, 551, 552) == '00')), mean)

doby <- cbind(PCP, TAVG$TAVG, PDSI$PDSI, PHDI$PHDI, ZNDX$ZNDX, PMDI$PMDI, CDD$CDD, HDD$HDD, SP01$SP01, SP02$SP02, SP03$SP03, SP06$SP06, SP09$SP09, SP12$SP12, SP24$SP24, TMIN$TMIN, TMAX$TMAX)

doby <- rename(doby, c("TAVG$TAVG" = "TAVG", "PDSI$PDSI" = "PDSI", "PHDI$PHDI" = "PHDI", "ZNDX$ZNDX" = "ZNDX", "PMDI$PMDI" = "PMDI", "CDD$CDD" = "CDD", "HDD$HDD" = "HDD", "SP01$SP01" = "SP01", "SP02$SP02" = "SP02",
                       "SP03$SP03" = "SP03", "SP06$SP06" = "SP06", "SP09$SP09" = "SP09", "SP12$SP12" = "SP12", "SP24$SP24" = "SP24", "TMIN$TMIN" = "TMIN", "TMAX$TMAX" = "TMAX"))

# rbind malfoy and doby

ClimateData <- rbind(doby, Malfoy)

saveRDS(ClimateData, file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/climate_data.RDS")
