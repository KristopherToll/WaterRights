# Kristopher C. Toll
# Useful Plots for sales and climate data


# Get ride of sicentific notation
options(scipen=999999)

MasterData <- readRDS(MasterData, file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/MasterData.RDS")



MasterData$Month <- factor(MasterData$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul/Aug", "Sep", "Oct", "Nov", "Dec"))

# Inflation Adjusted Price per acre Foot

Counts_by_state.year <-table(MasterData$Month, MasterData$State)

counts_by_state <- table(MasterData$State)

ggplot(data = MasterData, aes(x=Month, y=InflationAdjustedPricePerAnnualAcreFoot)) + geom_jitter()  + aes(colour=Month) + theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title="Price per Acre-Foot") 

ggplot(data = MasterData, aes(x=Month, y=InflationAdjustedPricePerAnnualAcreFoot)) + geom_jitter()  + aes(colour=Month) + facet_wrap(~State, ncol=4, scales = "free_y") + theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title="Prices per Acre-Foot by State") 

ggplot( data = RMasterData, aes(x = InflationAdjustedPricePerAnnualAcreFoot)) + geom_histogram(binwidth=500) + aes(colour=State) + facet_wrap(~State, ncol=4, scales = "free_y") + labs(title="Histogram of Price by State") + theme(legend.position="none") 


# Store and Remove Outliers
Outliers1 <- subset(MasterData, ObsNumber == "719")
Outliers2 <- subset(MasterData, ObsNumber == "722")
Outliers3 <- subset(MasterData, ObsNumber == "706")
Outliers <- rbind(Outliers1, Outliers2, Outliers3)

NoOutliers <- subset(MasterData, ObsNumber != "719" & ObsNumber != "722" & ObsNumber != "706")

ggplot(data = NoOutliers, aes(x=Month, y=InflationAdjustedPricePerAnnualAcreFoot)) + geom_jitter()  + aes(colour=Month) + theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title="Price per Acre-Foot by State W/O Outliers") 

ggplot(data = NoOutliers, aes(x=Month, y=InflationAdjustedPricePerAnnualAcreFoot)) + geom_jitter()  + aes(colour=Month) + facet_wrap(~State, ncol=4, scales = "free_y") + theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title="Price per Acre-Foot by State W/O Outliers") 



# Climate Data
ClimateData <- readRDS(file = "C:/Users/Kristopher/odrive/Google Drive/Water Transfer Project/Modified_Data_Models/Climate_data.RDS")
ClimateData$Month <- factor(ClimateData$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul/Aug", "Sep", "Oct", "Nov", "Dec"))


# PDSI by State
ggplot(data = ClimateData, aes(x=Month, y=PDSI)) + geom_jitter()  + aes(colour=Month)+ facet_wrap(~State, ncol=4, scales = "free_y") + theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title="PDSI by State") 

# PHID by state
ggplot(data = ClimateData, aes(x=Month, y=PHDI)) + geom_boxplot()  + aes(colour=Month) + facet_wrap(~State, ncol=4) + theme(legend.position="none") + labs(title="") 

ggplot(data = ClimateData, aes(x=Month, y=PHDI)) + geom_boxplot()  + aes(colour=Month) + facet_wrap(~State, ncol=4) + theme(legend.position="none") + labs(title="") 
