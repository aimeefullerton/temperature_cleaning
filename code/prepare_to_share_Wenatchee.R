# Get temperature data ready to share
# Aimee H Fullerton, 23 September 2021

data.dir <- "/Users/aimee_fullerton/OneDrive/Work/Research/StreamTemperature/Wenatchee/NOAA-USFS"
thisyear <- 2021
DataYears<- 2019:thisyear
LeapYears<- seq(2018, 2040, 4)
nYears<- length(DataYears)
nLeapYrs<- length(intersect(LeapYears, DataYears))
numdailyobs <- 24

source("code/cleaning_functions.R")

# Load "all data file"
file <- paste0(data.dir, "/wenatchee.wt.allyears_", thisyear, ".csv")
wtdat <- read.csv(file, header = T, stringsAsFactors = F)
date.format <- detect.date.format(wtdat$Date[1])
wtdat$Date <- as.Date(wtdat$Date, format = date.format)
sites <- colnames(wtdat[3:ncol(wtdat)])
colnames(wtdat) <- c("Date", "Time", sites)
summary(wtdat)

# Remove sites that are no longer operational
decommissioned.sites <- NULL
if(!is.null(decommissioned.sites)){
  idx <- which(colnames(wtdat) %in% decommissioned.sites)
  wtdat <- wtdat[,-idx]
  colnames(wtdat)
  sites <- sites[!sites %in% decommissioned.sites]
}

# Add water year column
wtdat$WaterYear <- NA
for(yy in c(DataYears[1] - 1, DataYears)){
  wtdat$WaterYear[wtdat$Date >= as.Date(paste0(yy - 1, "-10-01")) & wtdat$Date <= as.Date(paste0(yy, "-12-31"))] <- yy
}
#nrow(wtdat[wtdat$WaterYear == thisyear,])

wtdat$Time2 <- time.stamp(wtdat$Time)
range(wtdat$Date[!is.na(wtdat[,3])])

# Output individual site files
for(site in sites){
  idx <- which(colnames(wtdat) == site)
  td <- wtdat[,c("WaterYear", "Date", "Time", "Time2", site)]
  td <- td[order(td$Date, td$Time),]
  td <- td[,-3]
  colnames(td)[3] <- "Time"
  colnames(td)[4] <- "Temp_C"
  write.csv(td, paste0(data.dir, "/Data2Share/NOAA-USFS_", site,".csv"), row.names = F, na = "")
}  

summary(td[td$WaterYear == thisyear,]) #to examine

# end of script
