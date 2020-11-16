# Get temperature data ready to share
# Aimee H Fullerton, 16 November 2020

data.dir <- "/Users/aimee_fullerton/OneDrive/Work/Research/StreamTemperature/Wenatchee/NOAA-USFS"
DataYears<- 2018:2020
LeapYears<- seq(2018, 2040, 4)
nYears<- length(DataYears)
nLeapYrs<- length(intersect(LeapYears, DataYears))

# Load "all data file"
file <- paste0(data.dir1, "/wenatchee.wt.allyears.csv")
wtdat <- read.csv(file, header = T, stringsAsFactors = F)
wtdat$Date <- as.Date(wtdat$Date)
sites <- colnames(wtdat[3:ncol(wtdat)])
sites <- gsub("_.*","", sites)
colnames(wtdat) <- c("Date", "Time", sites)
summary(wtdat)



# Add water year column
wtdat$WaterYear <- NA
for(yy in c(DataYears[1] - 1, DataYears)){
  wtdat$WaterYear[wtdat$Date >= as.Date(paste0(yy - 1, "-10-01")) & wtdat$Date <= as.Date(paste0(yy, "-12-31"))] <- yy
}
#nrow(wtdat[wtdat$WaterYear == 2018,])

# Add time stamp column
fncTimeStamp <- function(x){
  thehour <- floor(x)
  theminutes <- rep("00", length.out = length(x))
  answer <- paste0(thehour, ":", theminutes, ":00 PDT")
  return(answer)
}
wtdat$Time2 <- fncTimeStamp(wtdat$Time)

# Output individual site files
for(site in sites){
  idx <- which(colnames(wtdat) == site)
  td <- wtdat[,c("WaterYear", "Date", "Time2", site)]
  colnames(td)[3] <- "Time"
  colnames(td)[4] <- "Temp_C"
  write.csv(td, paste0(data.dir1, "/Data2Share/NOAA-USFS_", site,".csv"), row.names = F, na = "")
}  

summary(td[td$WaterYear == 2020,]) #to examine

# end of script
