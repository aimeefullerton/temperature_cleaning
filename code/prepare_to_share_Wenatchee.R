# Get temperature data ready to share
# Aimee H Fullerton, 18 November 2020

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
  theminutes <- substr(x%%2, 3, 3)
  theminutes[theminutes == ""] <- "00"
  theminutes[theminutes == "5"] <- "30"
  answer <- paste0(thehour, ":", theminutes, ":00 PDT")
  return(answer)
}

wtdat$Time2 <- fncTimeStamp(wtdat$Time)
range(wtdat$Date[!is.na(wtdat$D1)])

# Output individual site files
for(site in sites){
  idx <- which(colnames(wtdat) == site)
  td <- wtdat[,c("WaterYear", "Date", "Time", "Time2", site)]
  td <- td[order(td$Date, td$Time),]
  td <- td[,-3]
  colnames(td)[3] <- "Time"
  colnames(td)[4] <- "Temp_C"
  write.csv(td, paste0(data.dir1, "/Data2Share/NOAA-USFS_", site,".csv"), row.names = F, na = "")
}  

summary(td[td$WaterYear == 2019,]) #to examine

# end of script
