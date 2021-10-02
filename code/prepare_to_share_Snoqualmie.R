# Get Snoqualmie watershed water temperature data ready to share on King County's website
# Aimee H Fullerton, 23 September 2021

data.dir <- "/Users/aimee_fullerton/OneDrive/Work/Research/StreamTemperature/Hobonet/Data"
thisyear <- 2021
DataYears<- 2012:thisyear
LeapYears<- seq(2008, 2040, 4)
nYears<- length(DataYears)
nLeapYrs<- length(intersect(LeapYears, DataYears))
numdailyobs <- 48

source("code/cleaning_functions.R")

# Load "all data file"
file <- paste0(data.dir, "/snoqualmie.wt.allyears_", thisyear, ".csv")
wtdat <- read.csv(file, header = T, stringsAsFactors = F)
date.format <- detect.date.format(wtdat$Date[1])
wtdat$Date <- as.Date(wtdat$Date, format = date.format)
sites <- colnames(wtdat)[3:ncol(wtdat)]
summary(wtdat)

# Remove sites that are no longer operational
decommissioned.sites <- c("B1", "E1", "I1", "L1", "L2", "M1", "R2", "R4", "MS7", "T2", "T4", "F1.R", "MS10.R", "MF4")
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

wtdat$Time2 <- time.stamp(wtdat$Time, seconds = F)
range(wtdat$Date[!is.na(wtdat[,3])])

wtdat$Date2 <- format(wtdat$Date, "%m/%d/%Y")
wtdat$DateTime <- paste0(wtdat$Date2, " ", wtdat$Time2)

# Output individual site files in King County preferred format and for specified period
for(site in sites){
  td <- wtdat[wtdat$Date >= as.Date("2019-09-01"),c("DateTime", site)]
  colnames(td) <- c("Timestamp (GMT - 07:00)", "Temp_C")
  td$Temp_C <- round(td$Temp_C, 1)
  write.csv(td, paste0(data.dir, "/Data2Share/NOAA_", site,".csv"), row.names = F, na = "")
}  

# # Output individual site files ALTERNATE FORMAT
# for(site in sites){
#   idx <- which(colnames(wtdat) == site)
#   td <- wtdat[,c("WaterYear", "Date", "Time", "Time2", site)]
#   td <- td[order(td$Date, td$Time),]
#   td <- td[,-3]
#   colnames(td)[3] <- "Time"
#   colnames(td)[4] <- "Temp_C"
#   write.csv(td, paste0(data.dir, "/Data2Share/NOAA_", site,".csv"), row.names = F, na = "")
# }  
summary(td[td$WaterYear == thisyear,]) #to examine

# end of script


# # Worth keeping Griffin replacement? It's a duplicate
# plot(wtdat$F1, wtdat$F1.r)
# summary(wtdat$F1[!is.na(wtdat$F1.r)])
# summary(wtdat$F1.r[!is.na(wtdat$F1)])
# foo = wtdat[!is.na(wtdat$F1.r), c("F1", "F1.r")]
# summary(foo[,1] - foo[,2])
# # all < 0.5 degrees; median 0.024
# # Nope, delete it.
# 
# # Worth keeping Mainstem Lowest replacement? It's a duplicate
# plot(wtdat$MS10, wtdat$MS10.r)
# summary(wtdat$MS10[!is.na(wtdat$MS10.r)])
# summary(wtdat$MS10.r[!is.na(wtdat$MS10.r)])
# foo = wtdat[!is.na(wtdat$MS10.r), c("MS10", "MS10.r")]
# summary(foo[,1] - foo[,2])
# # all < 0.72 degrees; median 0.074
# # Nope, delete it.
# 
# # Worth keeping MF below Granite?
# plot(wtdat$MF3, wtdat$MF4)
# summary(wtdat$MF3[!is.na(wtdat$MF4)])
# summary(wtdat$MF4[!is.na(wtdat$MF3)])
# foo = wtdat[!is.na(wtdat$MF4), c("MF3", "MF4")]
# summary(foo[,1] - foo[,2])
# # Keep; it's a separate location with a short time series
# 
# # Worth keeping Tolt Confluence data?
# plot(wtdat$MS8, wtdat$T2)
# plot(wtdat$MS8, wtdat$MS7)
# # Keep; they are separate locations with short time series

# Other sites were decommissioned in September of 2020 due to logistics with keeping that many loggers going

