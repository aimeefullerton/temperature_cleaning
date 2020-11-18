# Get Snoqualmie watershed water temperature data ready to share on King County's website
# Aimee H Fullerton, 18 November 2020

data.dir <- "/Users/aimee_fullerton/OneDrive/Work/Research/StreamTemperature/Hobonet/Data"
DataYears<- 2012:2020
LeapYears<- seq(2008, 2040, 4)
nYears<- length(DataYears)
nLeapYrs<- length(intersect(LeapYears, DataYears))

# Load "all data file"
file <- paste0(data.dir, "/snoqualmie.wt.allyears.csv")
wtdat <- read.csv(file, header = T, stringsAsFactors = F)
wtdat$Date <- as.Date(wtdat$Date)
sites <- colnames(wtdat)[4:ncol(wtdat)]
summary(wtdat)



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

# Remove the duplicate sites
idx <- which(colnames(wtdat) %in% c("F1.r", "MS10.r"))
wtdat <- wtdat[,-idx]
colnames(wtdat)
sites <- sites[!sites %in% c("F1.r", "MS10.r")]

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
  write.csv(td, paste0(data.dir, "/Data2Share/NOAA_", site,".csv"), row.names = F, na = "")
}  

summary(td[td$WaterYear == 2019,]) #to examine

# end of script
