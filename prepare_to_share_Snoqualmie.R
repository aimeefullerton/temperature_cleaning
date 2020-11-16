# Get Snoqualmie watershed water temperature data ready to share on King County's website
# Aimee H Fullerton, 16 November 2020

data.dir <- "/Users/aimee_fullerton/OneDrive/Work/Research/StreamTemperature/Hobonet/Data"
DataYears<- 2012:2020
LeapYears<- seq(2008, 2040, 4)
nYears<- length(DataYears)
nLeapYrs<- length(intersect(LeapYears, DataYears))

# Load "all data file"
file <- paste0(data.dir, "/snoqualmie.wt.allyears.csv")
wtdat <- read.csv(file, header = T, stringsAsFactors = F)
wtdat$Date <- as.Date(wtdat$Date)
#sites <- c("C1", "E1", "D1", "G1", "F1", "F1.r", "I1", "L1", "L3", "B1", "L2", "X1", "C2", "MF2", "MF1", "MF4", "MF3", "MF5", "MF6",
#           "MS5", "MS6", "MS9", "MS10", "MS10.r", "MS4", "NF3", "NF1", "NF2", "NF4", "R5b", "R5a", "R1", "R4", "R5c", "R2", "R3", 
#           "SF3", "SF1", "SF2", "M1", "S1", "Y1", "K1", "MS8", "T2", "MS7", "T4", "T1")
sites <- colnames(wtdat)[3:ncol(wtdat)]
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
  theminutes <- rep(c("30", "00"), length.out = length(x))
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
  write.csv(td, paste0(data.dir, "/Data2Share/NOAA_", site,".csv"), row.names = F, na = "")
}  

summary(td[td$WaterYear == 2020,]) #to examine

# end of script
