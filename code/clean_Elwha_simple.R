

library(tidyverse)
library(readxl)

# Load functions
numdailyobs <- 24
clock_24h <- T
source("code/cleaning_functions.R")

# Directories
first.year <- 2022
date.begin <- "-10-01"
date.end <- "-11-30"
root.dir <- gsub("temperature_cleaning", "", getwd())
data.dir <- paste0(root.dir, "Elwha_ST/data")
raw.data.folder <- paste0(first.year + 1, "/data.raw/March-April2023")
cleaned.data.folder <- paste0(first.year + 1, "/data.cleaned")
if(!dir.exists(paste0(data.dir, "/", cleaned.data.folder))){
  dir.create(paste0(data.dir, "/", cleaned.data.folder), showWarnings = F)
}
thefiles <- dir(paste0(data.dir, "/", raw.data.folder))
#sites.attr <- read.csv(paste0(data.dir, "/elwha.sites.attributed.csv"), header = T)
#xx <- sites.attr$FW_Code; names(xx) <- sites.attr$SiteCode; xx <- xx[xx!=""]


thefiles
i <- 1 # Look at 'thefiles' and pick sites one by one manually

while(!is.null(i)){
  data.file <- thefiles[i]
  site <- gsub("_.*","", data.file)
  cat(site, "\n")
  
  if(length(grep("xls", data.file)) > 0){
    dat <- prepare.xls.file(data.file = thefiles[i], directory = paste0(data.dir, "/", raw.data.folder))
  } else {
    dat <- prepare.file(data.file = thefiles[i], directory = paste0(data.dir, "/", raw.data.folder))
  }
  # note - may get warnings for xls format about 'Logged': this is OKAY and not a concern.

  # Proceed with cleaning the data
  plot.logger(dat)
  dat <- clean.deployment(dat) #clip off data before deployment date
  plot.logger(dat)
  dat <- clean.recovery(dat) #clip off data after recovery date
  plot.logger(dat)
  thedirectory <- paste0(data.dir, "/", cleaned.data.folder) #for choosing nearby sites
  dat <- clean.middle(dat, thedirectory)
  
  # Finalize, review, and save
  plot(dat$DateTime, dat$Temp, type = 'l', ylab = "Temperature (C)", xlab = "Date")
  summary(dat[!is.na(dat$Temp),])
  
  # Ensure the whole time series is filled out (even if some temperatures are NAs)
  #dat <- fill.time.series(dat, first.year, date.begin, date.end, numdailyobs)
  
  # Round up to nearest hour if needed in case logger wasn't deployed on the hour!
  time <- strsplit(as.character(dat$DateTime[1]), " ")[[1]][2]; minut <- as.numeric(strsplit(time, ":")[[1]][2])
  if(minut > 0){
    date_list <- sapply(dat$DateTime, function(x) round.timestamp(x))
    #date_list <- sapply(dat$DateTime, function(x) round.POSIXt(x, "hours")) #same result, but much slower
    dat$DateTime <- do.call("c", date_list)
    dat$Date <- as.Date(dat$DateTime)
    dat$Time <- dat$DateTime$hour
  }
  
  write.csv(dat, paste0(data.dir, "/", cleaned.data.folder, "/", site, ".csv"), row.names = F)
  cat(paste0("All done with ", site, "!"), "\n")
  i <- NULL
}

summary(dat[!is.na(dat$Temp),])


# Read back in and check that they look okay
thefiles <- dir(paste0(data.dir, "/", cleaned.data.folder))

png(paste0(data.dir, "/", (first.year + 1), "/check.plots.png"), width = 14, height = 12, units = "in", res = 300)
par(mfrow = c(5,4), las = 1)
for(f in 1:length(thefiles)){
  dat <- read.csv(paste0(data.dir, "/", cleaned.data.folder, "/", thefiles[f]))
  date.format <- detect.date.format(dat$DateTime[1])
  dat$DateTime <- as.POSIXlt(dat$DateTime, format = date.format)
  plot(dat$DateTime, dat$Temp, type = 'l', ylab = "Temperature (C)", xlab = "Date", main = gsub(".csv","",thefiles[f]))
}
dev.off()

# Get into single file
final.dat <- NA
for(f in 1:length(thefiles)){
  dat <- read.csv(paste0(data.dir, "/", cleaned.data.folder, "/", thefiles[f]))
  date.format <- detect.date.format(dat$DateTime[1])
  dat$DateTime <- as.POSIXlt(dat$DateTime, format = date.format)
  dat$Site <- gsub(".csv","",thefiles[f])
  final.dat <- rbind(final.dat, dat)
}
if(any(is.na(final.dat$DateTime))) final.dat <- final.dat[!is.na(final.dat$DateTime),]
write.csv(final.dat, paste0(data.dir, "/", (first.year + 1), "/st.data.csv"), row.names = F)
