

library(tidyverse)
library(readxl)

# Load functions
numdailyobs <- 24
source("code/cleaning_functions.R")

# Directories
first.year <- 2021
date.begin <- "-10-01"
date.end <- "-11-30"
data.dir <- "/Users/aimeefullerton/GitHub/Elwha_ST/data"
raw.data.folder <- paste0(first.year + 1, "/data.raw")
cleaned.data.folder <- paste0(first.year + 1, "/data.cleaned")
if(!dir.exists(paste0(data.dir, "/", cleaned.data.folder))){
  dir.create(paste0(data.dir, "/Data_Cleaned_", (first.year + 1)), showWarnings = F)
  dir.create(paste0(data.dir, "/", cleaned.data.folder), showWarnings = F)
}
thefiles <- dir(paste0(data.dir, "/", raw.data.folder))
sites.attr <- read.csv(paste0(data.dir, "/elwha.sites.attributed.csv"), header = T)
xx <- sites.attr$FW_Code; names(xx) <- sites.attr$SiteCode; xx <- xx[xx!=""]
thefiles <- dir(paste0(data.dir, "/", raw.data.folder))


thefiles
i <- 8 # Look at 'thefiles' and pick sites one by one manually

while(!is.null(i)){
  data.file <- thefiles[i]
  site <- gsub("_.*","", data.file)
  cat(site, "\n")
  
  if(length(grep("xls", data.file)) > 0){
    dat <- prepare.xls.file(data.file = thefiles[i], directory = paste0(data.dir, "/", raw.data.folder))
  } else {
    dat <- prepare.file(data.file = thefiles[i], directory = paste0(data.dir, "/", raw.data.folder))
  }

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
