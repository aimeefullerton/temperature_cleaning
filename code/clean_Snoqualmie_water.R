# Clean temperature data files to remove erroneous readings (e.g. from air, sediment, or ice)
# Aimee H Fullerton, 14 December 2022


# NOTE: If loggers were launched at random times and not on the hour, this may come in handy:
#dat$DateTime <- lubridate::round_date(dat$DateTime, unit = "hour", "%H") This date issue is really problematic!

# SETUP ####
# Load functions
numdailyobs <- 24
source("code/cleaning_functions.R")

# Directories
watershed <- "snoqualmie"
date.begin <- "-09-01"
date.end <- "-08-31"
data.dir <- "/Users/aimeefullerton/OneDrive/Work/Research/ST_Snoqualmie/Data"
data.type <- "water"

# CHOOSE An OPTION: 
# 1. Setup for back-filling the beginning of the current year's time series back to Sep 1 if loggers were downloaded after Sep started
first.year <- 2021
raw.data.folder <- paste0("Data_Raw_Sep", (first.year + 1), "/", data.type)
old.data.folder <- paste0("Data_Raw_Sep", first.year, "/", data.type)
cleaned.data.folder <- paste0("Data_Cleaned_", (first.year + 1), "/", data.type)
if(!dir.exists(paste0(data.dir, "/", cleaned.data.folder))){
  dir.create(paste0(data.dir, "/Data_Cleaned_", (first.year + 1)), showWarnings = F)
  dir.create(paste0(data.dir, "/", cleaned.data.folder), showWarnings = F)
}
oldfiles <- dir(paste0(data.dir, "/", old.data.folder))
thefiles <- dir(paste0(data.dir, "/", raw.data.folder))
oldfiles <- toupper(oldfiles); thefiles <- toupper(thefiles)

# 2. Setup for back-filling the end of the previous year's time series through Aug 31 if loggers were downloaded before the end of Aug
# First, check if last year needs any backfilling:
first.year <- 2021
last_year_data <- read.csv(paste0(data.dir, "/Data_Cleaned_", first.year, "/", watershed, ".wt.", first.year, ".csv"))
range(last_year_data$DateTime)
tail(last_year_data)
#If so, proceed:
first.year <- 2020
raw.data.folder <- paste0("Data_Raw_Sep", (first.year + 2), "/", data.type)
old.data.folder <- paste0("Data_Cleaned_", (first.year + 1), "/", data.type)
cleaned.data.folder <- paste0("Data_Cleaned_", (first.year + 1), "/", data.type)
oldfiles <- dir(paste0(data.dir, "/", old.data.folder))
thefiles <- dir(paste0(data.dir, "/", raw.data.folder))
oldfiles <- paste0("AIR_", toupper(oldfiles)); thefiles <- toupper(thefiles)



# Clean the data, processing sites individually ####
thefiles # Look at 'thefiles' and pick sites one manually
i <- 1
while(!is.null(i)){
  data.file <- thefiles[i]
  site <- gsub("_.*","", data.file)
  cat(site, "\n")
  
  # Prepare possible files to use
  old.loggers <- oldfiles[grep(site, oldfiles)]
  new.loggers <- thefiles[grep(site, thefiles)]
  old.list <- new.list <- NULL
  
  # Read in and prepare data from previous year at this site
  if(length(old.loggers) > 0){
    for(j in 1:length(old.loggers)){
      td <- prepare.file(data.file = old.loggers[j], directory = paste0(data.dir, "/", old.data.folder), numdailyobs = numdailyobs)
      old.list <- c(old.list, paste0(site, ".", j))
      assign(paste0(site, ".", j), td)
    }
  }
  
  # Read in and prepare data from current year at this site
  for(j in 1:length(new.loggers)){
    td <- prepare.file(data.file = new.loggers[j], directory = paste0(data.dir, "/", raw.data.folder), numdailyobs = numdailyobs)
    new.list <- c(new.list, paste0(site, ".", j + length(old.loggers)))
    assign(paste0(site, ".", j + length(old.loggers)), td)
  }
  
  # Choose files to stitch together based on dates printed from above steps:
  if(length(old.loggers) > 0){
    cat("Last year's file(s): ", old.loggers, "\n")
    oldfile2keep <- choose.file("past.year")
  } else {
    print("This site did not have a file last year.")
    oldfile2keep <- NA
  }
  if(length(new.loggers) > 0){
    cat("This year's file(s): ", new.loggers, "\n")
    newfile2keep <- choose.file("current.year")
  } else{
    print("This site did not have a file this year.")
    newfile2keep <- NA
  }
  
  # Stitch together raw data from previous September with raw data from the current year
  if(!is.na(newfile2keep)[1] & !is.na(oldfile2keep)[1]) dat <- get.complete.year(oldfile2keep, newfile2keep, first.year, numdailyobs, date.begin, date.end)
  # or clip the old or new year to boundary dates if only one year has data
  if(is.na(newfile2keep)[1] & !is.na(oldfile2keep)[1]) dat <- clip.single.file(oldfile2keep, first.year, numdailyobs, date.begin, date.end)
  if(is.na(oldfile2keep)[1] & !is.na(newfile2keep)[1]) dat <- clip.single.file(newfile2keep, first.year, numdailyobs, date.begin, date.end)
  
  # If there are multiple that need to be strung together:
  if(length(new.list) > 1){
    oldfile2keep <- dat[!is.na(dat$Temp),]
    newfile2keep <- choose.file("current.year")
    # Stitch together data from above with data from the secondary logger in the current year
    if(!is.na(newfile2keep)[1]) dat <- get.complete.year(oldfile2keep, newfile2keep, first.year, numdailyobs, date.begin, date.end)
    if(is.na(newfile2keep)[1]) dat <- clip.single.file(oldfile2keep, first.year, numdailyobs, date.begin, date.end)
  }
  
# Check for flags
check.logger(dat) 
plot.logger(dat)

# Proceed with cleaning the data
# note: deployment and recovery dates should already be dealt with from above stitching steps
dat <- clean.deployment(dat) #clip off data before deployment date
plot.logger(dat)
dat <- clean.recovery(dat) #clip off data after recovery date
plot.logger(dat)
thedirectory <- paste0(data.dir, "/", cleaned.data.folder) #for choosing nearby sites
dat <- clean.middle(dat, thedirectory)

# Finalize, review, and save
plot(dat$DateTime, dat$Temp, type = 'l', ylab = "Temperature (C)", xlab = "Date")
summary(dat[!is.na(dat$Temp),])

write.csv(dat, paste0(data.dir, "/", cleaned.data.folder, "/", site, ".csv"), row.names = F)
rm(list = old.list); rm(list = new.list); rm(td, dat, newfile2keep, oldfile2keep)
i <- NULL
cat(paste0("All done with ", site, "!"), "\n")
}

# Create a matrix of this year's data and plot each site's time series individually ####
create.matrix(type = "wt", data.dir, cleaned.data.folder, watershed, first.year, date.begin, date.end, numdailyobs, ylm = c(-5, 30))

# Merge with all other years ####
update.allyears(type = "wt", data.dir, watershed, first.year, ylm = c(-5, 30))



# NOTES
# 1. After backfilling, will need to re-run creation of single-year matrix 
# 2. Next, need to re-merge with allyears matrix
# This will require updating the last year's allyears matrix first.


