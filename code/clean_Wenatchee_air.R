# Stitch together complete air temperature records
# Aimee H Fullerton, 1 November 2022

# SETUP ####
# Load functions
numdailyobs <- 24
source("code/cleaning_functions.R")

# Directories
watershed <- "wenatchee"
date.begin <- "-09-01"
date.end <- "-08-31"
data.dir1 <- "/Users/aimeefullerton/OneDrive/Work/Research/ST_Wenatchee"
data.dir2 <-  "NOAA-USFS" #"WDFW"
data.dir <- paste0(data.dir1, "/", data.dir2)
data.type <- "air"

# CHOOSE An OPTION: 
# 1. Setup for back-filling the beginning of the time series back to Sep 1 if loggers were downloaded after Sep started
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

# 2. Setup for back-filling the end of the time series through Aug 31 if loggers were downloaded before the end of Aug
first.year <- 2020
raw.data.folder <- paste0("Data_Raw_Sep", (first.year + 2), "/", data.type)
old.data.folder <- paste0("Data_Cleaned_", (first.year + 1), "/", data.type)
cleaned.data.folder <- paste0("Data_Cleaned_", (first.year + 1), "/", data.type)
oldfiles <- dir(paste0(data.dir, "/", old.data.folder))
thefiles <- dir(paste0(data.dir, "/", raw.data.folder))
oldfiles <- toupper(oldfiles); thefiles <- toupper(thefiles)


# Stitch together data from different years; process sites individually ####
thefiles # Look at 'thefiles' and pick sites one at a time manually, then run the 'while' loop (may need to skip doubles)
i <- 1
while(!is.null(i)){
  data.file <- thefiles[i]
  site<- gsub("AIR_", "", data.file)
  site <- gsub("_.*","", site)
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
  ans <- readline("Are there multiple files for the current year that need to be strung together? (Choose y or n) ")
  if(ans == "y" & length(new.list) > 1){
    oldfile2keep <- dat[!is.na(dat$Temp),]
    cat("This year's file(s): ", new.loggers, "\n")
    newfile2keep <- choose.file("current.year")
    # Stitch together data from above with data from the secondary logger in the current year
    if(!is.na(newfile2keep)[1]) dat <- get.complete.year(oldfile2keep, newfile2keep, first.year, numdailyobs, date.begin, date.end)
    if(is.na(newfile2keep)[1]) dat <- clip.single.file(oldfile2keep, first.year, numdailyobs, date.begin, date.end)
  }
  
  # Save
  write.csv(dat, paste0(data.dir, "/", cleaned.data.folder, "/", site, ".csv"), row.names = F)
  rm(list = old.list); rm(list = new.list); rm(td, dat, newfile2keep, oldfile2keep)
  i <- NULL
  cat(paste0("All done with ", site, "!"), "\n")
}

# Create a matrix of this year's data and plot each site's time series individually ####
create.matrix(type = "at", data.dir, cleaned.data.folder, watershed, first.year, date.begin, date.end, numdailyobs, ylm = c(-10, 35))

# Merge with all other years ####
update.allyears(type = "at", data.dir, watershed, first.year, ylm = c(-10, 35))


# NOTES
# 1. After backfilling, will need to re-run creation of single-year matrix 
# 2. Next, need to re-merge with allyears matrix
# This will require updating the last year's allyears matrix first.
