# Clean temperature data files to remove erroneous readings (e.g. from air, sediment, or ice)
# Aimee H Fullerton, 24 September 2021

# SETUP ####
# Load functions
numdailyobs <- 48
source("code/cleaning_functions.R")

# Directories
watershed <- "snoqualmie"
first.year <- 2020
date.begin <- "-09-01"
date.end <- "-08-31"
data.dir <- "/Users/aimee_fullerton/OneDrive/Work/Research/StreamTemperature/Hobonet/Data"
data.type <- "water"
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


# Back-fill data into last year's records for sites that were downloaded before 1 September ####
  # Enter a list of the sites that need to be updated
thesites <- c("C1", "C2", "MF1", "MF2", "MF3", "D1", "F1", "MS10", "T1", "R5a", "R5b", "R5c", "SF1", "SF2", "SF3", "K1", "L3", "R3", "R1", "MF5", "MF6", "MS4", "MS5", "MS6", "MS8", "MS9", "Y1", "NF4")
thesites <- c("X1", "S1", "R4", "R2", "NF2", "NF1", "M1", "L2", "L1", "E1", "B1")
i <- 1
while(!is.null(i)){
  site <- thesites[i]
  cat(site, "\n")
  
  # Load cleaned data for site from last year
  if(paste0(site, ".csv") %in% dir(paste0(data.dir, "/Data_Cleaned_", first.year, "/", data.type, "/"))) {
    file2update <- read.csv(paste0(data.dir, "/Data_Cleaned_", first.year, "/", data.type, "/", site, ".csv"), header = T, stringsAsFactors = F)
    file2update$Date <- as.Date(file2update$Date)

    # Fix transposed times
    if(file2update$Time[1]%%1 == 0.5){
      file2update$NewTime <- file2update$Time
      file2update$NewTime[file2update$Time%%1 == 0]<- file2update$NewTime[(file2update$Time)%%1 == 0] + 0.5
      file2update$NewTime[file2update$Time%%1 == 0.5]<- floor(file2update$NewTime)[(file2update$Time)%%1 == 0.5]
      file2update <- file2update[,c(1,4,3)]; colnames(file2update) <- c("Date", "Time", "Temp")
      file2update <- file2update[order(file2update$Date, file2update$Time),]
    }
    
    # Add "DateTime" column if needed
    if(!"DateTime" %in% colnames(file2update)){
      foo <- paste0(file2update$Date, " ", sprintf("%02d", floor(file2update$Time)), ":00")
      if(numdailyobs == 48) foo[seq(2, length(foo), 2)] <- gsub(":00", ":30", foo[seq(2, length(foo), 2)])
      file2update$DateTime <- as.POSIXlt( foo, format = "%Y-%m-%d %H:%M")
      rm(foo)
      colnames(file2update)[3] <- "Temp"
      file2update <- file2update[, c("DateTime", "Date", "Time", "Temp")]
      plot(file2update$DateTime[1:100], file2update$Temp[1:100])
      
    } else {
      date.format <- detect.date.format(file2update$DateTime[1])
      file2update$DateTime <- as.POSIXlt(file2update$DateTime, origin = "1970-01-01", format = paste(date.format, "%H:%M"))
    }
    
    site <- toupper(site)
    
    # Load raw data for site from this year
    new.loggers <- thefiles[grep(site, thefiles)]
    new.list <- NULL
    
    if(length(new.loggers) > 0){
      # Read in and prepare data from current year at this site
      for(j in 1:length(new.loggers)){
        td <- prepare.file(data.file = new.loggers[j], directory = paste0(data.dir, "/", raw.data.folder), numdailyobs = numdailyobs)
        new.list <- c(new.list, paste0(site, ".", j))
        assign(paste0(site, ".", j), td)
      }
        cat("This year's file(s): ", new.loggers, "\n")
        file2copyfrom <- choose.file("current.year")
        
      # Stitch together raw data from previous September with raw data from the current year
      if(!is.na(file2update)[1] & !is.na(file2copyfrom)[1]) dat <- backfill.previous.fall(file2update, file2copyfrom, theyear = (first.year - 1), numdailyobs, date.begin, date.end)
      
    } else {
      cat("Data for that site do not exist for the current year. Check records and/or file names.", "\n")
    }
    
  } else{
    cat("Data for that site do not exist for the previous year. Check records and/or file names.", "\n")
  }
  i <- NULL
  
  if(!exists("dat")) dat <- file2update
  
  # Proceed with cleaning the new data added if necessary (unlikely since logger placed less than a month prior)
  plot.logger(dat)
  thedirectory <- paste0(data.dir, "/", cleaned.data.folder) #for choosing nearby sites
  dat <- clean.middle(dat, thedirectory)
  
  # Fill time series
  dat <- fill.time.series(file2update, (first.year - 1), date.begin, date.end, numdailyobs)
  
  # Finalize, review, and save
  plot(dat$Date, dat$Temp, type = 'l', ylab = "Temperature (C)", xlab = "Date")
  summary(dat[!is.na(dat$Temp),])
  
  write.csv(dat, paste0(data.dir, "/Data_Cleaned_", first.year, "/", data.type, "/", site, ".csv"), row.names = F)
  rm(list = new.list); rm(td, dat, file2update, file2copyfrom)
  i <- NULL
  cat(paste0("All done with ", site, "!"), "\n")
  
}

# NOTES
# 1. After backfilling, will need to re-run creation of single-year matrix 
# 2. Next, need to re-merge with allyears matrix
# This will require updating the last year's allyears matrix first.


