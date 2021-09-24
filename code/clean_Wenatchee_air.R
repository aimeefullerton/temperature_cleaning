# Stitch together complete air temperature records
# Aimee H Fullerton, 24 September 2021

# SETUP ####
# Load functions
numdailyobs <- 24
source("code/cleaning_functions.R")

# Directories
watershed <- "wenatchee"
first.year <- 2020
date.begin <- "-09-01"
date.end <- "-08-31"
data.dir1 <- "/Users/aimee_fullerton/OneDrive/Work/Research/StreamTemperature/Wenatchee"
data.dir2 <-  "NOAA-USFS" #"WDFW"
data.dir <- paste0(data.dir1, "/", data.dir2)
data.type <- "air"
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

# Stitch together sites; process individually ####
thefiles # Look at 'thefiles' and pick sites one manually
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

# Back-fill data into last year's records for sites that were downloaded before 1 September ####
thesites <- c() # Enter a list of the sites that need to be updated
i <- 1
while(!is.null(i)){
  site <- thesites[i]
  cat(site, "\n")
  
  # Load cleaned data for site from last year
  if(paste0(site, ".csv") %in% dir(paste0(data.dir, "/Data_Cleaned_", first.year, "/", data.type, "/"))) {
    file2update <- read.csv(paste0(data.dir, "/Data_Cleaned_", first.year, "/", data.type, "/", site, ".csv"), header = T, stringsAsFactors = F)
    file2update$Date <- as.Date(file2update$Date)
    file2update <- file2update[order(file2update$Date, file2update$Time),]
    # Add "DateTime" column if needed
    if(!"DateTime" %in% colnames(file2update)){
      foo <- paste0(file2update$Date, " ", sprintf("%02d", floor(file2update$Time)), ":00")
      if(numdailyobs == 48) foo[seq(2, length(foo), 2)] <- gsub(":00", ":30", foo[seq(2, length(foo), 2)])
      file2update$DateTime <- as.POSIXlt( foo, format = "%Y-%m-%d %H:%M")
      rm(foo)
      colnames(file2update)[3] <- "Temp"
    } else {
      date.format <- detect.date.format(file2update$DateTime[1])
      file2update$DateTime <- as.POSIXlt(file2update$DateTime, origin = "1970-01-01", format = paste(date.format, "%H:%M"))
    }
    
    site <- toupper(site)
    
    # Load raw data for site from this year
    new.loggers <- thefiles[grep(paste0("_", site), thefiles)]
    new.list <- NULL
    
    if(length(new.loggers) > 0){
      # Read in and prepare data from current year at this site
      for(j in 1:length(new.loggers)){
        td <- prepare.file(data.file = new.loggers[j], directory = paste0(data.dir, "/", raw.data.folder), numdailyobs = numdailyobs)
        new.list <- c(new.list, paste0(site, ".", j + length(old.loggers)))
        assign(paste0(site, ".", j + length(old.loggers)), td)
      }
      if(length(new.loggers) > 0){
        cat("This year's file(s): ", new.loggers, "\n")
        file2copyfrom <- choose.file("current.year")
      } else{
        print("This site did not have a file this year.")
        file2copyfrom <- NA
      }
      
      # Stitch together raw data from previous September with raw data from the current year
      if(!is.na(file2update)[1] & !is.na(file2copyfrom)[1]) dat <- backfill.previous.fall(file2update, file2copyfrom, theyear = (first.year - 1), numdailyobs, date.begin, date.end)
      
      # Save
      write.csv(dat, paste0(data.dir, "/Data_Cleaned_", first.year, "/", data.type, "/", site, ".csv"), row.names = F)
      cat(paste0("All done with ", site, "!"), "\n")
      
      rm(list = new.list); rm(td, dat, file2update, file2copyfrom)
    } else {
      cat("Data for that site do not exist for the current year. Check records and/or file names.", "\n")
    }
    
  } else{
    cat("Data for that site do not exist for the previous year. Check records and/or file names.", "\n")
  }
  i <- NULL
  
}

# NOTES 
# 1. After backfilling, will need to re-run creation of single-year matrix 
# 2. Next, need to re-merge with allyears matrix
# This will require updating the last year's allyears matrix first.