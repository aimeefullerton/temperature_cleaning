# Clean temperature data files to remove erroneous readings (e.g. from air, sediment, or ice)
# Aimee H Fullerton, 23 September 2021

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
raw.data.folder <- paste0("Data_Raw_Sep", (first.year + 1), "/water")
old.data.folder <- paste0("Data_Raw_Sep", first.year, "/water")
cleaned.data.folder <- paste0("Data_Cleaned_", (first.year + 1), "/water")
if(!dir.exists(paste0(data.dir, "/", cleaned.data.folder))){
  dir.create(paste0(data.dir, "/Data_Cleaned_", (first.year + 1)), showWarnings = F)
  dir.create(paste0(data.dir, "/", cleaned.data.folder), showWarnings = F)
}
oldfiles <- dir(paste0(data.dir, "/", old.data.folder))
thefiles <- dir(paste0(data.dir, "/", raw.data.folder))
oldfiles <- toupper(oldfiles); thefiles <- toupper(thefiles)


# Prepare WDFW data before cleaning ####
if(data.subdir == "WDFW"){
  # Subset PIT array data to hourly
  (thefiles = dir(paste0(data.dir, "/", raw.data.folder)))
  for(xx in c(6,7,8,10,13,14,16,19,23,29)){ # NEED TO PICK WHICH FILES BEFORE RUNNING!!!
    (data.file = thefiles[xx])
    
    td = read.csv(paste0(data.dir, "/", raw.data.folder, "/", data.file), header = T, stringsAsFactors = F)
    colnames(td) = c("Date","Time","WT")
    td = unique(td)
    
    #head(td)
    #str(td)
    td$date<- strptime(paste(td$Date, td$Time), format = "%m/%d/%Y %I:%M:%S %p")
    #summary(td)
    td<- td[!is.na(td$date),] #need to remove the NA dates or aggregate won't work
    td$dateRounded<- strptime(format(td$date, "%m/%d/%Y %H"), format = "%m/%d/%Y %H")
    td$drn<- as.numeric(td$dateRounded)
    #td$diff< -as.numeric(td$date - td$dateRounded)
    td<- td[order(td$drn),]
    
    test<- aggregate(x = td$date, by = list(td$drn), FUN = min )
    colnames(test)<- c("drn", "date")
    #head(test)
    test2<- merge(td, test, all.y = TRUE)
    #head(test2, 20)
    
    td3 = test2[,c("date", "WT")]; colnames(td3) = c("Date", "WT")
    write.csv(td3, paste0(data.dir, "/", raw.data.folder, "/", gsub(".csv","",data.file),".hourly.csv"))
  }
  
  # Fix date format for the above
  (thefiles = dir(paste0(data.dir, "/", raw.data.folder)))
  for(xx in 1:length(thefiles)){
    (data.file = thefiles[xx])
    if(length(grep("hourly", data.file)) > 0){
      td = read.csv(paste0(data.dir, "/", raw.data.folder, "/", data.file), header = T, stringsAsFactors = F)[,2:3]
      td$Date = as.POSIXlt(td$Date, origin = "1970-01-01")
      td$Date = format(td$Date,"%m/%d/%Y %I:%M:%S %p")
      write.csv(td, paste0(data.dir, "/", raw.data.folder, "/", data.file))
    }
  }
  
  # Get Heather Johnson's data in correct format
  (thefiles = dir(paste0(data.dir, "/", raw.data.folder)))
  for(xx in 1:length(thefiles)){
    (data.file = thefiles[xx])
    if(length(grep("-", data.file)) > 0){
      td = read.csv(paste0(data.dir, "/", raw.data.folder, "/", data.file), header = T, stringsAsFactors = F)
      td = cbind.data.frame("Date" = strptime(paste(td$Date, td$Time), format = "%m/%d/%Y %I:%M:%S %p"), "WT" = td$Temp)
      td$Date = format(td$Date,"%m/%d/%Y %I:%M:%S %p")
      write.csv(td, paste0(data.dir, "/", raw.data.folder, "/", data.file))
    }
  }
}



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
  ans <- readline("Are there multiple files for the current year that need to be strung together? (Choose y or n) ")
  if(ans == "y" & length(new.list) > 1){
    oldfile2keep <- dat[!is.na(dat$Temp),]
    cat("This year's file(s): ", new.loggers, "\n")
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
  plot(dat$Date, dat$Temp, type = 'l', ylab = "Temperature (C)", xlab = "Date")
  summary(dat[!is.na(dat$Temp),])
  
  # Ensure the whole time series is filled out (even if some temperatures are NAs)
  dat <- fill.time.series(dat, first.year, date.begin, date.end, numdailyobs)
  
  write.csv(dat, paste0(data.dir, "/", cleaned.data.folder, "/", site, ".csv"), row.names = F)
  rm(list = old.list); rm(list = new.list); rm(td, dat, newfile2keep, oldfile2keep)
  i <- NULL
  cat(paste0("All done with ", site, "!"), "\n")
}

# Create a matrix of this year's data and plot each site's time series individually ####
create.matrix(type = "wt", data.dir, cleaned.data.folder, watershed, first.year, date.begin, date.end, numdailyobs)
# Bring NOAA-USFS and WDFW data together ####

td1 <- read.csv(paste0("NOAA-USFS/Data_Cleaned_", yy, "/", watershed, ".wt.", yy, ".csv"), header = TRUE, stringsAsFactors = FALSE)
td2 <- read.csv(paste0("WDFW/Data_Cleaned_", yy, "/", watershed, ".wt.", yy, ".csv"), header = TRUE, stringsAsFactors = FALSE)
td1 <- td1[order(td1$Date, td1$Time),]; td2 <- td2[order(td2$Date, td2$Time),]
td3 <- cbind.data.frame(td1, td2[,3:ncol(td2)])
sitenames <- colnames(td3)[3:ncol(td3)]
sitenames2 <- substr(sitenames,1,6)
for(s in 1:length(sitenames2)){
  site <- sitenames2[s]
  if(length(grep("_",sitenames2[s])) > 0) sitenames2[s] <- substr(sitenames2[s],1,3)
  if(length(grep(".ho",sitenames2[s])) > 0) sitenames2[s] <- substr(sitenames2[s],1,3)
}
any(duplicated(sitenames2))
sitenames <- toupper(sitenames2)
colnames(td3) <- c("Date","Time", sitenames)


# fill 2-hourly data into hourly (Johnson sites)
td4 <- td3
for(r in 1:nrow(td3)-1){
  td4[r,][is.na(td4[r,])] <- as.numeric(td4[r+1,][is.na(td4[r,])])
}

# plot to check
td4$Date <- as.Date(td4$Date, origin = "1970-01-01")
par(mfrow = c(4, 4), las = 1, mar = c(3, 2, 2, 0.5))
for(p in 3:ncol(td4)){
  plot(td4$Date,td4[,p], pch = 19, cex = 0.2, ylab = "", xlab = "", main = colnames(td4)[p])
}

write.csv(td4, paste0(data.dir1, "/", watershed, ".wt", yy, ".csv"), row.names = FALSE)





# Merge with all other years ####
update.allyears(type = "wt", data.dir, watershed, first.year, ylm = c(-5, 25))
