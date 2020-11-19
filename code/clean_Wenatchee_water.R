# Clean temperature data files to remove erroneous readings (e.g. from air, sediment, or ice)
# Aimee H Fullerton, 16 November 2020

# Load functions
source("code/cleaning_functions.R")

# Directories
watershed <- "wenatchee"
first.year <- 2019
date.begin <- "-09-01"
date.end <- "-08-31"
numdailyobs <- 24
data.dir1 <- "/Users/aimee_fullerton/OneDrive/Work/Research/StreamTemperature/Wenatchee"
data.dir2 <-  "NOAA-USFS" #"WDFW"
data.dir <- paste0(data.dir1, "/", data.dir2)
raw.data.folder <- paste0("Data_Raw_Sep", (first.year + 1), "/water")
old.data.folder <- paste0("Data_Raw_Sep", first.year, "/water")
cleaned.data.folder <- paste0("Data_Cleaned_", (first.year + 1), "/water")


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



# Clean the data ####
(thefiles <- dir(paste0(data.dir, "/", raw.data.folder)))
(oldfiles <- dir(paste0(data.dir, "/", old.data.folder)))

i <- 1 # Look at 'thefiles' and pick sites one by one manually

while(!is.null(i)){
  data.file <- thefiles[i]
  site <- gsub("_.*","", data.file)
  cat(site, "\n")
  
  # Prepare possible files to use
  old.loggers <- oldfiles[grep(paste0(site, "_"), oldfiles)]
  new.loggers <- thefiles[grep(paste0(site, "_"), thefiles)]
  old.loggers <- old.loggers[which(gsub("_", "", substr(old.loggers,1,3)) == site)]
  new.loggers <- new.loggers[which(gsub("_", "", substr(new.loggers,1,3)) == site)]
  cat("Last year's file(s): ", old.loggers, "\n")
  cat("This year's file(s): ", new.loggers, "\n")
  old.list <- new.list <- NULL
  
  # Read in and prepare data from previous year at this site
  if(length(old.loggers) > 0){
    for(j in 1:length(old.loggers)){
      td <- prepare.file(data.file = old.loggers[j], directory = paste0(data.dir, "/", old.data.folder))
      old.list <- c(old.list, paste0(site, ".", j))
      assign(paste0(site, ".", j), td)
    }
  }
  
  # Read in and prepare data from current year at this site
  for(j in 1:length(new.loggers)){
    td <- prepare.file(data.file = new.loggers[j], directory = paste0(data.dir, "/", raw.data.folder))
    new.list <- c(new.list, paste0(site, ".", j + length(old.loggers)))
    assign(paste0(site, ".", j + length(old.loggers)), td)
  }
  
  # Choose files to stitch together based on dates printed from above steps:
  if(length(old.loggers) > 0){
    oldfile2keep <- choose.file("past.year")
  } else {
    print("This site did not have a file last year.")
    oldfile2keep <- NA
  }
  if(length(new.loggers) > 0){
    newfile2keep <- choose.file("current.year")
  } else{
    print("This site did not have a file this year.")
    newfile2keep <- NA
  }
  
  # Stitch together raw data from previous September with raw data from the current year
  if(!is.na(newfile2keep)[1] & !is.na(oldfile2keep)) dat <- get.complete.year(oldfile2keep, newfile2keep, first.year, numdailyobs, date.begin, date.end)
  # or clip the old or new year to boundary dates if only one year has data
  if(is.na(newfile2keep)[1] & !is.na(oldfile2keep)) dat <- clip.single.file(oldfile2keep, first.year, numdailyobs, date.begin, date.end)
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
  check.hobo(dat) 
  plot.hobo(dat)
  
  # Proceed with cleaning the data
  # note: deployment and recovery dates should already be dealt with from above stitching steps
  dat <- clean.deployment(dat) #clip off data before deployment date
  plot.hobo(dat)
  dat <- clean.recovery(dat) #clip off data after recovery date
  plot.hobo(dat)
  thedirectory <- paste0(data.dir, "/", cleaned.data.folder) #for choosing nearby sites
  dat <- clean.middle(dat, thedirectory)
  
  # Finalize, review, and save
  plot(dat$Date, dat$Temp, type = 'l', ylab = "Temperature (C)", xlab = "Date")
  summary(dat[!is.na(dat$Temp),])
  
  write.csv(dat, paste0(data.dir, "/", cleaned.data.folder, "/", data.file), row.names = F)
}

# Organize data as matrix with Date and Site columns; NAs where no data ####
# helpful for use in SSN models
(thefiles <- dir(paste0(data.dir, "/", cleaned.data.folder)))

dates <- seq(from = as.Date(paste0(first.year, date.begin)), to = as.Date(paste0(first.year + 1, date.end)), by = 1)
new.df <- data.frame(matrix(NA, nrow = length(dates) * numdailyobs, ncol = 2))
colnames(new.df) <- c("Date", "Time")
new.df$Date <- rep(dates, numdailyobs)
new.df <- new.df[order(new.df$Date),]
if(numdailyobs == 48){
  new.df$Time <- rep(seq(0, 23.5, 0.5), length(dates))
} else{
  new.df$Time <- rep(seq(0, 23, 1), length(dates))
}
foo <- paste0(new.df$Date, " ", sprintf("%02d", floor(new.df$Time)), ":00")
if(numdailyobs == 48) foo[seq(2, length(foo), 2)] <- gsub(":00", ":30", foo[seq(2, length(foo), 2)])
new.df$DateTime <- as.POSIXlt( foo, format = "%Y-%m-%d %H:%M")
row.names(new.df) <- NULL

for(i in 1:length(thefiles)){
  site.name <- gsub("_.*","", thefiles[i])
  td <- read.csv(paste0(data.dir, "/", cleaned.data.folder, "/", thefiles[i]), header = T, stringsAsFactors = F)
  # Add "DateTime" column if needed
  if(!"DateTime" %in% colnames(td)){
    foo <- paste0(td$Date, " ", sprintf("%02d", floor(td$Time)), ":00")
    if(numdailyobs == 48) foo[seq(2, length(foo), 2)] <- gsub(":00", ":30", foo[seq(2, length(foo), 2)])
    td$DateTime <- as.POSIXlt( foo, format = "%Y-%m-%d %H:%M")
    rm(foo)
    colnames(td)[3] <- "Temp"
  }
  new.df <- merge(new.df, td[, c("DateTime", "Temp")], by = c("DateTime"), all.x = T)
  colnames(new.df)[i + 3] <- site.name
}
write.csv(new.df, paste0(data.dir, "/Data_Cleaned_", (first.year + 1), "/", watershed, ".wt.", (first.year + 1), ".csv"), row.names = F)

# plot in individual panels to check
png(paste0(data.dir, "/Data_Cleaned_", (first.year + 1), "/", watershed, ".wt.", (first.year + 1), ".png"), width = 16, height = 10, units = "in", res = 300)
par(mfrow = c(6,8), las = 1, cex = 0.5)
for(i in 4:(ncol(new.df))){
  plot(new.df$Date, new.df[,i], type = 'l', ylim = c(-5, 25), main = colnames(new.df)[i], xlab = "", ylab = "")
}
dev.off()  



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
yy <- first.year + 1

wt.all <- read.csv(paste0(data.dir, "/", watershed, ".wt.allyears.csv"), header = T)
sites <- colnames(wt.all[3:ncol(wt.all)])
wt.all$Date <- as.Date(wt.all$Date)


wt.yy <- read.csv(paste0(data.dir, "/Data_Cleaned_", yy, "/", watershed, ".wt.", yy, ".csv"), header = T)
wt.yy$Date <- as.Date(wt.yy$Date)

# Merge this year with previous years
thesites <- sort(intersect(sites, colnames(wt.yy)))
wt.all.merged <- matrix(NA, nrow = nrow(wt.all) + nrow(wt.yy), ncol = (length(sites) + 2))
wt.all.merged <- as.data.frame(wt.all.merged)
colnames(wt.all.merged) <- c("Date", "Time", sites)
wt.all.merged$Date <- as.Date("01/01/01", "%m/%d/%y")

wt.all.merged[1:nrow(wt.all),] <- wt.all
idx <- ((nrow(wt.all) + 1) : nrow(wt.all.merged))
wt.all.merged$Date[idx] <- wt.yy$Date
wt.all.merged$Time[idx] <- wt.yy$Time

for(s in 1:length(thesites)){
  site <- thesites[s]
  wt.all.merged[idx, site] <- wt.yy[,site]
}
summary(wt.all.merged)
plot(wt.all.merged$Date, wt.all.merged$D1, type = 'l')

write.csv(wt.all.merged, paste0(data.dir, "/", watershed, ".wt.allyears.csv"), row.names = F)

# plot in individual panels to check
png(paste0(data.dir, "/", watershed, ".wt.allyears.png"), width = 16, height = 10, units = "in", res = 300)
par(mfrow = c(6,8), las = 1, cex = 0.5)

for(i in 3:(ncol(wt.all.merged))){
  plot(wt.all.merged$Date, wt.all.merged[,i], type = 'l', ylim = c(-5, 25), main = colnames(wt.all.merged)[i], xlab = "", ylab = "")
}
dev.off()  



## OLDER VERSIONS: #####
# Clean the data ####
(thefiles <- dir(paste0(data.dir, "/", raw.data.folder)))
(oldfiles <- dir(paste0(data.dir, "/", old.data.folder)))

i <- 1 # Look at 'thefiles' and pick sites one by one manually

data.file <- thefiles[i]
site <- gsub("_.*","", data.file)
cat(site, "\n")

# Prepare possible files to use
(old.loggers <- oldfiles[grep(paste0(site, "_"), oldfiles)])
(new.loggers <- thefiles[grep(paste0(site, "_"), thefiles)])
old.list <- new.list <- NULL

for(j in 1:length(old.loggers)){
  data.file <- old.loggers[j]
  
  td <- read.csv(paste0(data.dir, "/", old.data.folder, "/", data.file), skip = 1, header = T, stringsAsFactors = F)[,2:3]
  colnames(td) = c("DateTime","Temp")
  a <- substr(as.POSIXlt(td$DateTime[1], origin = "1970-01-01", format = "%m/%d/%Y"), 1, 2)
  if(a < 19) date.format <- "%m/%d/%y" else date.format <- "%m/%d/%Y"
  td$Date <- as.POSIXlt(td$DateTime, origin = "1970-01-01", format = date.format)
  td$DateTime <- as.POSIXlt(td$DateTime, origin = "1970-01-01", format = paste(date.format, "%I:%M:%S %p"))
  td$Time <- td$DateTime$hour
  #evens <- seq(2, length(td$Time),2)
  #td$Time[evens] <- td$Time[evens] + 0.5
  td <- td[!is.na(td$DateTime),]
  td <- td[order(td$Date, td$Time),]
  td$Date <- as.Date(td$Date)
  td <- td[,c(3,4,2)]
  print(paste0(site, ".", j, ": ", range(td$Date)))
  old.list <- c(old.list, paste0(site, ".", j))
  assign(paste0(site, ".", j), td)
}

for(j in 1:length(new.loggers)){
  data.file <- new.loggers[j]
  
  td <- read.csv(paste0(data.dir, "/", raw.data.folder, "/", data.file), skip = 1, header = T, stringsAsFactors = F)[,2:3]
  colnames(td) = c("DateTime","Temp")
  a <- substr(as.POSIXlt(td$DateTime[1], origin = "1970-01-01", format = "%m/%d/%Y"), 1, 2)
  if(a < 19) date.format <- "%m/%d/%y" else date.format <- "%m/%d/%Y"
  td$Date <- as.POSIXlt(td$DateTime, origin = "1970-01-01", format = date.format)
  td$DateTime <- as.POSIXlt(td$DateTime, origin = "1970-01-01", format = paste(date.format, "%I:%M:%S %p"))
  td$Time <- td$DateTime$hour
  #evens <- seq(2, length(td$Time),2)
  #td$Time[evens] <- td$Time[evens] + 0.5
  td <- td[!is.na(td$DateTime),]
  td <- td[order(td$Date, td$Time),]
  td$Date <- as.Date(td$Date)
  td <- td[,c(3,4,2)]
  print(paste0(site, ".", j + length(old.loggers), ": ", range(td$Date)))
  new.list <- c(new.list, paste0(site, ".", j + length(old.loggers)))
  assign(paste0(site, ".", j + length(old.loggers)), td)
  #if in Farenheight:
  #td$Temp <- (td$Temp - 32) * (5/9)
  
}

# CHOOSE MANUALLY based on dates printed from above steps:
if(length(old.list) > 1){
  oldfile2keep <- readline("Which file should be used for the end of last year? ") #e.g., C1.1
} else {
  oldfile2keep <- old.list[1]
}
oldfile2keep <- get(oldfile2keep)
if(length(new.list) > 1){
  newfile2keep <- readline("Which file should be used for this year? ") #e.g., C1.2
} else {
  newfile2keep <- new.list[1]
}
newfile2keep <- get(newfile2keep)
rm(list = old.list); rm(list = new.list); rm(td)

# Stitch together raw data from previous September, lining up on this year's deployment date
dat <- get.complete.year(oldfile2keep, newfile2keep, first.year)

# Check for flags
check.hobo(dat) 
summary(dat[!is.na(dat$Temp),])
plot.hobo(dat)

# Proceed with cleaning the data
# note: deployment and recovery dates should already be dealt with
dat <- clean.deployment(dat) #clip off data before deployment date
plot.hobo(dat)
dat <- clean.recovery(dat) #clip off data after recovery date
plot.hobo(dat)
thedirectory <- dir(paste0(data.dir, "/", cleaned.data.folder)) #for choosing nearby sites
dat <- clean.middle(dat, thedirectory)

# Finalize, review, and save
colnames(dat)[3] <- "WT"
plot(dat$Date, dat$WT, type = 'l', ylab = "Temperature (C)", xlab = "Date")
summary(dat[!is.na(dat$WT),])

write.csv(dat, paste0(data.dir, "/", cleaned.data.folder, "/", data.file), row.names = F)



# Organize data as matrix with Date and Site columns; NAs where no data ####
# helpful for use in SSN models
(thefiles <- dir(paste0(data.dir, "/", cleaned.data.folder)))

yy <- first.year + 1
dates <- rep(seq(as.Date(paste0(yy - 1, "-09-01")),as.Date(paste0(yy, "-08-31")), by = 1), 24)
dates <- dates[order(dates)]
times <- rep(seq(1, 24, 1), 366) #366 if leap year
new.df <- cbind.data.frame("Date" = dates, "Time" = times)

for(i in 1:length(thefiles)){
  site.name <- gsub(".csv","",thefiles[i])
  td <- read.csv(paste0(data.dir, "/", cleaned.data.folder, "/", thefiles[i]), header = T, stringsAsFactors = F)
  new.df <- merge(new.df, td, by.x = c("Date", "Time"), by.y = c("Date", "Time"), all.x = T)
  colnames(new.df)[i + 2] <- site.name
}
new.df <- new.df[order(new.df$Date,new.df$Time),]
new.df <- unique(new.df)
rownames(new.df) <- NULL
# explore & remove any duplicates
#new.df[duplicated(new.df[,1:2]),]
idx <- which(duplicated(new.df[,1:2]))
if(length(idx) > 0) new.df <- new.df[-idx,]
write.csv(new.df, paste0(data.dir, "/Data_Cleaned_", yy, "/", watershed, ".wt.", yy, ".csv"), row.names = F)

# plot in individual panels to check
png(paste0(data.dir, "/Data_Cleaned_", yy, "/", watershed, ".wt.", yy, ".png"), width = 16, height = 8, units = "in", res = 300)
par(mfrow = c(5,8), las = 1, cex = 0.5)
for(i in 3:(ncol(new.df) - 2)){
  plot(new.df$Date, new.df[,i], type = 'l', ylim = c(-5, 25), main = colnames(new.df)[i], xlab = "", ylab = "")
}
dev.off()  



# Merge with all other years ####
yy <- first.year + 1
wt.all <- read.csv(paste0(data.dir1, "/NOAA-USFS/", watershed, ".wt.allyears.csv"), header = T)
wt.all$Date <- as.Date(wt.all$Date)
sites <- colnames(wt.all[3:ncol(wt.all)])
sites <- gsub("_.*","", sites)
colnames(wt.all) <- c("Date", "Time", sites)

wt.yy <- read.csv(paste0(data.dir, "/Data_Cleaned_", yy, "/", watershed, ".wt.", yy, ".csv"), header = T)
wt.yy$Date <- as.Date(wt.yy$Date)
for(i in 3:ncol(wt.yy)){
  cn <- colnames(wt.yy)[i]
  colnames(wt.yy)[i] <- gsub("_.*","", cn)
}

# Merge this year with previous years
thesites <- sort(intersect(sites, colnames(wt.yy)))
wt.all.merged <- matrix(NA, nrow = nrow(wt.all) + nrow(wt.yy), ncol = (length(sites) + 2))
wt.all.merged <- as.data.frame(wt.all.merged)
colnames(wt.all.merged) <- c("Date", "Time", sites)
wt.all.merged$Date <- as.Date("01/01/01", "%m/%d/%y")

wt.all.merged[1:nrow(wt.all),] <- wt.all
idx <- ((nrow(wt.all) + 1) : nrow(wt.all.merged))
wt.all.merged$Date[idx] <- wt.yy$Date
wt.all.merged$Time[idx] <- wt.yy$Time

for(s in 1:length(thesites)){
  site <- thesites[s]
  wt.all.merged[idx, site] <- wt.yy[,site]
}
summary(wt.all.merged)
plot(wt.all.merged$Date, wt.all.merged[,3], type = 'l')

write.csv(wt.all.merged, paste0(data.dir1, "/NOAA-USFS/", watershed, ".wt.allyears.csv"), row.names = F)

# plot in individual panels to check
png(paste0(data.dir1, "/NOAA-USFS/", watershed, ".wt.allyears.png"), width = 16, height = 10, units = "in", res = 300)
par(mfrow = c(6,8), las = 1, cex = 0.5)

for(i in 3:(ncol(wt.all.merged))){
  plot(wt.all.merged$Date, wt.all.merged[,i], type = 'l', ylim = c(-5, 25), main = colnames(wt.all.merged)[i], xlab = "", ylab = "")
}
dev.off()  
