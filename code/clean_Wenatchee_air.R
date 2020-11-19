# Clean temperature data files to remove erroneous readings (e.g. from air, sediment, or ice)
# Aimee H Fullerton, 16 November 2020

# Load functions
numdailyobs <- 24
source("code/cleaning_functions.R")

# Directories
watershed <- "wenatchee"
first.year <- 2019
date.begin <- "-09-01"
date.end <- "-08-31"
data.dir1 <- "/Users/aimee_fullerton/OneDrive/Work/Research/StreamTemperature/Wenatchee"
data.dir2 <-  "NOAA-USFS" #"WDFW"
data.dir <- paste0(data.dir1, "/", data.dir2)
raw.data.folder <- paste0("Data_Raw_Sep", (first.year + 1), "/air")
old.data.folder <- paste0("Data_Raw_Sep", first.year, "/air")
cleaned.data.folder <- paste0("Data_Cleaned_", (first.year + 1), "/air")

# Clean the data ####
(oldfiles <- dir(paste0(data.dir, "/", old.data.folder)))
(thefiles <- dir(paste0(data.dir, "/", raw.data.folder)))

i <- 1 # Look at 'thefiles' and pick sites one by one manually

while(!is.null(i)){
  data.file <- thefiles[i]
  site<- gsub("AIR_", "", data.file)
  site <- gsub("_.*","", site)
  cat(site, "\n")
  
  # Prepare possible files to use
  old.loggers <- oldfiles[grep(paste0("_", site), oldfiles)]
  new.loggers <- thefiles[grep(paste0("_", site), thefiles)]
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
  
  # Proceed with cleaning the data (Not usually necessary for AIR)
  # note: deployment and recovery dates should already be dealt with
  #dat <- clean.deployment(dat) #clip off data before deployment date
  #plot.hobo(dat)
  #dat <- clean.recovery(dat) #clip off data after recovery date
  #plot.hobo(dat)
  #thedirectory <- dir(paste0(data.dir, "/", cleaned.data.folder)) #for choosing nearby sites
  #dat <- clean.middle(dat, thedirectory)
  
  # Save
  write.csv(dat, paste0(data.dir, "/", cleaned.data.folder, "/", site, ".csv"), row.names = F)
  rm(list = old.list); rm(list = new.list); rm(td, dat, newfile2keep, oldfile2keep)
  i <- NULL
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
  site.name <- gsub(".csv","",thefiles[i])
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
write.csv(new.df, paste0(data.dir, "/Data_Cleaned_", (first.year + 1), "/", watershed, ".at.", (first.year + 1), ".csv"), row.names = F)

# plot in individual panels to check
png(paste0(data.dir, "/Data_Cleaned_", (first.year + 1), "/", watershed, ".at.", (first.year + 1), ".png"), width = 16, height = 10, units = "in", res = 300)
par(mfrow = c(6,8), las = 1, cex = 0.5)
for(i in 4:(ncol(new.df))){
  plot(new.df$Date, new.df[,i], type = 'l', ylim = c(-5, 30), main = colnames(new.df)[i], xlab = "", ylab = "")
}
dev.off()  


# Merge with all other years ####
yy <- first.year + 1
at.all <- read.csv(paste0(data.dir, "/", watershed, ".at.allyears.csv"), header = T)
at.all$Date <- as.Date(at.all$Date)
sites <- colnames(at.all[3:ncol(at.all)])
sites <- gsub("_.*","", sites)

at.yy <- read.csv(paste0(data.dir, "/Data_Cleaned_", yy, "/", watershed, ".at.", yy, ".csv"), header = T)
at.yy$Date <- as.Date(at.yy$Date)
for(i in 3:ncol(at.yy)){
  cn <- colnames(at.yy)[i]
  colnames(at.yy)[i] <- gsub("_.*","", cn)
}

# Merge this year with previous years
thesites <- sort(intersect(sites, colnames(at.yy)))
at.all.merged <- matrix(NA, nrow = nrow(at.all) + nrow(at.yy), ncol = (length(sites) + 2))
at.all.merged <- as.data.frame(at.all.merged)
colnames(at.all.merged) <- c("Date", "Time", sites)
at.all.merged$Date <- as.Date("01/01/01", "%m/%d/%y")

at.all.merged[1:nrow(at.all),] <- at.all
idx <- ((nrow(at.all) + 1) : nrow(at.all.merged))
at.all.merged$Date[idx] <- at.yy$Date
at.all.merged$Time[idx] <- at.yy$Time

for(s in 1:length(thesites)){
  site <- thesites[s]
  at.all.merged[idx, site] <- at.yy[,site]
}
summary(at.all.merged)
plot(at.all.merged$Date, at.all.merged[,3], type = 'l')

write.csv(at.all.merged, paste0(data.dir, "/", watershed, ".at.allyears.csv"), row.names = F)

# plot in individual panels to check
png(paste0(data.dir, "/", watershed, ".at.allyears.png"), width = 16, height = 10, units = "in", res = 300)
par(mfrow = c(6,8), las = 1, cex = 0.5)

for(i in 3:(ncol(at.all.merged))){
  plot(at.all.merged$Date, at.all.merged[,i], type = 'l', ylim = c(-10, 35), main = colnames(at.all.merged)[i], xlab = "", ylab = "")
}
dev.off()  


## OLDER VERSIONS #####
# Clean the data ####
(oldfiles <- dir(paste0(data.dir, "/", old.data.folder)))
(thefiles <- dir(paste0(data.dir, "/", raw.data.folder)))

i <- 1 # Look at 'thefiles' and pick sites one by one manually

data.file <- thefiles[i]
site<- gsub("AIR_", "", data.file)
site <- gsub("_.*","", site)
cat(site, "\n")

# Prepare possible files to use
(old.loggers <- oldfiles[grep(paste0("_", site), oldfiles)])
(new.loggers <- thefiles[grep(paste0("_", site), thefiles)])
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
  evens <- seq(2, length(td$Time),2)
  td$Time[evens] <- td$Time[evens] + 0.5
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
  evens <- seq(2, length(td$Time),2)
  td$Time[evens] <- td$Time[evens] + 0.5
  td <- td[!is.na(td$DateTime),]
  td <- td[order(td$Date, td$Time),]
  td$Date <- as.Date(td$Date)
  td <- td[,c(3,4,2)]
  print(paste0(site, ".", j + length(old.loggers), ": ", range(td$Date)))
  new.list <- c(new.list, paste0(site, ".", j + length(old.loggers)))
  assign(paste0(site, ".", j + length(old.loggers)), td)
}
rm(td)

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

# # If there are multiple that need to be strung together, uncomment and run interactively:
# oldfile2keep <- dat
# if(length(new.list) > 1){
#   newfile2keep <- readline("Which file should be used for this year? ") #e.g., C1.2
# } else {
#   newfile2keep <- new.list[1]
# }
# newfile2keep <- get(newfile2keep)
# dat <- get.complete.year(oldfile2keep, newfile2keep, first.year) #choose new breakpoint date


# Proceed with cleaning the data (Not usually necessary for AIR)
# note: deployment and recovery dates should already be dealt with
#dat <- clean.deployment(dat) #clip off data before deployment date
#plot.hobo(dat)
#dat <- clean.recovery(dat) #clip off data after recovery date
#plot.hobo(dat)
#thedirectory <- dir(paste0(data.dir, "/", cleaned.data.folder)) #for choosing nearby sites
#dat <- clean.middle(dat, thedirectory)

# Finalize, review, and save
colnames(dat)[3] <- "AT"
plot(dat$Date, dat$AT, type = 'l', ylab = "Temperature (C)", xlab = "Date")
summary(dat[!is.na(dat$AT),])

write.csv(dat, paste0(data.dir, "/", cleaned.data.folder, "/", data.file), row.names = F)


# Organize data as matrix with Date and Site columns; NAs where no data ####
# helpful for use in SSN models
(thefiles <- dir(paste0(data.dir, "/", cleaned.data.folder)))

yy <- first.year + 1
dates <- rep(seq(as.Date(paste0(yy - 1, "-09-01")),as.Date(paste0(yy, "-08-31")), by = 1), 24)
dates <- dates[order(dates)]
times <- rep(seq(1, 24, 1), 365) #366 if leap year
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
write.csv(new.df, paste0(data.dir, "/Data_Cleaned_", yy, "/wenatchee.at.", yy, ".csv"), row.names = F)

# plot in individual panels to check
png(paste0(data.dir, "/Data_Cleaned_", yy, "/wenatchee.at.", yy, ".png"), width = 16, height = 8, units = "in", res = 300)
par(mfrow = c(5,8), las = 1, cex = 0.5)
for(i in 3:(ncol(new.df) - 2)){
  plot(new.df$Date, new.df[,i], type = 'l', ylim = c(-10, 35), main = colnames(new.df)[i], xlab = "", ylab = "")
}
dev.off()  







# FIX FORMAT OF LOGGERS SAVED IN EXCEL ####
data.file1 <- thefiles[28]
td <- read.csv(paste0(data.dir, "/", raw.data.folder, "/", data.file1), skip = 1, header = F, stringsAsFactors = F)
colnames(td) <- c("Date", "Time","AT")
td$Date <- as.Date(td$Date, origin = "1970-01-01", format = "%m/%d/%y")
td$Time <- as.numeric(gsub(":*", "", td$Time))/10000
td$Time[td$Time == 0] <- 24
td <- td[order(td$Date, td$Time),]
write.csv(td, paste0(data.dir, "/", cleaned.data.folder, "/", data.file1), row.names = F)


# Merge with all other years ####
yy <- 2020
at.all <- read.csv(paste0(data.dir1, "/NOAA-USFS/wenatchee.at.allyears.csv"), header = T)
at.all$Date <- as.Date(at.all$Date)
sites <- colnames(at.all[3:ncol(at.all)])
sites <- gsub("_.*","", sites)
colnames(at.all) <- c("Date", "Time", sites)

at.yy <- read.csv(paste0(data.dir, "/Data_Cleaned_2020/wenatchee.at.", yy, ".csv"), header = T)
at.yy$Date <- as.Date(at.yy$Date)
for(i in 3:ncol(at.yy)){
  cn <- colnames(at.yy)[i]
  colnames(at.yy)[i] <- gsub("_.*","", cn)
}

# Merge this year with previous years
thesites <- sort(intersect(sites, colnames(at.yy)))
at.all.merged <- matrix(NA, nrow = nrow(at.all) + nrow(at.yy), ncol = (length(sites) + 2))
at.all.merged <- as.data.frame(at.all.merged)
colnames(at.all.merged) <- c("Date", "Time", sites)
at.all.merged$Date <- as.Date("01/01/01", "%m/%d/%y")

at.all.merged[1:nrow(at.all),] <- at.all
idx <- ((nrow(at.all) + 1) : nrow(at.all.merged))
at.all.merged$Date[idx] <- at.yy$Date
at.all.merged$Time[idx] <- at.yy$Time

for(s in 1:length(thesites)){
  site <- thesites[s]
  at.all.merged[idx, site] <- at.yy[,site]
}
summary(at.all.merged)
plot(at.all.merged$Date, at.all.merged[,3], type = 'l')

write.csv(at.all.merged, paste0(data.dir1, "/NOAA-USFS/wenatchee.at.allyears.csv"), row.names = F)

# plot in individual panels to check
png(paste0(data.dir1, "/NOAA-USFS/wenatchee.at.allyears.png"), width = 16, height = 10, units = "in", res = 300)
par(mfrow = c(6,8), las = 1, cex = 0.5)

for(i in 3:(ncol(at.all.merged))){
  plot(at.all.merged$Date, at.all.merged[,i], type = 'l', ylim = c(-10, 35), main = colnames(at.all.merged)[i], xlab = "", ylab = "")
}
dev.off()  
