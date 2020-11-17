# Clean temperature data files to remove erroneous readings (e.g. from air, sediment, or ice)
# Aimee H Fullerton, 16 November 2020

# Load functions
source("code/cleaning_functions.R")

# Directories
watershed <- "snoqualmie"
first.year <- 2019
date.begin <- "-09-01"
date.end <- "-08-31"
numdailyobs <- 48
data.dir <- "/Users/aimee_fullerton/OneDrive/Work/Research/StreamTemperature/Hobonet/Data"
raw.data.folder <- paste0("Data_Raw_Sep", (first.year + 1), "/water")
old.data.folder <- paste0("Data_Raw_Sep", first.year, "/water")
cleaned.data.folder <- paste0("Data_Cleaned_", (first.year + 1), "/water")

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
    print("This site did not have a file last year. Choose the first file for the current year.")
    oldfile2keep <- choose.file("current.year")
  }
  newfile2keep <- choose.file("current.year")
  
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



# Merge with all other years ####
yy <- first.year + 1
  
#wt.all <- read.csv(paste0(data.dir, "/", "WT.2012-2019.csv"), header = T, row.names = 1)
#sites <- c("C1", "E1", "D1", "G1", "F1", "F1.r", "I1", "L1", "L3", "B1", "L2", "X1", "C2",
#                "MF2", "MF1", "MF4", "MF3", "MF5", "MF6", "MS5", "MS6", "MS9", "MS10", "MS10.r",
#                "MS4", "NF3", "NF1", "NF2", "NF4", "R5b", "R5a", "R1", "R4", "R5c", "R2", "R3",
#                "SF3", "SF1", "SF2", "M1", "S1", "Y1", "K1", "MS8", "T2", "MS7", "T4", "T1")
#colnames(wt.all)[3:ncol(wt.all)] <- sites
#wt.all$Time <- wt.all$Time / 2

wt.all <- read.csv(paste0(data.dir, "/", watershed, ".wt.allyears.csv"), header = T)
wt.all$Date <- as.Date(wt.all$Date)
sites <- colnames(wt.all[4:ncol(wt.all)])
sites <- gsub("_.*","", sites)
wt.all<- wt.all[,c("DateTime", "Date", "Time", sort(colnames(wt.all)[4:ncol(wt.all)]))]

wt.yy <- read.csv(paste0(data.dir, "/Data_Cleaned_", yy, "/", watershed, ".wt.", yy, ".csv"), header = T)
wt.yy$Date <- as.Date(wt.yy$Date)
for(i in 4:ncol(wt.yy)){
  cn <- colnames(wt.yy)[i]
  colnames(wt.yy)[i] <- gsub("_.*","", cn)
}
wt.yy<- wt.yy[,c("DateTime", "Date", "Time", sort(colnames(wt.yy)[4:ncol(wt.yy)]))]


# Merge this year with previous years
thesites <- sort(intersect(sites, colnames(wt.yy)))
wt.all.merged <- matrix(NA, nrow = nrow(wt.all) + nrow(wt.yy), ncol = (length(sites) + 3))
wt.all.merged <- as.data.frame(wt.all.merged)
colnames(wt.all.merged) <- c("DateTime", "Date", "Time", sort(sites))
wt.all.merged$Date <- as.Date("2000-01-01", "%Y-%m-%d")

wt.all.merged[1:nrow(wt.all),] <- wt.all

idx <- ((nrow(wt.all) + 1) : nrow(wt.all.merged))
wt.all.merged$Date[idx] <- wt.yy$Date
wt.all.merged$Time[idx] <- wt.yy$Time
wt.all.merged$DateTime[idx] <- wt.yy$DateTime
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

for(i in 4:(ncol(wt.all.merged))){
  plot(wt.all.merged$Date, wt.all.merged[,i], type = 'l', ylim = c(-5, 25), main = colnames(wt.all.merged)[i], xlab = "", ylab = "")
}
dev.off()  



