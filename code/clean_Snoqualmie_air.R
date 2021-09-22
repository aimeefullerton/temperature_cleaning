# Stitch together complete air temperature records
# Aimee H Fullerton, 21 September 2021

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
raw.data.folder <- paste0("Data_Raw_Sep", (first.year + 1), "/air")
old.data.folder <- paste0("Data_Raw_Sep", first.year, "/air")
cleaned.data.folder <- paste0("Data_Cleaned_", (first.year + 1), "/air")
if(!dir.exists(paste0(data.dir, "/", cleaned.data.folder))){
  dir.create(paste0(data.dir, "/Data_Cleaned_", (first.year + 1)), showWarnings = F)
  dir.create(paste0(data.dir, "/", cleaned.data.folder), showWarnings = F)
}
oldfiles <- dir(paste0(data.dir, "/", old.data.folder))
thefiles <- dir(paste0(data.dir, "/", raw.data.folder))
oldfiles <- toupper(oldfiles); thefiles <- toupper(thefiles)

# PROCESS SITES INDIVIDUALLY ####
thefiles # Look at 'thefiles' and pick sites one manually
i <- 1
while(!is.null(i)){
  data.file <- thefiles[i]
  site<- gsub("AIR_", "", data.file)
  site <- gsub("_.*","", site)
  cat(site, "\n")
  
  # Prepare possible files to use
  old.loggers <- oldfiles[grep(paste0("_", site), oldfiles)]
  new.loggers <- thefiles[grep(paste0("_", site), thefiles)]
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
    plot(new.df$Date, new.df[,i], type = 'l', ylim = c(-5, 35), main = colnames(new.df)[i], xlab = "", ylab = "")
    abline(v = as.Date(paste0(first.year, date.begin)), lty = 2)
    abline(v = as.Date(paste0(first.year + 1, date.end)), lty = 2)
  }
  dev.off()  
  
  
# Merge with all other years ####
yy <- first.year + 1
#at.all <- read.csv(paste0(data.dir, "/", watershed, ".at.earlyyears.csv"), header = T)
at.all <- read.csv(paste0(data.dir, "/", watershed, ".at.allyears.csv"), header = T)
at.all$Date <- as.Date(at.all$Date)
sites <- colnames(at.all[4:ncol(at.all)])
sites <- gsub("_.*","", sites)
at.all<- at.all[,c("DateTime", "Date", "Time", sort(colnames(at.all)[4:ncol(at.all)]))]

at.yy <- read.csv(paste0(data.dir, "/Data_Cleaned_", yy, "/", watershed, ".at.", yy, ".csv"), header = T)
at.yy$Date <- as.Date(at.yy$Date)
for(i in 4:ncol(at.yy)){
  cn <- colnames(at.yy)[i]
  colnames(at.yy)[i] <- gsub("_.*","", cn)
}
at.yy<- at.yy[,c("DateTime", "Date", "Time", sort(colnames(at.yy)[4:ncol(at.yy)]))]


# Merge this year with previous years
thesites <- sort(intersect(sites, colnames(at.yy)))
at.all.merged <- matrix(NA, nrow = nrow(at.all) + nrow(at.yy), ncol = (length(sites) + 3))
at.all.merged <- as.data.frame(at.all.merged)
colnames(at.all.merged) <- c("DateTime", "Date", "Time", sort(sites))
at.all.merged$Date <- as.Date("2000-01-01", "%Y-%m-%d")

at.all.merged[1:nrow(at.all),] <- at.all

idx <- ((nrow(at.all) + 1) : nrow(at.all.merged))
at.all.merged$Date[idx] <- at.yy$Date
at.all.merged$Time[idx] <- at.yy$Time
at.all.merged$DateTime[idx] <- at.yy$DateTime
for(s in 1:length(thesites)){
  site <- thesites[s]
  at.all.merged[idx, site] <- at.yy[,site]
}

at.all.merged <- at.all.merged[order(at.all.merged$Date, at.all.merged$Time),]
summary(at.all.merged)
plot(at.all.merged$Date, at.all.merged[,5], type = 'l')

write.csv(at.all.merged, paste0(data.dir, "/", watershed, ".at.allyears.csv"), row.names = F)

# plot in individual panels to check
png(paste0(data.dir, "/", watershed, ".at.allyears.png"), width = 16, height = 10, units = "in", res = 300)
par(mfrow = c(6,8), las = 1, cex = 0.5)

for(i in 4:(ncol(at.all.merged))){
  plot(at.all.merged$Date, at.all.merged[,i], type = 'l', ylim = c(-10, 35), main = colnames(at.all.merged)[i], xlab = "", ylab = "")
}
dev.off()  

# Getting first few years ready ####
first <- read.csv(paste0(data.dir, "/Data_Raw_Sep2015/AllSnoqualmieSites_WaterAir.csv"), header = T)
first$Date <- as.Date(first$Date, format = "%m/%d/%Y")
idx <- grep(".Air", colnames(first))
idx <- colnames(first)[idx]
first <- first[, c("Date", "Time", idx)]
colnames(first) <- c("Date", "Time", "L3", "MS10", "NF3", "NF1", "NF4", "R1", "R5c", "SF1", "SF2", "C1", "T1", "MS4")
first$Time <- (first$Time / 2) - 0.5
foo <- paste0(first$Date, " ", sprintf("%02d", floor(first$Time)), ":00")
if(numdailyobs == 48) foo[seq(2, length(foo), 2)] <- gsub(":00", ":30", foo[seq(2, length(foo), 2)])
first$DateTime <- as.POSIXlt( foo, format = "%Y-%m-%d %H:%M")
summary(first)

second <- read.csv(paste0(data.dir, "/snoqualmie_at_Sep2015-Sep2017.csv"), header = T)
sites <- c(colnames(second[3:ncol(second)]), "T4")
second$Date <- as.Date(second$Date, format = "%m/%d/%y")
second$Time <- (second$Time / 2) - 0.5
foo <- paste0(second$Date, " ", sprintf("%02d", floor(second$Time)), ":00")
if(numdailyobs == 48) foo[seq(2, length(foo), 2)] <- gsub(":00", ":30", foo[seq(2, length(foo), 2)])
second$DateTime <- as.POSIXlt( foo, format = "%Y-%m-%d %H:%M")
summary(second)

dates <- seq(from = as.Date("2014-09-01"), to = as.Date(paste0("2017", date.end)), by = 1)
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
new.df <- cbind.data.frame(new.df, matrix(NA, nrow(new.df), length(sites)))
colnames(new.df) <- c("Date", "Time", "DateTime", sites)

# Merge
thesites <- sort(intersect(sites, colnames(first)))
for(s in 1:length(thesites)){
  site <- thesites[s]
  idx <- seq(1, nrow(first))
  new.df[idx, site] <- first[,site]
}
thesites <- sort(intersect(sites, colnames(second)))
for(s in 1:length(thesites)){
  site <- thesites[s]
  idx <- seq((nrow(first) + 1), (nrow(first) + nrow(second)))
  new.df[idx, site] <- second[,site]
}
summary(new.df)

write.csv(new.df, paste0(data.dir, "/", watershed, ".at.earlyyears.csv"), row.names = F)

# plot in individual panels to check
png(paste0(data.dir, "/", watershed, ".at.earlyyears.png"), width = 16, height = 10, units = "in", res = 300)
par(mfrow = c(6,8), las = 1, cex = 0.5)

for(i in 3:(ncol(new.df))){
  plot(new.df$Date, new.df[,i], type = 'l', ylim = c(-10, 35), main = colnames(new.df)[i], xlab = "", ylab = "")
}
dev.off()  
