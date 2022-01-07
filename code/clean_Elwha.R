# Clean temperature data files to remove erroneous readings (e.g. from air, sediment, or ice)
# Aimee H Fullerton, 7 Jan 2022 (9 December 2020)

# Load functions
numdailyobs <- 24
source("code/cleaning_functions.R")

# Directories
watershed <- "elwha"
first.year <- 2020
date.begin <- "-10-01"
date.end <- "-11-30"
data.dir <- "/Users/aimee_fullerton/GitHub/Elwha_ST/data"
#data.dir2 <- "/Users/aimee_fullerton/OneDrive/Work/Research/StreamTemperature/Elwha.ST/data_from_George"
raw.data.folder <- paste0(first.year + 1, "/data.raw")
old.data.folder <- paste0(first.year, "/data.raw")
cleaned.data.folder <- paste0(first.year + 1, "/data.cleaned")
#cleaned.data.folder2 <- "found_data_cleaned"
if(!dir.exists(paste0(data.dir, "/", cleaned.data.folder))){
  dir.create(paste0(data.dir, "/Data_Cleaned_", (first.year + 1)), showWarnings = F)
  dir.create(paste0(data.dir, "/", cleaned.data.folder), showWarnings = F)
}
oldfiles <- dir(paste0(data.dir, "/", old.data.folder))
thefiles <- dir(paste0(data.dir, "/", raw.data.folder))
#oldfiles <- toupper(oldfiles); thefiles <- toupper(thefiles)

sites.attr <- read.csv(paste0(data.dir, "/elwha.sites.attributed.csv"), header = T)
xx <- sites.attr$Site.Name; names(xx) <- sites.attr$SiteCode
# get_value(sites, xx) # look up site name(s) based on (vector of) site codes


# Clean the data ####
thefiles
i <- 15 # Look at 'thefiles' and pick sites one by one manually

while(!is.null(i)){
  data.file <- thefiles[i]
  site <- gsub("_.*","", data.file)
  cat(site, "\n")
  
  # Prepare possible files to use
  old.loggers <- oldfiles[grep(paste0(site, "_"), oldfiles)]
  new.loggers <- thefiles[grep(paste0(site, "_"), thefiles)]
  old.loggers <- old.loggers[grep(site, old.loggers)]
  new.loggers <- new.loggers[grep(site, new.loggers)]
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
  
  # Fix time stamp if minutes is not on the hour, by truncating 
  if(!is.na(newfile2keep)[1] & numdailyobs == 24) newfile2keep$DateTime <- time.truncate(newfile2keep$DateTime)
  if(!is.na(oldfile2keep)[1] & numdailyobs == 24) oldfile2keep$DateTime <- time.truncate(oldfile2keep$DateTime)
  
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
    # Fix time stamp if minutes is not on the hour, by truncating 
    if(!is.na(newfile2keep)[1] & numdailyobs == 24) newfile2keep$DateTime <- time.truncate(newfile2keep$DateTime)
    
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
  
  # Ensure the whole time series is filled out (even if some temperatures are NAs)
  dat <- fill.time.series(dat, first.year, date.begin, date.end, numdailyobs)
  
  write.csv(dat, paste0(data.dir, "/", cleaned.data.folder, "/", site, ".csv"), row.names = F)
  rm(list = old.list); rm(list = new.list); rm(td, dat, newfile2keep, oldfile2keep)
  i <- NULL
  cat(paste0("All done with ", site, "!"), "\n")
}

(thefiles <- dir(paste0(data.dir, "/", cleaned.data.folder)))
par(mfrow = c(4,4))
for(i in 1:length(thefiles)){
  site.name <- gsub("_.*","", thefiles[i])
  td <- read.csv(paste0(data.dir, "/", cleaned.data.folder, "/", thefiles[i]), header = T, stringsAsFactors = F)
  td$DateTime <- as.POSIXlt(td$DateTime, format = "%Y-%m-%d %H:%M"); td$Date <- as.Date(td$Date)
  td <- td[td$DateTime > min(td$DateTime[!is.na(td$Temp)]),]
  td <- td[td$DateTime < max(td$DateTime[!is.na(td$Temp)]),]
  plot(td$DateTime, td$Temp, type = 'l', main = site.name, xlab = "", ylab = "")
}

# Organize current year's data as matrix with Date and Site columns; NAs where no data ####
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
if(first.year == 2018) colnames(new.df) <- c("DateTime", "Date", "Time", "BO1", "MS34", "MS25", "MS21", "FP28", "FP9", "IC2", "IC9", "LL1", "LR11", "LO1", "FP11", "FP10")
if(first.year == 2019) colnames(new.df) <- c("DateTime", "Date", "Time", "MS25", "MS21", "MS39", "GR1", "HU1", "FP9", "IC2", "LR11", "FP11", "FP4", "MS11")
if(first.year == 2020) colnames(new.df) <- c("DateTime", "Date", "Time", "EUTRBC", "EUMSBC", "EMMSLA2", "EMMSAL", "ELMSFW", "EMTRGC", "ELMSHR", "EMTRIC1", "EMTRIC19", "EMTRLLR", "EMTRMC", "ELSC3", "ELSC8", "EMSC15")
write.csv(new.df, paste0(data.dir, "/", (first.year + 1), "/", watershed, ".wt.", (first.year + 1), ".csv"), row.names = F)

# plot in individual panels to check
png(paste0(data.dir, "/", (first.year + 1), "/", watershed, ".wt.", (first.year + 1), ".png"), width = 16, height = 10, units = "in", res = 300)
par(mfrow = c(4,4), las = 1, cex = 0.5)
for(i in 4:(ncol(new.df))){
  plot(new.df$DateTime, new.df[,i], type = 'l', ylim = c(-5, 25), main = colnames(new.df)[i], xlab = "", ylab = "", xaxt = 'n', las = 1)
  axis.POSIXct(1, at = seq(new.df$DateTime[1], new.df$DateTime[length(new.df$DateTime)-1], by = "month"), new.df$DateTime, format = "%m-%y")
}
dev.off()  

# Merge current year with all other years ####
yy <- first.year + 1

## Reformatting previous file to merge with new ones (no need to run this going forward)
#wt.all <- read.csv(paste0(data.dir, "/", watershed, ".st.final.csv"), header = T, row.names = 1)
#site.nums <- colnames(wt.all[3:ncol(wt.all)]); site.nums <- as.numeric(gsub("X", "", site.nums))
#xx <- sites.attr$SiteCode; names(xx) <- sites.attr$Site.No; xx <- xx[order(as.numeric(names(xx)))]
#sites <- get_value(site.nums, xx)
#colnames(wt.all) <- c("Date", "Time", sites)
#wt.all$Date <- as.Date(wt.all$Date)
#wt.all$DateTime <- as.POSIXlt( paste0(wt.all$Date, " ", sprintf("%02d", floor(wt.all$Time)), ":00"), format = "%Y-%m-%d %H:%M")
#wt.all <- wt.all[,c("DateTime", "Date", "Time", sort(sites))]
#write.csv(wt.all, paste0(data.dir, "/", watershed, ".wt.allyears.csv"), row.names = F)
wt.all <- read.csv(paste0(data.dir, "/", watershed, ".wt.allyears.csv"), header = T)
wt.all$Date <- as.Date(wt.all$Date)

wt.yy <- read.csv(paste0(data.dir, "/", yy, "/", watershed, ".wt.", yy, ".csv"), header = T)
wt.yy$Date <- as.Date(wt.yy$Date)

# Merge this year with previous years
oldsites <- colnames(wt.all[4:ncol(wt.all)])
yysites <- colnames(wt.yy[4:ncol(wt.yy)])
newsites <- yysites[!yysites %in% oldsites]
get_value(yysites, xx) # to check

wt.all.merged <- matrix(NA, nrow = nrow(wt.all) + nrow(wt.yy), ncol = (length(oldsites) + length(newsites) + 3))
wt.all.merged <- as.data.frame(wt.all.merged)
colnames(wt.all.merged) <- c("DateTime", "Date", "Time", oldsites, newsites)

wt.all.merged[1:nrow(wt.all),oldsites] <- wt.all[,oldsites]
wt.all.merged[1:nrow(wt.all), 1:3] <- wt.all[,1:3]
wt.all.merged$Date <- as.Date(wt.all.merged$Date, origin = "1970-01-01")

idx <- ((nrow(wt.all) + 1) : nrow(wt.all.merged))
wt.all.merged$DateTime[idx] <- wt.yy$DateTime
wt.all.merged$Date[idx] <- wt.yy$Date
wt.all.merged$Time[idx] <- wt.yy$Time
for(s in 1:length(yysites)){
  site <- yysites[s]
  wt.all.merged[idx, site] <- as.numeric(wt.yy[,site])
}
wt.all.merged <- wt.all.merged[,c("DateTime", "Date", "Time", sort(c(oldsites, newsites)))]
summary(wt.all.merged)
plot(wt.all.merged$Date, wt.all.merged$MS11, type = 'l')

wt.all.merged <- unique(wt.all.merged)
write.csv(wt.all.merged, paste0(data.dir, "/", watershed, ".wt.allyears.csv"), row.names = F)

# plot in individual panels to check
png(paste0(data.dir, "/", watershed, ".wt.allyears.png"), width = 19, height = 12, units = "in", res = 300)
par(mfrow = c(8,10), las = 1, cex = 0.5)

for(i in 4:(ncol(wt.all.merged))){
  
  if(any(!is.na(wt.all.merged[,i]))) plot(wt.all.merged$DateTime, wt.all.merged[,i], type = 'l', ylim = c(-5, 25), main = get_value(colnames(wt.all.merged)[i], xx), xlab = "", ylab = "")
}
dev.off()  

# plot sites with best data
png(paste0(data.dir, "/", watershed, ".sites_with_best_data.png"), width = 10, height = 6, units = "in", res = 300)
par(mfrow = c(4,5), las = 1, cex = 0.5)

sites2plot <- c("LR2", "IC2", "MS11", "MS5", "MS22", "MS20", "MS26", "MS27", "FP10", "FP2", "IC1", "IC9", "IC11", "IC12", "IC19", "LR11", "LR14", "LR22")
for(i in 1:length(sites2plot)){
  site <- sites2plot[i]
  plot(wt.all.merged$DateTime, wt.all.merged[,site], type = 'l', ylim = c(-5, 25), main = get_value(site, xx), xlab = "", ylab = "")
}
dev.off()  


# Read back in:
td <- read.csv(paste0(data.dir, "/elwha.wt.allyears.csv"), header = T) 
td$Date <- as.Date(td$Date)
td$DateTime <- as.POSIXlt( td$DateTime, format = "%Y-%m-%d %H:%M")



# Read in & clean old WDOE data and other "found" data ####
data.dir <- "/Users/aimee_fullerton/OneDrive/Work/Research/StreamTemperature/Elwha.ST/data_from_George/found_data_Dec2020"
cleaned.data.folder <- "/Users/aimee_fullerton/OneDrive/Work/Research/StreamTemperature/Elwha.ST/data_from_George/found_data_cleaned"

thefiles <- dir(data.dir)
for(f in 1:length(thefiles)){
  ff <- thefiles[f]
  if(length(grep(".txt", ff)) > 0){
    site <- substr(ff, 1, 2)
    year <- as.numeric(substr(ff, 11, 14))
    if(year <2006) skiplines = 4 else skiplines = 12
    td <- read.table(paste0(data.dir, "/", ff), skip = skiplines, header = F, fill = T)[,1:3]
    colnames(td) <- c("Date", "Time", "Temp")
    td$Date <- as.Date(td$Date, format = "%m/%d/%Y", origin = "1970")
    td <- td[!is.na(td$Date),] # to remove trailing comment
    write.csv(td, paste0(data.dir, "/", site, ".", year, ".DOE.csv"), row.names = F)
  }
}

thefiles <- dir(data.dir)
for(f in 1:length(thefiles)){
  ff <- thefiles[f]
  cat(ff,"\n")
  if(length(grep(".csv", ff)) > 0){
    site <- substr(ff, 1, 2)
    year <- as.numeric(substr(ff, 4, 7))
    td <- read.csv(paste0(data.dir, "/", ff), header = T)
    if(is.na(year)) td$Date <- as.Date(td$Date, format = "%m/%d/%y") else td$Date <- as.Date(td$Date)
    #td$DateTime <- as.POSIXlt(strptime(paste(td$Date, td$Time), "%Y-%m-%d %I:%M %p"), format = "%H:%M")
    #td$DateTime <- td$DateTime + 60*5
    #td$DateTime <- as.POSIXlt(td$DateTime)
    #td$Time <- td$DateTime$hour
    td <- td[!is.na(td$Temp),]
    dat <- td[, c("DateTime", "Date", "Time", "Temp")]
    dat <- dat[dat$Temp < 30 & dat$Temp > 0,] #remove crazy weird readings
    
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
    plot(dat$DateTime, dat$Temp, type = 'l', ylab = "Temperature (C)", xlab = "Date")
    summary(dat[!is.na(dat$Temp),])
    
    write.csv(dat, paste0(cleaned.data.folder, "/", ff), row.names = F)
    
  }
}

# Condense 15-min data to hourly
thefiles <- dir(cleaned.data.folder)
for(f in 1:length(thefiles)){
  ff <- thefiles[f]
  cat(ff,"\n")
  if(length(grep(".csv", ff)) > 0){
    year <- as.numeric(substr(ff, 4, 7))
    if(!is.na(year)){
      # read in
      td <- read.csv(paste0(cleaned.data.folder, "/", ff), header = T)
      td$Date <- as.Date(td$Date)
      td$DateTime <- as.POSIXlt(td$DateTime, format = "%Y-%m-%d %H:%M")
      
      # summarize to hourly
      foo <- tapply(td$Temp, list(td$Date, td$DateTime$hour), mean, na.rm = T)
      dates <- NULL; for(i in 1:length(row.names(foo))) dates <- c(dates,rep(row.names(foo)[i], 24))
      td.hourly <- cbind.data.frame("Date" = dates, "Time" = colnames(foo), "Temp" = c(t(foo)))
      td.hourly <- td.hourly[!is.na(td.hourly$Temp),]
      td.hourly$Time <- paste0(td.hourly$Time, ":00")
      td.hourly$DateTime <- paste(td.hourly$Date, td.hourly$Time); td.hourly$DateTime <- as.POSIXlt( td.hourly$DateTime, format = "%Y-%m-%d %H:%M")
      td.hourly <- td.hourly[, c("DateTime", "Date", "Time", "Temp")]
      
      write.csv(td.hourly, paste0(cleaned.data.folder, "/hourly_", ff), row.names = F)
      #plot(td.hourly$DateTime, td.hourly$Temp, type = 'l')
    }
  }
}

#Everything except the DOE data have funky times that we need to correct
thefiles <- c("IC1.csv", "IC11.csv", "IC12.csv", "IC9.csv", "LR11.csv", "LR14.csv", "LR2.csv", "LR22.csv", "SC6.csv", "SC26.csv")
td <- read.csv(paste0(data.dir2, "/", cleaned.data.folder2, "/", thefiles[i]), header = T, stringsAsFactors = F)
td$DateTime <- as.POSIXlt(td$DateTime, format = "%Y-%m-%d %I:%M:%S")
td$DateTime <- as.POSIXlt(td$DateTime, format = "%Y-%m-%d %H:%M")
td$Date <- as.Date(paste0(td$DateTime$year, "-", td$DateTime$mon, "-", td$DateTime$mday))



## Merge into big temperature database
wt.all <- read.csv("/users/aimee_fullerton/GitHub/Elwha_ST/data/elwha.wt.allyears.csv", header = T)
wt.all$Date <- as.Date(wt.all$Date)
wt.all$DateTime <- as.POSIXlt( wt.all$DateTime, format = "%Y-%m-%d %H:%M")

# REALIZED THAT NOT ALL DATES ARE IN THIS FILE DUE TO NA RECORDS but need them there to fill in found data; this is the fix
first.year <- "2000"; last.year <- "2020"
date.begin <- "-07-01"; date.end <- "-10-01"
dates <- seq(from = as.Date(paste0(first.year, date.begin)), to = as.Date(paste0(last.year, date.end)), by = 1)
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

wt.all <- merge(new.df, wt.all[, c("DateTime", colnames(wt.all)[4:ncol(wt.all)])], by = c("DateTime"), all.x = T)
write.csv(wt.all, paste0(data.dir, "/", watershed, ".wt.allyears.csv"), row.names = F)


(thefiles <- dir(paste0(data.dir2, "/", cleaned.data.folder2)))
for(i in 1:length(thefiles)){
  site.name <- gsub("_.*","", thefiles[i])
  if(site.name == "IC.DOE") site.name <- "IC2"
  if(site.name == "LR.DOE") site.name <- "LR2"
  if(length(grep(".csv", site.name)) > 0) site.name <- gsub(".csv", "", site.name)
  td <- read.csv(paste0(data.dir2, "/", cleaned.data.folder2, "/", thefiles[i]), header = T, stringsAsFactors = F)
  td$Date <- as.Date(td$Date)
  td$DateTime <- as.POSIXlt( td$DateTime, format = "%Y-%m-%d %H:%M")
  plot(td$DateTime, td$Temp, type = 'l')
  
  wt.all <- merge(wt.all, td[, c("DateTime", "Temp")], by = c("DateTime"), all.x = T)
  #this adds the column to the right of existing data with correct dates/times; next have to merge it into the existing column
  c <- which(colnames(wt.all) == site.name)
  wt.all[,c][is.na(wt.all[,c]) & !is.na(wt.all[,ncol(wt.all)])] <- wt.all[,ncol(wt.all)][is.na(wt.all[,c]) & !is.na(wt.all[,ncol(wt.all)])] 
  plot(wt.all$DateTime, wt.all[,c], type = 'l') #to check
  lines(wt.all$DateTime, wt.all$Temp, col = 2) #to check
  wt.all <- wt.all[,-ncol(wt.all)] #remove temporary column
}
write.csv(wt.all, paste0(data.dir, "/", watershed, ".wt.allyears.csv"), row.names = F)

# For data from John McMillan - had weird time stamp problem that had to be fixed in Excel
jm <- read.csv(paste0(data.dir2, "/found_data_Dec2020/McMillan_temperature_data_notcleaned.csv"), header = T)
colnames(jm)[13] <- "MA1"
colnames(jm)[12] <- "FP22" #SC16, data is already in the database though
colnames(jm)[14] <- "FP17" #SC26, ditto
jm$Date <- as.Date(jm$Date, format = "%m/%d/%y")
jm$DateTime <- as.POSIXlt(paste(jm$Date, jm$Time), format = "%Y-%m-%d %I:%M:%S %p")

for(i in 1:(ncol(jm) - 1)){
  site.name <- colnames(jm)[i]
  dat <- jm[,c("DateTime", site.name)]
  colnames(dat)[2] <- "Temp"

  # Clean the data
  plot.hobo(dat)
  dat <- clean.deployment(dat) #clip off data before deployment date
  plot.hobo(dat)
  dat <- clean.recovery(dat) #clip off data after recovery date
  plot.hobo(dat)
  dat <- clean.middle(dat)
  
  # Finalize, review, and put back into dataframe
  plot.hobo(dat)
  summary(dat[!is.na(dat$Temp),])
  jm[,i] <- dat$Temp
  
  wt.all <- merge(wt.all, dat, by = c("DateTime"), all.x = T)
  #this adds the column to the right of existing data with correct dates/times; next have to merge it into the existing column
  c <- which(colnames(wt.all) == site.name)
  wt.all[,c][is.na(wt.all[,c]) & !is.na(wt.all[,ncol(wt.all)])] <- wt.all[,ncol(wt.all)][is.na(wt.all[,c]) & !is.na(wt.all[,ncol(wt.all)])] 
  plot(wt.all$DateTime, wt.all[,c], type = 'l') #to check
  lines(wt.all$DateTime, wt.all$Temp, col = 2) #to check
  wt.all <- wt.all[,-ncol(wt.all)] #remove temporary column
}
write.csv(wt.all, paste0(data.dir, "/", watershed, ".wt.allyears.csv"), row.names = F)


# OLDER VERSION: #####
# Begin cleaning ####
(thedir = dir(paste0(data.folder, "/", raw.data.folder)))
for(i in 1:length(thedir)){
  (data.file = thedir[i])
    lines2skip = 1
    td = read.csv(paste0(data.folder, "/", raw.data.folder, "/", data.file), skip = lines2skip, header = T, stringsAsFactors = F)[,2:3]
    colnames(td) = c("DateTime","WT")
    a = substr(as.POSIXlt(td$DateTime[1], origin = "1970-01-01", format = "%m/%d/%Y"), 1, 2)
    if(a < 19) date.format = "%m/%d/%y" else date.format = "%m/%d/%Y"
    td$Date = as.POSIXlt(td$DateTime, origin = "1970-01-01", format = date.format)
    td$DateTime = as.POSIXlt(td$DateTime, origin = "1970-01-01", format = paste(date.format, "%I:%M:%S %p"))
    td$Time = td$DateTime$hour; td$Time[td$Time == 0] = 24
    td = td[!is.na(td$DateTime),]
    td = td[order(td$Date, td$Time),]
  
  # Plot to see what needs to be cleaned
  plot(td$Date, td$WT, pch=19,cex=0.6,ylab="Stream temperature (C)",xlab="Date")
  #lines(td$Date, td$WT, col="darkgray",lwd=0.5)
  points(td$Date[td$WT>25], td$WT[td$WT>25],col=2,pch=20) #highlight absurdly warm ones
  # also look for obvious breaks e.g. from vehicle/building into water
  
  # Get data in format for manual cleaning and check for flags
  mydata = td[,c("Date","Time","WT")]; colnames(mydata)[3] = "Temp"
  mydata$Date = as.Date(mydata$Date)
  #checkhobo(mydata) #optional
  
  #Range of years of data for this site
  (first.year = as.POSIXlt(range(mydata$Date)[1])$year+1900)
  (last.year = as.POSIXlt(range(mydata$Date)[2])$year+1900)
  
  begin.date = as.Date(paste0(first.year,"-06-01"))
  end.date = as.Date(paste0(first.year + 1,"-08-31"))
  
  dat = mydata[mydata$Date >= begin.date & mydata$Date <= end.date,]
  
  #Proceed if there are any data for this year:
  if(nrow(dat) > 1){
    #Get maximum 2-day change in temperature (assumes hourly data):
    (twoday.range = fnc_Check2dRange(dat))
  
    #Manually clip out temperatures that look like sensor was out of water
    plot.hobo(dat)
    ans=readline("Does deployment date need to be cleaned? (y or n) ")
    if(ans == "y") dat = clean.deployment(dat) #clips out beginning before in water
    plot.hobo(dat)
    ans = readline("Does recovery date need to be cleaned? (y or n) ")
    if(ans == "y") dat = clean.recovery(dat) #clips out end after came out of water
    plot.hobo(dat)
    ans = readline("Do any data in the middle need to be cleaned? (y or n) ")
    if(ans == "y"){ 
     ans = readline("Clip a whole section out? (y or n) ") 
     while(ans == "y"){  #helps clip out places that obviously came out of the water
       code = substr(data.file, 1, 2)
       thedirectory = dir(paste0(data.folder, "/", cleaned.data.folder))
       print(thedirectory[grep(code,thedirectory)])
       ans = as.numeric(readline("Choose a nearby location that has already been cleaned (1, 2, 3, ...) or use 0 if no sites qualify. "))
       if(ans > 0){ # use a nearby site to help see if the 'anomaly' is reasonable
          near.file = thedirectory[grep(code,thedirectory)][ans]
          nearby.site = read.csv(paste0(data.folder, "/", cleaned.data.folder, "/", near.file), header = T, stringsAsFactors = F)
          colnames(nearby.site)[3] = "Temp"
          dat = clean.range(dat, close.site = nearby.site) 
       } else{ # no nearby site
         dat = clean.range(dat)
       }
       ans = readline("Do any other sections need to be removed? (y or n) ") 
     }
     ans = readline("Remove specific values? (y or n) ")
     if(ans == "y") dat = fnc_RemovePoints(dat)
    }
    colnames(dat)[3] = "WT"
    write.csv(dat, paste0(data.folder, "/", cleaned.data.folder, "/", data.file), row.names = F)
  }
}

# Check & plot:
png(paste0(data.folder, "/data_timeseries_2019.png"), width = 10, height = 7, units = "in", res = 150)
par(mfrow=c(4,4), mar = c(3,4,2,1), las = 1)
for(i in 1:length(thedir)){
td = read.csv(paste0(data.folder, "/", cleaned.data.folder, "/", thedir[i]), header = T, stringsAsFactors = F)
td$Date = as.Date(td$Date, origin = "1970-01-01")
plot(td$Date, td$WT, pch=19,cex=0.6,ylab="Stream temperature (C)",xlab="Date", ylim = c(-5,25), type = 'l', main = thedir[i], cex.main = 0.8)
abline(h = seq(-5,25,5),lty = 3, col = "gray50")
}
dev.off()


# Organize data as matrix with Date and Site columns; NAs where no data ####
  # helpful for use in SSN models
  (thedir = dir(paste0(data.folder, "/", cleaned.data.folder))); thedir = thedir[thedir!="champ" & thedir!="wenatchee.wt.2019.csv"]

  yy = 2019
  dates = rep(seq(as.Date(paste0(yy-1, "-06-01")),as.Date(paste0(yy, "-08-31")), by=1),24)
  dates = dates[order(dates)]
  times = rep(seq(1,24,1),457)
  new.df = cbind.data.frame("Date" = dates, "Time" = times)
  
  idx = which(thedir == "readme.txt")
  thedir = thedir[-idx]
  for(i in 1:length(thedir)){
    site.name = gsub(".csv","",thedir[i])
    td = read.csv(paste0(data.folder, "/", cleaned.data.folder, "/", thedir[i]), header = T, stringsAsFactors = F)
    new.df = merge(new.df, td, by.x = c("Date", "Time"), by.y = c("Date", "Time"), all.x = T)
    colnames(new.df)[i+2] = site.name
  }
  new.df = new.df[order(new.df$Date,new.df$Time),]
  new.df = unique(new.df)
  rownames(new.df) = NULL
  # explore & remove any duplicates
  new.df[duplicated(new.df[,1:2]),]
  #new.df[new.df$Date == as.Date("2019-08-27") & new.df$Time %in% c(9,10,11,16),]
  #new.df[new.df$Date == as.Date("2019-08-26") & new.df$Time == 14,]
  #new.df = new.df[-c(10858,10860,10863,10869),]
  write.csv(new.df,paste0(data.folder, "/elwha.wt.2019.csv"),row.names=F)
  
  
