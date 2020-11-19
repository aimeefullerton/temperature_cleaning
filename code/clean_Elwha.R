# Clean temperature data files to remove erroneous readings (e.g. from air, sediment, or ice)
# Aimee H Fullerton, 18 November 2020

# Load functions
numdailyobs <- 24
source("code/cleaning_functions.R")

# Directories
watershed <- "elwha"
first.year <- 2019
date.begin <- "-08-01"
date.end <- "-09-30"
data.dir <- "/Users/aimee_fullerton/GitHub/Elwha_ST/data"
raw.data.folder <- paste0(first.year + 1, "/data.raw")
old.data.folder <- paste0(first.year, "/data.raw")
cleaned.data.folder <- paste0(first.year + 1, "/data.cleaned")


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
  
  
