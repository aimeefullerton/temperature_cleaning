# Functions for cleaning temperature data from loggers
# Aimee H Fullerton, Last updated 16 November 2020
# Adapted from Colin Sowder's original functions

# Get number of observations per day
numdailyobs <- get("numdailyobs")

# Read in and prepare raw file exported directly from Onset
prepare.file <- function(data.file, directory)
{

  td <- read.csv(paste0(directory, "/", data.file), skip = 1, header = T, stringsAsFactors = F)[,2:3]

  colnames(td) = c("DateTime","Temp")
  td <- td[!is.na(td$Temp),] # remove any records of bad battery etc. important step!
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
  
  
  #td <- td[,c(3,4,2)]
  ## Add "DateTime" column back for better indexing and plotting
  #foo <- paste0(td$Date, " ", sprintf("%02d", floor(td$Time)), ":00")
  #if(numdailyobs == 48) foo[seq(2, length(foo), 2)] <- gsub(":00", ":30", foo[seq(2, length(foo), 2)])
  #td$DateTime <- as.POSIXlt( foo, format = "%Y-%m-%d %H:%M")
  #rm(foo)
  
return(td)
}

# Read in and prepare file with "Date", "Time", and "Temp"
prepare.file2 <- function(data.file, directory)
{
  
  td <- read.csv(paste0(directory, "/", data.file), header = T, stringsAsFactors = F)
  td$Date <- as.POSIXlt(td$Date, origin = "1970-01-01", format = "%m/%d/%y")
  td$DateTime <- paste(td$Date, td$Time)
  td$DateTime <- as.POSIXlt(td$DateTime, origin = "1970-01-01", format ="%Y-%m-%d %H:%M")
  td <- td[, c("DateTime", "Temp", "Date", "Time")]
  td$Date <- as.Date(td$Date)
  
  td <- td[!is.na(td$Temp),] # remove any records of bad battery etc. important step!
  td <- td[!is.na(td$DateTime),]
  td <- td[order(td$Date, td$Time),]

  return(td)
}

# Plot time series
plot.hobo <- function(hobo, type = "l")
{
  plot(hobo$Date, hobo$Temp, xlab = "Date", ylab = "Temperature (C)", type = type, cex = 0.3)
}

# Do some automated checking
check.hobo <- function(hobo, response = "n")
{
  ##first check for min
  if (min(hobo$Temp, na.rm = T) < -1)
  {
    stop <- TRUE
    i <- 1
    while(stop)
    {
      if(hobo$Temp[i] < -1)
      {
        day <- hobo$Date[i]
        cat("The hobo was below -1 first on ", as.character(day), "\n")
        stop <- FALSE	
      }
      i <- i + 1
    }
  }
  else
  {
    cat("The hobo was never below the minimum threshold \n")
  }
  
  ##check for max threshold (25)
  if (max(hobo$Temp > 25, na.rm = T))
  {
    stop = TRUE
    i <- 1
    while(stop)
    {
      if(hobo$Temp[i] > 25)
      {
        day <- hobo$Date[i]
        cat("The hobo was above 25 first on ", as.character(day), "\n")
        stop <- FALSE	
      }
      i <- i + 1
    }
  }
  else
  {
    cat("The hobo was never above the maximum threshold \n")
  }
  
  ###remind the user to check the notes
  response <- readline("Are there any notes that the hobo was out of the water? ")
  if (response == 'y')
  {
    cat("Consult the notes \n")
  }
  else
  {
    cat("There are no notes indicating the hobo was out of the water \n")
  }
  
  ###check the variance, for right now let's look at the smaller  of two day differences?
  temp <- hobo$Temp[!is.na(hobo$Temp)]
  n <- length(temp) - (numdailyobs * 2) + 1
  day1 <- 0
  day2 <- 0
  smdiff <- 0
  when <- ''
  bsmdiff <- 0
  for (i in 1:n)
  {
    st <- i
    ed <- i + (numdailyobs - 1)
    day1 <- diff(range(temp[st:ed], na.rm = TRUE))
    st <- ed + 1
    ed <- ed + numdailyobs
    day2 <- diff(range(temp[st:ed], na.rm = TRUE))
    smdiff <-  min(day1, day2)
    if(smdiff > bsmdiff)
    {
      bsmdiff <- smdiff
      when <- hobo$Date[i]		
    }	
  }
  
  cat("The largest two day difference began on ", as.character(when), "and was ", bsmdiff, "degrees \n")
  cat("The first observation was on ", as.character(hobo$Date[1]),"at ", hobo$Time[1],"\n")
  cat("The last observation was on ", as.character(hobo$Date[length(hobo$Date)]),"at ", hobo$Time[length(hobo$Time)], "\n")
}

# Plot time series over a single day
plot.day <- function(hobo, day)
{
  Temps <- hobo$Temp[which(hobo$Date == day)]
  obs <- 1:length(Temps)
  plot(obs, Temps, xlab = "Observation", ylab = "Temperature (C)", main = paste("Temperatures on ", as.character(day)))
}

# Plot time series over a week
plot.week <- function(hobo, day)
{
  obs.week <- which(hobo$Date == day)[1]
  obs.week <- seq(from = obs.week, length.out = (numdailyobs * 7), by = 1)
  obs.week <- intersect(obs.week, obs.week[1]:length(hobo$Temp))
  Temps <- hobo$Temp[obs.week]
  obs <- 1:length(Temps)
  plot(obs, Temps, xlab = "Observation", ylab = "Temperature (C)", main = paste("Temperatures of week starting on ", as.character(day)))
}

# Choose which file to use from a list
choose.file <- function(theyear = "current.year")
{
  if(theyear == "current.year") {
    thelist = new.list
    if(length(thelist) > 1){
      for(i in 1:length(thelist)){
        tmp1 <- get(thelist[i])
        print(paste("choice ", i, ": ", range(tmp1$Date)))
      }
      choice <- readline("Which file should be used next for the current year? (Choose 1, 2, etc., or 0 for none) ")
      if(choice > 0 ) thefile <- thelist[as.numeric(choice)] else thefile <- NA
    } else {
      thefile <- thelist[1]
    }
  }
  if(theyear == "past.year"){
    thelist = old.list
    if(length(thelist) > 1){
      for(i in 1:length(thelist)){
        tmp1 <- get(thelist[i])
        print(paste("choice ", i, ": ", range(tmp1$Date)))
      }
      choice <- readline("Which file should be used for the end of the previous year? (Choose 1, 2, etc., or 0 for none) ")
      if(choice > 0 ) thefile <- thelist[as.numeric(choice)] else thefile <- NA
    } else {
      thefile <- thelist[1]
    }
  }
    
  if(!is.na(thefile)){
    thefile <- get(thefile)
    return(thefile)
  } else{
    return(NA)
  }
}

# Clip a single file to the date endpoints specified and format
clip.single.file <- function(myfile, first.year, numdailyobs, date.begin = "-09-01", date.end = "-08-31")
{
  
  # Clip dataset to correct dates
  myfile <- myfile[myfile$Date >= as.Date(paste0(first.year, date.begin)) & myfile$Date <= as.Date(paste0(first.year + 1, date.end)),]
  myfile <- unique(myfile)
  
  # Create empty dataframe with all dates/times
  dates <- seq(from = as.Date(paste0(first.year, date.begin)), to = as.Date(paste0(first.year + 1, date.end)), by = 1)
  file2keep <- data.frame(matrix(NA, nrow = length(dates) * numdailyobs, ncol = 3))
  colnames(file2keep) <- c("Date", "Time", "Temp")
  file2keep$Date <- rep(dates, numdailyobs)
  file2keep <- file2keep[order(file2keep$Date),]
  if(numdailyobs == 48){
    file2keep$Time <- rep(seq(0, 23.5, 0.5), length(dates))
  } else{
    file2keep$Time <- rep(seq(0, 23, 1), length(dates))
  }
  foo <- paste0(file2keep$Date, " ", sprintf("%02d", floor(file2keep$Time)), ":00")
  if(numdailyobs == 48) foo[seq(2, length(foo), 2)] <- gsub(":00", ":30", foo[seq(2, length(foo), 2)])
  file2keep$DateTime <- as.POSIXlt( foo, format = "%Y-%m-%d %H:%M")
  row.names(file2keep) <- NULL
  
  
  # Merge
  file2keep <- merge(file2keep[, c("DateTime", "Date", "Time")], myfile[, c("DateTime", "Temp")], by = "DateTime", all.x = T)
  file2keep <- unique(file2keep)
  file2keep <- file2keep[order(file2keep$Date, file2keep$Time),]
  file2keep <- file2keep[, c("DateTime", "Date", "Time", "Temp")]
  # NAs might happen if there was a bad battery warning.
  
  # Plot and print diagnostics
  plot(file2keep$Date, file2keep$Temp, type = 'l', ylab = "Temperature (C)", xlab = "Date")
  print(summary(file2keep[!is.na(file2keep$Temp),]))
  cat("number of records: ", nrow(file2keep), "\n")
  cat("number of obs: ", nrow(file2keep[!is.na(file2keep$Temp),]), "\n")
  
  return(file2keep)
}

# Stitch together raw data from this year and raw data from previous September
get.complete.year <- function(oldfile2keep, newfile2keep, first.year, numdailyobs, date.begin = "-09-01", date.end = "-08-31")
{
  
  ddate <- readline("What is the transition date (as yyyy-mm-dd)? Leave blank and hit enter if unknown: ")
  if(ddate != ""){
    ddate <- as.Date(ddate)
  } else {
    ddate <- max(oldfile2keep$Date) #if not known, assume the old logger was stopped when it was pulled out
  }
  
  # Clip old and new datasets to correct dates
  oldfile2keep <- oldfile2keep[oldfile2keep$Date >= as.Date(paste0(first.year, date.begin)) & oldfile2keep$Date <= ddate,]
  oldfile2keep <- unique(oldfile2keep)
  newfile2keep <- newfile2keep[newfile2keep$Date >= ddate & newfile2keep$Date <= as.Date(paste0(first.year + 1, date.end)),]
  newfile2keep <- unique(newfile2keep)

  last.obs <- NULL
  first.obs <- NULL
  # Error handling
  if(!ddate %in% oldfile2keep$Date){
    last.obs <- which(oldfile2keep$DateTime == max(oldfile2keep$DateTime, na.rm = T))
    #stop("Error: The selected date does not occur in the first dataset. Examine data to see if there is a big gap.")
  }
  if(!ddate %in% newfile2keep$Date){
    first.obs <- which(newfile2keep$DateTime == min(newfile2keep$DateTime, na.rm = T)  )
    #stop("Error: The selected date does not occur in the second dataset. Examine data to see if there is a big gap.")
  }
  
  # Create empty dataframe with all dates/times
  dates <- seq(from = as.Date(paste0(first.year, date.begin)), to = as.Date(paste0(first.year + 1, date.end)), by = 1)
  file2keep <- data.frame(matrix(NA, nrow = length(dates) * numdailyobs, ncol = 3))
  colnames(file2keep) <- c("Date", "Time", "Temp")
  file2keep$Date <- rep(dates, numdailyobs)
  file2keep <- file2keep[order(file2keep$Date),]
  if(numdailyobs == 48){
    file2keep$Time <- rep(seq(0, 23.5, 0.5), length(dates))
  } else{
    file2keep$Time <- rep(seq(0, 23, 1), length(dates))
  }
  foo <- paste0(file2keep$Date, " ", sprintf("%02d", floor(file2keep$Time)), ":00")
  if(numdailyobs == 48) foo[seq(2, length(foo), 2)] <- gsub(":00", ":30", foo[seq(2, length(foo), 2)])
  file2keep$DateTime <- as.POSIXlt( foo, format = "%Y-%m-%d %H:%M")
  row.names(file2keep) <- NULL
  
  # Plot to see and pick the exact time of the switch
  obs.week1 <- which(oldfile2keep$Date == ddate)[1]
  obs.week1 <- seq(from = obs.week1 - numdailyobs, length.out = (numdailyobs*3), by = 1)
  obs.week1 <- intersect(obs.week1, obs.week1[1]:length(oldfile2keep$Temp))
  if(length(newfile2keep$Date[newfile2keep$Date == ddate]) > 1) obs.week2 <- which(newfile2keep$Date == ddate)[1] else obs.week2 <- 1
  obs.week2 <- seq(from = obs.week2, length.out = (numdailyobs*2), by = 1)
  
  while(is.null(last.obs)){
    xlm <- c((oldfile2keep$DateTime[oldfile2keep$Date == ddate][1] - (numdailyobs * 2500)), (oldfile2keep$DateTime[oldfile2keep$Date == ddate][1] + (numdailyobs * 2500)))
    plot(oldfile2keep$DateTime[obs.week1], oldfile2keep$Temp[obs.week1], xlab = "Observation", ylab = "Temperature (C)", 
         main = paste("Pick the LAST obs from the OLD timeseries near transition date ", as.character(ddate)), ylim = c(0,30), 
         xlim = xlm)
    points(newfile2keep$DateTime[obs.week2], newfile2keep$Temp[obs.week2], col = 2)
    obs <- identify(oldfile2keep$DateTime[obs.week1], oldfile2keep$Temp[obs.week1], n = 1)
    points(oldfile2keep$DateTime[obs.week1[1] + obs - 1], oldfile2keep$Temp[obs.week1[1] + obs - 1], pch = 19, cex = 1.2)
    
    cat("You have selected", as.character(oldfile2keep$DateTime[obs.week1[1] + obs - 1]), "\n")
    check <- readline("Does this look correct? (y or n) ")
    if (check == 'y'){last.obs <- obs.week1[1] + obs - 1}			
  }
  (last.date <- oldfile2keep$DateTime[last.obs])
  oldfile2keep <- oldfile2keep[oldfile2keep$DateTime <= last.date,]
  
  while(is.null(first.obs)){
    xlm <- c((newfile2keep$DateTime[newfile2keep$Date == ddate][1] - (numdailyobs * 2500)), (newfile2keep$DateTime[newfile2keep$Date == ddate][1] + (numdailyobs * 2500)))
    plot(oldfile2keep$DateTime[obs.week1], oldfile2keep$Temp[obs.week1], xlab = "Observation", ylab = "Temperature (C)", 
         main = paste("Pick the FIRST obs from the NEW timeseries near transition date ", as.character(ddate)), ylim = c(0,30), 
         xlim = xlm)
    points(newfile2keep$DateTime[obs.week2], newfile2keep$Temp[obs.week2], col = 2)
    obs <- identify(newfile2keep$DateTime[obs.week2], newfile2keep$Temp[obs.week2], n = 1)
    points(newfile2keep$DateTime[obs], newfile2keep$Temp[obs], col = 2, pch = 19, cex = 1.2)
    
    cat("You have selected", as.character(newfile2keep$DateTime[obs]), "\n")
    check <- readline("Does this look correct? (y or n) ")
    if (check == 'y'){first.obs <- obs}			
  }
  if(first.obs == 1 & !ddate %in% newfile2keep$Date){
    plot(newfile2keep$DateTime[obs.week2], newfile2keep$Temp[obs.week2], xlab = "Observation", ylab = "Temperature (C)", 
         main = paste("Pick the FIRST obs from the NEW timeseries near transition date ", as.character(ddate)), ylim = c(0,30), col = 2)
    obs <- identify(newfile2keep$DateTime[obs.week2], newfile2keep$Temp[obs.week2], n = 1)
    points(newfile2keep$DateTime[obs], newfile2keep$Temp[obs], col = 2, pch = 19, cex = 1.2)
    
    cat("You have selected", as.character(newfile2keep$DateTime[obs]), "\n")
    check <- readline("Does this look correct? (y or n) ")
    if (check == 'y'){first.obs <- obs}			

  }
  (first.date <- newfile2keep$DateTime[first.obs])
  newfile2keep <- newfile2keep[newfile2keep$DateTime >= first.date,]
  plot(oldfile2keep$DateTime[obs.week1], oldfile2keep$Temp[obs.week1], xlab = "Observation", ylab = "Temperature (C)", 
       main = paste("Pick the FIRST obs from the NEW timeseries near transition date ", as.character(ddate)), ylim = c(0,30), 
       xlim = xlm)
  points(newfile2keep$DateTime[obs.week2], newfile2keep$Temp[obs.week2], col = 2)
  
  # Merge
  file2keep <- merge(file2keep[, c("DateTime", "Date", "Time")], oldfile2keep[, c("DateTime", "Temp")], by = "DateTime", all.x = T)
  file2keep <- merge(file2keep, newfile2keep[,c("DateTime", "Temp")], by = "DateTime", all.x = T)
  file2keep$Temp[!is.na(file2keep$Temp.x)] <- file2keep$Temp.x[!is.na(file2keep$Temp.x)]
  file2keep$Temp[!is.na(file2keep$Temp.y)] <- file2keep$Temp.y[!is.na(file2keep$Temp.y)]
  file2keep <- unique(file2keep)
  file2keep <- file2keep[order(file2keep$Date, file2keep$Time),]
  file2keep <- file2keep[, c("DateTime", "Date", "Time", "Temp")]
  # NAs might happen if there was a bad battery warning.

  # Plot and print diagnostics
  plot(file2keep$Date, file2keep$Temp, type = 'l', ylab = "Temperature (C)", xlab = "Date")
  print(summary(file2keep[!is.na(file2keep$Temp),]))
  cat("number of records: ", nrow(file2keep), "\n")
  cat("number of obs: ", nrow(file2keep[!is.na(file2keep$Temp),]), "\n")
  cat("summary of transition date ", as.character(ddate), ": ", "\n")
  print(summary(file2keep$Temp[file2keep$Date == ddate], na.rm = T))
  
  return(file2keep)
}

# Remove data before date of deployment
clean.deployment <- function(hobo, deploy.date = NULL)
{
  date.firstobs <- min(hobo$Date[!is.na(hobo$Temp)])
  cat("The first observation was on", as.character(date.firstobs), "\n")
  ans <- readline("Does deployment date need to be cleaned? (y or n) ")
  if(ans == "y"){
    ddate <- readline("What is the deployment date (as yyyy-mm-dd)? Enter NA if unknown: ")
    if(ddate != ""){
      deploy.date <- as.Date(ddate)
    }else{ 
      deploy.date <- NULL
    }
  
  while(is.null(deploy.date))
  {
    obs <- 1:length(hobo$Temp)
    plot(obs, hobo$Temp, ylab = "Temperature (C)", xlab = "Observation", main = "Pick an observation from deployment date", cex = 0.7)
    manual.deploy <- identify(obs, hobo$Temp, n = 1)
    cand.date <- hobo$Date[manual.deploy]
    plot.day(hobo, cand.date)
    cat("The selected date is", as.character(cand.date), "\n")
    good.choice <- readline("Is this the deployment date? (y or n) ")
    if (good.choice == 'y')
    {
      deploy.date <- cand.date	
    }			
  }
  first.obs = NULL
  while(is.null(first.obs))
  {
    day.obs <- which(hobo$Date == deploy.date)
    plot(day.obs, hobo$Temp[day.obs], xlab = "Observation number", ylab = "Temperature (C)", main = "Choose the first valid temperature", cex = 0.7)
    first.in <- identify(day.obs,hobo$Temp[day.obs], n = 1)
    first.in <- day.obs[first.in]
    new.obs <- setdiff(day.obs,day.obs[1]:(first.in - 1))
    points(new.obs,hobo$Temp[new.obs], lwd = 2, col = 2)
    cat("You have selected", hobo$Time[first.in], "\n")
    check <- readline("Does this look correct? (y or n) ")
    if (check == 'y')
    {
      first.obs <- first.in	
    }			
  }
  hobo <- hobo[first.in:length(hobo$Temp),]
  }
  return(hobo)
}

# Remove data after date of recovery
clean.recovery <- function(hobo, recover.date = NULL)
{
  date.lastobs <- max(hobo$Date[!is.na(hobo$Temp)])
  cat("The last observation was on", as.character(date.lastobs), "\n")
  ans <- readline("Does recovery date need to be cleaned? (y or n) ")
  if(ans == "y"){
    rdate <- readline("What is the recovery date (as yyyy-mm-dd)? Enter NA if unknown: ")
    if(rdate != ""){
      recover.date <- as.Date(rdate)
    }else{ 
      recover.date <- NULL
    }
  
  while(is.null(recover.date))
  {
    obs <- 1:length(hobo$Temp)
    plot(obs, hobo$Temp, ylab = "Temperature (C)", xlab = "Observation", main = "Pick an observation from recovery date", cex = 0.7)
    manual.recov <- identify(obs, hobo$Temp, n = 1)
    cand.date <- hobo$Date[manual.recov]
    plot.day(hobo, cand.date)
    cat("The selected date is",as.character(cand.date),"\n")
    good.choice = readline("Is this the recovery date? (y or n) ")
    if (good.choice == 'y')
    {
      recover.date = cand.date	
    }			
  }
  last.obs <- NULL	
  while(is.null(last.obs))
  {
    day.obs <- which(hobo$Date == recover.date)
    plot(day.obs, hobo$Temp[day.obs], xlab = "Observation number", ylab = "Temperature (C)", main = "Choose the last valid temperature", cex = 0.7)
    last.in <- identify(day.obs,hobo$Temp[day.obs], n = 1)
    last.in <- day.obs[last.in]
    new.obs <- setdiff(day.obs, (last.in + 1):day.obs[length(day.obs)])
    points(new.obs,hobo$Temp[new.obs], lwd = 2, col = 2)
    cat("You have selected",hobo$Time[last.in],"\n")
    check <- readline("Does this look correct? (y or n) ")
    if (check == 'y')
    {
      last.obs <- last.in	
    }			
  }
  hobo <- hobo[1:last.obs,]
  }
  return(hobo)	
}

# Clean middle stretches and specific values; calls 'clean.range' and 'remove.points'
clean.middle <- function(hobo, thedirectory = NA)
{
  ans <- readline("Do any data in the middle need to be cleaned? (y or n) ")
  if(ans == "y"){ 
    ans <- readline("Clip a whole section out? (y or n) ") 
    while(ans == "y"){  #helps clip out places that obviously came out of the water
      if(!is.na(thedirectory)) print(dir(thedirectory))
      ans <- as.numeric(readline("Choose a nearby location that has already been cleaned (1, 2, 3, ...) or use 0 if no sites qualify. "))
      if(ans > 0){ # use a nearby site to help see if the 'anomaly' is reasonable
        near.file <- dir(thedirectory)[ans]
        nearby.site <- read.csv(paste0(thedirectory, "/", near.file), header = T, stringsAsFactors = F)
        #colnames(nearby.site)[3] <- "Temp"
        hobo <- clean.range(hobo, close.site = nearby.site) 
      } else{ # no nearby site
        hobo <- clean.range(hobo)
      }
      ans <- readline("Do any other sections need to be removed? (y or n) ") 
    }
    ans <- readline("Remove specific values? (y or n) ")
    while(ans == "y"){
      hobo <- remove.points(hobo)
      ans <- readline("Do any other specific values need to be removed? (y or n) ")
    }
  }
  return(hobo)
}

# Clean a range of values, aided by plots of a nearby site if applicable
clean.range <- function(hobo, start.date = NULL, end.date = NULL, close.site = NULL)
{
  while(is.null(start.date))
  {
    obs <- 1:length(hobo$Temp)
    plot(obs, hobo$Temp, ylab = "Temperature", xlab = "Observation", main = "Pick an observation from before bad data")
    if(is.null(close.site) == F)
    {
      lines(close.site$Temp, col = 'red')
    }
    manual.deploy <- identify(obs, hobo$Temp, n = 1)
    cand.date <- hobo$Date[manual.deploy]
    plot.week(hobo, cand.date)
    if(is.null(close.site) == F)
    {
      fr.week <- which(close.site$Date == cand.date)[1]
      fr.week <- seq(from = fr.week, length.out = (numdailyobs * 7), by = 1)
      fr.week <- intersect(fr.week, fr.week[1]:length(close.site$Temp))
      lines(1:length(fr.week), close.site$Temp[fr.week], col = 'red')
    }
    cat("The selected date is", as.character(cand.date), "\n")
    good.choice <- readline("Is this include the bad data date? (y or n) ")
    if (good.choice == 'y')
    {
      start.date <- cand.date  
    }			
  }
  first.obs <- NULL
  while(is.null(first.obs))
  {
    obs.week <- which(hobo$Date == start.date)[1]
    obs.week <- seq(from = obs.week, length.out = (numdailyobs * 7), by = 1)
    obs.week <- intersect(obs.week, obs.week[1]:length(hobo$Temp))
    plot(obs.week, hobo$Temp[obs.week], xlab = "Observation number", ylab = "Temperature (C)", main = "Choose the first invalid temperature")
    if(is.null(close.site) == F)
    {
      fr.week <- which(close.site$Date == cand.date)[1]
      fr.week <- seq(from = fr.week, length.out = (numdailyobs * 7), by = 1)
      fr.week <- intersect(fr.week, fr.week[1]:length(close.site$Temp))
      lines(obs.week, close.site$Temp[fr.week], col = 'red')
    }
    first.in <- identify(obs.week, hobo$Temp[obs.week], n = 1)
    first.in <- obs.week[first.in]
    new.obs <- setdiff(obs.week, obs.week[1]:(first.in - 1))
    points(new.obs ,hobo$Temp[new.obs], lwd = 2, col = 2)
    cat("You have selected", hobo$Time[first.in], "\n")
    check <- readline("Does this look correct? (y or n) ")
    if (check == 'y')
    {
      first.obs <- first.in	
    }			
  }
  while(is.null(end.date))
  {
    obs <- 1:length(hobo$Temp)
  plot(obs, hobo$Temp, ylab = "Temperature (C)", xlab = "Observation", main = "Pick an observation from before good data")
    if(is.null(close.site) == F)
    {
      lines(close.site$Temp, col = 'red')
    }
    manual.recov <- identify(obs, hobo$Temp, n = 1)
    cand.date <- hobo$Date[manual.recov]
    plot.week(hobo, cand.date)
    if(is.null(close.site) == F)
    {
      fr.week <- which(close.site$Date == cand.date)[1]
      fr.week <- seq(from = fr.week, length.out = (numdailyobs * 7), by = 1)
      fr.week <- intersect(fr.week, fr.week[1]:length(close.site$Temp))
      lines(1:length(fr.week), close.site$Temp[fr.week], col = 'red')
    }
    cat("The selected date is", cand.date, "\n")
    good.choice <- readline("Is this the good data date? (y or n) ")
    if (good.choice == 'y')
    {
      end.date <- cand.date  
    }			
  }
  last.obs <- NULL	
  while(is.null(last.obs))
  {
    obs.week <- which(hobo$Date == end.date)[1]
    obs.week <- seq(from = obs.week, length.out = (numdailyobs * 7), by = 1)
    obs.week <- intersect(obs.week, obs.week[1]:length(hobo$Temp))
    plot(obs.week, hobo$Temp[obs.week], xlab = "Observation number", ylab = "Temperature (C)", main = "Choose the last invalid temperature")
    if(is.null(close.site) == F)
    {
      fr.week <- which(close.site$Date == cand.date)[1]
      fr.week <- seq(from = fr.week, length.out = (numdailyobs * 7), by = 1)
      fr.week <- intersect(fr.week, fr.week[1]:length(close.site$Temp))
      lines(obs.week, close.site$Temp[fr.week], col = 'red')
    }
    last.in <- identify(obs.week, hobo$Temp[obs.week], n = 1)
    last.in <- obs.week[last.in]
    
    new.obs <- setdiff(obs.week, (last.in + 1):obs.week[length(obs.week)])
    points(new.obs, hobo$Temp[new.obs], lwd = 2, col = 2)
    cat("You have selected", hobo$Time[last.in], "\n")
    check <- readline("Does this look correct? (y or n) ")
    if (check == 'y')
    {
      last.obs <- last.in	
    }			
  }
  hobo$Temp[first.in:last.in] <- rep(NA, length(first.in:last.in))
  plot.hobo(hobo)
  if(is.null(close.site) == F)
  {
    lines(close.site$Temp, col = 'red')
  }
  return(hobo)
}

# Remove specific points
remove.points <- function(hobo)
{
  obs <- 1:length(hobo$Temp)
  plot(hobo$Temp, main = "Click on the points you want to remove")
  npoints <- as.numeric(readline("How many points do you want to remove? (enter a number greater than you'll need). "))
  data2rmv <- NULL
  i <- 1
  while(i < npoints){
    point2rmv <- identify(obs, hobo$Temp, n = 1)
    data2rmv <- c(data2rmv, point2rmv)
    i <- i + 1
  }
  data2rmv <- unique(data2rmv)
  
  hobo$Temp[data2rmv] <- rep(NA, length(data2rmv))
  plot.hobo(hobo)
  return(hobo)
}


# Lookup values using named vectors:
get_value <- function(mykey, mylookupvector)
{
  myvalue <- mylookupvector[mykey]
  myvalue <- unname(myvalue)
  return(myvalue)
}

# No longer used:

# Read in file
read.hobo <- function(filename)
{
  data <- read.csv(filename, header = T, colClasses = c("character", "character", "numeric"))
  names(data) <- c("Date", "Time", "Temp")
  data <- remove.na(data)
  plot.hobo(data)
  return(data)
}

# Remove NAs
remove.na <- function(hobo)
{
  hobo2 <- hobo[which(is.na(hobo$Temp) == FALSE),]
  ###check obs/day except on enddates
  dates <- unique(hobo2$Date)
  end.dates <- c(hobo2$Date[1], hobo2$Date[length(hobo2$Temp)])
  dates.check <- setdiff(dates, end.dates)
  for(i in dates.check)
  {
    if(length(which(hobo2$Date == i)) != numdailyobs)
    {
      cat("There is an error on day", i, "\n")
      cat("Returning original hobo \n")
      return(hobo)	
    }	
  }	
  return(hobo2)
}

# Save as a .csv file
write.hobo <- function(hoboname, filename)
{
  write.csv(hoboname, filename, row.names = FALSE)
}

