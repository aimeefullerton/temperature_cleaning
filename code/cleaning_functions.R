# Functions for cleaning temperature data from loggers
# Aimee H Fullerton, Last updated 30 Jan 2023
# Adapted from Colin Sowder's original functions

# Get number of observations per day
numdailyobs <- get("numdailyobs")

detect.date.format <- function(testdate)
{
  clock_24h <- get("clock_24h")
  date.format <- NULL
  
  a <- substr(as.POSIXlt(testdate, origin = "1970-01-01", format = "%m/%d/%Y"), 1, 2)
  if(is.na(a)) a <- substr(as.POSIXlt(testdate, origin = "1970-01-01", format = "%Y-%m-%d"), 1, 4)
  a <- as.numeric(a)
  if(!is.na(a)){
    if(a > 1900) date.format <- "%Y-%m-%d"
    if(a < 19) date.format <- "%m/%d/%y" 
    if(a > 19 & a < 1900) date.format <- "%m/%d/%Y"
  }
  
  if(nchar(testdate) > 14){
    if(clock_24h == T){
      date.format = paste(date.format, "%H:%M:%S")
    } else {
      date.format = paste(date.format, "%I:%M:%S %p")
    }
  }
  
  if(nchar(testdate) >=12 & nchar(testdate) <= 14){
    if(clock_24h == T){
      date.format = paste(date.format, "%H:%M")
    } else {
      date.format = paste(date.format, "%I:%M %p")
    }
  }

  return(date.format)
}

# Read in and prepare file; flexible format
prepare.file <- function(data.file, directory)
{
  worked <- NULL
  worked <- tryCatch(
    read.csv(paste0(directory, "/", data.file)), 
    error = function(error_condition) {return(NULL)}
  )
  if(length(worked) > 0 & length(setdiff(colnames(worked), c("DateTime", "Date", "Time", "Temp"))) == 0){
    type <- "preprocessed"
  } else { 
    type <- "raw"
  }
  
  #raw Onset files
  if(type == "raw"){
    td <- read.csv(paste0(directory, "/", data.file), skip = 1, header = T, stringsAsFactors = F)[,2:3]
    colnames(td) = c("DateTime","Temp")
    date.format <- detect.date.format(td$DateTime[1])
    td$DateTime <- as.POSIXlt(td$DateTime, origin = "1970-01-01", format = date.format)
    td$Date <- as.Date(td$Date)
    td$Time <- td$DateTime$hour
    #if(numdailyobs == 48){
    #  evens <- seq(2, length(td$Time),2)
    #  td$Time[evens] <- td$Time[evens] + 0.5
    #}
  }
  
  #pre-processed files
  if(type == "preprocessed") {
    td <- read.csv(paste0(directory, "/", data.file), header = T, stringsAsFactors = F) 
    td$Date <- as.Date(td$Date)
    td$DateTime <- as.POSIXlt(td$DateTime, origin = "1970-01-01", format = "%Y-%m-%d %H:%M")
  }
  
  td <- td[!is.na(td$DateTime),]
  td <- td[!is.na(td$Temp),] # remove any records of bad battery etc. important step!
  td <- td[order(td$DateTime),]
  td <- td[, c("DateTime", "Date", "Time", "Temp")]
  
  return(td)
}

# Read in and prepare file saved as MS Excel (ie MX bluetooth loggers)
prepare.xls.file <- function(data.file, directory)
{
  td <- read_xlsx(paste0(data.dir, "/", raw.data.folder, "/", data.file))[,c(2:3)]
  colnames(td) = c("DateTime","Temp")
  
  date.format <- detect.date.format(td$DateTime[1])
  td$DateTime <- as.POSIXlt(td$DateTime, origin = "1970-01-01", format = date.format, tz = Sys.timezone(location = TRUE)) #tz = "PST")
  td$Date <- as.Date(td$DateTime)
  td$Time <- td$DateTime$hour
  td <- td[!is.na(td$DateTime),]
  td <- td[!is.na(td$Temp),] # remove any records of bad battery etc. important step!
  td <- td[order(td$DateTime),]
  td <- td[, c("DateTime", "Date", "Time", "Temp")]
  
  return(td)
}

# Round timestamp (if it wasn't launched on the hour)
# see also round.POSIXt() - same result but much slower
round.timestamp <- function(testdate){
  
  date <- strsplit(as.character(testdate), " ")[[1]][1]
  time <- strsplit(as.character(testdate), " ")[[1]][2]
  hour <- as.numeric(strsplit(time, ":")[[1]][1])
  min <- as.numeric(strsplit(time, ":")[[1]][2])
  sec <- as.numeric(strsplit(time, ":")[[1]][3])
  
  if(sec/60 < 0.5) {sec <- 0}
  if(min/60 < 0.5) {min <- 0}
  if(sec/60 >= 0.5) {min <- min + 1; sec <- 0}
  if(min/60 >= 0.5) {hour <- hour + 1; min <- 0}
  
  newdate <- as.POSIXlt(paste0(date, " ", hour, ":00:00"), format = "%Y-%m-%d %H:%M:%S")

  return(newdate)
  
}

# Plot time series
plot.logger <- function(logger, type = "l")
{
  plot(logger$DateTime, logger$Temp, xlab = "Date", ylab = "Temperature (C)", type = type, cex = 0.3)
}

# Do some automated checking
check.logger <- function(logger, response = "n")
{
  ##first check for min
  if (min(logger$Temp, na.rm = T) < -1)
  {
    stop <- TRUE
    i <- 1
    while(stop)
    {
      if(!is.na(logger$Temp[i]) & logger$Temp[i] < -1)
      {
        day <- logger$Date[i]
        cat("The logger was below -1 first on ", as.character(day), "\n")
        stop <- FALSE	
      }
      i <- i + 1
    }
  }
  else
  {
    cat("The logger was never below the minimum threshold \n")
  }
  
  ##check for max threshold (25)
  if (max(logger$Temp > 25, na.rm = T))
  {
    stop = TRUE
    i <- 1
    while(stop)
    {
      if(!is.na(logger$Temp[i]) & logger$Temp[i] > 25)
      {
        day <- logger$Date[i]
        cat("The logger was above 25 first on ", as.character(day), "\n")
        stop <- FALSE	
      }
      i <- i + 1
    }
  }
  else
  {
    cat("The logger was never above the maximum threshold \n")
  }
  
  ###remind the user to check the notes
  response <- readline("Are there any notes that the logger was out of the water? ")
  if (response == 'y')
  {
    cat("Consult the notes \n")
  }
  else
  {
    cat("There are no notes indicating the logger was out of the water \n")
  }
  
  ###check the variance, for right now let's look at the smaller  of two day differences?
  temp <- logger$Temp[!is.na(logger$Temp)]
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
      when <- logger$Date[i]		
    }	
  }
  
  cat("The largest two day difference began on ", as.character(when), "and was ", bsmdiff, "degrees \n")
  cat("The first observation was on ", as.character(min(logger$Date[!is.na(logger$Temp)])),"at ", min(logger$Time[logger$Date == max(logger$Date[!is.na(logger$Temp)])]),"\n")
  cat("The last observation was on ", as.character(max(logger$Date[!is.na(logger$Temp)])),"at ", max(logger$Time[logger$Date == max(logger$Date[!is.na(logger$Temp)])]), "\n")
}

# Plot time series over a single day
plot.day <- function(logger, day)
{
  Temps <- logger$Temp[which(logger$Date == day)]
  obs <- 1:length(Temps)
  plot(obs, Temps, xlab = "Observation", ylab = "Temperature (C)", main = paste("Temperatures on ", as.character(day)))
}

# Plot time series over a week
plot.week <- function(logger, day)
{
  obs.week <- which(logger$Date == day)[1]
  obs.week <- seq(from = obs.week, length.out = (numdailyobs * 7), by = 1)
  obs.week <- intersect(obs.week, obs.week[1]:length(logger$Temp))
  Temps <- logger$Temp[obs.week]
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
clip.single.file <- function(myfile, first.year, numdailyobs, date.begin = "-09-01", date.end = "-08-31", echo = F)
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
  if(echo == T){
    plot(file2keep$DateTime, file2keep$Temp, type = 'l', ylab = "Temperature (C)", xlab = "Date")
    print(summary(file2keep[!is.na(file2keep$Temp),]))
    cat("number of records: ", nrow(file2keep), "\n")
    cat("number of obs: ", nrow(file2keep[!is.na(file2keep$Temp),]), "\n")
  }
  
  return(file2keep)
}

# Stitch together raw data from this year and raw data from previous September
get.complete.year <- function(oldfile2keep, newfile2keep, first.year, numdailyobs, date.begin = "-09-01", date.end = "-08-31")
{
  # Create empty dataframe with all dates/times
  dates <- seq(from = as.Date(paste0(first.year, date.begin)), to = as.Date(paste0(first.year + 1, date.end)), by = 1)
  file2keep <- data.frame(matrix(NA, nrow = length(dates) * numdailyobs, ncol = 3))
  colnames(file2keep) <- c("Date", "Time", "Temp")
  file2keep$Date <- rep(dates, numdailyobs)
  file2keep <- file2keep[order(file2keep$Date),]
  if(numdailyobs == 48){
    file2keep$Time <- rep(seq(0, 23.5, 0.5), length(dates))
  } else if(numdailyobs == 24){
    file2keep$Time <- rep(seq(0, 23, 1), length(dates))
  }
  foo <- paste0(file2keep$Date, " ", sprintf("%02d", floor(file2keep$Time)), ":00")
  if(numdailyobs == 48) foo[seq(2, length(foo), 2)] <- gsub(":00", ":30", foo[seq(2, length(foo), 2)])
  file2keep$DateTime <- as.POSIXlt( foo, format = "%Y-%m-%d %H:%M")
  row.names(file2keep) <- NULL
  
  # Pick transition date for stitching together
  ddate <- readline("What is the transition date (as yyyy-mm-dd)? Leave blank and hit enter if unknown: ")
  if(ddate != ""){
    ddate <- as.Date(ddate)
  } else {
    ddate <- max(oldfile2keep$Date) #if not known, assume the old logger was stopped when it was pulled out
  }
  cat("You have selected", as.character(ddate), "\n")
  check <- readline("Is this correct? (y or n) ")
  if (check == 'n'){
    ddate <- readline("Enter a preferred transition date (as yyyy-mm-dd)?: ")
    ddate <- as.Date(ddate)
  }			
  
  
  # Clip old and new datasets to correct dates
  oldfile2keep <- oldfile2keep[oldfile2keep$Date >= as.Date(paste0(first.year, date.begin)) & oldfile2keep$Date <= ddate,]
  oldfile2keep <- unique(oldfile2keep)
  newfile2keep <- newfile2keep[newfile2keep$Date >= ddate & newfile2keep$Date <= as.Date(paste0(first.year + 1, date.end)),]
  newfile2keep <- unique(newfile2keep)

  last.obs <- NULL
  first.obs <- NULL
  # Error handling
  if(nrow(oldfile2keep) > 0) { #only process if this file has records
    if(!ddate %in% oldfile2keep$Date){
      last.obs <- which(oldfile2keep$DateTime == max(oldfile2keep$DateTime, na.rm = T))
      #stop("Error: The selected date does not occur in the first dataset. Examine data to see if there is a big gap.")
    }
    if(!ddate %in% newfile2keep$Date){
      first.obs <- which(newfile2keep$DateTime == min(newfile2keep$DateTime, na.rm = T))
      #stop("Error: The selected date does not occur in the second dataset. Examine data to see if there is a big gap.")
    } 
  
  # Plot to see and pick the exact time of the switch
  obs.week1 <- which(oldfile2keep$Date == ddate)[1]
  obs.week1 <- seq(from = obs.week1 - numdailyobs, length.out = (numdailyobs*3), by = 1)
  obs.week1 <- intersect(obs.week1, obs.week1[1]:length(oldfile2keep$Temp))
  if(length(newfile2keep$Date[newfile2keep$Date == ddate]) > 1) obs.week2 <- which(newfile2keep$Date == ddate)[1] else obs.week2 <- 1
  obs.week2 <- seq(from = obs.week2, length.out = (numdailyobs*2), by = 1)
  
  while(is.null(last.obs)){
    xlm <- c((oldfile2keep$DateTime[oldfile2keep$Date == ddate][1] - (numdailyobs * 2500)), (oldfile2keep$DateTime[oldfile2keep$Date == ddate][1] + (numdailyobs * 2500)))
    plot(oldfile2keep$DateTime[obs.week1], oldfile2keep$Temp[obs.week1], xlab = "Observation", ylab = "Temperature (C)", 
         main = paste("Pick the LAST obs from the OLD timeseries near transition date ", as.character(ddate)), ylim = c(0,35), 
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
         main = paste("Pick the FIRST obs from the NEW timeseries near transition date ", as.character(ddate)), ylim = c(0,35), 
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
         main = paste("Pick the FIRST obs from the NEW timeseries near transition date ", as.character(ddate)), ylim = c(0,35), col = 2)
    points(newfile2keep$DateTime[obs.week2], newfile2keep$Temp[obs.week2], col = 2)
    obs <- identify(newfile2keep$DateTime[obs.week2], newfile2keep$Temp[obs.week2], n = 1)
    points(newfile2keep$DateTime[obs], newfile2keep$Temp[obs], col = 2, pch = 19, cex = 1.2)
    
    cat("You have selected", as.character(newfile2keep$DateTime[obs]), "\n")
    check <- readline("Does this look correct? (y or n) ")
    if (check == 'y'){first.obs <- obs}			

  }
  (first.date <- newfile2keep$DateTime[first.obs])
  newfile2keep <- newfile2keep[newfile2keep$DateTime >= first.date,]
  plot(oldfile2keep$DateTime[obs.week1], oldfile2keep$Temp[obs.week1], xlab = "Observation", ylab = "Temperature (C)", 
       main = paste("Pick the FIRST obs from the NEW timeseries near transition date ", as.character(ddate)), ylim = c(0,35), 
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
  
  } else {
    first.date <- newfile2keep$DateTime[1]
    newfile2keep <- newfile2keep[newfile2keep$DateTime >= first.date,]
    file2keep <- merge(file2keep, newfile2keep[,c("DateTime", "Temp")], by = "DateTime", all.x = T)
    file2keep$Temp[!is.na(file2keep$Temp.x)] <- file2keep$Temp.x[!is.na(file2keep$Temp.x)]
    file2keep$Temp[!is.na(file2keep$Temp.y)] <- file2keep$Temp.y[!is.na(file2keep$Temp.y)]
    file2keep <- unique(file2keep)
    file2keep <- file2keep[order(file2keep$Date, file2keep$Time),]
    file2keep <- file2keep[, c("DateTime", "Date", "Time", "Temp")]
  } #END empty oldfile2keep
  
  
  # Plot and print diagnostics
  plot(file2keep$DateTime, file2keep$Temp, type = 'l', ylab = "Temperature (C)", xlab = "Date")
  abline(v = as.Date(paste0(first.year, date.begin)), lty = 2)
  abline(v = as.Date(paste0(first.year + 1, date.end)), lty = 2)
  
  print(summary(file2keep[!is.na(file2keep$Temp),]))
  cat("number of records: ", nrow(file2keep), "\n")
  cat("number of obs: ", nrow(file2keep[!is.na(file2keep$Temp),]), "\n")
  cat("summary of transition date ", as.character(ddate), ": ", "\n")
  print(summary(file2keep$Temp[file2keep$Date == ddate], na.rm = T))
  
  return(file2keep)
}

# Stitch together raw data from this year and raw data from previous fall
# (helpful if data were downloaded before the end of the water year)
backfill.previous.fall <- function(file2update, file2copyfrom, theyear, numdailyobs, date.begin = "-09-01", date.end = "-08-31")
{
  # Create empty dataframe with all dates/times
  dates <- seq(from = as.Date(paste0(theyear, date.begin)), to = as.Date(paste0(theyear + 1, date.end)), by = 1)
  file2keep <- data.frame(matrix(NA, nrow = length(dates) * numdailyobs, ncol = 3))
  colnames(file2keep) <- c("Date", "Time", "Temp")
  file2keep$Date <- rep(dates, numdailyobs)
  file2keep <- file2keep[order(file2keep$Date),]
  if(numdailyobs == 48){
    file2keep$Time <- rep(seq(0, 23.5, 0.5), length(dates))
  } else if(numdailyobs == 24){
    file2keep$Time <- rep(seq(0, 23, 1), length(dates))
  }
  foo <- paste0(file2keep$Date, " ", sprintf("%02d", floor(file2keep$Time)), ":00")
  if(numdailyobs == 48) foo[seq(2, length(foo), 2)] <- gsub(":00", ":30", foo[seq(2, length(foo), 2)])
  file2keep$DateTime <- as.POSIXlt( foo, format = "%Y-%m-%d %H:%M")
  row.names(file2keep) <- NULL
  
  # Pick transition date for stitching together
  ddate <- readline("What is the transition date (as yyyy-mm-dd)? Leave blank and hit enter if unknown: ")
  if(ddate != ""){
    ddate <- as.Date(ddate)
  } else {
    ddate <- max(file2update$Date[!is.na(file2update$Temp)]) #if not known, assume the old logger was stopped when it was pulled out
  }
  cat("You have selected", as.character(ddate), "\n")
  check <- readline("Is this correct? (y or n) ")
  if (check == 'n'){
    ddate <- readline("Enter a preferred transition date (as yyyy-mm-dd)?: ")
    ddate <- as.Date(ddate)
  }			
  
  
  # Clip old and new datasets to correct dates
  file2update <- file2update[file2update$Date <= ddate,]
  file2copyfrom <- file2copyfrom[file2copyfrom$Date >= ddate & file2copyfrom$Date <= as.Date(paste0((theyear + 1), date.end)),]
  
  last.obs <- NULL
  first.obs <- NULL
  # Error handling
  if(nrow(file2copyfrom) > 0) { #only process if this file has records
    if(!ddate %in% file2update$Date){
      last.obs <- which(file2update$DateTime == max(file2update$DateTime, na.rm = T))
    }
    if(!ddate %in% file2copyfrom$Date){
      first.obs <- which(file2copyfrom$DateTime == min(file2copyfrom$DateTime, na.rm = T))
    } 
    
    # Plot to see and pick the exact time of the switch
    obs.week1 <- which(file2update$Date == ddate)[1]
    obs.week1 <- seq(from = obs.week1 - numdailyobs, length.out = (numdailyobs*3), by = 1)
    obs.week1 <- intersect(obs.week1, obs.week1[1]:length(file2update$Temp))
    if(length(file2copyfrom$Date[file2copyfrom$Date == ddate]) > 1) obs.week2 <- which(file2copyfrom$Date == ddate)[1] else obs.week2 <- 1
    obs.week2 <- seq(from = obs.week2, length.out = (numdailyobs*2), by = 1)
    
    while(is.null(last.obs)){
      xlm <- c((file2update$DateTime[file2update$Date == ddate][1] - (numdailyobs * 2500)), (file2update$DateTime[file2update$Date == ddate][1] + (numdailyobs * 2500)))
      plot(file2update$DateTime[obs.week1], file2update$Temp[obs.week1], xlab = "Observation", ylab = "Temperature (C)", 
           main = paste("Pick the LAST obs from the OLD timeseries near transition date ", as.character(ddate)), ylim = c(0,35), 
           xlim = xlm)
      points(file2copyfrom$DateTime[obs.week2], file2copyfrom$Temp[obs.week2], col = 2)
      obs <- identify(file2update$DateTime[obs.week1], file2update$Temp[obs.week1], n = 1)
      points(file2update$DateTime[obs.week1[1] + obs - 1], file2update$Temp[obs.week1[1] + obs - 1], pch = 19, cex = 1.2)
      
      cat("You have selected", as.character(file2update$DateTime[obs.week1[1] + obs - 1]), "\n")
      check <- readline("Does this look correct? (y or n) ")
      if (check == 'y'){last.obs <- obs.week1[1] + obs - 1}			
    }
    (last.date <- file2update$DateTime[last.obs])
    file2update <- file2update[file2update$DateTime <= last.date,]
    
    while(is.null(first.obs)){
      xlm <- c((file2copyfrom$DateTime[file2copyfrom$Date == ddate][1] - (numdailyobs * 2500)), (file2copyfrom$DateTime[file2copyfrom$Date == ddate][1] + (numdailyobs * 2500)))
      plot(file2update$DateTime[obs.week1], file2update$Temp[obs.week1], xlab = "Observation", ylab = "Temperature (C)", 
           main = paste("Pick the FIRST obs from the NEW timeseries near transition date ", as.character(ddate)), ylim = c(0,35), 
           xlim = xlm)
      points(file2copyfrom$DateTime[obs.week2], file2copyfrom$Temp[obs.week2], col = 2)
      obs <- identify(file2copyfrom$DateTime[obs.week2], file2copyfrom$Temp[obs.week2], n = 1)
      points(file2copyfrom$DateTime[obs], file2copyfrom$Temp[obs], col = 2, pch = 19, cex = 1.2)
      
      cat("You have selected", as.character(file2copyfrom$DateTime[obs]), "\n")
      check <- readline("Does this look correct? (y or n) ")
      if (check == 'y'){first.obs <- obs}			
    }
    if(first.obs == 1 & !ddate %in% file2copyfrom$Date){
      plot(file2copyfrom$DateTime[obs.week2], file2copyfrom$Temp[obs.week2], xlab = "Observation", ylab = "Temperature (C)", 
           main = paste("Pick the FIRST obs from the NEW timeseries near transition date ", as.character(ddate)), ylim = c(0,35), col = 2)
      obs <- identify(file2copyfrom$DateTime[obs.week2], file2copyfrom$Temp[obs.week2], n = 1)
      points(file2copyfrom$DateTime[obs], file2copyfrom$Temp[obs], col = 2, pch = 19, cex = 1.2)
      
      cat("You have selected", as.character(file2copyfrom$DateTime[obs]), "\n")
      check <- readline("Does this look correct? (y or n) ")
      if (check == 'y'){first.obs <- obs}			
      
    }
    (first.date <- file2copyfrom$DateTime[first.obs])
    file2copyfrom <- file2copyfrom[file2copyfrom$DateTime >= first.date,]
    plot(file2update$DateTime[obs.week1], file2update$Temp[obs.week1], xlab = "Observation", ylab = "Temperature (C)", 
         main = paste("Pick the FIRST obs from the NEW timeseries near transition date ", as.character(ddate)), ylim = c(0,35), 
         xlim = xlm)
    points(file2copyfrom$DateTime[obs.week2], file2copyfrom$Temp[obs.week2], col = 2)
    
    # Merge
    file2keep <- merge(file2keep[, c("DateTime", "Date", "Time")], file2update[, c("DateTime", "Temp")], by = "DateTime", all.x = T)
    file2keep <- merge(file2keep, file2copyfrom[,c("DateTime", "Temp")], by = "DateTime", all.x = T)
    file2keep$Temp[!is.na(file2keep$Temp.x)] <- file2keep$Temp.x[!is.na(file2keep$Temp.x)]
    file2keep$Temp[!is.na(file2keep$Temp.y)] <- file2keep$Temp.y[!is.na(file2keep$Temp.y)]
    file2keep <- unique(file2keep)
    file2keep <- file2keep[order(file2keep$Date, file2keep$Time),]
    file2keep <- file2keep[, c("DateTime", "Date", "Time", "Temp")]
    # NAs might happen if there was a bad battery warning.
    
  } else {
    cat("There are no data for the missing period in the new year's file. Keeping existing file.", "\n")
    file2keep <- file2update
  } #END empty file2update
  
  
  # Plot and print diagnostics
  plot(file2keep$DateTime, file2keep$Temp, type = 'l', ylab = "Temperature (C)", xlab = "Date")
  abline(v = as.Date(paste0(theyear, date.begin)), lty = 2)
  abline(v = as.Date(paste0(first.year, date.end)), lty = 2)
  
  print(summary(file2keep[!is.na(file2keep$Temp),]))
  cat("number of records: ", nrow(file2keep), "\n")
  cat("number of obs: ", nrow(file2keep[!is.na(file2keep$Temp),]), "\n")
  cat("summary of transition date ", as.character(ddate), ": ", "\n")
  print(summary(file2keep$Temp[file2keep$Date == ddate], na.rm = T))
  
  return(file2keep)
}

# Remove data before date of deployment
clean.deployment <- function(logger, deploy.date = NULL)
{
  date.firstobs <- min(logger$Date[!is.na(logger$Temp)])
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
    obs <- 1:length(logger$Temp)
    plot(obs, logger$Temp, ylab = "Temperature (C)", xlab = "Observation", main = "Pick an observation from deployment date", cex = 0.7)
    manual.deploy <- identify(obs, logger$Temp, n = 1)
    cand.date <- logger$Date[manual.deploy]
    plot.day(logger, cand.date)
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
    day.obs <- which(logger$Date == deploy.date)
    plot(day.obs, logger$Temp[day.obs], xlab = "Observation number", ylab = "Temperature (C)", main = "Choose the first valid temperature", cex = 0.7)
    first.in <- identify(day.obs,logger$Temp[day.obs], n = 1)
    first.in <- day.obs[first.in]
    new.obs <- setdiff(day.obs,day.obs[1]:(first.in - 1))
    points(new.obs,logger$Temp[new.obs], lwd = 2, col = 2)
    cat("You have selected", logger$Time[first.in], "\n")
    check <- readline("Does this look correct? (y or n) ")
    if (check == 'y')
    {
      first.obs <- first.in	
    }			
  }
  logger <- logger[first.in:length(logger$Temp),]
  }
  return(logger)
}

# Remove data after date of recovery
clean.recovery <- function(logger, recover.date = NULL)
{
  date.lastobs <- max(logger$Date[!is.na(logger$Temp)])
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
    obs <- 1:length(logger$Temp)
    plot(obs, logger$Temp, ylab = "Temperature (C)", xlab = "Observation", main = "Pick an observation from recovery date", cex = 0.7)
    manual.recov <- identify(obs, logger$Temp, n = 1)
    cand.date <- logger$Date[manual.recov]
    plot.day(logger, cand.date)
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
    day.obs <- which(logger$Date == recover.date)
    plot(day.obs, logger$Temp[day.obs], xlab = "Observation number", ylab = "Temperature (C)", main = "Choose the last valid temperature", cex = 0.7)
    last.in <- identify(day.obs,logger$Temp[day.obs], n = 1)
    last.in <- day.obs[last.in]
    new.obs <- setdiff(day.obs, (last.in + 1):day.obs[length(day.obs)])
    points(new.obs,logger$Temp[new.obs], lwd = 2, col = 2)
    cat("You have selected",logger$Time[last.in],"\n")
    check <- readline("Does this look correct? (y or n) ")
    if (check == 'y')
    {
      last.obs <- last.in	
    }			
  }
  logger <- logger[1:last.obs,]
  }
  return(logger)	
}

# Clean middle stretches and specific values; calls 'clean.range' and 'remove.points'
clean.middle <- function(logger, thedirectory = NA)
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
        logger <- clean.range(logger, close.site = nearby.site) 
      } else{ # no nearby site
        logger <- clean.range(logger)
      }
      ans <- readline("Do any other sections need to be removed? (y or n) ") 
    }
    ans <- readline("Remove specific values? (y or n) ")
    while(ans == "y"){
      logger <- remove.points(logger)
      ans <- readline("Do any other specific values need to be removed? (y or n) ")
    }
  }
  return(logger)
}

# Clean a range of values, aided by plots of a nearby site if applicable
clean.range <- function(logger, start.date = NULL, end.date = NULL, close.site = NULL)
{
  while(is.null(start.date))
  {
    obs <- 1:length(logger$Temp)
    plot(obs, logger$Temp, ylab = "Temperature", xlab = "Observation", main = "Pick an observation from before bad data")
    if(is.null(close.site) == F)
    {
      lines(close.site$Temp, col = 'red')
    }
    manual.deploy <- identify(obs, logger$Temp, n = 1)
    cand.date <- logger$Date[manual.deploy]
    plot.week(logger, cand.date)
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
    obs.week <- which(logger$Date == start.date)[1]
    obs.week <- seq(from = obs.week, length.out = (numdailyobs * 7), by = 1)
    obs.week <- intersect(obs.week, obs.week[1]:length(logger$Temp))
    plot(obs.week, logger$Temp[obs.week], xlab = "Observation number", ylab = "Temperature (C)", main = "Choose the first invalid temperature")
    if(is.null(close.site) == F)
    {
      fr.week <- which(close.site$Date == cand.date)[1]
      fr.week <- seq(from = fr.week, length.out = (numdailyobs * 7), by = 1)
      fr.week <- intersect(fr.week, fr.week[1]:length(close.site$Temp))
      lines(obs.week, close.site$Temp[fr.week], col = 'red')
    }
    first.in <- identify(obs.week, logger$Temp[obs.week], n = 1)
    first.in <- obs.week[first.in]
    new.obs <- setdiff(obs.week, obs.week[1]:(first.in - 1))
    points(new.obs ,logger$Temp[new.obs], lwd = 2, col = 2)
    cat("You have selected", logger$Time[first.in], "\n")
    check <- readline("Does this look correct? (y or n) ")
    if (check == 'y')
    {
      first.obs <- first.in	
    }			
  }
  while(is.null(end.date))
  {
    obs <- 1:length(logger$Temp)
  plot(obs, logger$Temp, ylab = "Temperature (C)", xlab = "Observation", main = "Pick an observation from before good data")
    if(is.null(close.site) == F)
    {
      lines(close.site$Temp, col = 'red')
    }
    manual.recov <- identify(obs, logger$Temp, n = 1)
    cand.date <- logger$Date[manual.recov]
    plot.week(logger, cand.date)
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
    obs.week <- which(logger$Date == end.date)[1]
    obs.week <- seq(from = obs.week, length.out = (numdailyobs * 7), by = 1)
    obs.week <- intersect(obs.week, obs.week[1]:length(logger$Temp))
    plot(obs.week, logger$Temp[obs.week], xlab = "Observation number", ylab = "Temperature (C)", main = "Choose the last invalid temperature")
    if(is.null(close.site) == F)
    {
      fr.week <- which(close.site$Date == cand.date)[1]
      fr.week <- seq(from = fr.week, length.out = (numdailyobs * 7), by = 1)
      fr.week <- intersect(fr.week, fr.week[1]:length(close.site$Temp))
      lines(obs.week, close.site$Temp[fr.week], col = 'red')
    }
    last.in <- identify(obs.week, logger$Temp[obs.week], n = 1)
    last.in <- obs.week[last.in]
    
    new.obs <- setdiff(obs.week, (last.in + 1):obs.week[length(obs.week)])
    points(new.obs, logger$Temp[new.obs], lwd = 2, col = 2)
    cat("You have selected", logger$Time[last.in], "\n")
    check <- readline("Does this look correct? (y or n) ")
    if (check == 'y')
    {
      last.obs <- last.in	
    }			
  }
  logger$Temp[first.in:last.in] <- rep(NA, length(first.in:last.in))
  plot.logger(logger)
  if(is.null(close.site) == F)
  {
    lines(close.site$Temp, col = 'red')
  }
  return(logger)
}

# Remove specific points
remove.points <- function(logger)
{
  obs <- 1:length(logger$Temp)
  plot(logger$Temp, main = "Click on the points you want to remove")
  npoints <- as.numeric(readline("How many points do you want to remove? (enter a number greater than you'll need). "))
  data2rmv <- NULL
  i <- 1
  while(i < npoints){
    point2rmv <- identify(obs, logger$Temp, n = 1)
    data2rmv <- c(data2rmv, point2rmv)
    i <- i + 1
  }
  data2rmv <- unique(data2rmv)
  
  logger$Temp[data2rmv] <- rep(NA, length(data2rmv))
  plot.logger(logger)
  return(logger)
}

# Lookup values using named vectors:
get_value <- function(mykey, mylookupvector)
{
  myvalue <- mylookupvector[mykey]
  myvalue <- unname(myvalue)
  return(myvalue)
}

# Add time stamp column
time.stamp <- function(x, seconds = F)
{
  thehour <- floor(x)
  theminutes <- substr(x%%2, 3, 3)
  theminutes[theminutes == ""] <- "00"
  theminutes[theminutes == "5"] <- "30"
  if(seconds == T) theseconds <- ":00 PDT" else theseconds <- ""
  answer <- paste0(thehour, ":", theminutes, theseconds)
  return(answer)
}

# Truncate times not on the hour
time.truncate <- function(dt)
{
  d <- lubridate::day(dt)
  m <- lubridate::month(dt)
  y <- lubridate::year(dt)
  h <- lubridate::hour(dt)
  i <- rep(0, length(dt)) #lubridate::minute(dt)
  s <- rep(0, length(dt)) #lubridate::second(dt)
  
  return(as.POSIXlt(paste0(y, "-", m, "-", d, " ", h, ":", i, ":", s)))
}

# Ensure the timeseries has a complete set of dates/times even if temperatures are NA
fill.time.series <- function(time.series, first.year, date.begin, date.end, numdailyobs)
{

time.series <- as.data.frame(time.series)

# Create empty dataframe with all dates/times
dates <- seq(from = as.Date(paste0(first.year, date.begin)), to = as.Date(paste0(first.year + 1, date.end)), by = 1)
file2keep <- data.frame(matrix(NA, nrow = length(dates) * numdailyobs, ncol = 3))
colnames(file2keep) <- c("Date", "Time", "Temp")
file2keep$Date <- rep(dates, numdailyobs)
file2keep <- file2keep[order(file2keep$Date),]
if(numdailyobs == 48){
  file2keep$Time <- rep(seq(0, 23.5, 0.5), length(dates))
} else if(numdailyobs == 24){
  file2keep$Time <- rep(seq(0, 23, 1), length(dates))
}
foo <- paste0(file2keep$Date, " ", sprintf("%02d", floor(file2keep$Time)), ":00")
if(numdailyobs == 48) foo[seq(2, length(foo), 2)] <- gsub(":00", ":30", foo[seq(2, length(foo), 2)])
file2keep$DateTime <- as.POSIXlt( foo, format = "%Y-%m-%d %H:%M")
row.names(file2keep) <- NULL
file2keep <- file2keep[!is.na(file2keep$DateTime),] #removes weird March date that is NA in this field but not really


# Merge
file2keep <- merge(file2keep[, c("DateTime", "Date", "Time")], time.series[, c("DateTime", "Temp")], by = "DateTime", all.x = T)
file2keep$Temp[!is.na(file2keep$Temp.x)] <- file2keep$Temp.x[!is.na(file2keep$Temp.x)]
file2keep$Temp[!is.na(file2keep$Temp.y)] <- file2keep$Temp.y[!is.na(file2keep$Temp.y)]
file2keep <- unique(file2keep)
file2keep <- file2keep[order(file2keep$Date, file2keep$Time),]
file2keep <- file2keep[, c("DateTime", "Date", "Time", "Temp")]

return(file2keep)
}

# Organize data as matrix with Date/Time vs Site columns (NAs where no data) and plot
create.matrix <- function(type, data.dir, cleaned.data.folder, watershed, first.year, date.begin, date.end, numdailyobs, ylm)
{
  
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
  td$DateTime <- as.POSIXlt( td$DateTime, format = "%Y-%m-%d %H:%M")
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

# sort
new.df <- new.df[order(new.df$DateTime),]

# save
write.csv(new.df, paste0(data.dir, "/Data_Cleaned_", (first.year + 1), "/", watershed, ".", type, ".", (first.year + 1), ".csv"), row.names = F)

# plot in individual panels to check
png(paste0(data.dir, "/Data_Cleaned_", (first.year + 1), "/", watershed, ".", type, ".", (first.year + 1), ".png"), width = 16, height = 10, units = "in", res = 300)
par(mfrow = c(6,8), las = 1, cex = 0.5)
for(i in 4:(ncol(new.df))){
  plot(new.df$DateTime, new.df[,i], type = 'l', ylim = ylm, main = colnames(new.df)[i], xlab = "", ylab = "")
}
dev.off()  

cat("Files created.", "\n")
}

# Merge a year with all other years into Date/Time by Site matrix ####
update.allyears <- function(type, data.dir, watershed, first.year, ylm)
{
  
yy <- first.year + 1
dat.all <- read.csv(paste0(data.dir, "/", watershed, ".", type, ".allyears_", first.year, ".csv"), header = T)
date.format <- detect.date.format(dat.all$Date[1])
dat.all$Date <- as.Date(dat.all$Date, format = date.format)
sites <- toupper(colnames(dat.all[3:ncol(dat.all)]))
colnames(dat.all) <- c("Date", "Time", sites)

dat.yy <- read.csv(paste0(data.dir, "/Data_Cleaned_", yy, "/", watershed, ".", type, ".", yy, ".csv"), header = T)
date.format <- detect.date.format(dat.yy$Date[1])
dat.yy$Date <- as.Date(dat.yy$Date, format = date.format)
for(i in 4:ncol(dat.yy)){
  cn <- colnames(dat.yy)[i]
  colnames(dat.yy)[i] <- gsub("_.*","", cn)
}

# Merge this year with previous years
thesites <- sort(intersect(sites, colnames(dat.yy)))
dat.all.merged <- matrix(NA, nrow = nrow(dat.all) + nrow(dat.yy), ncol = (length(sites) + 2))
dat.all.merged <- as.data.frame(dat.all.merged)
colnames(dat.all.merged) <- c("Date", "Time", sites)
dat.all.merged$Date <- as.Date("2001-01-01")

dat.all.merged[1:nrow(dat.all),] <- dat.all
idx <- ((nrow(dat.all) + 1) : nrow(dat.all.merged))
dat.all.merged$Date[idx] <- dat.yy$Date
dat.all.merged$Time[idx] <- dat.yy$Time

for(s in 1:length(thesites)){
  site <- thesites[s]
  dat.all.merged[idx, site] <- dat.yy[,site]
}
#summary(dat.all.merged)
#plot(dat.all.merged$DateTime, dat.all.merged[,3], type = 'l')

# sort
new.df <- dat.all.merged[order(dat.all.merged$Date, dat.all.merged$Time),]

# save
write.csv(dat.all.merged, paste0(data.dir, "/", watershed, ".", type, ".allyears_", yy, ".csv"), row.names = F)

# plot in individual panels to check
png(paste0(data.dir, "/", watershed, ".", type, ".allyears_", yy, ".png"), width = 16, height = 10, units = "in", res = 300)
par(mfrow = c(6,8), las = 1, cex = 0.5)

for(i in 3:(ncol(dat.all.merged))){
  plot(dat.all.merged$DateTime, dat.all.merged[,i], type = 'l', ylim = ylm, main = colnames(dat.all.merged)[i], xlab = "", ylab = "")
}
dev.off()  

cat("Files created.", "\n")

}

# Read in and prepare raw file exported directly from Onset
prepare.file1 <- function(data.file, directory)
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


