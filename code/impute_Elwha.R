# Elwha water temperature data imputation
# AH Fullerton, last updated 8 Feb 2021


# Setup #####
library(missMDA)

# Directories
data.dir <- "/Users/aimee_fullerton/GitHub/Elwha_ST/data"
results.dir <- "/Users/aimee_fullerton/OneDrive/Work/Research/StreamTemperature/Elwha.ST/Imputing"

# Lookup values using named vectors:
get_value <- function(mykey, mylookupvector){
  myvalue <- mylookupvector[mykey]
  myvalue <- unname(myvalue)
  return(myvalue)
}

# Load site attributes
sites.attr <- read.csv(paste0(data.dir, "/elwha.sites.attributed.csv"), header = T)
xx <- sites.attr$Site.Name; names(xx) <- sites.attr$SiteCode
# get_value(sites, xx) # look up site name(s) based on (vector of) site codes

# Load dataframe sorted by DateTime, with columns = site-specific temperatures. 
td <- read.csv(paste0(data.dir, "/elwha.wt.allyears.csv"), header = T) 
td$Date <- as.Date(td$Date)
td <- td[order(td$Date, td$Time),]
td$DateTime <- as.POSIXlt( td$DateTime, format = "%Y-%m-%d %H:%M")
td <- td[td$Date >= as.Date("2009-01-01") & td$Date < as.Date("2015-10-01"),] #the period where we have the most data and it overlaps with food web samples
dat <- td[,4:ncol(td)] # Keep only temperature columns included (remove dates)
dat <- dat[,apply(dat, 2, function(x) !all(is.na(x)))] # drop any sites with all NA
#all_na <- apply(apply(dat, 1, is.na), 2, all); dat <- dat[!all_na,] # remove rows with all NA
    #decided removing NA rows was a bad idea because then there wouldn't be an expected number of hours in each day etc.


# Examine data #####
# Z. Johnson & D. Isaak recommend using missMDA when there are at least ~30-50% of data for each site (lower thresholds possible when there are nearby sites)

# Which sites have at least X% of data?
X <- 0.5
zo.mat <- dat; zo.mat[is.na(zo.mat)] <- 0; zo.mat[zo.mat > 0] <- 1 # matrix of which sites have data (1s) or NAs (0s)
goodsites <- which(colSums(zo.mat) > X * nrow(dat)) #these sites have data in > X% of records 
get_value(names(goodsites), xx) # show which sites meet criteria

# Instead evaluate a list of hand-selected sites
sites2keep <- c("IC2", "LR2", "FP10", "FP2", "MS5", "MS11", "MS20", "MS22", "MS26", "MS27", "LR11", "LR14", "LR22", "IC19", "IC9", "IC12", "IC1", "IC11")
dat <- dat[,sites2keep]

# Get proportion missing data for each site
nas <- summary(dat)[7,]
nas <- gsub("NA's   :", "", nas); nas<- gsub("  ", "", nas)
nas <- as.numeric(nas); nas <- round(nas/nrow(dat), 2)
names(nas) <- sites2keep
nas #we are breaking the rule of thumb here, as many of these sites have 50, 60, 70% missing data

# For better coverage, combine Little River sites and combine upper Indian Creek sites, group mainstem sites and 2 floodplain sites
  # based on our knowledge and inspection of plots previously
LR.sites <- dat[,c("LR2", "LR11", "LR14", "LR22")]
IC.upper <- dat[,c("IC19", "IC9", "IC12", "IC1", "IC11")]
LR.sites$LRavg <- apply(LR.sites, 1, mean, na.rm = T); LR.sites$LRavg[is.nan(LR.sites$LRavg)] <- NA
IC.upper$ICUavg <- apply(IC.upper, 1, mean, na.rm = T); IC.upper$ICUavg[is.nan(IC.upper$ICUavg)] <- NA
MS.sites <- dat[,c("MS5", "MS11", "MS20", "MS22", "MS26", "MS27")]
MS.sites$Upper <- apply(MS.sites[,c("MS26", "MS27")], 1, mean, na.rm = T); MS.sites$Upper[is.nan(MS.sites$Upper)] <- NA
MS.sites$Middle <- apply(MS.sites[,c("MS20", "MS22")], 1, mean, na.rm = T); MS.sites$Middle[is.nan(MS.sites$Middle)] <- NA
MS.sites$Lower <- apply(MS.sites[,c("MS5", "MS11")], 1, mean, na.rm = T); MS.sites$Lower[is.nan(MS.sites$Lower)] <- NA
FP.sites <- dat[,c("FP10", "FP2")]
FP.sites$FPavg <- apply(FP.sites[, c("FP10", "FP2")], 1, mean, na.rm = T); FP.sites$FPavg[is.nan(FP.sites$FPavg)] <- NA
dat <- cbind("LR" = LR.sites$LRavg, "ICU" = IC.upper$ICUavg, "ICL" = dat$IC2, "MSL" = MS.sites$Lower, 
             "MSM" = MS.sites$Middle, "MSU" = MS.sites$Upper, "SC" = FP.sites$FPavg)
dates <- td$DateTime

nas <- summary(dat)[7,]
nas <- gsub("NA's   :", "", nas); nas<- gsub("  ", "", nas)
nas <- as.numeric(nas); nas <- round(nas/nrow(dat), 2)
names(nas) <- colnames(dat)
nas # better coverage now


# Plot some relationships 

png("LittleVsIndian.png", width = 12, height = 6, units = "in", res = 300)
par(las = 1)
plot(dates, dat[,"LR"], type = 'n', ylim = c(0, 25), ylab = "Temperature (C)", xlab = "")
lines(dates, dat[,"LR"], col = "dodgerblue", lwd = 0.5)
lines(dates, dat[,"ICU"], col = "darkorange2", lwd = 0.5)
lines(dates, dat[,"ICL"], col = "orange", lwd = 0.5)
legend("topleft", legend = c("Little River", "Indian Creek Upper", "Indian Creek Lower"), col = c("dodgerblue", "darkorange2", "orange"), lwd = 2, bty = 'n')
dev.off()

plot(dat[,"LR"], dat[,"ICL"], ylab = "Lower Indian temperature", xlab = "Little River temperature", ylim = c(0,20), xlim = c(0,20), cex = 0.2)
abline(a = 0, b = 1)

png("Mainstem.png", width = 12, height = 6, units = "in", res = 300)
par(las = 1)
plot(dates, dat[,"MSL"], type = 'n', ylim = c(0, 25), ylab = "Temperature (C)", xlab = "")
lines(dates, dat[,"MSL"], col = "darkgreen", lwd = 0.5)
lines(dates, dat[,"MSM"], col = "mediumseagreen", lwd = 0.5)
lines(dates, dat[,"MSU"], col = "lightgreen", lwd = 0.5)
legend("topleft", legend = c("Lower mainstem", "Middle mainstem", "Upper mainstem"), col = c("darkgreen", "mediumseagreen", "lightgreen"), lwd = 2, bty = 'n')
dev.off()

plot(dat[,"MSU"], dat[,"MSL"], ylab = "Lower mainstem temperature", xlab = "Upper mainstem temperature", ylim = c(0,20), xlim = c(0,20), cex = 0.2); abline(a = 0, b = 1)
plot(dat[,"MSU"], dat[,"MSM"], ylab = "Middle mainstem temperature", xlab = "Upper mainstem temperature", ylim = c(0,20), xlim = c(0,20), cex = 0.2); abline(a = 0, b = 1)
plot(dat[,"MSM"], dat[,"MSL"], ylab = "Lower mainstem temperature", xlab = "Middle mainstem temperature", ylim = c(0,20), xlim = c(0,20), cex = 0.2); abline(a = 0, b = 1)

png("LittleVsMainstem.png", width = 12, height = 6, units = "in", res = 300)
par(las = 1)
plot(dates, dat[,"LR"], type = 'n', ylim = c(0, 25), ylab = "Temperature (C)", xlab = "")
lines(dates, dat[,"LR"], col = "dodgerblue", lwd = 0.5)
lines(dates, dat[,"MSM"], col = "mediumseagreen", lwd = 0.5)
legend("topleft", legend = c("Middle mainstem", "Little River"), col = c("mediumseagreen", "dodgerblue"), lwd = 2, bty = 'n')
dev.off()

plot(dat[,"LR"], dat[,"MSM"], ylab = "Middle mainstem temperature", xlab = "Little River temperature", ylim = c(0,20), xlim = c(0,20), cex = 0.2)
abline(a = 0, b = 1)


# Impute missing values ####
  # WARNING: the following processes can take a long time (especially Bayesian)!
nbdim <- estim_ncpPCA(dat) # estimate the number of dimensions to impute
# for all sites not combined: 2, criterion: 14.587, 1.225, 0.763, 0.830, 0.789, 1.734
# for selected/combined sites: 1

res.comp1 <- MIPCA(dat, ncp = nbdim$ncp, nboot = 1000, verbose = T) # Bootstrap version, good for displaying in PCA
save(res.comp1, file = "res.comp1.RData")
#plot(res.comp1)
results <- res.comp1$res.imputePCA
write.csv(results, paste0(results.dir, "/Elwha_imputed_bootstrap.csv"), row.names = F)

res.comp2 <- MIPCA(dat, ncp = nbdim$ncp, method.mi = "Bayes", nboot = 1000, verbose = T) # Bayesian, better for statistical analyses
save(res.comp2, file = "res.comp2.RData")
results2 <- res.comp2$res.imputePCA
write.csv(results2, paste0(results.dir, "/Elwha_imputed_bayesian.csv"), row.names = F)

# Both methods appeared to have exact same output, as did using ncp = 1 or 2. Hmmmm. Makes sense for 1 PCA vector but 2?
plot(results[,1], results2[,1]) 

results <- read.csv(paste0(results.dir, "/Elwha_imputed_bayesian.csv"), header = T)
png(paste0(results.dir, "/Elwha_imputed.png"), width = 10, height = 10, units = "in", res = 300)
  par(mfrow = c(3,3), las = 1)
  for(var in colnames(results)){  
  plot(dates, results[,var],  ylab = "Temperature", xlab = "", pch = 19, cex = 0.5, main = var, ylim = c(0, 30), col = 2)
  points(dates, dat[, var], pch = 19, cex = 0.5, col = 1)
  }     
dev.off()
# Notice some weird values in plots!


# Sleuthing weird values
pts <- identify(dates, results[,var])
td[pts,]
# The dates around end of Nov 2011 through Jan 2012 seem odd and all imputed datasets have this anomaly which seems to stem from only 
  # one site (LR) which had a few anomalous values that didn't seem to be cleaned well in the first place.
plot(dates,dat[,"LR"])
for(c in 2:length(colnames(dat))) points(dates, dat[,colnames(dat)[c]], col = c)
# Examine which site this came from
plot(td$DateTime, td$LR2, type = 'n')
for(c in 45:48) points(td$DateTime, td[,c], col = c)
legend("topright", legend = colnames(td)[45:48], pch = 19, col = 45:48)
# So it's coming from LR2!
# Solution: I think we need to clip that small stretch out of all of the sites - there is really nothing to base it on.
foo <- td[td$Date > as.Date("2011-10-20") & td$Date < as.Date("2012-02-10"),c(1,47)]
rng <- range(foo$Date[is.na(foo$LR2)])

# Clip out and save
results2 <- cbind.data.frame(td[,c("DateTime", "Date", "Time")], results)
results2[results2$Date > as.Date(rng[1]) - 1 & results2$Date < as.Date(rng[2]) + 1, 4:ncol(results2)] <- NA
write.csv(results2, paste0(results.dir, "/Elwha_imputed_bayesian_clipped.csv"), row.names = F)


# Plot raw and imputed results
png(paste0(results.dir, "/Elwha_raw-v-imputed.png"), width = 7, height = 10, units = "in", res = 300)
par(las = 1, mfrow = c(2,1), cex = 0.9, oma = rep(0,4))
colr <- c("gray", "darkgreen", "mediumseagreen", "lightgreen", "darkorange2", "orange", "dodgerblue")
# raw data
d2plot <- dat[,c(7,4:6,2:3,1)]
plot(dates, d2plot[,1], type = 'n', ylim = c(0, 30), ylab = "Temperature (C)", xlab = "Date", main = "Raw data")
for(i in 1:ncol(d2plot)) lines(dates, d2plot[,i], col = colr[i ])
legend("topleft", legend = c(colnames(d2plot)), lty = 1, col = colr, bty = 'n', cex = 0.8)

# imputed data
r2plot <- results2[,c(7,4:6,2:3,1) + 3]
plot(dates, r2plot[,1], type = 'n', ylim = c(0, 30), ylab = "Temperature (C)", xlab = "Date", main = "Imputed")
for(i in 1:ncol(r2plot)) lines(dates, r2plot[,i], col = colr[i ])
legend("topleft", legend = c(colnames(r2plot)), lty = 1, col = colr, bty = 'n', cex = 0.8)
dev.off()

# Looks pretty good!
# NOW, how to assess how reasonable this is other than looking at the plots??!! Would need to do some leave one out analysis probably.







## EXAMPLE missMDA

#data(orange)
#nbdim <- estim_ncpPCA(orange) # estimate the number of dimensions to impute
#res.comp <- MIPCA(orange, ncp = nbdim$ncp, nboot = 1000)
#plot(res.comp)

#mis.ind <- matrix("o", nrow = nrow(MyData), ncol = ncol(MyData))
#mis.ind[is.na(MyData)] <- "m"
#dimnames(mis.ind) <- dimnames(MyData)
#library("FactoMineR")
#resMCA <- MCA(mis.ind, graph=FALSE)
#plot(resMCA, invis = "ind", title = "MCA graph of the categories")

