library(missMDA)
library(abind)

# Load functions
numdailyobs <- 24
source("code/cleaning_functions.R")

# Directories
watershed <- "elwha"
data.dir <- "/Users/aimee_fullerton/GitHub/Elwha_ST/data"

#get dataframe sorted by DateTime, with columns = site-specific temperatures. Only numeric columns included (remove dates)
sites.attr <- read.csv(paste0(data.dir, "/elwha.sites.attributed.csv"), header = T)
xx <- sites.attr$Site.Name; names(xx) <- sites.attr$SiteCode
# get_value(sites, xx) # look up site name(s) based on (vector of) site codes

td <- read.csv(paste0(data.dir, "/elwha.wt.allyears.csv"), header = T) 
td$Date <- as.Date(td$Date)
td <- td[td$Date >= as.Date("2004-01-01"),]
datetime <- as.POSIXlt( td$DateTime, format = "%Y-%m-%d %H:%M")
td <- td[,4:ncol(td)]
td <- td[,apply(td, 2, function(x) !all(is.na(x)))] # drop any sites with all NA
foo <- td; foo[is.na(foo)] <- 0; foo[foo > 0] <- 1 # matrix of which sites have data (1s)
#all_na <- apply(apply(td, 1, is.na), 2, all); td <- td[!all_na,] # remove rows with all NA
goodsites <- which(colSums(foo) > 0.3 * nrow(td)) #these sites have data in > X% of records 
get_value(names(goodsites), xx) # show which sites meet criteria
dat <- td[,c(names(goodsites), "IC12", "IC19", "IC9", "LR14", "LR2", "LR22")]
summary(dat)

# Impute missing values
nbdim <- estim_ncpPCA(dat) # estimate the number of dimensions to impute
#res.comp <- MIPCA(dat, ncp = nbdim$ncp, nboot = 1000, verbose = T) # Bootstrap version, good for displaying in PCA
res.comp <- MIPCA(dat, ncp = nbdim$ncp, method.mi = "Bayes", nboot = 1000, verbose = T) # Bayesian, better for statistical analyses
plot(res.comp)

# Compute mean of the 1000 iterations & save data
dat<- matrix(NA, nrow(dat), 12)
for(i in 1:1000){
  dat <- abind(dat, res.comp$res.MI[[i]], along = 3)
}
dat <- dat[,,-1]
dat.mean <- apply(dat, c(1, 2), mean, na.rm = T)
dat.mean <- cbind.data.frame(datetime, dat.mean)
dat.mean$LR <- apply(cbind(dat.mean$LR2, dat.mean$LR11, dat.mean$LR14, dat.mean$LR22), 1, mean, na.rm = T)
dat.mean$ICl <- apply(cbind(dat.mean$IC1, dat.mean$IC9, dat.mean$IC19, dat.mean$IC11), 1, mean, na.rm = T)
dat.mean$ICu <- apply(cbind(dat.mean$IC12, dat.mean$IC12.ET), 1, mean, na.rm = T)
#dat.mean$ICl <- apply(cbind(dat.mean$IC1, dat.mean$IC9, dat.mean$IC19, dat.mean$IC11), 1, mean, na.rm = T)
#dat.mean$ICu <- apply(cbind(dat.mean$IC12, dat.mean$IC12.ET), 1, mean, na.rm = T)
dat.mean$MS <- apply(cbind(dat.mean$BachelorMeander, dat.mean$SpruceHole), 1, mean, na.rm = T)

write.csv(dat.mean, "/Users/aimee_fullerton/OneDrive/Work/Research/StreamTemperature/Elwha.ST/Imputation/Elwha_imputed_mean.csv", row.names = F)

# Plot

# raw data
colr <- c(4,4,4,4,2,2,2,2,2,2,1,1)
plot(datetime, dat[,2], type = 'n', ylim = c(0, 30), ylab = "Temperature (C)", xlab = "Date")
for(i in 2:ncol(dat)) lines(datetime, dat[,i], col = colr[i - 1])
legend("topleft", legend = c(colnames(dat)[2:ncol(dat)]), lty = 1, col = colr, bty = 'n', cex = 0.8)

# imputed data
plot(dat.mean$Date, dat.mean$LR, type = 'n', ylim = c(0, 30), ylab = "Temperature (C)", xlab = "Date")
colr <- c(4,2,3,1); vars <- c("LR", "ICu", "ICl", "MS")
for(i in 1:length(vars)) lines(dat.mean$DateTime, dat.mean[,vars[i]], col = colr[i])
legend("topleft", legend = c("Little", "Indian upper", "Indian lower", "Mainstem"), lty = 1, col = colr, bty = 'n', cex = 0.8)


## EXAMPLE

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

