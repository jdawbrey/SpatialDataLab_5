Functions to process raw data
#
#  (c) Copyright 2013-2014 Jean-Olivier Irisson
#       and Jessica Luo
#      GNU General Public License v3
#
#--------------------------------------------------------------------------
library("lubridate")
library("plyr")
library("stringr")
library("pastecs")
library("oce")

options("digits.secs"=3)

# SET OPTIONS & IDENTIFIERS ----

source("functions.R")

# set local time zone of data collection. We say it is America/New York when it is in fact local time, just to avoid having to deal with time zones
# shift time of all physical data if not collected in Eastern Time Zone by appropriate number of hours. If collected in the Eastern Time Zone, then set change value to 0
time.zone.change <- 1 #number of hours offset from Eastern Standard Time (i.e. "American/New_York" time zone)

#PROCESS FILES ----
message("Read and process physical data")

##{ Read and reformat physical data

# This section performs the following tasks:
# 1) read in the physical data files from a folder
# 2) cleans up the names by removing the extra characters, units and making everything lowercase
# 3) from the time format in the physical data, converts it to the universal time format
# 4) corrects the time zone and adds in transect number, and converts lat/long into decimal degrees

# list all the physical data files in a given directory
phyFiles <- list.files(paste0("Spatial Data Lab 5"), full=TRUE)
phy <- adply(phyFiles, 1, function(file) {
  # read the data
  d <- read.table(file, fileEncoding="ISO-8859-1", quote="\"", 
                  check.names=FALSE,encoding="UTF-8", na.strings="9999.99", 
                  sep = "\t", skip = 10, header = TRUE, stringsAsFactors =FALSE)
  
  # clean names
  head <- names(d)
  head <- str_replace(head, "\\(.*\\)", "")
  head <- str_trim(head)
  head <- make.names(head)
  head <- tolower(head)
  head <- str_replace(head, fixed(".."), ".")
  
  # assign names
  names(d) <- head
  
  # create a proper date + time format
  date <- scan(file[1], what="character", skip=1, nlines=1, quiet=TRUE)
  date <- date[2]
  mm <- str_sub(date,1,2)
  
  date <- scan(file[1], what="character", skip=1, nlines=1, quiet=TRUE)
  date <- date[2]
  dd <- str_sub(date,4,5)
  dd <- as.numeric(dd)
  
  date <- scan(file[1], what="character", skip=1, nlines=1, quiet=TRUE)
  date <- date[2]
  yy <- str_sub(date,7,8)
  
  dateNextDay<-str_c(mm,as.character(dd+1),yy,sep = "/")
  # shift by one day when we cross midnight
  d$hour <- as.numeric(str_sub(d$time,1,2))
  d$date <- date
  d$dateTime <- str_c(d$date, d$time, sep=" ")
  d$dateTime <- as.POSIXct(strptime(d$dateTime, format="%m/%d/%y %H:%M:%OS",
                                    tz="America/New_York"))
  d$dateTime <- d$dateTime - time.zone.change * 3600
  
  date <- scan(file, what="character", skip=1, nlines=1, quiet=TRUE)
  date <- date[2]
  YY <- str_sub(date,7,8)
  YY <- str_c("2","0",YY, sep = "")
  
  date <- scan(file, what="character", skip=1, nlines=1, quiet=TRUE)
  date <- date[2]
  MM <- str_sub(date,1,2)
  
  date <- scan(file, what="character", skip=1, nlines=1, quiet=TRUE)
  date <- date[2]
  DD <- str_sub(date,4,5)
  DD <- as.numeric(DD)
  
  time <- scan(file, what="character", skip=11, nlines=1, quiet=TRUE)
  time <- time[1]
  hh <- str_sub(time,1,2)
  
  time <- scan(file, what="character", skip=11, nlines=1, quiet=TRUE)
  time <- time[1]
  mm <- str_sub(time,4,5)
  
  time <- scan(file, what="character", skip=11, nlines=1, quiet=TRUE)
  time <- time[1]
  ss <- str_sub(time,7,8)
  
  d$time <- str_c(YY,MM,DD,hh,mm,ss,sep = "")
  
  # detect midnight shifts
  midnightShift <- which(diff(d$dateTime) < 0)
  if (length(midnightShift) > 0) {
    d$dateTime[(midnightShift+1):nrow(d)] <- d$dateTime[(midnightShift+1):nrow(d)] + 24 * 3600} #added +1 to midnight shift row because original was changing the last time of the previous day to dateNextDay -KR 25-Nov-2015
  
  
  d$date <- NULL
  
  # code in a transect number. Use the file name as a dummy variable for transect number. Will assign proper transect number later in the pipeline.
  d$transect <- basename(file)
  
  #create time field in format for input into GIS
  
  
  # reformat the lat and long in decimal degrees.
  names(d)[names(d)=="lat"]<-"lat"
  d$lat <- to.dec.2015(d$lat)
  names(d)[names(d)=="long"]<-"lon"
  d$lon <- to.dec.2015(d$lon)
  
  # columns that are all zero are not possible. They are actually missing data. Detect them
  totCol <- colSums(d[llply(d, class) == "numeric"])
  allZeroCols <- names(totCol)[which(totCol == 0)]
  d[,allZeroCols] <- NA # replace the content with NAs
  
  names(d)[names(d)=="irrandiance"]<-"irradiance"
  
  # keep only interesting data
  d <- d[,c("transect", "dateTime", "time", "depth", "lat", "lon", "temp", "salinity", "pressure", "fluoro", "oxygen", "irradiance")]
  
  d$sw.density<-swRho(salinity = d$salinity, temperature = d$temp, pressure = d$pressure, eos = "unesco")
  return(d)
}, .inform = T, .progress="text")

##{ EXERCISE 3: Clean up data & add transect names -------------------------

# the depth gets stuck from time to time and that results in jumps afterwards. Remove those stuck points and reinterpolate the depth linarly using time.
# assign the depths in which the difference before the previous depth is 0 to be NA
phy$depth[which(diff(phy$depth)==0)+1] <- NA
phy<-phy[,-1]
# interpolation depth using dateTime
phy$depth <- approx(phy$dateTime, phy$depth, phy$dateTime, method="linear")$y

# remove some erroneous values
phy$oxygen <- ifelse(phy$oxygen < 0, NA, phy$oxygen)
phy$temp <- ifelse(phy$temp < 0, NA, phy$temp)
phy$salinity <- ifelse(phy$salinity < 0, NA, phy$salinity)
phy$irradiance <- ifelse(phy$irradiance < 0, NA, phy$irradiance)


# Add transect names here --
transect.names <- read.csv("transect file names.csv", header = TRUE, stringsAsFactors = FALSE)
transect.names <- as.data.frame(transect.names)
phy_t <- merge(x=phy, y=transect.names, by.x = "transect", by.y = "physicaldatafilename", all.x=T)

phy_t$depth <- -1*(phy_t$depth)

#interpolate missing lat and lon data using time
phy_t$lat <- approx(x=as.numeric(phy_t$dateTime), y=phy_t$lat, xo=as.numeric(phy_t$dateTime))$y
phy_t$lon <- approx(x=as.numeric(phy_t$dateTime), y=phy_t$lon, xo=as.numeric(phy_t$dateTime))$y

#save phy frame (non-averaged data) as R object
save(phy_t, file = paste0("processed_phys_data.Rdata"))

#replace NAs with non-sensical number
phy_t[is.na(phy_t)] <- -999.99

#Subset and re-order columns
phy_t <- phy_t[c("cruise", "transect.id", "region", "area", "haul", "tow", "dateTime", "time", 
                 "lat", "lon", "depth", "temp", "salinity", "pressure", "fluoro", "oxygen", 
                 "irradiance", "sw.density")]

#write and export data
write.table(phy_t,file = "Peru.txt", row.names = FALSE, col.names = TRUE)
