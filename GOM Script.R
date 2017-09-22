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
library("measurements")

options("digits.secs"=3)

# SET OPTIONS & IDENTIFIERS ----


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
#phy <- adply(phyFiles, 1, function(file) {
  # read the data
  g <- GOM
  a <- Alaska
  p <- Peru
  
  # clean names
  head <- names(g)
  head <- str_replace(head, "\\(.*\\)", "")
  head <- str_trim(head)
  head <- make.names(head)
  head <- tolower(head)
  head <- str_replace(head, fixed(".."), ".")
  
  heada <- names(a)
  heada <- str_replace(head, "\\(.*\\)", "")
  heada <- str_trim(head)
  heada <- make.names(head)
  heada <- tolower(head)
  heada <- str_replace(head, fixed(".."), ".")
  
  headp <- names(p)
  headp <- str_replace(head, "\\(.*\\)", "")
  headp <- str_trim(head)
  headp <- make.names(head)
  headp <- tolower(head)
  headp <- str_replace(head, fixed(".."), ".")

  # create a proper date + time format
  M <- g$month
  M <- sprintf("%02d",M)
  D <- g$day
  D <- sprintf("%02d",D)
  g$date <- paste(g$year,M,D, sep = "")
  
  m <- a$month
  m <- sprintf("%02d",m)
  a$date <- paste(a$year,m, sep = "")

  # reformat the lat and long in decimal degrees.
  p$LONG <- gsub("W","", p$LONG)
  p$LAT <- gsub("S","", p$LAT)
  p$LONG <- gsub("°","", p$LONG)
  p$LAT <- gsub("°","", p$LAT)
  
  # columns that are all zero are not possible. They are actually missing data. Detect them
  totCol <- colSums(d[llply(d, class) == "numeric"])
  allZeroCols <- names(totCol)[which(totCol == 0)]
  d[,allZeroCols] <- NA # replace the content with NAs
  
#}, .inform = T, .progress="text")


#write and export data
write.table(phy_t,file = "Peru.txt", row.names = FALSE, col.names = TRUE)
