# Levi's R Standard Library

## load at the beginning of a new file with this command:
# source("C:/Users/lwolberg/Desktop/R/Wolberg.R")


## packages I use constantly
print("Load package data.table:", quote=F)
library(data.table, quietly=T, warn.conflicts=F)

print("Load package bit64:", quote=F)
library(bit64, quietly=T, warn.conflicts=F) # so fread doesn't bork phone numbers

print("Load package dplyr & tidyr:", quote=F)
library(dplyr, quietly=T, warn.conflicts = F)
options(tibble.print_max = Inf) # to always show all rows
library(tidyr, quietly=T, warn.conflicts = F) # mostly for spread()

## improve write.table defaults:
.write <- write.table
formals(.write)$row.names <- FALSE
formals(.write)$sep <- "\t"
formals(.write)$na <- ""
#also useful for exploring defaults:
#args(.write), formals(.write)

## zip3 extraction
## usage: sapply(dt$`Billing Zip`, zipthree) -> dt$Zip3
zipthree <- function(zip) {
    #as.character(zip) -> zip #just in case
    if(is.na(zip)) return(NA) #so it doesn't get stuck
    unlist(strsplit(zip,"-"))[1] -> zip
    if(nchar(zip) == 8) { #if it's a Zip+4 that starts with zero
        return(substr(zip, 1,2))
    }
    else if(nchar(zip) > 5) {
        substr(zip, 1,5) -> zip
    }
    return(substr(zip, 1, nchar(zip)-2))
}

## return a MM/DD/YYYY date in YYYYMMDD format, or optionally with another separator
datefix <- function(d, sep="") {
    return(paste(substr(d,7,10), substr(d,1,2), substr(d,4,5), sep=sep))
}
## NOTE:
# paste(
#   tstrsplit(x$DateCanvassed, "/")[[3]],
#   tstrsplit(x$DateCanvassed, "/")[[1]],
#   tstrsplit(x$DateCanvassed, "/")[[2]]
#   ,sep="-")

## % table:
# proptab <- function(x,y) {
#   round(prop.table(table(x,y),2),digits=3)*100
# }

## distance between two coordinates:
arcdist <- function(lat1, lon1, lat2, lon2, R=3959) {
  #further info: http://janmatuschek.de/LatitudeLongitudeBoundingCoordinates
  #R is the radius of the Earth in miles---switch it to 6371 if you want the result in km.
  #convert to radians: (now vectorized for great justice!)
    lat1*pi/180->lat1
    lon1*pi/180->lon1
    lat2*pi/180->lat2
    lon2*pi/180->lon2
  return(acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2)*cos(lon1-lon2))*R)
}

## Tell user what's been loaded:
print("Other functions available:", quote=F)
print(".write(x, file)                # write a tab-delimited file with the usual options", quote=F)
print("zipthree(zip)                  # usage: sapply(dt$BillingZip, zipthree) -> dt$Zip3", quote=F)
print("datefix(\"MM/DD/YYYY\"<, \"\">)  # outputs \"YYYYMMDD\", with optional separator", quote=F)
#print("proptab(x, y)                  # outputs crosstab with percents", quote=F)
print("arcdist(lat1,lon1,lat2,lon2)   # distance in miles between two geolocations", quote=F)
print("betterxtabs(by,data,questions) # crosstabs with percentage columns included", quote=F)

######## NOTES:

## mark unique DWID/State once, on the first record:
# http://stackoverflow.com/questions/15776064/r-first-observation-by-group-using-data-table-self-join
#y[, DWIDState := paste(State.x, DWID, sep=".")]
#setkey(y, DWIDState)
#y[unique(y[,key(y), with = FALSE]), CtUnique := 1, mult = 'first']
#sum(y$CtUnique)
#nrow(y)-nrow(y[is.na(CtUnique)]) #3558----yay, it worked!!

## CTRL+SHIFT+C comments the highlighted lines out in RStudio

## get the unique respondents' IDs, then merge back to get only their responses:
# as.factor(z[SQ=="16 Presidentl G (WA)" & SR=="Undecided 3", DWIDState]) -> undecided
# data.table(DWIDState = levels(undecided)) -> undecided
# setkey(undecided, DWIDState)
# merge(z, undecided, by="DWIDState", all.x=F, all.y=T) -> z.undecided


## IMPORTING A WHOLE DIRECTORY OF FILES:
## http://stackoverflow.com/questions/11433432/importing-multiple-csv-files-into-r
# list.files() -> files
# (files[c(2:8,10:12,14,16:18)] -> files) #pick the ones you want
# 
# sapply(files, FUN=substr, start=1, stop=18) -> importnames
# 
# for (i in 1:length(files)) {
#   print(paste("starting",importnames[i]))
#   assign(importnames[i], fread(files[i]))
#   print(paste("FINISHED",importnames[i]))
# }
## MAGIC!!

## AGE BINS:
#summary(as.factor(x[, Age])) # 18 to 94 again
#min(x[, Age]); max(x[, Age])

# agebins <- function(x) {
#   x[Age<100,AgeBin := "90s" ]
#   x[Age<90, AgeBin := "80s" ]
#   x[Age<80, AgeBin := "70s" ]
#   x[Age<70, AgeBin := "60s" ]
#   x[Age<60, AgeBin := "50s" ]
#   x[Age<50, AgeBin := "40s" ]
#   x[Age<40, AgeBin := "30s" ]
#   x[Age<30, AgeBin := "20s" ]
#   x[Age<20, AgeBin := "18+" ]
# }
## Income Bins:
# incomebins <- function(x) {
#   x[`2016:Cat:Income`=="Less than $20,000", Income := "a.Under20k"]
#   x[`2016:Cat:Income`=="$20,000 - $30,000", Income := "b.20_30k"]
#   x[`2016:Cat:Income`=="$30,000 - $50,000", Income := "c.30_50k"]
#   x[`2016:Cat:Income`=="$50,000 - $75,000", Income := "d.50_75k"]
#   x[`2016:Cat:Income`=="$75,000 - $100,000", Income := "e.75_100k"]
#   x[`2016:Cat:Income`=="$100,000 - $150,000", Income := "f.100_150k"]
#   x[`2016:Cat:Income`=="", Income := "g._"]
# }