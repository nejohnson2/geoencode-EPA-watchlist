# Using R to geoEncode data from the EPA website
# Nicholas Johnson
# www.lifeoftrash.com

# Read and Write files
wl = read.csv("~/ITP/RStudio/GeoEncode_EPA_Watchlist/EPA_Watchlist.csv", as.is=TRUE)
write.table(wl, file="~/ITP/RStudio/GeoEncode_EPA_Watchlist/EPA_Watchlist_geoencoded.csv", sep=", ")

# Combine three columns to form an address
wl$unique.Address <- paste(wl$Facility_Street, wl$Facility_City, wl$Facility_.State, wl$Facility_ZIP._Code, sep=", ")

# Geo Ecode that shit (returns long, lat)!!!
wl$latlon <- getGeo(wl$unique.Address)


######################################  GeoCodeing Functions  ##############################################
library(RCurl)
library(RJSONIO)

# Run through the values
getGeo = function(loc){
  geo = ""
  for(i in 1:length(loc)){   
    #print(length(loc))
    if(geo[1] == ""){
      geo = c(gGeoCode(loc[i]))
    } else {
      geo = append(geo, gGeoCode(loc[i]), length(geo))
    }
    print(geo)
    Sys.sleep(0.1)
  }
  return(geo)
}

# Construct URL
construct.geocode.url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

# Execute 'GET'
gGeoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- construct.geocode.url(address)
  doc <- getURL(u)  #make get request
  x <- fromJSON(doc,simplify = FALSE)
  #print(x)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    res <- x$results[[1]]$formatted_address
    print(res)
    return(paste(lng, lat, sep=", "))
  } else {
    return(paste(NA,NA, sep=", "))
  }
}