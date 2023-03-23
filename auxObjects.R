
TIME <- as.Date("2000-01-01") + 0:(366 + 3*365 +
                                     366 + 3*365 +
                                     366 + 3*365 +
                                     366 + 3*365 +
                                     366 + 3*365 +
                                     366 + 365)

INDEX <- c()

for(i in 1:22){
  TEMP <- seq(1,365,by=16) + (i-1) * 365
  INDEX <- c(INDEX,  TEMP)
}


# sustican_terra_rTp <- LoadToEnvironment(paste0(getwd(), "/RData/sustican.RData"))$sustican_terra_rTp
# 
# FileIn <- data.frame(SiteID = 1:nrow(sustican_terra_rTp),
#                      Longitude = longlat_sp@coords[,1],
#                      Latitude = longlat_sp@coords[,2])

# coord <- CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")

coord <- CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +datum=WGS84 +units=m +no_defs")

pathRData <- paste0(getwd(), "/RData")
  
  # "C:/Users/inder/OneDrive/Desktop/shinyRStudio/viSTA_update/RData"
  
  # "/home/itecuapetla/Desktop/shinyRStudio/viSTA_update/RData"

listFilesRData <- list.files(path=pathRData, pattern=".RData",
                             full.names=TRUE)

# objectCRS <- LoadToEnvironment("C:/Users/inder/OneDrive/Desktop/proyectoAlbedo/data/crsObject.RData")$objetoCRS

# POLIGONO_RDATA <- LoadToEnvironment(listFilesRData[1])$dfRaster
# 
# xy_sp <- SpatialPoints(cbind(POLIGONO_RDATA$coords[,1], POLIGONO_RDATA$coords[,2]),
#                        proj4string=coord)
# longlat_sp <- spTransform(xy_sp, CRS("+proj=longlat +datum=WGS84"))
# 
# FileIn <- data.frame(SiteID = 1:nrow(POLIGONO_RDATA$values),
#                      Longitude = longlat_sp@coords[,1],
#                      Latitude = longlat_sp@coords[,2])
# 
# sustican_terra_rTp <- POLIGONO_RDATA$values


BASE <- drbasis(nn=100, qq=2)

DAYS <- c(31,28,31,30,31,30,31,31,30,31,30,31)

DAYS_AT_YEAR <- c(1,cumsum(DAYS))

MONTHS <- c("Jan", "Feb", "March", "Abr", "May", "Jun", "Jul",
            "Aug", "Sep", "Oct", "Nov", "Dec")

cgu <- rgb(173/255,221/255,142/255)
csos <- rgb(120/255,198/255,121/255)
cmat <- rgb(49/255, 163/255,84/255)
csen <- rgb(217/255, 95/255, 14/255)
ceos <- rgb(254/255, 153/255, 41/255)
cdor <- rgb(208/255, 209/255, 230/255)

colores <- c(cgu,csos,cmat,csen,ceos,cdor)
