
install_neededPackages <- function(package, version){
  
  if( system.file(package = package) != "" ){
    if( packageVersion(pkg=package) == version ){
      # remove.packages(package)
      require(remotes)
      install_version(package=package, version=version,
                      repos = "http://cran.us.r-project.org")
    }
  }
}

install_neededPackages(package = "fontawesome", version = "0.2.2")
install_neededPackages(package = "htmltools", version = "0.5.2")
install_neededPackages(package = "shiny", version = "1.7.1")

library(shiny) # How we create the app.
library(shinycssloaders) # Adds spinner icon to loading outputs.
library(shinydashboard) # The layout used for the ui page.
library(leaflet) # Map making. Leaflet is more supported for shiny.
library(dplyr) # Used to filter data for plots.
library(raster)
library(leafem)
library(htmltools)
library(geoTS)
library(sephora)
library(sta)
library(rintrojs)
library(shinyjs)
library(shinyBS)
library(ggplot2)
library(ggnewscale)
library(tidyverse)
library(tidyquant)
library(shinyWidgets)


fluid_design <- function(id, w, x, y, z) {
  fluidRow(
    div(
      id = id,
      column(
        width = 6,
        uiOutput(w),
        uiOutput(y)
      ),
      column(
        width = 6,
        uiOutput(x),
        uiOutput(z)
      )
    )
  )
}

spRast_ValuesCoords <- function(spRaster, na_rm=FALSE){
  spPoints <- as.points(spRaster, na.rm=na_rm)
  
  spValues <- extract(spRaster, spPoints)
  
  DIM <- dim(spValues)
  
  spRasterToPoints <- as.matrix(spValues[1:DIM[1],2:DIM[2]])
  
  spCoords <- crds(spRaster, na.rm=na_rm)
  
list(values=spRasterToPoints, coords=spCoords)  
}

# --- tests

# stacksDIR <- list.files(path = paste0(getwd(), "/data/rasterStacks"),
#                         pattern = ".tif",
#                         full.names = TRUE)
# 
# 
# system.time({
#   wirikuta_terra_aux <- rast(stacksDIR[6])  
# })
# 
# wirikuta_terra <- subset(wirikuta_terra_aux, 1:(20 + 23*21))
# 
# df_wirikuta <- spRast_ValuesCoords(spRaster = wirikuta_terra)


# ---

getCRS <- function(lat, long){
  sampleData <- data.frame(num = 1, lat = lat, long = long)
  
  coordinates(sampleData) <- ~ long + lat
  proj4string(sampleData) <- "+init=epsg:4326"
  
  # cRs <- "+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +datum=WGS84 +units=m +no_defs
  # +ellps=WGS84 +towgs84=0,0,0"
  
  # sampleData <- spTransform(sampleData, 
  #                           CRS = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")
  
  sampleData <- spTransform(sampleData, 
                            CRS = "+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +datum=WGS84 +units=m +no_defs
  # +ellps=WGS84 +towgs84=0,0,0")
  
  
  sampleData
}
# 

textCRS <- function(lat,long){
  
  # lat = 20.30399; long = -90.38977
  
  coor <- extent(getCRS(lat=lat,long=long))
  
  v <- c(coor[1], coor[3])
  
  v
}


get_timeSeries_byClicking <- function(toPlot, df){
  
  # toPlot = c(LONG, LAT); df = TEST
  
  # toPlot = AUX 
  # df = TEST
  
  
  nRow <- length(unlist(toPlot)) / 2
  
  mat_toPlot <- matrix(as.numeric(unlist(toPlot)), nrow = nRow)
  
  dX <- matrix(nrow = nrow(df))
  
  dY <- matrix(nrow = nrow(df))
  
  aproxX <- numeric(nRow)
  
  aproxY <- numeric(nRow)
  
  dX <- sapply(1:nRow, function(s) abs(df[,1] - mat_toPlot[s,1]))
  
  aproxX <- sapply(1:nRow, function(s) df[which.min(dX[,s]),1] )
  
  dY <- sapply(1:nRow, function(s) abs(df[,2] - mat_toPlot[s,2]))
  
  aproxY <- sapply(1:nRow, function(s) df[which.min(dY[,s]),2] )
  
  toExtract <- matrix(nrow = nRow, ncol = 2)
  
  toExtract[,1] <- aproxX
  toExtract[,2] <- aproxY
  
  # IND <- 1:length(df)
  xTemp <- which(df[,1] == toExtract[1,1])
  yTemp <- which(df[xTemp,2] == toExtract[1,2])
  # df[945,1:10]
  xyRow <- xTemp[yTemp]#df[xTemp[yTemp],1:2]
  
  list(coord = xyRow)
}

getTheDates <- function(x, names){
  
  DATES <- getDates(phenoDates = x)
  
  df_text <- c()
  for(i in 1:nrow(DATES)){
    df_text[i] <- paste("<b> <font color=\"", colores[i], "\">", names[i], "</font>", 
                        DATES$month[i], ", ", DATES$day[i], "</b>")
  }  
  
  df_text
}

LoadToEnvironment <- function(RData, env = new.env()){
  load(RData, env)
  return(env) 
}

# --- to export from sephora

# --- added on March 17, 2023

# convertInitialDate <- function(x){
#   pasteo()
# }

# --- added on March 15, 2023
# --- should use it to correct get_metadata_years in sephora

get_years_metadata <- function (x, startYear = 2000, endYear = 2021, frequency = 23) 
{

  years <- startYear:endYear
  initial_dates <- sapply(1:length(years), function(s) paste0(years[s], 
                                                              "-01-01"))
  x_dates <- as.Date(c())
  for (i in 1:(length(x)/frequency)) {
    x_dates <- c(x_dates, 
                 as.Date(initial_dates[i]) + seq(1, 365, length.out=frequency) - 1)
  }
  startLabel <- startYear%%100
  endLabel <- endYear%%100
  labels <- startLabel:endLabel
  for (i in 1:length(labels)) {
    if (nchar(labels[i]) == 1) {
      labels[i] <- paste0(0, labels[i])
    }
  }
  list(xDates = x_dates, xLabels = labels)
}


plot.phenopar <- function(x, startYear, endYear, frequency=NULL,
                          type=c("ts", "rep.mea."),
                          sizeLine=1, sizePoint=2, position_legend="none",
                          # title_legend=NULL,
                          xLab="Time (years)", yLab="NDVI",
                          # width_breaks, date_label,
                          xLim, ...){
  if( inherits(x, "phenopar") ){
    toPlot <- x$x
    freq <- x$freq
  } else {
    if( is.null(frequency) ){
      stop("frequency must be provided when x is not of class 'phenopar'")
    } else {
      toPlot <- x
      freq <- frequency
    }
  }
  
  type <- match.arg(type)
  
  if( type == "ts" ){
    if( missing(startYear) | missing(endYear) ){
      stop( "startYear and endYear must be provided" )
    } else {
      out <- tsPlot(x=toPlot, startYear=startYear, endYear=endYear,
                    frequency=freq,
                    sizeLine=sizeLine, sizePoint=sizePoint,
                    position_legend=position_legend,
                    # title_legend=title_legend,
                    xLab=xLab, yLab=yLab,
                    # width_breaks=width_breaks, date_label=date_label,
                    xLim=xLim)
    }
  }
  
  if( type == "rep.mea." ){
    if( missing(startYear) | missing(endYear) ){
      stop( "startYear and endYear must be provided" )
    } else {
      out <- rmPlot(toPlot, startYear=startYear, endYear=endYear,
                    frequency=freq,
                    sizeLine=sizeLine, sizePoint=sizePoint,
                    position_legend=position_legend,
                    # title_legend=title_legend,
                    xLab=xLab, yLab=yLab, xLim=xLim)
    }
  }
  
  out
}

# --- some aux functions from sephora

rmPlot <- function(x, startYear, endYear, frequency, 
                   sizeLine=1, sizePoint=2, position_legend="none", 
                   # title_legend=NULL,
                   xLab, yLab, xLim){
  
  out_aux <- tsPlot(x=x, startYear = startYear, endYear = endYear, 
                    frequency = frequency, xLab=xLab, yLab=xLab)
  # title_legend = NULL)
  COLORS <- unique( ggplot_build(out_aux)$data[1][[1]]$colour )
  
  # DdA <- seq(1, 365, by=ceiling(365/frequency))
  DdA <- seq(1, 365, length.out=frequency)
  
  x_axis <- get_years_metadata(x=x, startYear=startYear, endYear=endYear,
                               frequency=frequency)
  
  if( missing(xLim) ){
    df <- data.frame(DoY=factor(DdA, levels=DdA),
                     years=as.factor(rep(x_axis$xLabels, each=frequency)),
                     values=x)
    
    out <- ggplot(data=df, 
                  # aes(x=df$DoY, y=df$values, group=df$years, colour=df$years)) +
                  aes(x=DoY, y=values, group=years, colour=years)) +
      ggplot2::geom_line(size = sizeLine) + geom_point(size = sizePoint) +
      ggplot2::labs(y=yLab, x=xLab) +
      ggplot2::theme(legend.position = position_legend)#,
    # legend.title = element_text(title_legend))
  } else {
    ind_begin <- (1:length(x_axis$xDates))[x_axis$xDates == xLim[1]]
    
    ind_end <- which.min( (as.numeric(x_axis$xDates)-as.numeric(xLim[2]))^2 )
    
    A <- strsplit(as.character(xLim[1]),"-")
    B <- strsplit(as.character(xLim[2]),"-")
    
    LABELS <- (as.numeric(A[[1]][1]) %% 100):(as.numeric(B[[1]][1]) %% 100)
    
    remainder <- length(ind_begin:ind_end) %% frequency
    
    x <- x[ind_begin:(ind_end-remainder)]
    
    df <- data.frame(DoY=factor(DdA, levels=DdA),
                     years=as.factor(rep(x_axis$xLabels[LABELS+1], 
                                         each=frequency)),
                     values=x)
    
    out <- ggplot(data=df, 
                  # aes(x=df$DoY, y=df$values, group=df$years, colour=df$years )) +
                  aes(x=DoY, y=values, group=years, colour=years )) +
      ggplot2::geom_line(size = sizeLine) + geom_point(size = sizePoint) +
      ggplot2::labs(y=yLab, x=xLab) +
      ggplot2::theme(legend.position = position_legend) +
      # legend.title = element_text(title_legend)) +
      ggplot2::scale_color_manual(values = COLORS[LABELS+1])
  }
  
  out
}

# ---
tsPlot <- function(x, startYear, endYear, frequency,
                   sizeLine=1, sizePoint=2, position_legend="none", 
                   title_legend=NULL, xLab, yLab,
                   # width_breaks, date_label,
                   xLim){
  
  # x=c(t(pixelio_MATRIX))
  # startYear=startYear
  # endYear=endYear
  # xLab="Years"
  # yLab="NMDI"
  # frequency=36
  # xLim = xLim
  # sizeLine=1
  # sizePoint=2
  # position_legend="none"
  
  # X_axis <- get_metadata_years(x=x, startYear=startYear, endYear=endYear,
  #                              frequency=frequency)
  
  # x_axis <- get_years_metadata(x=x, startYear=startYear, endYear=endYear,
  #                              frequency=frequency)

  x_axis <- get_years_metadata(x=x, startYear=2000, endYear=2020,
                               frequency=frequency)
    
  df <- data.frame(values=x,
                   years=as.factor(rep(x_axis$xLabels, each=frequency)),
                   x=x_axis$xDates)
  
  if( missing(xLim) ){
    out <- ggplot(df, 
                  # aes(x=df$x, y=df$values, color=df$years)) + 
                  aes(x=x, y=values, color=years)) + 
      ggplot2::geom_line(size=sizeLine) + #geom_point(size=sizePoint) +
      ggplot2::theme(legend.position = position_legend, legend.title = title_legend) +
      ggplot2::labs(x=xLab, y=yLab)
  } else {
    out_aux <- ggplot(data = df, 
                      # aes(x=df$x, y=df$values, color=df$years)) + 
                      aes(x=x, y=values, color=years)) + 
      ggplot2::geom_line(size=sizeLine) + #geom_point(size=sizePoint) +
      ggplot2::theme(legend.position = position_legend) +
      ggplot2::labs(x=xLab, y=yLab)
    
    COLORS <- unique( ggplot_build(out_aux)$data[1][[1]]$colour )
    
    ind_begin <- (1:length(x_axis$xDates))[x_axis$xDates == xLim[1]]
    
    ind_end <- which.min( (as.numeric(x_axis$xDates)-as.numeric(xLim[2]))^2 )
    
    # as.numeric(x_axis$xDates)-as.numeric(xLim[2])
    
    A <- strsplit(as.character(xLim[1]),"-")
    B <- strsplit(as.character(xLim[2]),"-")
    
    LABELS <- (as.numeric(A[[1]][1]) %% 100):(as.numeric(B[[1]][1]) %% 100)
    
    remainder <- length(ind_begin:ind_end) %% frequency
    
    x <- x[ind_begin:(ind_end-remainder)]
    
    df <- data.frame(x=x_axis$xDates[ind_begin:(ind_end-remainder)], 
                     years=as.factor(rep(x_axis$xLabels[LABELS+1], 
                                         each=frequency)),
                     values=x)
    
    out <- ggplot(data=df, 
                  # aes(x=df$x, y=df$values, colour=df$years)) +
                  aes(x=x, y=values, colour=years)) +
      ggplot2::geom_line(size = sizeLine) + #geom_point(size = sizePoint) +
      ggplot2::labs(y=yLab, x=xLab) +
      ggplot2::theme(legend.position = position_legend, #) +
                     legend.title = ggplot2::element_text(title_legend) ) +
      ggplot2::scale_color_manual(values = COLORS[LABELS+1])
  }
  
  out
}

# --- added on March 15, 2023

boxPlot <- function(x, startYear=2000, endYear=2020, frequency=36,
                    position_legend="none", title, subtitle){
  
  x_axis <- get_years_metadata(x=x, 
                               startYear=startYear, 
                               endYear=endYear,
                               frequency=frequency)
  
  df <- data.frame(values=x,
                   years=as.factor(rep(x_axis$xLabels, each=frequency)),
                   x= as.factor(round(seq(1,365,length.out=36))) )
  
  out_boxplot <- ggplot(data=df,
                        aes(y=values, x=x, fill=x)) +
    geom_boxplot(
      width=0.215,
      alpha=0.5)
  
  out_boxplot + 
    scale_fill_tq() +
    theme(legend.position = position_legend) +
    labs(
      title=title,
      subtitle=subtitle,
      x="DoA",
      y="NDMI")
}

get_smoothing <- function(m, method=c("OLS", "WLS"), numFreq=3, delta=0, 
                          sigma=NULL){
  
  method <- match.arg(method)
  
  if(method=="OLS"){
    method <- "harmR"
  } else {
    method <- "wls_harmR"
    if(is.null(sigma)){
      stop("sigma must be provided when WLS method is used")
    }
  }
  
  smooth_m <- matrix(nrow = nrow(m), ncol = ncol(m))
  
  for(i in 1:nrow(m)){
    smooth_m[i,] <- haRmonics(y=m[i,], method=method, sigma=sigma, 
                              numFreq=numFreq, delta=delta)$fitted
  }
  
  smooth_m
}

# --- Added on March 15
# --- A little twist to fill_initialgap_MOD13Q1
fill_initialgap_NDMI <- function(m, fun = stats::median){
  apply(m[-1,1:5], MARGIN = 2, FUN = fun)
}


