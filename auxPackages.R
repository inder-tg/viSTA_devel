
neededPackages <- c("shiny", "shinycssloaders", "shinydashboard", 
                    "leaflet", "dplyr", "raster", "leafem",
                    "htmltools", "geoTS", "sta",
                    "rintrojs", "shinyjs","shinyBS",
                    "ggplot2", "ggnewscale", "tidyverse",
                    "tidyquant", "shinyWidgets",
                    "dtwclust", "eBsc", "spiralize", "rootSolve")

packagesToInstall <- neededPackages[!(neededPackages %in% installed.packages()[,"Package"])]

if( length(packagesToInstall) ){
  for( i in 1:length(packagesToInstall) ){
    message("Installing package", packagesToInstall[i], "\n")
    install.packages(packagesToInstall[i], dependencies = TRUE)
  }
} 

library(shiny) # How we create the app.
library(shinycssloaders) # Adds spinner icon to loading outputs.
library(shinydashboard) # The layout used for the ui page.
library(leaflet) # Map making. Leaflet is more supported for shiny.
library(dplyr) # Used to filter data for plots.
library(raster)
library(leafem)
library(htmltools)
library(geoTS)
library(sta)
library(rintrojs)
library(shinyjs)
library(shinyBS)
library(ggplot2)
library(ggnewscale)
library(tidyverse)
library(tidyquant)
library(shinyWidgets)

sephoraToInstall <- !("sephora" %in% installed.packages()[,"Package"])

if(sephoraToInstall){
  message("WARNiNG:", "\n")
  message("sephora must be installed")
} else {
  library(sephora)
}
