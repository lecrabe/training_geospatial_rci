
packages <- function(x){
  x <- as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

packages(ggplot2)
packages(xtable)
packages(raster)
packages(shiny)
packages(shinydashboard)
packages(dismo)
packages(stringr)
packages(snow)
packages(plyr)
packages(leaflet)
packages(RColorBrewer)
packages(DT)

packages(ggmap)
packages(rgeos)
packages(maptools)
packages(dplyr)
packages(tidyr)
packages(tmap)

update.packages(checkBuilt=FALSE, ask=FALSE)

?library
.libPaths()
