library(dslabs)
library(dplyr)
library(ggplot2)
options(stringsAsFactors = FALSE)
library(tidyverse)
library(gridExtra)
library(Lahman)
library(dslabs)
library(AER)

library(tseries)

library(dynlm)
library(stargazer)
library(forecast)
library(mFilter)
library(data.table)
library(caTools)
library(janitor)
library(tidyquant)
ds_theme_set()


library(zoo)
library(lubridate)

library(tikzDevice)
require(tikzDevice)





library(sf) 
library(tmap)
library(readr)
library(stringr)
library(raster)

library(spData)
#library(spDataLarge)

# Course on tmap: https://r-tmap.github.io/tmap-book/visual-variables.html



setwd("D:/R-Learning/R-Geospatial-Data-Analysis/Code")




# Plot Pakistan Map with Provincial Boundaries:

# Level 1: Provinces or Other Divisions.

pak_admin1 <- sf::st_read("pakistan-administrative-boundaries/pak_admbnda_adm1_wfp_20220909.shp", quiet = TRUE)

# Level 2: District

pak_admin2 <- sf::st_read("pakistan-administrative-boundaries/pak_admbnda_adm2_wfp_20220909.shp", quiet = TRUE)


# Level 3: Talka.


pak_admin3 <- sf::st_read("pakistan-administrative-boundaries/pak_admbnda_adm3_wfp_20220909.shp", quiet = TRUE)


tmap::tmap_mode("plot")

tmap::qtm(pak_admin1)

head(pak_admin1)

View(pak_admin1)

# Graph type 1: symbols.


tm_shape(pak_admin1) +
  tm_fill(col = "green", alpha = 0.3) +
  tm_borders(col = "black", lwd = 2) +
  tm_symbols(text = "ADM1_EN",
             title.shape = "Division:",
             shapes.labels = c("Azad Kashmir", "Baluchistan", "Gilgit Baltistan",
                               "Islamabad", "Khybder Pakhtunkhwa",
                               "Punjab",
                               "Sindh")) +
  tm_layout(title = "Pakistan Administrative Boundaries", title.position = c('center', 'top'),
            frame = FALSE,
            inner.margins = c(0.01, 0.01, 0.22 ,0.01))



# Graph Type 2A: Outer Tiles with Green color palette.

tm_shape(pak_admin1) +
  tm_borders(col = "black", lwd = 2) +
  tm_polygons("ADM1_EN", palette = "YlGn", alpha = 0.5, title = "Divisions")+
  tm_layout(legend.position = c("right", "bottom")) +
  tm_layout(title = "Pakistan Administrative Divisions", 
            title.position = c('center', 'top'),
            frame = FALSE,
            inner.margins = c(0.01, 0.01, 0.3 ,0.5))


# Graph Type 2B: Outer Tiles with Manual color palette.





View(pak_admin1)

tm_shape(pak_admin1) +
  tm_borders(col = "black", lwd = 2) +
  tm_polygons("ADM1_EN", palette = c(
    "Punjab" = "green4",
    "Sindh" = "red", 
    "Khyber Pakhtunkhwa" = "goldenrod1", 
    "Azad Kashmir" = "green",
    "Gilgit Baltistan" = "red4",
    "Balochistan" = "red3",
    "Islamabad" = "gray"), title = "Divisions") +
  tm_layout(legend.position = c("right", "bottom")) +
  tm_layout(title = "Pakistan Administrative Divisions", 
            title.position = c('center', 'top'),
            frame = FALSE,
            inner.margins = c(0.1, 0.1, 0.3 ,0.5))





