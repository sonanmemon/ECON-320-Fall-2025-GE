library(tinytex)

library(tidyverse)

library(dslabs)
library(dplyr)

library(ggplot2)

library(tibble)

library(maps)
library(mapdata)
library(usmap)



data(us.cities)

#head(us.cities)


plot_us_capitals <- function() {
  map(database = "usa")
  capitals <- subset(us.cities, capital == 2)
  points(x = capitals$long, y = capitals$lat, col = "blue",
         cex = capitals$pop /500000, pch = 19)
  title("US state capitals")
}


plot_us_capitals()




plot_us_states <- function() {
  map(database = "state")
  capitals <- subset(us.cities, capital == 2)
  points(x = capitals$long, y= capitals$lat, col = "blue",
         cex = capitals$pop / 500000, pch=19)
  title("US state capitals")
}


plot_us_states()



plot_us_counties <- function() {
  plot_usmap(regions = "counties") +
    labs(title = "US Counties",
         subtitle = "") +
    theme(panel.background = element_rect(color = "black", 
                                          fill = "lightblue"))
}



plot_us_counties()



plot_china_cities <- function() {
  
  map("world", "China")
  
  # Load world cities dataset
  data(world.cities)
  
  # Subset all cities in Italy (or capitals only if needed)
  china_cities <- subset(world.cities, country.etc == "China")
  
  # Plot cities using map.cities
  map.cities(x = china_cities,
             pch = 21,
             bg = "green",
             col = "black",
             cex = china_cities$pop/500000)
}


## China Cities:


plot_china_cities()





plot_usmap(include = c("CA", "ID", "NV", "OR", "WA")) +
  labs(title = "Western US States",
       subtitle = "In Pacific Timezone.")



plot_usa_pop <- function() {
  plot_usmap(data = statepop, values = "pop_2022", 
             color = "red") + 
    scale_fill_continuous(low = "white", high = "red", 
                          name = "Population (2022)", 
                          label = scales::comma) +
    theme(legend.position = "right")
}




plot_usa_pop()

