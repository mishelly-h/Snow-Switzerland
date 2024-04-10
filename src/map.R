library(tidyverse)
library(plotly)
library(rjson)

plot_mapbox(maps::canada.cities) %>%
  add_markers(
    x = ~long, 
    y = ~lat, 
    size = ~pop, 
    color = ~country.etc,
    colors = "Accent",
    text = ~paste(name, pop),
    hoverinfo = "text"
  )
