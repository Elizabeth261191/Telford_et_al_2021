

setwd("C:/Users/etelford.IC.003/Dropbox/Masters")

library(ggplot2)  # ggplot() fortify()
library(dplyr)  # %>% select() filter() bind_rows()
library(rgdal)  # readOGR() spTransform()
library(raster)  # intersect()
library(ggsn)  # north2() scalebar()
library(rworldmap)

VS <- read.csv("VS.csv")
VE <- read.csv("VE.csv")
map <- read.csv("Map_1.csv")
View(map)

(prelim_plot <- ggplot(map, aes(x = long, y = lat, 
                                    colour = species)) +
    geom_point())

map <- map %>% filter(long > -50)
map <- map %>% filter(long < 50)
  
(prelim_plot <- ggplot(map, aes(x = long, y = lat, 
                                colour = species)) +
    geom_point())


world <- getMap(resolution = "low")

(with_world <- ggplot() +
    geom_polygon(data = world, 
                 aes(x = long, y = lat, group = group),
                 fill = NA, colour = "black") + 
    geom_point(data = map,  # Add and plot species data
               aes(x = long, y = lat, 
                   colour = species)) +
    coord_quickmap() +  # Prevents stretching when resizing
    theme_classic() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude") + 
    guides(colour=guide_legend(title="Species")))