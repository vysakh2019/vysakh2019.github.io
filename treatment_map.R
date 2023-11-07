##############################################################################
# Creating Treatment and control district maps
# Date : 05/02/2021, Friday
# Data files needed : treat_assign.csv
  #Folder with all the mapping data
  # Note: Make sure to place all the files including the .shp, .prj .sbx file 
#   etc. 
  
##############################################################################

# Installing necessary packages
install.packages("virids")
install.packages("ggthemes")
install.packages("leaflet")
install.packages("tidyverse")
install.packages("sf")
install.packages("htmlwidgets")
install.packages("htmltools")

# Loading packages
library(tidyverse)
library(sf)
library(rgdal)
library(viridis)
library(ggthemes)
library(leaflet)
library(htmlwidgets)
library(htmltools)

# set working directory
setwd("~/Desktop/Profersonal/R sessions/IND_adm")

# Load Census Shape file as a data-frame 
ind_district <- st_read("2011_Dist.shp")
# Load treatment assignment file
treat_assgn <- read_csv("treat_assign.csv")
# Merge shape file with treatment assignment 
merged <- full_join(ind_district,treat_assgn,by ="censuscode") 

levels(merged$treatment) <- c(levels(merged$treatment), 88)
merged$treatment[is.na(merged$treatment)] = 88


treat_map <- merged %>% 
  mutate(treat_num = case_when(treatment =="CS Control only" ~ 0 ,
                               treatment == "CS Treatment" ~ 1 ,
                               treatment == "SHG Control only" ~ 2,
                               treatment == "SHG Treatment" ~ 3, 
                               treatment == "CS & SHG control only"~ 4,
                               treatment == 88 ~ 5 )) 

factor(treat_map$treat_num, labels = c("CS Control only", "CS Treatment","SHG Control only",
                                          "SHG Treatment","CS & SHG control only" ,"Non-Neighbour"))

# Generating PDF plot 
map1 <- treat_map %>% 
  ggplot() +
  aes(fill = as.factor(treatment)) +
  geom_sf(lwd = 0.1) +
  labs(title = 'Map of Treatment and control') +
  scale_fill_viridis(discrete = TRUE, option="viridis") +
  theme_map()

plot(map1)

#library(mapview)
#mapview(treat_map, map.types = "OpenTopoMap" ,
        #label = treat_map$DISTRICT)

# Creating interactive map
# Defining colour palette for the factor variable
factpal <- colorFactor(c("#3399FF", "#34DDDD","#FFCC00" ,"#FFFF66","#5BC236" , "#E9E0DB") , treat_map$treat_num)
labels = c("CS Control only", "CS Treatment","SHG Control only",
           "SHG Treatment","CS & SHG control only" ,"Non-Neighbour")
# Generating map using leaflet package
intr_map <- leaflet(treat_map) %>%
  setView(lng = 77.947998, lat = 23.473324, zoom = 5) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons( color = "grey", weight = 1, 
               smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0.8,fillColor = ~factpal(treat_num),
               label = ~paste0(DISTRICT),
               popup = ~ paste0("Census Code"," ",censuscode, "Treatment type:",treatment," ", " ", "State:", ST_NM),
               highlightOptions = highlightOptions(color = "white", weight = 2,
                                                   bringToFront = TRUE),
               group = "MHS") %>% 
  addLegend("bottomright", pal = factpal, values = ~treat_num,
            title = "Treatment type",
            labFormat = function(type, cuts, p) {  
              paste0(labels)
            })
# Viewing the Map (Show up in the viewer)
intr_map
# Exporting to HTML format 
saveWidget(intr_map, file= "~/Documents/GitHub/vysakh2019.github.io/index.html")
