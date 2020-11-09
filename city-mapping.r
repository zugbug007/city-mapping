library(osmdata)
library(ggplot2)
library(tidyverse)

bbx <- getbb("Royal Wootton Bassett, Wiltshire, South West England, England, SN4, United Kingdom")
#RWB
min_lon <- -1.9476463; max_lat <- -1.8605055
min_lat <- 51.5116457; max_lon <- 51.5617722


bbx <- rbind(x=c(min_lon,max_lon),y=c(min_lat,max_lat))
colnames(bbx) <- c("min","max")
osmdata::available_tags("highway")

highways <- bbx %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value=c("motorway", "trunk",
                          "primary","secondary", 
                          "tertiary","motorway_link",
                          "trunk_link","primary_link",
                          "secondary_link",
                          "tertiary_link")) %>%
osmdata_sf()

require(sf)
ggplot() +
  geom_sf(data = highways$osm_lines,
          aes(color=highway),
          size = .4,
          alpha = .65)+
  theme_void()

streets <- bbx %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "service","unclassified",
                            "pedestrian", "footway",
                            "track","path")) %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = streets$osm_lines,
          aes(color=highway),
          size = .4,
          alpha = .65)+
  theme_void()

color_roads <- rgb(0.42,0.449,0.488)
ggplot() +
  geom_sf(data = streets$osm_lines,
          col = color_roads,
          size = .4,
          alpha = .65) +
  geom_sf(data = highways$osm_lines,
          col = color_roads,
          size = .6,
          alpha = .8)+
  coord_sf(xlim = c(min_lon,max_lon),
           ylim = c(min_lat,max_lat),
           expand = FALSE)+
  theme(legend.position = F) + theme_void()

require(tigris)
counties_MA <- counties(state="MA",cb=T,class="sf",)
counties_MA <- st_crop(counties_MA,
                       xmin=min_lon,xmax=max_lon,
                       ymin=min_lat,ymax=max_lat)
ggplot() + 
  geom_sf(data=counties_MA,fill="gray",lwd=0)+
  coord_sf(xlim = c(min(bbx[1,]), max(bbx[1,])), 
           ylim = c(min(bbx[2,]), max(bbx[2,])),
           expand = FALSE)+
  theme(legend.position = F) + theme_void()

get_water <- function(county_GEOID){
  area_water("MA", county_GEOID, class = "sf")
}
water <- do.call(rbind, 
                 lapply(counties_MA$COUNTYFP,get_water))
water <- st_crop(water,
                 xmin=min_lon,xmax=max_lon,
                 ymin=min_lat,ymax=max_lat)

ggplot() + 
  geom_sf(data=counties_MA)+
  geom_sf(data=water,
          inherit.aes = F,
          col="red")+
  coord_sf(xlim = c(min(bbx[1,]), max(bbx[1,])), 
           ylim = c(min(bbx[2,]), max(bbx[2,])),
           expand = FALSE)+
  theme(legend.position = F) + theme_void()


st_erase <- function(x, y) {
  st_difference(x, st_union(y))
}
counties_MA <- st_erase(counties_MA,water)

ggplot() + 
  geom_sf(data=counties_MA,
          lwd=0)+
  coord_sf(xlim = c(min(bbx[1,]), max(bbx[1,])), 
           ylim = c(min(bbx[2,]), max(bbx[2,])),
           expand = FALSE)+
  theme(legend.position = F) + theme_void()

ggplot() + 
  geom_sf(data=counties_MA,
          inherit.aes= FALSE,
          lwd=0.0,fill=rgb(0.203,0.234,0.277))+
  coord_sf(xlim = c(min(bbx[1,]), max(bbx[1,])), 
           ylim = c(min(bbx[2,]), max(bbx[2,])),
           expand = FALSE)+
  theme(legend.position = F) + theme_void()+
  theme(panel.background=
          element_rect(fill = rgb(0.92,0.679,0.105)))+
  ggtitle("Dark + Yellow theme")

ggplot() + 
  geom_sf(data=counties_MA,
          inherit.aes= FALSE,
          lwd=0.0,fill="white")+
  coord_sf(xlim = c(min(bbx[1,]), max(bbx[1,])), 
           ylim = c(min(bbx[2,]), max(bbx[2,])),
           expand = FALSE)+
  theme(legend.position = F) + theme_void()+
  theme(panel.background=
          element_rect(fill = rgb(0.9,0.9,0.9)))+
  ggtitle("Black + White theme")

ggplot() + 
  geom_sf(data=counties_MA,
          inherit.aes= FALSE,
          lwd=0.0,fill=rgb(0.95,0.95,0.95))+
  coord_sf(xlim = c(min(bbx[1,]), max(bbx[1,])), 
           ylim = c(min(bbx[2,]), max(bbx[2,])),
           expand = FALSE)+
  theme(legend.position = F) + theme_void()+
  theme(panel.background=element_rect(fill = "black"))+
  ggtitle("White + Black theme")

ggplot() + 
  geom_sf(data=counties_MA,
          inherit.aes= FALSE,
          lwd=0.0,fill=rgb(0.203,0.234,0.277))+
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color=color_roads,
          size = .4,
          alpha = .65) +
  geom_sf(data = highways$osm_lines,
          inherit.aes = FALSE,
          color=color_roads,
          size = .6,
          alpha = .65) +
  coord_sf(xlim = c(min(bbx[1,]), max(bbx[1,])), 
           ylim = c(min(bbx[2,]), max(bbx[2,])),
           expand = FALSE) +
  theme(legend.position = F) + theme_void()+
  theme(panel.background=
          element_rect(fill = rgb(0.92,0.679,0.105)))

