#JMEC kmz provided by Carmen
library(rgdal)
library(tidyverse)
library(tigris)
library(sf)
library(knitr)
library(kableExtra)
library(RColorBrewer)
library(here)
library(usethis)

JMEC <- read_sf("JMEC_Service_Boundaries.kml")

tiles <- read_sf("2020-01-01_performance_fixed_tiles/gps_fixed_tiles.shp") %>%
  mutate(avg_d_kbps = as.numeric(avg_d_kbps),
         avg_u_kbps = as.numeric(avg_u_kbps),
         avg_lat_ms = as.numeric(avg_lat_ms))

## mapping data
NM_counties <- tigris::counties(state = "New Mexico") %>%
  select(state_code = STATEFP, 
         geoid = GEOID, name = NAME) %>% # only keep useful variables 
  st_transform(4326) # transform to the same CRS as the tiles

Luna_county <- NM_counties %>% filter(name=="Luna")
#NM_tiles <- st_join(tiles, NM_counties)

# tiles_in_nm_counties <- st_join(NM_counties, tiles, left = FALSE)
# JMEC_counties <- c("McKinley","Sandoval","Rio Arriba","San Juan")

#JMEC_TF <- st_join(tiles,JMEC)
JMEC_tiles <- st_join(JMEC, tiles) # tiles %>% filter(geometry%in%JMEC$geometry)


test2 <- data.frame(JMEC_tiles)
JMEC_tiles <- left_join(test2,tiles,by="quadkey")
JMEC_tiles <- JMEC_tiles %>% select(-geometry.x)%>% mutate(avg_d_mbps=avg_d_kbps.y*0.0009765625)

luna_tiles <- st_join(Luna_county,tiles)

luna_tiles2 <- data.frame(luna_tiles)
luna_tiles2 <- left_join(luna_tiles2,tiles,by="quadkey")

## prep the data
JMEC_stats <- JMEC_tiles %>%
  st_set_geometry(NULL) %>%
  group_by(state_code, geoid, name) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests)) %>%
  ungroup() %>%
  left_join(fips_codes %>% 
              mutate(geoid = paste0(state_code, county_code)) %>% 
              # get nicer county and state names
              select(state, geoid, long_name = county, county), by = c("geoid")) 


county_stats_sf <- NM_counties %>%
  select(geoid) %>%
  left_join(county_stats %>% mutate(geoid = as.character(geoid)), by = c("geoid")) %>%
  mutate(mean_dl_mbps_wt = case_when(tests < 50 ~ NA_real_,
                                     TRUE ~ mean_dl_mbps_wt)) %>% # at least 50 tests
  mutate(dl_cat = cut(mean_dl_mbps_wt, c(0, 25, 50, 100, 150, 200), ordered_result = TRUE))

## make the map

# ggplot() +
#   geom_sf(data = county_stats_sf, 
#           aes(fill = dl_cat), color = "gray20", lwd = 0.1) +
#   # geom_sf_text(data = NM_counties, 
#   #              aes(label = NAME), color = "black", size = 2) +
#   theme_void() +
#   scale_fill_manual(values = brewer.pal(n = 6, name = "BuPu"),  
#                     na.value = "gray80", 
#                     labels = c("0 - 25", 
#                                "25.1 - 50", 
#                                "50.1 - 100", 
#                                "100.1 - 150", 
#                                "150.1 - 200", 
#                                "Insuf. data"), 
#                     name = "Mean download speed (Mbps)", 
#                     guide = guide_legend(direction = "horizontal", 
#                                          title.position = "top", 
#                                          nrow = 1, 
#                                          label.position = "bottom", 
#                                          keyheight = 0.5, 
#                                          keywidth = 1,
#                                          size=1)) +
#   theme(text = element_text(color = "gray25"),
#         legend.position = "top")


ggplot()+
  geom_sf(data=NM_counties,color="gray",fill="white")+
  geom_sf(data=JMEC,color="gray",fill="lightgray")+
  geom_sf(data=JMEC_tiles, mapping= aes(geometry=geometry.y,color=avg_d_mbps))+
 # scale_color_gradientn(colours=brewer.pal(7,"YlOrBr"))+
  theme_void()

ggplot()+
  geom_sf(data=JMEC,color="gray",fill="lightgray")+
  geom_sf(data=JMEC_tiles, mapping= aes(geometry=geometry.y,color=avg_d_mbps))+
  labs(color="Average Download Speed (mbps)")+
 # scale_color_gradientn(colours=brewer.pal(7,"YlOrBr"))+
  theme_void()

ggplot()+
  geom_sf(data=JMEC,color="gray",fill="lightgray")+
  geom_sf(data=luna_tiles2, mapping= aes(geometry=geometry.y,color=avg_d_kbps.y))+
  labs(color="Average Download Speed (mbps)")+
  # scale_color_gradientn(colours=brewer.pal(7,"YlOrBr"))+
  theme_void()
