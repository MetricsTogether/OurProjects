### UNM/NRGCC NSF RIE proposal

library(tidyverse)
library(censusapi)
library(lubridate)
library(foreach)
library(tigris)
library(stringr)
library(sf)


# Functions for data processing
# These functions are used to clean ACS pulled data 

yeargeog <- function(year,state,vars){
  data.frame(year = year, 
             getCensus(name="acs/acs5",
                       vintage=year,
                       vars = vars,
                       key=key,
                       region = "tract:*",
                       regionin = paste0("state:",unique(fips_codes$state_code)[state])
             )
  )
  
}

fips_to_acs <- function(data){
  t <- left_join(data,fips_codes,by=c("state"="state_code","county"="county_code"))
  t
}

native <- tigris::native_areas()
navajo <- native %>% filter(grepl("Navajo",NAME))
us_counties <- tigris::counties(state=c(35,48,04))

#all NM Counties, Navajo reservation, TX - Bailey Cochran Yakum Gianes Andrews, Winkler, Loving, Culberson, Hudspeth, El Paso

TX_counties <- c("Bailey","Cochran","Reeves","Yoakum","Gaines","Andrews","Winkler","Loving","Culberson","Hudspeth","El Paso")
AZ_counties <- c("Navajo","Apache")

AL_counties <- tigris::counties(state=01)

servicearea <- us_counties %>% filter(STATEFP==35 | NAME %in% c(TX_counties,AZ_counties))

native_served <- st_join(servicearea,native,left=TRUE) 
native_served <- native_served[,18:33]
names(native_served) <- names(native)
native_served <- native_served %>% drop_na(NAME)


AL_comp <- st_join(AL_counties,native,left=TRUE)

service2 <- rbind2(select(servicearea,NAME,ALAND,AWATER,INTPTLAT,INTPTLON,geometry),select(native[native$NAME %in% native_served$NAME,],NAME,ALAND,AWATER,INTPTLAT,INTPTLON,geometry))
service2$nativeflag <- ifelse(service2$NAME%in%native$NAME,"Y","N")


# service area plot
ggplot(service2)+
  geom_sf(aes(fill=nativeflag))+
  theme_classic()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  labs(fill="Tribal Land")


water_awards <- read.csv("~/Metrics Together/Energy exploration/Water Awards/Assistance_PrimeAwardSummaries_2022-06-15_H21M35S57_1.csv")
sba_awards <-read.csv("~/Metrics Together/Energy exploration/SBA Awards/Assistance_PrimeAwardSummaries_2022-06-15_H21M16S53_1.csv")
