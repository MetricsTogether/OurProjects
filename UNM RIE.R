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
us_counties <- tigris::counties(state=c(35,54,48))
