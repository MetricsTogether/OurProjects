library(readxl)
library(tidyverse)
library(censusapi)
library(lubridate)
library(foreach)
library(tigris)
library(stringr)

key <- "095f223bd2d9dd69b8546222bde6c171c540da9b"


IDOC <- read_xls("Restore Justice/March 2021 Prison Stock.xls",skip = 5)



IDOC$yeartrans <- ifelse(grepl("^[[:digit:]]+$",IDOC$`Sentence Years`),IDOC$`Sentence Years`,NA)
IDOC$yeartrans <- as.numeric(IDOC$yeartrans)
IDOC <- IDOC %>% mutate(age=year(Sys.Date())-year(`Date of Birth`),life=ifelse(`Sentence Years`=="LIFE",1,(ifelse(yeartrans>49,"De Facto",0))),sdp=ifelse(`Sentence Years`=="SDP",1,0)) %>%
  select(-yeartrans)

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



race_vars <- c("B01001A_001E","B01001B_001E","B01001C_001E","B01001D_001E","B01001E_001E","B01001F_001E","B01001G_001E","B01001H_001E","B01001I_001E")
# acs5_race <-foreach(i = rep(2009:2019,17),.combine = 'rbind') %do% {
#   yeargeog(year=i,state=17,vars=race_vars)
# }
acs5_race_17 <- getCensus(name = "acs/acs5",
                       vintage = 2017,
                       vars = race_vars,
                       key = key,
                       region="county:*",
                       regionin = "state:17")
acs5_race_17$year <- 2017

acs5_race_20 <- getCensus(name = "acs/acs5",
                          vintage = 2020,
                          vars = race_vars,
                          key = key,
                          region="county:*",
                          regionin = "state:17")
acs5_race_20$year <- 2020

acs5_race <- rbind(acs5_race_17,acs5_race_20)

acs5_race <- fips_to_acs(acs5_race)
acs5_race <- acs5_race %>% select (year,state_name,county.y,"B01001A_001E","B01001B_001E","B01001C_001E","B01001D_001E","B01001E_001E","B01001F_001E","B01001G_001E","B01001H_001E","B01001I_001E" )

names(acs5_race) <- c("year","state","county", 
                      "white alone",
                      "black alone",
                      "american indian or alaska native alone",
                      "asian alone",
                      "native hawaiian or pacific islander alone",
                      "some other race alone",
                      "two or more races",
                      "white alone, not hispanic or latino",
                      "hispanic or latino, any race category"
)
acs5_race$county <- str_remove(acs5_race$county," County")
acs5_race$total <- rowSums(acs5_race[,4:10])
acs5_race_perc <- data.frame(acs5_race[,1:3],lapply(acs5_race[,4:10],function(x) (x/acs5_race$total)*100))
acs5_race_perc$check <- rowSums(acs5_race_perc[,4:10])

IL_counties <- tigris::counties(state=17) %>% select(NAME,geometry)

ggplot(IDOC) +
  aes(x = life, fill = Race) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal()


# Overwhelmingly, the people in Illinois serving life and de facto life for offenses that occurred before the age of 26 are young Black men from Cook and its neighboring counties. Figure 1 shows the distribution of sentences by race, compared with Illinois demographics.
county_life <- IDOC[IDOC$life %in% c("De Facto", 1),]%>% group_by(`Sentencing County`,Race) %>% summarise(number=n()) %>% mutate(percent=(number/sum(number))*100)
county_life2 <- left_join(county_life,acs5_race_perc[acs5_race_perc$year==2017,],by=c("Sentencing County"="county"))



# Eighty-three of Illinois’s 102 counties have sentenced at least one person to life or de facto life (among the people still serving these sentences) for an offense that occurred before the person’s 26th birthday. Sixty-five percent (1,706) of the people currently serving life or de facto life for crimes that occurred before they turned 26 are from Cook County. 
# 


# About 96 percent of youth life and de facto life sentences are for homicide-related offenses. 
length(IDOC$life[IDOC$life %in% c("De Facto", 1) & IDOC$`Holding Offense`=="Black"])/length(IDOC$life[IDOC$life %in% c("De Facto", 1)])


# Sixty-eight percent (1,790) of the people serving life or de facto life for crimes that occurred in their youth are Black.
blacklife <- length(IDOC$life[IDOC$life %in% c("De Facto", 1) & IDOC$Race=="Black"])/length(IDOC$life[IDOC$life %in% c("De Facto", 1)])
blacklife
