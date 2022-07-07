library(tnum)
library(censusapi)
library(tidyverse)
library(tigris)
library(jsonlite)

tnum.loadLibs()

creds <- ""
ip <- ""
tnum.authorize(ip=ip,creds=creds)

##Do not run this. 
#tnum.deleteByQuery(query = "subj:*")

test <- tnum.query(query = "subj:*birth",max=1000)
tnum.deleteByQuery(query ="subj:*birth" )


# Ingesting the IDOC Data
library(readxl)

IDOC <- read_xls("March 2021 Prison Stock.xls",skip = 5)
names(IDOC)[c(1,3,10,11)] <- c("ID Number","Birth Date","Projected Mandatory Supervised Release Date","Projected Discharge Date")
IDOC$`Sentence Months` <- as.numeric(IDOC$`Sentence Months`)

# function to rewrite wrong birthdates (lubridate)
date_redo <- function(x, year=2022){
  m <- year(x)
  n <- year(x) %% 100
  year(x) <- ifelse(m-year > 0, 1900+n, m)
  x
}
IDOC$`Birth Date` <- ymd(IDOC$`Birth Date`)
IDOC$`Birth Date` <- date_redo(IDOC$`Birth Date`)
IDOC$`Current Admission Date` <- ymd(IDOC$`Current Admission Date`)
IDOC$`Projected Mandatory Supervised Release Date` <- ymd(IDOC$`Projected Mandatory Supervised Release Date`)
IDOC$`Projected Discharge Date` <- ymd(IDOC$`Projected Discharge Date`)
IDOC$`Custody Date`<- ymd(IDOC$`Custody Date`)
IDOC$`Sentence Date` <- ymd(IDOC$`Sentence Date`)



template <- list(
  c("Name of inmate $(ID Number) in $(Parent Institution) prison in $(Sentencing County) county is $(Name)",
    "IDOC:2021:March, test"))

tnum.ingestDataFrame(head(IDOC),template = template,"testIngest3.txt")



  template_loop <- c()
for (j in 1:length(IDOC)){
  template_loop[[(length(template_loop) + 1)]] <- c(paste0(names(IDOC)[j],
                                                           " of inmate $(ID Number) in $(Parent Institution) prison in $(Sentencing County) county is $(",
                                                           names(IDOC)[j],")")
                                                    ,"IDOC:2021:March,ingest:June20")
}
  
tnum.ingestDataFrame(IDOC,template = template_loop)



#	IL-DOC/prison:Big_Muddy_River/A02008:inmate:county:Cook
## check what was ingested
file <- read.delim("testIngest2.txt")
`%notin%` <- negate(`%in%`)
newfile <- file %notin% data_tnum_og$wrapper



# functions for ACS data cleaning
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



key <- "095f223bd2d9dd69b8546222bde6c171c540da9b"

race_vars <- c("B01001A_001E","B01001B_001E","B01001C_001E","B01001D_001E","B01001E_001E","B01001F_001E","B01001G_001E","B01001H_001E","B01001I_001E")

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
                      "white",
                      "black",
                      "american indian or alaska native",
                      "asian",
                      "native hawaiian or pacific islander",
                      "other",
                      "multi racial",
                      "white non hispanic",
                      "hispanic or latino"
)
templates <- c()

for (i in 5:length(acs5_race)-1) {
  templates[[(length(templates) + 1)]] <- c(paste0(" $(county) $(state) has ",names(acs5_race)[i],  " population = $(",names(acs5_race)[i],")"),"USFederal:ACS5:$(year)")
  i <- i+1
}

tnum.ingestDataFrame(acs5_race,templates,"testIngest1.txt")

