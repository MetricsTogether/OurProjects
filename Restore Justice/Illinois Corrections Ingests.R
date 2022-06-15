library(tnum)
library(censusapi)

tnum.loadLibs()

creds <- "creds"
ip <- "ip"
tnum.authorize(ip=ip,creds=creds)

##Do not run this. 
#tnum.deleteByQuery(query = "subj:*")


# Ingesting the IDOC Data
library(readxl)

IDOC <- read_xls("March 2021 Prison Stock.xls",skip = 5)
template <- list(
  c(paste0("Name of inmate $(IDOC #) at $(Parent Institution) in $(Sentencing County) is $(Name)"),"IDOC:2021:March"))


template_loop <- c()
template_loop <- for (i in length(IDOC)){
  template_loop[[(length(template_loop) + 1)]] <- c(paste0(names(IDOC)[i]," of inmate $(IDOC #) at $(Parent Institution) in $(Sentencing County) is $(",names(IDOC)[i],")"),"IDOC:2021:March")
  i <- i+1
}
  
tnum.ingestDataFrame(head(IDOC),templates = template,
                     outfile = "testIngest1.txt")
#	IL-DOC/prison:Big_Muddy_River/A02008:inmate:county:Cook


# functions for ACVVS data cleaning
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

names(acs5_race) <- c("year","state","county", "total", 
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

for (i in 4:length(acs5_race)-1) {
  templates[[(length(templates) + 1)]] <- c(paste0(" $(county) $(state) has ",names(acs5_race)[i],  " population = $(",names(acs5_race)[i],")"),"USFederal:ACS5:$(year)")
  i <- i+1
}

# do not run until you're ready, this will go straight to the tnum space
tnum.ingestDataFrame(acs5_race,templates,"testIngest1.txt")

