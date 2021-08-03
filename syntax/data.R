
##################################################
##################################################
##
##################################################
##################################################

#install.packages("shinydashboard")
#install.packages('rsconnect')
#install.packages("reshape2")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("pmdplyr")
#install.packages("pspline")
#install.packages("zoo")
#install.packages("xlsx") 
#install.packages("maptools") #for maps

#rsconnect::setAccountInfo(name='XXXXXXX', token='XXXXXXX', secret='XXXXXXX')

##################################################
##################################################
##
##################################################
##################################################

library(shinydashboard)
library(shiny)
library(rsconnect)
library(reshape2)
library(RCurl)
library(tidyr)
library(pmdplyr)
library(dplyr)
library(tidyverse)
library(purrr)
library(zoo)
library(xlsx)

list=c("shinydashboard","zoo")
lapply(list,library,character.only=TRUE)







##################################################
##################################################
#ARCHIVE CURRENT DATA
##################################################
##################################################

#LAG 2
# identify the folders
current.folder <- "C:/Users/Jonathan/OneDrive/# CODING/R DASHBOARD/data/original/lag1"
new.folder <- "C:/Users/Jonathan/OneDrive/# CODING/R DASHBOARD/data/original/lag2"
# find the files that you want
list.of.files <- list.files(current.folder, "*",recursive = TRUE, full.names = TRUE)
# copy the files to the new folder
file.copy(list.of.files, new.folder, copy.date = TRUE, overwrite = TRUE)

#LAG 1
# identify the folders
current.folder <- "C:/Users/Jonathan/OneDrive/# CODING/R DASHBOARD/data/original"
new.folder <- "C:/Users/Jonathan/OneDrive/# CODING/R DASHBOARD/data/original/lag1"
# find the files that you want
list.of.files <- list.files(current.folder, "*",recursive = TRUE, full.names = TRUE)
# copy the files to the new folder
file.copy(list.of.files, new.folder, copy.date = TRUE, overwrite = TRUE)








##################################################
##################################################
#EXTRACT DATA
##################################################
##################################################

#UNIFIED COVID DATASET FROM JHU CSSE
#Global & County/State, United States
#https://github.com/CSSEGISandData/COVID-19_Unified-Dataset

  #CASE COUNTS
  urlin <- "https://github.com/CSSEGISandData/COVID-19_Unified-Dataset/raw/master/COVID-19.rds"
  urlout <- "C:/Users/Jonathan/OneDrive/# CODING/R DASHBOARD/data/original/COVID-19.rds"
  download.file(urlin,urlout)
  jhu_timeseries_1<-readRDS(urlout)
  #View(jhu_timeseries_1)
    
    #NAMIBIA IS CODED AS NA, NOT AS MISSING, NO NEED TO RECODE SINCE IT WAS ALREADY CODED IN RAW RDS FILE
    jhu_timeseries_1%>%filter(ID=="NA")%>%count(ID)
    
  #STATIC DATA WITH DEMOGRAPHIC DATA - USED FOR PREDICTION MODELLING
  urlin <- "https://github.com/CSSEGISandData/COVID-19_Unified-Dataset/raw/master/COVID-19_Static.rds"
  urlout <- "C:/Users/Jonathan/OneDrive/# CODING/R DASHBOARD/data/original/COVID-19_Static.rds"
  download.file(urlin,urlout)
  jhu_static_1<-readRDS(urlout)
  #View(jhu_static_1)
  
    #NAMIBIA IS CODED AS NA, NOT AS MISSING, NO NEED TO RECODE SINCE IT WAS ALREADY CODED IN RAW RDS FILE
    jhu_static_1%>%filter(ID=="NA")

  #LOOKUP TABLE WITH IDENTIFIER AND GEOGRAPHIC DATA
  #MUST INCLUDE NA.STRINGS="" SINCE NAMIBIA HAS AN ID CODE OF "NA", OTHERWISE WILL BE CONVERTED TO MISSING; ALTERNATIVELY, COULD RECODE NAMIBIA TO NAM
  urlin <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19_Unified-Dataset/master/COVID-19_LUT.csv"
  urlout <- "C:/Users/Jonathan/OneDrive/# CODING/R DASHBOARD/data/original/COVID-19_LUT.csv"
  download.file(urlin,urlout)
  jhu_lookup_1 <- read.csv(urlout, na.strings = "")
  #View(jhu_lookup_1)
  
#UOFT CASES
#https://github.com/ccodwg/Covid19Canada
  
  #PROVINCE LEVEL - DUPLICATED IN JHU, DO NOT USE THESE
    
    #CASES
    urlin <- "https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/timeseries_prov/cases_timeseries_prov.csv"
    urlout <- "C:/Users/Jonathan/OneDrive/# CODING/R DASHBOARD/data/original/cases_timeseries_prov.csv"
    download.file(urlin,urlout)
    uoft_cases_province_1 <- read.csv(urlout)
    #View(uoft_cases_province_1)
    
    #DEATHS
    urlin <- "https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/timeseries_prov/mortality_timeseries_prov.csv"
    urlout <- "C:/Users/Jonathan/OneDrive/# CODING/R DASHBOARD/data/original/mortality_timeseries_prov.csv"
    download.file(urlin,urlout)
    uoft_deaths_province_1 <- read.csv(urlout)
    #View(uoft_deaths_province_1)
  
  #HEALTH REGION LEVEL
  
    #CASES
    urlin <- "https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/timeseries_hr/cases_timeseries_hr.csv"
    urlout <- "C:/Users/Jonathan/OneDrive/# CODING/R DASHBOARD/data/original/cases_timeseries_hr.csv"
    download.file(urlin,urlout)
    uoft_cases_healthregion_1 <- read.csv(urlout)
    #View(uoft_cases_healthregion_1)
    
    #DEATHS
    urlin <- "https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/timeseries_hr/mortality_timeseries_hr.csv"
    urlout <- "C:/Users/Jonathan/OneDrive/# CODING/R DASHBOARD/data/original/mortality_timeseries_hr.csv"
    download.file(urlin,urlout)
    uoft_deaths_healthregion_1 <- read.csv(urlout)
    #View(uoft_deaths_healthregion_1)
    
##################################################
##################################################
#TRANSFORM 
##################################################
##################################################

##################################################
#EXPLORE DATA
##################################################

#type of data varies by source
table1 <- jhu_timeseries_1 %>% count(Type,Source)
View(table1)
  
  #hospitalizations only available in CTP, DPC, JRC, NYC
  #JHU does not include hospitalizations
  table1 <- jhu_timeseries_1 %>% filter(Type=="Hospitalized") %>% count(Source)
  table1
  View(table1)
  
  #cases available from multiple sources
  table1 <- jhu_timeseries_1 %>% filter(Type=="Confirmed") %>% count(Source)
  table1
  
  #data for JHU
  table1 <- jhu_timeseries_1 %>% filter(Source=="JHU") %>% count(Type)
  table1
  
  #data for CTP
  table1 <- jhu_timeseries_1 %>% filter(Source=="CTP") %>% count(Type)
  table1
  
  #data for JRC
  table1 <- jhu_timeseries_1 %>% filter(Source=="JRC") %>% count(Type)
  table1
  
#Data has total age only
table1 <- jhu_timeseries_1 %>% count(Age,Source)
table1

#Data has total sex only
table1 <- jhu_timeseries_1 %>% count(Sex,Source)
table1

#Uunique ID
table1 <- jhu_timeseries_1 %>% count(ID,Source)
table1













##################################################
#CREATE HARMONIZED DATA
##################################################

#LOGIC: KEEP VARIABLE NAMES THE SAME, BUT HAVE DIFFERENT DATA SETS FOR EACH TYPE OF CASE

####################
#LOOKUP
####################

jhu_lookup_2 <- jhu_lookup_1 %>%
  mutate(infile2=1) %>%
  mutate(xid=toupper(NameID)) %>%
  rename(id=ID,level=Level,admin=Admin,admin0=Admin0,admin1=Admin1,admin2=Admin2,admin3=Admin3,population=Population) %>%
  select(-NameID)

####################
#CASES
####################

jhu_cases_1 <- jhu_timeseries_1 %>% 
  filter(Source=="JHU") %>%
  filter(Type=="Confirmed") %>%
  mutate(Type=toupper(Type)) %>%
  rename(id=ID,date=Date,cases_cum=Cases,cases_new=Cases_New,type=Type,source=Source) %>%
  select(id,date,cases_cum,cases_new,type,source) %>%
  mutate(infile1=1)
  
  #MERGE CASES WITH LOOKUP
  jhu_cases_2 <- jhu_cases_1 %>% 
    full_join(jhu_lookup_2,by = c("id"))
  
      #RESULTS OF MERGE
      table1<-jhu_cases_2%>%count(infile1,infile2)
      table1
      #IN CASES, BUT NOT LOOKUP; NONE, ALL CASES HAVE LOOKUP DATA
      table1<-jhu_cases_2%>%filter(infile1==1 & is.na(infile2))
      table1
      #IN LOOKUP, BUT NOT CASES; MOSTLY LOWER LEVEL REGIONS, ONLY "Western Sahara" AS COUNTRY REGION WITH NO CASES
      table1<-jhu_cases_2%>%filter(is.na(infile1) & infile2==1) %>% count(admin)
      table1
      table1<-jhu_cases_2%>%filter(is.na(infile1) & infile2==1) %>% count(admin,xid)
      table1

#KEEP DATA WHERE CASES ARE AVAILABLE
jhu_cases_3<-jhu_cases_2%>%filter(infile1==1)%>%select(-infile1,-infile2)
#View(jhu_cases_3)

        #DISPLAY INFO FOR CANADA 
        table1<-jhu_cases_3%>%filter(grepl("CANADA",xid))%>%count(admin,admin0,id)
        table1
        
        #DISPLAY INFO FOR USA
        table1<-jhu_cases_3%>%filter(grepl("UNITED STATES",xid))%>%count(admin,admin0,id)
        table1

####################
#DEATHS
####################

jhu_deaths_1 <- jhu_timeseries_1 %>% 
  filter(Source=="JHU") %>%
  filter(Type=="Deaths") %>%
  mutate(Type=toupper(Type)) %>%
  rename(id=ID,date=Date,cases_cum=Cases,cases_new=Cases_New,type=Type,source=Source) %>%
  select(id,date,cases_cum,cases_new,type,source) %>%
  mutate(infile1=1)
    
    #MERGE CASES WITH LOOKUP
    jhu_deaths_2 <- jhu_deaths_1 %>% 
      full_join(jhu_lookup_2,by = c("id"))
    
    #RESULTS OF MERGE
    table1<-jhu_deaths_2%>%count(infile1,infile2)
    table1
    #IN DEATHS, BUT NOT LOOKUP; NONE, ALL CASES HAVE LOOKUP DATA
    table1<-jhu_deaths_2%>%filter(infile1==1 & is.na(infile2))
    table1
    #IN LOOKUP, BUT NOT DEATHS; MOSTLY LOWER LEVEL REGIONS, SOME HIGHER LEVEL REGIONS LIKE "Western Sahara" AS COUNTRY REGION WITH NO DEATHS
    table1<-jhu_deaths_2%>%filter(is.na(infile1) & infile2==1) %>% count(admin)
    table1
    table1<-jhu_deaths_2%>%filter(is.na(infile1) & infile2==1) %>% count(admin,xid)
    table1

#KEEP DATA WHERE DEATHS ARE AVAILABLE
jhu_deaths_3<-jhu_deaths_2%>%filter(infile1==1)%>%select(-infile1,-infile2)
#View(jhu_deaths_3)

    #DISPLAY INFO FOR CANADA 
    table1<-jhu_deaths_3%>%filter(grepl("CANADA",xid))%>%count(admin,admin0,id)
    table1
    
    #DISPLAY INFO FOR USA
    table1<-jhu_deaths_3%>%filter(grepl("UNITED STATES",xid))%>%count(admin,admin0,id)
    table1





####################
#HOSPITALIZATIONS
####################

jhu_hosp_1 <- jhu_timeseries_1 %>% 
  filter(Source=="CTP" | Source=="JRC") %>%
  filter(Type=="Hospitalized") %>%
  mutate(Type=toupper(Type)) %>%
  rename(id=ID,date=Date,cases_cum=Cases,cases_new=Cases_New,type=Type,source=Source) %>%
  select(id,date,cases_cum,cases_new,type,source) %>%
  mutate(infile1=1)
    
#MERGE HOSP WITH LOOKUP
jhu_hosp_2 <- jhu_hosp_1 %>% 
  full_join(jhu_lookup_2,by = c("id"))

    #RESULTS OF MERGE
    table1<-jhu_hosp_2%>%count(infile1,infile2)
    table1
    #IN HOSP, BUT NOT LOOKUP; NONE, ALL CASES HAVE LOOKUP DATA
    table1<-jhu_hosp_2%>%filter(infile1==1 & is.na(infile2))
    table1
    #IN LOOKUP, BUT NOT HOSP; MOSTLY LOWER LEVEL REGIONS, SOME HIGHER LEVEL REGIONS WITH NO HOSP
    table1<-jhu_hosp_2%>%filter(is.na(infile1) & infile2==1) %>% count(admin)
    table1
    table1<-jhu_hosp_2%>%filter(is.na(infile1) & infile2==1) %>% count(admin,xid)
    table1

#KEEP DATA WHERE HOSP ARE AVAILABLE
jhu_hosp_3<-jhu_hosp_2%>%filter(infile1==1)%>%select(-infile1,-infile2)
#View(jhu_hosp_3)

    #DISPLAY INFO FOR CANADA - NONE
    table1<-jhu_hosp_3%>%filter(grepl("CANADA",xid))%>%count(admin,admin0,id)
    table1
    
    #DISPLAY INFO FOR USA
    table1<-jhu_hosp_3%>%filter(grepl("UNITED STATES",xid))%>%count(admin,admin0,id)
    table1







##################################################
#CANADA LOCAL DATA IS NOT PART OF JHU UNIFIED DATASET; APPEND CANADA LOCAL DATA (ADMIN LEVEL 2) FROM UOFT TO JHU DATASET; ALSO RECREATE VARIABLES TO MATCH JHU DATASET
##################################################

####################
#NEED LOOKUP TABLE FOR POPULATION COUNTS
####################

    
    
    
####################
#CASES
####################

uoft_cases_healthregion_2 <- uoft_cases_healthregion_1 %>%
  mutate(type="CONFIRMED") %>%
  mutate(source="UOFT") %>%
  mutate(level="Country") %>%
  mutate(admin=2) %>%
  mutate(admin0="Canada") %>%
  mutate(admin1=province) %>%
  mutate(admin2=health_region) %>%
  mutate(admin3="") %>%
  mutate(xid=toupper(paste0(admin0,", ",admin1,", ",admin2))) %>%
  mutate(id=toupper(paste0(substr(admin0,1,2),substr(admin1,1,3),substr(admin2,1,7)))) %>%
  rename(date=date_report,cases_cum=cumulative_cases,cases_new=cases) %>%
  mutate(date=as.Date(date,format="%d-%m-%Y")) %>% #USE CAPITAL Y FOR 4 DIGIT YEAR
  select(id,date,cases_cum,cases_new,type,source,level,admin,admin0,admin1,admin2,admin3,xid,-province,-health_region)
#View(uoft_cases_healthregion_2)
#str(uoft_cases_healthregion_2)
    
    table1<-uoft_cases_healthregion_2%>%count(id,xid)
    table1
    
    #ensure ID uniquely identifies records; compare unique count against xid
    table1<-uoft_cases_healthregion_2%>%count(id)
    table1

####################
#DEATHS
####################

uoft_deaths_healthregion_2 <- uoft_deaths_healthregion_1 %>%
  mutate(type="DEATHS") %>%
  mutate(source="UOFT") %>%
  mutate(level="Country") %>%
  mutate(admin=2) %>%
  mutate(admin0="Canada") %>%
  mutate(admin1=province) %>%
  mutate(admin2=health_region) %>%
  mutate(admin3="") %>%
  mutate(xid=toupper(paste0(admin0,", ",admin1,", ",admin2))) %>%
  mutate(id=toupper(paste0(substr(admin0,1,2),substr(admin1,1,3),substr(admin2,1,7)))) %>%
  rename(date=date_death_report,cases_cum=cumulative_deaths,cases_new=deaths) %>%
  mutate(date=as.Date(date,format="%d-%m-%Y")) %>% #USE CAPITAL Y FOR 4 DIGIT YEAR
  select(id,date,cases_cum,cases_new,type,source,level,admin,admin0,admin1,admin2,admin3,xid,-province,-health_region)
#View(uoft_deaths_healthregion_2)

    table1<-uoft_deaths_healthregion_2%>%count(id,xid)
    table1
    
    #ensure ID uniquely identifies records; compare unique count against xid
    table1<-uoft_deaths_healthregion_2%>%count(id)
    table1




    
    
    # 
    # 
    # #reshape wide to long using tidyr
    # #data, new stub name for sequence identifier (all of the variables not specified as constant will be reshaped), new variable containing values of reshaped variables, all constant variables
    # cases1 <- cases0 %>% 
    #   select(starts_with("X"),region2,region1,lat,long,filesource,region3) %>% 
    #   gather(date, cases_cum, -region2, -region1, -lat, -long, -filesource, -region3)
    # 
    
    # 
    # #convert string to date
    # cases4 <- cases3 %>% mutate(date=as.Date(date,format="X%m.%d.%Y"))
    # 

##################################################
#CREATE FINAL HARMONIZED DATA
#APPEND UOFT DATA
##################################################

cases0 <- bind_rows(jhu_cases_3,uoft_cases_healthregion_2) %>% group_by(xid)
deaths0 <- bind_rows(jhu_deaths_3,uoft_deaths_healthregion_2) %>% group_by(xid)
hosp0 <- jhu_hosp_3 %>% group_by(xid)

#View(cases0)

  table1 <- cases0 %>% count(xid)
  table1
  table1 <- cases0 %>% count(source,xid)
  table1

  #no data for admin3
  cases0%>%ungroup()%>%count(admin3)

  
  
  
  
  
##################################################
#VALIDATE DATA BEFORE ANALYSIS
##################################################

#confirm data is balanced panel data
cases1 <- cases0 %>% 
  group_by(xid) %>% 
  arrange(date,.by_group=TRUE) %>% 
  mutate(gaptoprevious=as.numeric(date-lag(date,n=1))) %>%
  mutate(gaptoprevious=if_else(is.na(gaptoprevious),1,gaptoprevious)) #first sequence of group
count(cases1)

        #should all be 1 to indicate consecutive dates; if 0, then dates are duplicated; if >1, then gaps in dates
        cases1 %>% ungroup(xid) %>% count(gaptoprevious)
        table1<-cases1 %>% filter(gaptoprevious>1)
        table1(table1)
        
        #Colombia, province level unassigned, jump from 2020-08-10 to 2020-09-21
        table1<-cases1 %>% filter(id=="COZZ" & date>"2020-08-01" & date<"2020-10-01")
        View(table1)

#expand using weights defined as gap in dates across rows, then fill time series, then create sequence indicator to denote row expansion (subtract 1 to indicate actual observations that were filled); need to do before calculating lagged measures
#recode date for filled in observations, keeping all other variables the same for duplicates
#recalculate cases new; due to duplication of leading row, jump in cases for a given gap will be applied at first new row, and subsequent duplicated rows will have 0 new cases
#sort data due to the way the expanded dates are calculated (if using gaptonext instead of gaptoprevious, would not have to sort, but would have to reverse the logic of the coding)
#Per above, this is only done for Colombia, province level unassigned, jump from 2020-08-10 to 2020-09-21
cases2 <- cases1 %>% 
  uncount(gaptoprevious,.id="row_expanded") %>% 
  mutate(row_expanded=row_expanded-1) %>% 
  mutate(newdate=date-row_expanded) %>% 
  mutate(row_expanded=if_else(row_expanded>1,1,row_expanded)) %>%
  mutate(date=newdate) %>% 
  group_by(xid) %>% 
  arrange(date,.by_group=TRUE) %>% 
  mutate(cases_new = cases_cum-lag(cases_cum,n=1)) %>%
  mutate(cases_new=if_else(is.na(cases_new),cases_cum,cases_new)) %>% #first sequence of group, assign to cases_cum
  dplyr::mutate(xid_seq = 1:n()) %>%
  #mutate(cases_new2 = cases_cum-lag(cases_cum,n=1)) %>% #check cases_new2 equals cases_new where not expanded
  select(-c(newdate))
#View(cases2)

        # #check cases_new2 equals cases_new where not expanded
        # temp1<-cases2%>%filter(cases_new != cases_new2)
        # View(temp1)

        # #validate cases_new NA equals to first date
        # temp1<-cases2 %>% filter(id=="NLZE")
        # View(temp1)
        # temp1<-cases2 %>% filter(is.na(cases_new)) %>% ungroup() %>% count(date)
        # View(temp1)
        # temp1<-cases2 %>% filter(is.na(cases_new)) %>% ungroup() %>% count(xid_seq)
        # View(temp1)
        
        #check gap was expanded properly
        temp1<-cases2 %>% filter(id=="COZZ" & date>"2020-08-01" & date<"2020-10-01")
        View(temp1)
        
        #should all be 0 if no expansion
        cases2 %>% ungroup(xid) %>% count(row_expanded)



##################################################
#xid - lowest level of aggregation
##################################################

#moving average past DATE using tidyr, past 6 periods (7 including current period)
#create dynamic variable names using loops
#pipe in data into dplyr function, with groupby option piped into mutate function which creates dynamic name for lag variable
cases3<-cases2
for(i in 0:6) {
  #cases6 <- cases5 %>% group_by(region1) %>% mutate(cases_cum_L1 = lag(cases_cum,n=1))
  #cases6 <- cases5 %>% group_by(xid3) %>% mutate(cases_cum_L1 = tlag(cases_cum,.n=5,.i=xid3,.t=date))
  #cases5 <- cases5 %>% group_by(xid3) %>% mutate(!!quo_name(paste0("cases_cum_", i)) := lag(cases_cum,n=i)) #PERIOD LAG
  cases3 <- cases3 %>% 
    group_by(xid) %>% 
    arrange(date,.by_group=TRUE) %>% 
    mutate(!!quo_name(paste0("cases_new_", i)) := if_else((date - lag(date,n=i)) > 6, 0, as.numeric(lag(cases_new,n=i)))) #DATE LAG, 0 OR NA?; since interested in rolling average, include any of the past 7 periods as long as they are within 7 days; also, the data are a balanced panel
}
head(cases3)
#View(cases3)

#rowmeans of the current + 6 lagged case counts using base r
cases4 <- cases3 %>% 
  mutate(cases_new_rolling7=rowMeans(data.frame(cases_new_0,cases_new_1,cases_new_2,cases_new_3,cases_new_4,cases_new_5,cases_new_6),na.rm = TRUE)) %>%
  mutate(cases_new_rolling7=round(cases_new_rolling7,4)) %>%
  select(-cases_new_0,-cases_new_1,-cases_new_2,-cases_new_3,-cases_new_4,-cases_new_5,-cases_new_6)

#simple percent change day over day, using rolling average to smooth the data
cases5 <- cases4 %>% group_by(xid) %>% arrange(date,.by_group=TRUE) %>% 
  mutate(cases_new_change1=(cases_new_rolling7-lag(cases_new_rolling7))/lag(cases_new_rolling7)*100) %>%
  mutate(cases_new_change1=round(cases_new_change1,2))
  
  temp<-cases5%>%filter(grepl("NEW YORK",xid) & admin==1)
  #View(temp)

# #merge population counts; default join using all common variables; excluding LAT LONG, which is coded differently
# cases6<-cases5%>%mutate(infile1=1)
# pop2<-pop1%>%mutate(infile2=1) %>% select(-region2,-region1,-region3)
# #cases13 <- cases12 %>% full_join(pop2)
# #cases13 <- cases12 %>% full_join(pop2,by = c("region2", "region1", "region3", "xid1", "xid2", "xid3"))
# cases13 <- cases12 %>% full_join(pop2,by = c("xid1", "xid2", "xid3"))

# cases12 %>% ungroup(xid3) %>% count(infile1)
# pop2 %>% count(infile2)
  
  # #results of merge
  # cases13 %>% ungroup(xid3) %>% count(infile1) #master + both (left join)
  # cases13 %>% ungroup(xid3) %>% count(infile2) #using + both (right join)
  # cases13 %>% ungroup(xid3) %>% count(infile1,infile2) #inner join, outer join (both, master only, using only)
  # table<-cases13%>%count(infile1,infile2)
  # #view(table)
  
  # #in MASTER only (case data, but no population counts)
  # temp <- cases13 %>% filter(infile1==1 & is.na(infile2))
  # #view(temp)
  # temp <- cases13 %>% filter(infile1==1 & is.na(infile2)) %>% count(xid3)
  # #view(temp)
  
  # #in USING only (population counts, but no case data)
  # temp <- cases13 %>% filter(is.na(infile1))
  # #view(temp)
  # temp <- cases13 %>% filter(is.na(infile1)) %>% count(xid3)
  # #view(temp)
  
  # #left join, MASTER and BOTH; count of unique regions
  # temp <- cases13 %>% filter(infile1==1) %>% count(xid3)
  # #view(temp)
  
  # temp1<-cases12%>%filter(xid3=="CANADA_DIAMOND PRINCESS_NA")
  # view(temp1)
  # temp2<-pop1%>%filter(xid3=="CANADA_DIAMOND PRINCESS_NA")
  # view(temp2)
  # temp3<-combined%>%filter(xid3=="CANADA_DIAMOND PRINCESS_NA")
  # view(temp3)
  # temp4<-combined%>%filter(lat.x!=lat.y)
  # view(temp4)

# #keep left join only; MASTER and BOTH
# cases14 <- cases13 %>% filter(infile1==1) %>% select(-infile1,-infile2)
# count(cases14)
# cases14 %>% ungroup(xid3) %>% count()

#rates
  cases6 <- cases5 %>% 
  mutate(cases_cum_per_100k=(as.double(cases_cum)/as.double(population))*100000) %>%
  mutate(cases_new_per_100k=(as.double(cases_new)/as.double(population))*100000) %>%
    mutate(cases_cum_per_100k=round(cases_cum_per_100k,2)) %>%
    mutate(cases_new_per_100k=round(cases_new_per_100k,2))
    
  temp<-cases6%>%filter(grepl("NEW YORK",xid) & admin==1)
  #View(temp)
  
  
  
  
  
  
  
  
  
  
  
  
  
  saveRDS(cases6, "C:/Users/Jonathan/OneDrive/# CODING/R DASHBOARD/data/working/cases6.rds")
  #cases6 <- readRDS("C:/Users/Jonathan/OneDrive/# CODING/R DASHBOARD/data/working/cases6.rds")
    
  ##################################################
  #final file - xid - lowest level of aggregation
  ##################################################

  #type,source,level,starts_with("admin")
  cases_final <- cases6 %>%
    select(xid,xid_seq,date,admin,admin0,admin1,admin2,starts_with("cases")) %>%
    relocate(xid,xid_seq,date,admin,admin0,admin1,admin2,starts_with("cases"))
  #View(cases_final)
  
    #export csv copy
    #write.csv(cases_final, "C:/Users/Jonathan/OneDrive/# CODING/R DASHBOARD/data/working/cases_final.csv")
    
    #export R data copy
    saveRDS(cases_final, "C:/Users/Jonathan/OneDrive/# CODING/R DASHBOARD/data/working/cases_final.rds")
    saveRDS(cases_final, "C:/Users/Jonathan/OneDrive/# CODING/R DASHBOARD/app/cases_final.rds")
  
  #load final data
  #cases_final <- readRDS("C:/Users/Jonathan/OneDrive/# CODING/R DASHBOARD/data/working/cases_final.rds")
  
  ##################################################
  #final file - reduced, keeping local data for canada and some US states (NEW YORK)
  ##################################################

    cases_final%>%ungroup()%>%count(admin)
    cases_final%>%ungroup()%>%filter(admin==2)%>%count(admin0)
    cases_final%>%ungroup()%>%filter(grepl("CANADA",xid))%>%count(admin0)
    cases_final%>%ungroup()%>%filter(grepl("UNITED STATES",xid))%>%count(admin0)
    cases_final%>%ungroup()%>%filter(grepl("CANADA",xid))%>%count(admin)
    cases_final%>%ungroup()%>%filter(grepl("UNITED STATES",xid))%>%count(admin)
    cases_final%>%ungroup()%>%filter(grepl("UNITED STATES",xid))%>%count(admin1)
    
    #type,source,level,starts_with("admin")
    cases_final_reduced <- cases_final %>%
      filter(admin %in% c(0,1) | (admin==2 & admin0 %in% c("Canada")) | (admin==2 & admin0 %in% c("United States") & admin1 %in% c("New York"))) %>%
      select(xid,xid_seq,date,admin,admin0,admin1,admin2,starts_with("cases")) %>%
      relocate(xid,xid_seq,date,admin,admin0,admin1,admin2,starts_with("cases"))
    #View(cases_final_reduced)
    
      cases_final_reduced%>%ungroup()%>%count(admin)
      cases_final_reduced%>%ungroup()%>%count(admin0)
      cases_final_reduced%>%ungroup()%>%count(admin1)
      cases_final_reduced%>%ungroup()%>%filter(admin0=="Canada")%>%count(admin1)
      cases_final_reduced%>%ungroup()%>%filter(admin0=="Canada")%>%count(admin2)
      cases_final_reduced%>%ungroup()%>%filter(admin0=="United States")%>%count(admin1)
      cases_final_reduced%>%ungroup()%>%filter(admin0=="United States")%>%count(admin2)
      cases_final_reduced%>%ungroup()%>%filter(admin==2 & admin0=="United States")%>%count(admin1)
      cases_final_reduced%>%ungroup()%>%filter(admin==2 & admin0=="United States")%>%count(admin2)
      cases_final_reduced%>%ungroup()%>%count(admin2)
      
    #export csv copy
    #write.csv(cases_final_reduced, "C:/Users/Jonathan/OneDrive/# CODING/R DASHBOARD/data/working/cases_final_reduced.csv")
    
    #export R data copy
    saveRDS(cases_final_reduced, "C:/Users/Jonathan/OneDrive/# CODING/R DASHBOARD/data/working/cases_final_reduced.rds")
    saveRDS(cases_final_reduced, "C:/Users/Jonathan/OneDrive/# CODING/R DASHBOARD/app/cases_final_reduced.rds")
    
    
    
 

    
  
  ##################################################
  #FOR MAPS
  ##################################################

  # #export copy for graphs
  # cases_final_maps <- cases_final %>% select(xid,admin,region1,region2,region3,FIPS,date,cases_cum,cases_new,cases_new_rolling7,cases_new_abschange1)
  # saveRDS(cases_final_maps, "C:/Users/Jonathan/OneDrive/# CODING/R DASHBOARD/app/cases_final_maps.rds")


  
  
  
  
  
  

  
  
  
  
  
  
  
  
  
  ##################################################
  #VALIDATION
  ##################################################
  
  table1=cases_final%>%filter(grepl("CANADA",xid))%>%count(admin,xid)
  View(table1)
  
  table1=cases_final%>%filter(admin==2)%>%count(xid)
  View(table1)
  
  #IF INCLUDING MULTIPLE LEVELS OF DATA, XID MUST UNIQUELY DISTINGUISH BETWEEN DIFFERENT ADMIN LEVELS; THIS MEANS THAT FOR ADMIN LEVEL 2, FOR EXAMPLE, ADMIN2 CANNOT BE NULL, OTHERWISE XID FOR ADMIN LEVEL 2 AND ADMIN LEVEL 1 WOULD BE THE SAME
  table1=cases_final%>%filter(admin==2)%>%count(admin2)
  View(table1)
  #THESE TWO COUNTS OF UNIQUE ROWS SHOULD BE THE SAME; IF NOT, XID MAY BE THE SAME ACROSS DIFFERENT ADMIN LEVELS
  table1=cases_final%>%count(admin,xid)
  table1
  table1=cases_final%>%count(xid)
  table1
  
  #RANGE OF DATES IN DATA
  mindate=min(cases_final$date)
  mindate
  maxdate=max(cases_final$date)
  maxdate
  
  
  
  
  
  
  
  
  