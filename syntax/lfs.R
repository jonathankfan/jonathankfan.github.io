rm(list = ls()) #clear global environment of objects

getwd()

library(plyr)
library(dplyr)
install.packages("tidyverse")
library(haven) #to import SPSS or STATA
library(labelled) #to label data
library(ggplot2)

##################################################
##################################################
##
##################################################
##################################################

#unzip all files in directory
# rootpath="C:/Users/Jonathan/Desktop/lfs/2016/dataverse_files/Data/"
# outpath="C:/Users/Jonathan/Desktop/lfs/2016/dataverse_files/Data/unzipped"
# filelist=list.files(rootpath, pattern=".zip$")
# filelist
# for (i in filelist) {
#   unzip(paste0(rootpath,i), exdir=outpath)
# }











##################################################
##################################################
##
##################################################
##################################################

#THIS CODING REQUIRES A KNOWN STRUCTURE TO THE FILES
# yearlist=c(2015,2016,2017,2018,2019,2020)
# for (iyear in yearlist) {
#   print(iyear)
#   
#   #data is saved as SPSS file; read into R
#   rootpath=paste0("C:/Users/Jonathan/Desktop/lfs/data/",iyear,"/")
#   print(rootpath)
#   filelist=list.files(rootpath, pattern=".sav$")
#   #print(filelist)
#   for (ifile in filelist) {
#     print(ifile)
#     path = paste0(rootpath,ifile)
#     ii=ifile
#     ii=gsub("-","_",ifile)
#     ii=gsub("_PUMF_EN.sav","",ii)
#     ii=gsub(".sav","",ii)
#     ii=gsub("_v2","",ii)
#     ii=gsub("January","01",ii)
#     ii=gsub("February","02",ii)
#     ii=gsub("March","03",ii)
#     ii=gsub("April","04",ii)
#     ii=gsub("May","05",ii)
#     ii=gsub("June","06",ii)
#     ii=gsub("July","07",ii)
#     ii=gsub("August","08",ii)
#     ii=gsub("September","09",ii)
#     ii=gsub("October","10",ii)
#     ii=gsub("November","11",ii)
#     ii=gsub("December","12",ii)
#     ii=gsub("Jan","01",ii)
#     ii=gsub("Feb","02",ii)
#     ii=gsub("Mar","03",ii)
#     ii=gsub("Apr","04",ii)
#     ii=gsub("May","05",ii)
#     ii=gsub("Jun","06",ii)
#     ii=gsub("Jul","07",ii)
#     ii=gsub("Aug","08",ii)
#     ii=gsub("Sep","09",ii)
#     ii=gsub("Oct","10",ii)
#     ii=gsub("Nov","11",ii)
#     ii=gsub("Dec","12",ii)
#     ii=gsub("2019a","2019",ii)
#     ii=gsub("LFS_","",toupper(ii))
#     ii=gsub("LFS","",toupper(ii))
#     print(ii)
#     assign(ii,read_sav(path))
#   }
# 
# }

##################################################
##################################################
##
##################################################
##################################################

#THIS CODING IS GENERIC AND ASSUMES NO STRUCTURE TO THE FILES; HOWEVER, MUST MANUALLY ENSURE THAT ONLY 12 MONTHLY FILES ARE IN THE RESPECTIVE ANNUAL FOLDER
yearlist=c(2015,2016,2017,2018,2019,2020)
for (iyear in yearlist) {
  print(iyear)
  
  #data is saved as SPSS file; read into R
  rootpath=paste0("C:/Users/Jonathan/Desktop/lfs/data/",iyear,"/")
  print(rootpath)
  filelist=list.files(rootpath, pattern=".sav$")
  print(paste0("NUMBER OF FILES IN ANNNUAL FOLDER: ",length(filelist))) #ASSERT THIS EQUALS 12
  iifile=0 #reset file naming counter for each annual folder
  for (ifile in filelist) {
    print(ifile)
    path = paste0(rootpath,ifile)
    iifile=iifile+1
    print(iifile)
    assign(paste0(iyear,"_",iifile),read_sav(path))
  }
  
}

##################################################
##################################################
##
##################################################
##################################################

#check if error for missing columns
LFS2015 = rbind(`2015_1`,`2015_2`,`2015_3`,`2015_4`,`2015_5`,`2015_6`,`2015_7`,`2015_8`,`2015_9`,`2015_10`,`2015_11`,`2015_12`)
LFS2015 = rbind.fill(`2015_1`,`2015_2`,`2015_3`,`2015_4`,`2015_5`,`2015_6`,`2015_7`,`2015_8`,`2015_9`,`2015_10`,`2015_11`,`2015_12`) %>%
  rename(FINALWT=FWEIGHT)
View(LFS2015)
  
  table1 <- LFS2015 %>% count(SURVMNTH)
  View(table1)

#check if error for missing columns
LFS2016 = rbind(`2016_1`,`2016_2`,`2016_3`,`2016_4`,`2016_5`,`2016_6`,`2016_7`,`2016_8`,`2016_9`,`2016_10`,`2016_11`,`2016_12`)
LFS2016 = rbind.fill(`2016_1`,`2016_2`,`2016_3`,`2016_4`,`2016_5`,`2016_6`,`2016_7`,`2016_8`,`2016_9`,`2016_10`,`2016_11`,`2016_12`) %>%
  rename(FINALWT=FWEIGHT)
View(LFS2016)

  table1 <- LFS2016 %>% count(SURVMNTH)
  View(table1)
  
#check if error for missing columns
LFS2017 = rbind(`2017_1`,`2017_2`,`2017_3`,`2017_4`,`2017_5`,`2017_6`,`2017_7`,`2017_8`,`2017_9`,`2017_10`,`2017_11`,`2017_12`)
LFS2017 = rbind.fill(`2017_1`,`2017_2`,`2017_3`,`2017_4`,`2017_5`,`2017_6`,`2017_7`,`2017_8`,`2017_9`,`2017_10`,`2017_11`,`2017_12`)
View(LFS2017)

  table1 <- LFS2017 %>% count(SURVMNTH)
  View(table1)

#check if error for missing columns
LFS2018 = rbind(`2018_1`,`2018_2`,`2018_3`,`2018_4`,`2018_5`,`2018_6`,`2018_7`,`2018_8`,`2018_9`,`2018_10`,`2018_11`,`2018_12`)
LFS2018 = rbind.fill(`2018_1`,`2018_2`,`2018_3`,`2018_4`,`2018_5`,`2018_6`,`2018_7`,`2018_8`,`2018_9`,`2018_10`,`2018_11`,`2018_12`)
View(LFS2018)

  table1 <- LFS2018 %>% count(SURVMNTH)
  View(table1)

#check if error for missing columns
LFS2019 = rbind(`2019_1`,`2019_2`,`2019_3`,`2019_4`,`2019_5`,`2019_6`,`2019_7`,`2019_8`,`2019_9`,`2019_10`,`2019_11`,`2019_12`)
LFS2019 = rbind.fill(`2019_1`,`2019_2`,`2019_3`,`2019_4`,`2019_5`,`2019_6`,`2019_7`,`2019_8`,`2019_9`,`2019_10`,`2019_11`,`2019_12`)
View(LFS2019)

  table1 <- LFS2019 %>% count(SURVMNTH)
  View(table1)

#check if error for missing columns
LFS2020 = rbind(`2020_1`,`2020_2`,`2020_3`,`2020_4`,`2020_5`,`2020_6`,`2020_7`,`2020_8`,`2020_9`,`2020_10`,`2020_11`,`2020_12`)
LFS2020 = rbind.fill(`2020_1`,`2020_2`,`2020_3`,`2020_4`,`2020_5`,`2020_6`,`2020_7`,`2020_8`,`2020_9`,`2020_10`,`2020_11`,`2020_12`)
View(LFS2020)

  table1 <- LFS2020 %>% count(SURVMNTH)
  View(table1)
  
  

  
  
#need to harmonize LFSSTAT

  # #2016
  # $LFSSTAT
  # Employed, at work   Employed, absent from work Unemployed, temporary layoff 
  # 1                            2                            3 
  # Unemployed, job searcher     Unemployed, future start          Not in labour force 
  # 4                            5                            6 
  # 
  # #2017
  # $LFSSTAT
  # Employed, at work Employed, absent from work                 Unemployed 
  # 1                          2                          3 
  # Not in labour force 
  # 4 

table1 <- LFS2016 %>% count(LFSSTAT)
View(table1)

table1 <- LFS2017 %>% count(LFSSTAT)
View(table1)

table1 <- LFS2018 %>% count(LFSSTAT)
View(table1)

table1 <- LFS2019 %>% count(LFSSTAT)
View(table1)

table1 <- LFS2020 %>% count(LFSSTAT)
View(table1)













#HARMONIZE LFSSTAT
#create working file with selected variables and new dummy indicators
LFS2016_working=zap_labels(LFS2016) %>% 
  select(SURVYEAR,SURVMNTH,PROV,LFSSTAT,SEX,FINALWT,NAICS_18,NAICS_43,NOCS_01_47) %>%
  mutate(HEALTHCARE=if_else(NAICS_18==14,1,0,9)) %>%
  mutate(MANUFACTURING=if_else(NAICS_18==5 | NAICS_18==6,1,0,9)) %>%
  mutate(INDUSTRY=if_else(HEALTHCARE==1,1,if_else(MANUFACTURING==1,2,3,9),9)) %>%
  mutate(lfs_all=FINALWT) %>%
  mutate(lfs_emp=if_else(LFSSTAT==1 | LFSSTAT==2 | LFSSTAT==3,FINALWT,0,0)) %>%
  set_variable_labels(INDUSTRY="INDUSTRY TYPE") %>%
  set_value_labels(INDUSTRY=c(HC = 1, MF = 2, OTHER = 3))
View(LFS2016_working)
#val_labels(LFS2016)
#val_labels(LFS2016_working)

LFS2017_working=zap_labels(LFS2017) %>% 
  select(SURVYEAR,SURVMNTH,PROV,LFSSTAT,SEX,FINALWT,NAICS_21,NOC_40) %>%
  mutate(HEALTHCARE=if_else(NAICS_21==17,1,0,9)) %>%
  mutate(MANUFACTURING=if_else(NAICS_21==7 | NAICS_21==8,1,0,9)) %>%
  mutate(INDUSTRY=if_else(HEALTHCARE==1,1,if_else(MANUFACTURING==1,2,3,9),9)) %>%
  mutate(lfs_all=FINALWT) %>%
  mutate(lfs_emp=if_else(LFSSTAT==1 | LFSSTAT==2,FINALWT,0,0)) %>%
  set_variable_labels(INDUSTRY="INDUSTRY TYPE") %>%
  set_value_labels(INDUSTRY=c(HC = 1, MF = 2, OTHER = 3))
View(LFS2017_working)
#val_labels(LFS2017)
#val_labels(LFS2017_working)

LFS2018_working=zap_labels(LFS2018) %>% 
  select(SURVYEAR,SURVMNTH,PROV,LFSSTAT,SEX,FINALWT,NAICS_21,NOC_40) %>%
  mutate(HEALTHCARE=if_else(NAICS_21==17,1,0,9)) %>%
  mutate(MANUFACTURING=if_else(NAICS_21==7 | NAICS_21==8,1,0,9)) %>%
  mutate(INDUSTRY=if_else(HEALTHCARE==1,1,if_else(MANUFACTURING==1,2,3,9),9)) %>%
  mutate(lfs_all=FINALWT) %>%
  mutate(lfs_emp=if_else(LFSSTAT==1 | LFSSTAT==2,FINALWT,0,0)) %>%
  set_variable_labels(INDUSTRY="INDUSTRY TYPE") %>%
  set_value_labels(INDUSTRY=c(HC = 1, MF = 2, OTHER = 3))
View(LFS2018_working)
#val_labels(LFS2018)
#val_labels(LFS2018_working)
#val_labels(LFS2018_working)=NULL #REMOVE LABELS
LFS2018_working %>% count(INDUSTRY)

LFS2019_working=zap_labels(LFS2019) %>% 
  select(SURVYEAR,SURVMNTH,PROV,LFSSTAT,SEX,FINALWT,NAICS_21,NOC_40) %>%
  mutate(HEALTHCARE=if_else(NAICS_21==17,1,0,9)) %>%
  mutate(MANUFACTURING=if_else(NAICS_21==7 | NAICS_21==8,1,0,9)) %>%
  mutate(INDUSTRY=if_else(HEALTHCARE==1,1,if_else(MANUFACTURING==1,2,3,9),9)) %>%
  mutate(lfs_all=FINALWT) %>%
  mutate(lfs_emp=if_else(LFSSTAT==1 | LFSSTAT==2,FINALWT,0,0)) %>%
  set_variable_labels(INDUSTRY="INDUSTRY TYPE") %>%
  set_value_labels(INDUSTRY=c(HC = 1, MF = 2, OTHER = 3))
View(LFS2019_working)
LFS2019_working %>% count(INDUSTRY)
#val_labels(LFS2019)
#val_labels(LFS2019_working)
#val_labels(LFS2019_working)=NULL #REMOVE LABELS

LFS2020_working=zap_labels(LFS2020) %>% 
  select(SURVYEAR,SURVMNTH,PROV,LFSSTAT,SEX,FINALWT,NAICS_21,NOC_40) %>%
  mutate(HEALTHCARE=if_else(NAICS_21==17,1,0,9)) %>%
  mutate(MANUFACTURING=if_else(NAICS_21==7 | NAICS_21==8,1,0,9)) %>%
  mutate(INDUSTRY=if_else(HEALTHCARE==1,1,if_else(MANUFACTURING==1,2,3,9),9)) %>%
  mutate(lfs_all=FINALWT) %>%
  mutate(lfs_emp=if_else(LFSSTAT==1 | LFSSTAT==2,FINALWT,0,0)) %>%
  set_variable_labels(INDUSTRY="INDUSTRY TYPE") %>%
  set_value_labels(INDUSTRY=c(HC = 1, MF = 2, OTHER = 3))
View(LFS2020_working)
LFS2020_working %>% count(INDUSTRY)
#val_labels(LFS2020)
#val_labels(LFS2020_working)
#val_labels(LFS2020_working)=NULL #REMOVE LABELS








#append data
#check if error for missing columns
#LFSALL = rbind(LFS2016_working,LFS2017_working,LFS2018_working)
#LFSALL = rbind.fill(LFS2016_working,LFS2017_working,LFS2018_working)
LFSALL = bind_rows(LFS2016_working,LFS2017_working,LFS2018_working,LFS2019_working,LFS2020_working)
View(LFSALL)

  #confirm 12 months for each survey year
  table1 <- LFSALL %>% count(SURVYEAR,SURVMNTH)
  View(table1)
  
  #need to harmonize LFSSTAT
  table1 <- LFSALL %>% count(SURVYEAR,LFSSTAT)
  View(table1)
  
  #confirm industry variable
  table1 <- LFSALL %>% count(SURVYEAR,INDUSTRY,HEALTHCARE,MANUFACTURING)
  View(table1)
  table1 <- LFSALL %>% count(INDUSTRY,HEALTHCARE,MANUFACTURING)
  View(table1)
  
          
          












          
          
          
          
          
##################################################
##################################################
##
##################################################
##################################################

#Labour force characteristics by province, monthly, unadjusted for seasonality (x 1,000)
#https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1410001702

head(LFSALL)

#table1 <- LFSALL %>% filter(PROV==46 & (LFSSTAT==1 | LFSSTAT==2)) %>% group_by(SURVYEAR,SURVMNTH) %>% select(FINALWT) %>% summarise(n(),sum(FINALWT),min(FINALWT,na.rm=TRUE),mean(FINALWT,na.rm=TRUE),median(FINALWT,na.rm=TRUE),max(FINALWT,na.rm=TRUE))
table1 <- LFSALL %>% 
  filter(PROV==46) %>% 
  group_by(SURVYEAR,SURVMNTH,INDUSTRY) %>% 
  summarise(n(),lfs_all=sum(lfs_all),min(lfs_all,na.rm=TRUE),mean(lfs_all,na.rm=TRUE),median(lfs_all),max(lfs_all,na.rm=TRUE),lfs_emp=sum(lfs_emp),min(lfs_emp,na.rm=TRUE),mean(lfs_emp,na.rm=TRUE),median(lfs_emp),max(lfs_emp,na.rm=TRUE))
View(table1)
head(table1)
  
  write.csv(table1, file = "C:/Users/Jonathan/Desktop/lfs/data/2016/table1.csv")

#table2 <- table1 %>% group_by(SURVYEAR) %>% summarise(n(),mean(`sum(FINALWT)`,na.rm=TRUE))
table2 <- table1 %>% 
  group_by(SURVYEAR,INDUSTRY) %>% 
  summarise(n(),lfs_all=mean(lfs_all,na.rm=TRUE),lfs_emp=mean(lfs_emp,na.rm=TRUE))
View(table2)
head(table2)
  
  df<-table2 %>% filter(INDUSTRY==1 | INDUSTRY==2)
  ggplot(df) + 
    geom_line(aes(x=SURVYEAR,y=lfs_emp,group=INDUSTRY,color=factor(INDUSTRY))) +
    scale_color_manual(name="Industry", breaks=c(1,2), labels = c("1 HC","2  MF"), values = c("blue", "red")) +
    labs(title="LFS - EMPLOYED",y="Pop",x="Year")























































##################################################
##################################################
##
##################################################
##################################################

#data is saved as SPSS file; read into R
rootpath="C:/Users/Jonathan/Desktop/lfs/2021/"
filelist=list.files(rootpath, pattern=".sav$")
filelist
for (i in filelist) {
  path = paste0(rootpath,i)
  ii=i
  #ii=gsub("-","_",i)
  #ii=gsub("_PUMF_EN.sav","",ii)
  ii=gsub(".sav","",ii)
  print(ii)
  assign(ii,read_sav(path))
}

LFS2021=LFS_May_2021
View(LFS2021)

##################################################
##################################################
##
##################################################
##################################################

#Labour force characteristics by province, monthly, unadjusted for seasonality (x 1,000)
#https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1410001702

table1 <- LFS2021 %>% filter(PROV==46 & (LFSSTAT==1 | LFSSTAT==2)) %>% group_by(SURVMNTH) %>% select(FINALWT) %>% summarise(n(),sum(FINALWT),min(FINALWT,na.rm=TRUE),mean(FINALWT,na.rm=TRUE),median(FINALWT,na.rm=TRUE),max(FINALWT,na.rm=TRUE))
View(table1)

table1 <- LFS2021 %>% filter(PROV==59 & (LFSSTAT==1 | LFSSTAT==2)) %>% group_by(SURVMNTH) %>% select(FINALWT) %>% summarise(n(),sum(FINALWT),min(FINALWT,na.rm=TRUE),mean(FINALWT,na.rm=TRUE),median(FINALWT,na.rm=TRUE),max(FINALWT,na.rm=TRUE))
View(table1)





