
####################################################################################################
####################################################################################################
#ABOUT#
#This syntax provides supplementary analysis (with some modifications) for a published paper examining the relationship between psychosocial work conditions and mental health outcomes among workers, using data from the CCHS 2012 Mental Health survey.
#The analysis below uses files from the Public Use Microdata Files. The orgiinal syntax files and raw data are located at the Statistics Canada Research Data Centre (Toronto), given privacy considerations.
#The paper was published by Fan et al. (2019) in the Annals of Work Exposures and Health (2019;63(5):546-559).
#See here for published paper: https://raw.githubusercontent.com/jonathankfan/jonathankfan.github.io/main/publications/Fan.2019.ANNWEH.pdf
####################################################################################################
####################################################################################################

####################################################################################################
####################################################################################################
#DOWNLOAD DATA#
#CCHS 2012: Mental Health Component
#http://sda.chass.utoronto.ca/cgi-bin/sda/hsda?harcsda3+cchs2012mh
#Download -> Customized Subset (allow popups)
#Choose "CSV file"
#Check "Data definitions for Stata"
#Select ALL variables (file is small, 32 MB)
#Click "Continue"
#Click "Create the files"
#It should say "587 variables for 25113 cases in subset"
####################################################################################################
####################################################################################################

####################################################################################################
####################################################################################################
#LOAD PACKAGES#
####################################################################################################
####################################################################################################

#install.packages("haven")
library(haven) #read stata

#install.packages("ClusterR")
#install.packages("cluster")
library(ClusterR)
library(cluster)

#install.packages("mlogit")
library(mlogit) #mulinomial logistic models

#install.packages("nnet")
library(nnet) #mulinomial logistic models

library(tidyverse)  #data manipulation

library(ggplot2) #graphs

#install.packages("epiDisplay")
library(epiDisplay) #lroc

#install.packages("survey")
library(survey) #surveyglm

#install.packages("srvyr")
library(srvyr) #allow survey weights with dplyr summarize

#install.packages("tableone")
library(tableone) #to create tables with standardized mean difference

##################################################
#LOAD CCHS 2012 MENTAL HEALTH SURVEY#
##################################################

data1 <- read_dta("[FILEPATH]/cchs2012_mh_old.dta") 
str(data1)
glimpse(data1)
count(data1)

####################################################################################################
####################################################################################################
#DATA PROCESSING#
####################################################################################################
####################################################################################################

#filters
#WORKED IN PAST 12 MONTHS
#EMPLOYEES ONLY (EXCLUDING SELF-EMPLOYED)
data2<-data1%>%
  rename_all(tolower)%>%
  filter(gen_08==1 & lbsg31==1)
count(data2)

#outcome variable: depression in past 12 months: yes/no
#exposure variable: job control (combination of skill discretion and decision authority): continuous, binary, categorical
data3<-data2%>%
  mutate(dep_yn=if_else(depddy==2,0,if_else(depddy==1,1,NA_real_))) %>%
  mutate(jobcontrol=wstdski+wstdaut) %>%
  mutate(jobcontrol_binary=if_else(jobcontrol>=8 & !is.na(jobcontrol),1,if_else(jobcontrol<8 & !is.na(jobcontrol),0,NA_real_))) %>%
  mutate(jobcontrol_q4=if_else(jobcontrol>=9 & !is.na(jobcontrol),4,if_else(jobcontrol>=7 & jobcontrol<9,3,if_else(jobcontrol>=5 & jobcontrol<7,2,if_else(jobcontrol>=0 & jobcontrol<5,1,NA_real_)))))
data3%>%count(dep_yn,depddy)
data3%>%group_by(jobcontrol_q4)%>%summarize(min(jobcontrol,na.rm=TRUE),max(jobcontrol,na.rm=TRUE))

####################################################################################################
####################################################################################################
#BINARY TREATMENT#
####################################################################################################
####################################################################################################

##################################################
#PSCORE MODELS#
##################################################

#pscore model
logit_treatment<-glm(jobcontrol_binary ~ wstdpsy + wstdsoc + wstdphy + wstdjin + as.factor(geo_prv) + as.factor(dhh_sex) + as.factor(dhhgms) + as.factor(dhhghsz) + as.factor(dhhgdwe), data=data3, na.action=na.exclude, family="binomial")
  
  summary(logit_treatment)
  exp(cbind(OR = coef(logit_treatment), confint(logit_treatment)))
  lroc(logit_treatment)

#pscore model - base, for stabilized weights that sum to study sample
#this calculates the baseline prevalence of the treatment variable
logit_treatment_base<-glm(jobcontrol_binary ~ 1, data=data3, na.action=na.exclude, family="binomial")
  
  summary(logit_treatment_base)
  exp(cbind(OR = coef(logit_treatment_base), confint(logit_treatment_base)))

#predict pscore based on above models, and merge back to data 
data4<-data3
data4$pscore<-predict(logit_treatment,newdata=data3,type="response")
data4$pscore_base<-predict(logit_treatment_base,newdata=data3,type="response")

##################################################
#DIAGNOSTICS#
##################################################

#check region of common support, i.e., positivity assumption
#if there are regions outside of common support, then we could use pscore matching with calipers or restrict IPTW analysis to region of common support (or to pscores within 0.1 to 0.9), to reduce bias (see: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5564952)
data4%>%filter(!is.na(jobcontrol_binary))%>%
  ggplot(aes(x = pscore, fill=jobcontrol_binary)) +
  geom_histogram(color = "white") + 
  facet_wrap(~jobcontrol_binary)

#check the mean pscore or distribution of pscore by treatment, within strata of pscore
  
  #pscore 0.2 to 0.29
  data4%>%filter(pscore>=.2 & pscore<.3)%>%group_by(jobcontrol_binary)%>%summarize(mean(pscore,na.rm=TRUE))

  #pscore 0.4 to 0.49
  data4%>%filter(pscore>=.4 & pscore<.5)%>%group_by(jobcontrol_binary)%>%summarize(mean(pscore,na.rm=TRUE))

  #pscore 0.7 to 0.79
  data4%>%filter(pscore>=.7 & pscore<.8)%>%group_by(jobcontrol_binary)%>%summarize(mean(pscore,na.rm=TRUE))

##################################################
#IPTW#
##################################################

#generate IPTW - unstabilized and stabilized versions
#for stabilized weights, multiply the weights by the baseline prevalence of treatment and control in the overall sample (marginal probability of treatment)
#use 1/1-pr for control group, which would be same as using the reverse scored outcome to generate pr followed by 1/pr
#note, this formula is the same, but in one line: generate iptw=(treatment/pscore) + ((1-treatment)/(1-pscore))
data5<-data4%>%
  mutate(iptw=if_else(jobcontrol_binary==1,1/pscore,if_else(jobcontrol_binary==0,1/(1-pscore),NA_real_)))%>%
  mutate(iptw_stab=if_else(jobcontrol_binary==1,pscore_base/pscore,if_else(jobcontrol_binary==0,(1-pscore_base)/(1-pscore),NA_real_)))

#check weights do not sum to study population (approximately twice the size)
data5%>%summarize(iptw_sum=sum(iptw,na.rm = TRUE))

#check stabilized weights sum to study population
data5%>%summarize(iptw_stab_sum=sum(iptw_stab,na.rm = TRUE))

#can trim weights at the 1/99 percentiles to stabilize

##################################################
#ASSESS BALANCE#
##################################################

#assess balance of covariates across treatment groups using standardized differences or boxplots
#check balance in the unmatched sample, within strata of pscore, and within the IPTW weighted sample

  #unweighted
  unweighted_mean<-data5%>%group_by(jobcontrol_binary)%>%summarize_at(c("wstdpsy","wstdsoc","wstdphy","wstdjin","geo_prv","dhh_sex","dhhgms","dhhghsz","dhhgdwe"),mean,na.rm=TRUE)
  view(unweighted_mean)
  unweighted_sd<-data5%>%group_by(jobcontrol_binary)%>%summarize_at(c("wstdpsy","wstdsoc","wstdphy","wstdjin","geo_prv","dhh_sex","dhhgms","dhhghsz","dhhgdwe"),sd,na.rm=TRUE)
  view(unweighted_sd)

  #weighted
  weighted_mean<-data5%>%filter(!is.na(iptw))%>%as_survey(weights=iptw)%>%group_by(jobcontrol_binary)%>%summarize_at(c("wstdpsy","wstdsoc","wstdphy","wstdjin","geo_prv","dhh_sex","dhhgms","dhhghsz","dhhgdwe"),survey_mean,na.rm=TRUE)
  view(weighted_mean)
  
#standardized differences allow comparison of means without regard to units or scales, and without regard to sample size, and since we are not concerned with the population from which the sample was drawn

  #unweighted
  unweighted<-CreateTableOne(vars=c("wstdpsy","wstdsoc","wstdphy","wstdjin","geo_prv","dhh_sex","dhhgms","dhhghsz","dhhgdwe"),strata="jobcontrol_binary",data=data5,test=FALSE)
  print(unweighted,smd=TRUE)
  
  #weighted - this doesn't work - need to fix - but can manually calculate weighted SMD using weighted mean and weighted SD using the formula: smd = (`mean1'-`mean0')/((((`sd1'^2)+(`sd0'^2))/2)^(1/2))
  #data6<-data5%>%filter(!is.na(iptw))
  #svydesign<-svydesign(id=~adm_rno, weights=~iptw, data=data6)
  #weighted<-svyCreateTableOne(vars=c("wstdpsy","wstdsoc","wstdphy","wstdjin","geo_prv","dhh_sex","dhhgms","dhhghsz","dhhgdwe"),strata="jobcontrol_binary",data=svydesign,test=FALSE)
  
##################################################
#OUTCOME MODELS#
##################################################

#outcome model (marginal structural model) with IPTW weights
data6<-data5%>%filter(!is.na(iptw))
svydesign<-svydesign(id=~adm_rno, weights=~iptw, data=data6)
logit_outcome<-svyglm(dep_yn ~ jobcontrol_binary, data=data6, family="quasibinomial", design=svydesign)
  
  summary(logit_outcome)
  exp(coef(logit_outcome))
  confint(logit_outcome)
  exp(cbind(OR = coef(logit_outcome), confint(logit_outcome)))
  lroc(logit_outcome)

#outcome model (marginal structural model) with IPTW weights stabilized
data6<-data5%>%filter(!is.na(iptw_stab))
svydesign<-svydesign(id=~adm_rno, weights=~iptw_stab, data=data6)
logit_outcome<-svyglm(dep_yn ~ jobcontrol_binary, data=data6, family="quasibinomial", design=svydesign)
  
  summary(logit_outcome)
  exp(coef(logit_outcome))
  confint(logit_outcome)
  exp(cbind(OR = coef(logit_outcome), confint(logit_outcome)))
  lroc(logit_outcome)

#outcome model (marginal structural model) with IPTW weights - double robust with covariates used for pscore model 
data6<-data5%>%filter(!is.na(iptw))
svydesign<-svydesign(id=~adm_rno, weights=~iptw, data=data6)
logit_outcome<-svyglm(dep_yn ~ jobcontrol_binary + wstdpsy + wstdsoc + wstdphy + wstdjin + as.factor(geo_prv) + as.factor(dhh_sex) + as.factor(dhhgms) + as.factor(dhhghsz) + as.factor(dhhgdwe), data=data6, family="quasibinomial", design=svydesign)

  summary(logit_outcome)
  exp(coef(logit_outcome))
  confint(logit_outcome)
  exp(cbind(OR = coef(logit_outcome), confint(logit_outcome)))
  lroc(logit_outcome)

#can also run regression models, direct adjustment with pscore, stratification by pscore, matching on pscore

####################################################################################################
####################################################################################################
#CATEGORICAL TREATMENT#
####################################################################################################
####################################################################################################

##################################################
#PSCORE MODELS#
##################################################

#pscore model
#specify base outcome; first, convert outcome to factor variable
data3_mlogit<-data3%>%mutate(jobcontrol_q4=as.factor(jobcontrol_q4))
data3_mlogit$jobcontrol_q4<-relevel(data3_mlogit$jobcontrol_q4,ref=1)
mlogit_treatment<-multinom(jobcontrol_q4 ~ wstdpsy + wstdsoc + wstdphy + wstdjin + as.factor(geo_prv) + as.factor(dhh_sex) + as.factor(dhhgms) + as.factor(dhhghsz) + as.factor(dhhgdwe), data=data3_mlogit, na.action=na.exclude)
  
  summary(mlogit_treatment)
  exp(coef(mlogit_treatment))
  exp(confint(mlogit_treatment))

#pscore model - base, for stabilized weights that sum to study sample
#this calculates the baseline prevalence of the treatment variable
#specify base outcome; first, convert outcome to factor variable
data3_mlogit<-data3%>%mutate(jobcontrol_q4=as.factor(jobcontrol_q4))
data3_mlogit$jobcontrol_q4<-relevel(data3_mlogit$jobcontrol_q4,ref=1)
mlogit_treatment_base<-multinom(jobcontrol_q4 ~ 1, data=data3_mlogit, na.action=na.exclude)
  
  summary(mlogit_treatment_base)
  exp(coef(mlogit_treatment_base))
  exp(confint(mlogit_treatment_base))

#predict pscore based on above models, and merge back to data; note, all outcome probabilities across levels add to "1"
data4<-cbind(data3,fitted(mlogit_treatment,newdata=data3))%>%
  rename(pscore_1=`1`,pscore_2=`2`,pscore_3=`3`,pscore_4=`4`)
data5<-cbind(data4,fitted(mlogit_treatment_base,newdata=data3))%>%
  rename(pscore_1_base=`1`,pscore_2_base=`2`,pscore_3_base=`3`,pscore_4_base=`4`)

##################################################
#DIAGNOSTICS#
##################################################

#check region of common support, i.e., positivity assumption
#if there are regions outside of common support, then we could use pscore matching with calipers or restrict IPTW analysis to region of common support (or to pscores within 0.1 to 0.9), to reduce bias (see: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5564952)

  data5%>%filter(!is.na(jobcontrol_q4))%>%
    ggplot(aes(x = pscore_1, fill=jobcontrol_q4)) +
    geom_histogram(color = "white") + 
    facet_wrap(~jobcontrol_q4)
    
  data5%>%filter(!is.na(jobcontrol_q4))%>%
    ggplot(aes(x = pscore_2, fill=jobcontrol_q4)) +
    geom_histogram(color = "white") + 
    facet_wrap(~jobcontrol_q4)
  
  data5%>%filter(!is.na(jobcontrol_q4))%>%
    ggplot(aes(x = pscore_3, fill=jobcontrol_q4)) +
    geom_histogram(color = "white") + 
    facet_wrap(~jobcontrol_q4)
  
  data5%>%filter(!is.na(jobcontrol_q4))%>%
    ggplot(aes(x = pscore_4, fill=jobcontrol_q4)) +
    geom_histogram(color = "white") + 
    facet_wrap(~jobcontrol_q4)

#check the mean pscore or distribution of pscore by treatment, within strata of pscore
    
  #pscore 0.2 to 0.29
  data5%>%filter(pscore_1>=.2 & pscore_1<.3)%>%group_by(jobcontrol_q4)%>%summarize(mean(pscore_1,na.rm=TRUE))
  data5%>%filter(pscore_2>=.2 & pscore_2<.3)%>%group_by(jobcontrol_q4)%>%summarize(mean(pscore_2,na.rm=TRUE))
  data5%>%filter(pscore_3>=.2 & pscore_3<.3)%>%group_by(jobcontrol_q4)%>%summarize(mean(pscore_3,na.rm=TRUE))
  data5%>%filter(pscore_4>=.2 & pscore_4<.3)%>%group_by(jobcontrol_q4)%>%summarize(mean(pscore_4,na.rm=TRUE))
  
  #pscore 0.4 to 0.49
  data5%>%filter(pscore_1>=.4 & pscore_1<.5)%>%group_by(jobcontrol_q4)%>%summarize(mean(pscore_1,na.rm=TRUE))
  data5%>%filter(pscore_2>=.4 & pscore_2<.5)%>%group_by(jobcontrol_q4)%>%summarize(mean(pscore_2,na.rm=TRUE))
  data5%>%filter(pscore_3>=.4 & pscore_3<.5)%>%group_by(jobcontrol_q4)%>%summarize(mean(pscore_3,na.rm=TRUE))
  data5%>%filter(pscore_4>=.4 & pscore_4<.5)%>%group_by(jobcontrol_q4)%>%summarize(mean(pscore_4,na.rm=TRUE))
  
  #pscore 0.7 to 0.79
  data5%>%filter(pscore_1>=.7 & pscore_1<.8)%>%group_by(jobcontrol_q4)%>%summarize(mean(pscore_1,na.rm=TRUE))
  data5%>%filter(pscore_2>=.7 & pscore_2<.8)%>%group_by(jobcontrol_q4)%>%summarize(mean(pscore_2,na.rm=TRUE))
  data5%>%filter(pscore_3>=.7 & pscore_3<.8)%>%group_by(jobcontrol_q4)%>%summarize(mean(pscore_3,na.rm=TRUE))
  data5%>%filter(pscore_4>=.7 & pscore_4<.8)%>%group_by(jobcontrol_q4)%>%summarize(mean(pscore_4,na.rm=TRUE))
  
##################################################
#IPTW#
##################################################

#generate IPTW - unstabilized and stabilized versions
#for stabilized weights, multiply the weights by the baseline prevalence of treatment and control in the overall sample (marginal probability of treatment)
#use 1/1-pr for control group, which would be same as using the reverse scored outcome to generate pr followed by 1/pr
#note, this formula is the same, but in one line: generate iptw=(treatment/pscore) + ((1-treatment)/(1-pscore))
data6<-data5%>%
  mutate(iptw_q4=if_else(jobcontrol_q4==1,1/pscore_1,
                         if_else(jobcontrol_q4==2,1/pscore_2,
                                 if_else(jobcontrol_q4==3,1/pscore_3,
                                         if_else(jobcontrol_q4==4,1/pscore_4,
                                                 NA_real_)))))%>%
  mutate(iptw_stab_q4=if_else(jobcontrol_q4==1,pscore_1_base/pscore_1,
                         if_else(jobcontrol_q4==2,pscore_2_base/pscore_2,
                                 if_else(jobcontrol_q4==3,pscore_3_base/pscore_3,
                                         if_else(jobcontrol_q4==4,pscore_4_base/pscore_4,
                                                 NA_real_)))))

#check weights do not sum to study population (approximately twice the size)
data6%>%summarize(iptw_q4_sum=sum(iptw_q4,na.rm = TRUE))

#check stabilized weights sum to study population
data6%>%summarize(iptw_stab_q4_sum=sum(iptw_stab_q4,na.rm = TRUE))

#can trim weights at the 1/99 percentiles to stabilize

##################################################
#ASSESS BALANCE#
##################################################

#assess balance of covariates across treatment groups using standardized differences or boxplots
#check balance in the unmatched sample, within strata of pscore, and within the IPTW weighted sample

  #unweighted
  unweighted_mean<-data6%>%group_by(jobcontrol_q4)%>%summarize_at(c("wstdpsy","wstdsoc","wstdphy","wstdjin","geo_prv","dhh_sex","dhhgms","dhhghsz","dhhgdwe"),mean,na.rm=TRUE)
  view(unweighted_mean)
  unweighted_sd<-data6%>%group_by(jobcontrol_q4)%>%summarize_at(c("wstdpsy","wstdsoc","wstdphy","wstdjin","geo_prv","dhh_sex","dhhgms","dhhghsz","dhhgdwe"),sd,na.rm=TRUE)
  view(unweighted_sd)
  
  #weighted
  weighted_mean<-data6%>%filter(!is.na(iptw_q4))%>%as_survey(weights=iptw_q4)%>%group_by(jobcontrol_q4)%>%summarize_at(c("wstdpsy","wstdsoc","wstdphy","wstdjin","geo_prv","dhh_sex","dhhgms","dhhghsz","dhhgdwe"),survey_mean,na.rm=TRUE)
  view(weighted_mean)

#standardized differences allow comparison of means without regard to units or scales, and without regard to sample size, and since we are not concerned with the population from which the sample was drawn

  #unweighted
  unweighted<-CreateTableOne(vars=c("wstdpsy","wstdsoc","wstdphy","wstdjin","geo_prv","dhh_sex","dhhgms","dhhghsz","dhhgdwe"),strata="jobcontrol_q4",data=data6,test=FALSE)
  print(unweighted,smd=TRUE)
  
  #weighted - this doesn't work - need to fix - but can manually calculate weighted SMD using weighted mean and weighted SD using the formula: smd = (`mean1'-`mean0')/((((`sd1'^2)+(`sd0'^2))/2)^(1/2))
  #data6<-data5%>%filter(!is.na(iptw_q4))
  #svydesign<-svydesign(id=~adm_rno, weights=~iptw_q4, data=data6)
  #weighted<-svyCreateTableOne(vars=c("wstdpsy","wstdsoc","wstdphy","wstdjin","geo_prv","dhh_sex","dhhgms","dhhghsz","dhhgdwe"),strata="jobcontrol_q4",data=svydesign,test=FALSE)

##################################################
#OUTCOME MODELS#
##################################################

#outcome model (marginal structural model) with IPTW weights; note: drop missing data, as svyglm does not work with missing weights
data7<-data6%>%filter(!is.na(iptw_q4))
svydesign<-svydesign(id=~adm_rno, weights=~iptw_q4, data=data7)
logit_outcome<-svyglm(dep_yn ~ as.factor(jobcontrol_q4), data=data7, family="quasibinomial", design=svydesign)
  
  summary(logit_outcome)
  exp(coef(logit_outcome))
  confint(logit_outcome)
  exp(cbind(OR = coef(logit_outcome), confint(logit_outcome)))
  lroc(logit_outcome)

#outcome model (marginal structural model) with IPTW weights stabilized; note: drop missing data, as svyglm does not work with missing weights
data7<-data6%>%filter(!is.na(iptw_stab_q4))
svydesign<-svydesign(id=~adm_rno, weights=~iptw_stab_q4, data=data7)
logit_outcome<-svyglm(dep_yn ~ as.factor(jobcontrol_q4), data=data7, family="quasibinomial", design=svydesign)
  
  summary(logit_outcome)
  exp(coef(logit_outcome))
  confint(logit_outcome)
  exp(cbind(OR = coef(logit_outcome), confint(logit_outcome)))
  lroc(logit_outcome)
  
#outcome model (marginal structural model) with IPTW weights - double robust with covariates used for pscore model; note: drop missing data, as svyglm does not work with missing weights
data7<-data6%>%filter(!is.na(iptw_q4))
svydesign<-svydesign(id=~adm_rno, weights=~iptw_q4, data=data7)
logit_outcome<-svyglm(dep_yn ~ as.factor(jobcontrol_q4) + wstdpsy + wstdsoc + wstdphy + wstdjin + as.factor(geo_prv) + as.factor(dhh_sex) + as.factor(dhhgms) + as.factor(dhhghsz) + as.factor(dhhgdwe), data=data7, family="quasibinomial", design=svydesign)

  summary(logit_outcome)
  exp(coef(logit_outcome))
  confint(logit_outcome)
  exp(cbind(OR = coef(logit_outcome), confint(logit_outcome)))
  lroc(logit_outcome)

#can also run regression models, direct adjustment with pscore, stratification by pscore, matching on pscore

####################################################################################################
####################################################################################################
#FOR ILLUSTRATION - BINARY TREATMENT - SURVEY WEIGHTED#
####################################################################################################
####################################################################################################

#pscore model with survey weights; could also incorporate survey weights as a covariate in the model
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5802372
#https://academic.oup.com/biostatistics/article/20/1/147/4780267
#https://journals.sagepub.com/doi/full/10.1177/0193841X20938497
#normally, na.action=na.exclude will pad rows with missing data with NA to allow merging with original data, but does not work with svyglm for some reason; to merge, must restrict sample first and then merge on index
#must use quasibinomial for svyglm
data3_subcohort<-data3%>%dplyr::select(jobcontrol_binary,wstdpsy,wstdsoc,wstdphy,wstdjin,geo_prv,dhh_sex,dhhgms,dhhghsz,dhhgdwe)%>%
  na.omit()
count(data3_subcohort)
svydesign<-svydesign(id=~adm_rno, weights=~wts_m, data=data3_subcohort)
svylogit<-svyglm(jobcontrol_binary ~ wstdpsy + wstdsoc + wstdphy + wstdjin + as.factor(geo_prv) + as.factor(dhh_sex) + as.factor(dhhgms) + as.factor(dhhghsz) + as.factor(dhhgdwe), data=data3_subcohort, family="quasibinomial", design=svydesign)

summary(svylogit)
exp(cbind(OR = coef(svylogit), confint(svylogit)))
#note: accuracy measures/c-statistics/auc are not useful for assessing peformance of the propensity score, as it is not a prediction score (see: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4213057); also, inclusion of non-confounders of the outcome relationship could increase c-statistic without decreasing bias in treatment-outcome relationship, and in the extreme case of a randomized trial, balance will be achieved with a specific propensity score model despite the c-statistic equal to only 0.5
lroc(svylogit)

#predict pscore and merge to full cohort
data3_subcohort_pscore<-data3_subcohort
data3_subcohort_pscore$pscore<-predict(svylogit,newdata=data3_subcohort,type="response")
data3_subcohort_pscore<-data3_subcohort%>%dplyr::select(adm_rno,pscore)
data4<-data3%>%left_join(data3_subcohort_pscore,by="adm_rno")
rm(data3_subcohort,data3_subcohort_pscore)
gc()

#in addition to using the survey weights in the propensity score model, multiply the IPTW weights by the survey weights
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5802372
#https://academic.oup.com/biostatistics/article/20/1/147/4780267
#https://journals.sagepub.com/doi/full/10.1177/0193841X20938497