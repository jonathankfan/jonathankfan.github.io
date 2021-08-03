
##################################################
##################################################
# LOAD NHANES DATA
##################################################
##################################################

#import data to R
#install.packages("NHANES")
library(NHANES)
small.nhanes <- na.omit(NHANES[NHANES$SurveyYr=="2011_12"
                               & NHANES$Age > 17,c(3,4,8:11,13,25,61)])
head(small.nhanes)

#EXPORT TO SAS
outpath <- "C:/Users/"
library(foreign)
write.foreign(small.nhanes,
              file.path(outpath, "nhanes.csv"),
              file.path(outpath, "nhanes.txt"), package = c("SAS"))

##################################################
##################################################
#RECODE VARIABLES
##################################################
##################################################

#GENDER
print(table(small.nhanes$Gender))
small.nhanes$female<-ifelse(small.nhanes$Gender=='female','Women',ifelse(small.nhanes$Gender=='male','Men',99))
print(table(small.nhanes$Gender,small.nhanes$female))

#RACE
print(table(small.nhanes$Race3))
small.nhanes$racecat<-ifelse(small.nhanes$Race3=='White','White',ifelse(small.nhanes$Race3=='Black','Black',ifelse(small.nhanes$Race3 %in% c('Asian','Hispanic','Mexican','Other'),'Other',99)))
print(table(small.nhanes$Race3,small.nhanes$racecat))

#EDUCATION
print(table(small.nhanes$Education))
small.nhanes$educat<-ifelse(small.nhanes$Education %in% c('8th Grade','9 - 11th Grade','High School'),'High school and below',ifelse(small.nhanes$Education=='Some College','Some college',ifelse(small.nhanes$Education=='College Grad','College grad',99)))
print(table(small.nhanes$Education,small.nhanes$educat))

#MARSTAT
print(table(small.nhanes$MaritalStatus))
small.nhanes$marstat<-ifelse(small.nhanes$MaritalStatus %in% c('LivePartner','Married'),'Married/partner',ifelse(small.nhanes$MaritalStatus %in% c('Divorced','NeverMarried','Separated','Widowed'),'Divorced/never married/separated/widowed',99))
print(table(small.nhanes$MaritalStatus,small.nhanes$marstat))

#INCOME
trimws(small.nhanes$HHIncome,which=c("both"))
print(table(small.nhanes$HHIncome))
small.nhanes$incomecat<-ifelse(small.nhanes$HHIncome %in% c(' 0-4999',' 5000-9999','10000-14999','15000-19999','20000-24999'),'0-24999',ifelse(small.nhanes$HHIncome %in% c('25000-34999','35000-44999','45000-54999','55000-64999','65000-74999'),'25000-74999',ifelse(small.nhanes$HHIncome %in% c('75000-99999','more 99999'),'75000+',99)))
print(table(small.nhanes$HHIncome,small.nhanes$incomecat))

#POVERTY
print(table(small.nhanes$Poverty))
small.nhanes$povcat<-ifelse(small.nhanes$Poverty>=0 & small.nhanes$Poverty<1,'0-0.99',
                            ifelse(small.nhanes$Poverty>=1 & small.nhanes$Poverty<2,'1-1.99',
                                   ifelse(small.nhanes$Poverty>=2 & small.nhanes$Poverty<3,'2-2.99',
                                          ifelse(small.nhanes$Poverty>=3 & small.nhanes$Poverty<4,'3-3.99',
                                                 ifelse(small.nhanes$Poverty>=4 & small.nhanes$Poverty<5,'4-4.99',
                                                        ifelse(small.nhanes$Poverty==5,'5+',
                                                               99))))))
print(table(small.nhanes$povcat))
tapply(small.nhanes$Poverty,small.nhanes$povcat,summary)

#SMOKING
print(table(small.nhanes$SmokeNow))
small.nhanes$smokeyn<-ifelse(small.nhanes$SmokeNow=='No',0,ifelse(small.nhanes$SmokeNow=='Yes',1,99))
print(table(small.nhanes$SmokeNow,small.nhanes$smokeyn))

#AGE
print(table(small.nhanes$Age))
small.nhanes$age7cat<-ifelse(small.nhanes$Age>=20 & small.nhanes$Age<30,'20-29',
                             ifelse(small.nhanes$Age>=30 & small.nhanes$Age<40,'30-39',
                                    ifelse(small.nhanes$Age>=40 & small.nhanes$Age<50,'40-49',
                                           ifelse(small.nhanes$Age>=50 & small.nhanes$Age<60,'50-59',
                                                  ifelse(small.nhanes$Age>=60 & small.nhanes$Age<70,'60-69',
                                                         ifelse(small.nhanes$Age>=70 & small.nhanes$Age<80,'70-79',
                                                                ifelse(small.nhanes$Age==80,'80+',
                                                                       99)))))))
print(table(small.nhanes$age7cat))
tapply(small.nhanes$Age,small.nhanes$age7cat,summary)

#generate polynomials variables
  
  small.nhanes$lnbp<-log(small.nhanes$BPSysAve)
  summary(small.nhanes$lnbp)
  
  small.nhanes$agesq<-small.nhanes$Age^2
  summary(small.nhanes$agesq)

##################################################
##################################################
#CREATE DATA
##################################################
##################################################

#create working data
small.nhanes.working<-small.nhanes
head(small.nhanes.working)

##################################################
##################################################
#COVARIATE BALANCE - UNWEIGHTED
##################################################
##################################################

#install.packages("tableone")
library(tableone)

table <- CreateTableOne(vars=c("female","age7cat","racecat","educat","marstat","incomecat","povcat","Age","Poverty"), strata="SmokeNow", data=small.nhanes, smd=TRUE, test=TRUE)
print(table, smd=TRUE)


##################################################
##################################################
#NAIVE CRUDE ESTIMATE
##################################################
##################################################

naive <- glm(BPSysAve ~ smokeyn, data=small.nhanes.working, family=gaussian(link=identity))
print(summary(naive))







##################################################
# PS MODELS TO GENERATE IPTW
##################################################

  #set model variables here
  modelvarlist <- c("smokeyn ~ Age + female + racecat",
                    "smokeyn ~ Age + female + racecat + educat + marstat",
                    "smokeyn ~ Age + female + racecat + educat + marstat + incomecat",
                    "smokeyn ~ Age + agesq + female + racecat + educat + marstat + incomecat",
                    "smokeyn ~ age7cat + female + racecat + educat + marstat + incomecat",
                    "smokeyn ~ Age*educat + female + racecat + marstat + incomecat",
                    "smokeyn ~ Age*incomecat + female + racecat + educat + marstat",
                    "smokeyn ~ Age*educat + Age*incomecat + female + racecat + educat + marstat",
                    "smokeyn ~ Age + female + Race3 + Education + MaritalStatus + HHIncome")

  for (modelvars in modelvarlist){
      
      print("##################################################")
      print("##################################################")
      print("##################################################")
      print("##################################################")
      
      print(modelvars)
      
      print("##################################################")
      print("##################################################")
      print("##################################################")
      print("##################################################")
      
      #model estimates

        psmodel <- glm(modelvars, data=small.nhanes.working, family=binomial(link=logit))
        print(summary(psmodel))
        small.nhanes.working$ps <- predict(psmodel, type="response")
        #print(summary(small.nhanes.working$ps))
        
      #generate IPW weights and progressively truncate
        
        print("##################################################")
        print("##################################################")
        
        #original  
        small.nhanes.working$ipw<-(small.nhanes.working$smokeyn/small.nhanes.working$ps) + (1-small.nhanes.working$smokeyn)/(1-small.nhanes.working$ps)  
  
          #generate estimates using original IPW
          small.nhanes.working$est1<-small.nhanes.working$smokeyn * small.nhanes.working$BPSysAve/small.nhanes.working$ps
          small.nhanes.working$est0<-(1-small.nhanes.working$smokeyn) * small.nhanes.working$BPSysAve/(1-small.nhanes.working$ps)
          
        #1 and 99
        small.nhanes.working$ipw_truncated_1=small.nhanes.working$ipw
        ipw_plower<-as.numeric(quantile(small.nhanes.working$ipw,prob=c(0.01)))
        ipw_pupper<-as.numeric(quantile(small.nhanes.working$ipw,prob=c(0.99)))
        small.nhanes.working$ipw_truncated_1<-ifelse(small.nhanes.working$ipw>0 & small.nhanes.working$ipw<ipw_plower,ipw_plower,
                                                     ifelse(small.nhanes.working$ipw>ipw_pupper,ipw_pupper,small.nhanes.working$ipw))
        
        #5 and 95
        small.nhanes.working$ipw_truncated_2=small.nhanes.working$ipw_truncated_1
        ipw_plower<-as.numeric(quantile(small.nhanes.working$ipw_truncated_1,prob=c(0.05)))
        ipw_pupper<-as.numeric(quantile(small.nhanes.working$ipw_truncated_1,prob=c(0.95)))
        small.nhanes.working$ipw_truncated_2<-ifelse(small.nhanes.working$ipw_truncated_1>0 & small.nhanes.working$ipw_truncated_1<ipw_plower,ipw_plower,
                                                     ifelse(small.nhanes.working$ipw_truncated_1>ipw_pupper,ipw_pupper,small.nhanes.working$ipw_truncated_1))
        
        #10 and 90
        small.nhanes.working$ipw_truncated_3=small.nhanes.working$ipw_truncated_2
        ipw_plower<-as.numeric(quantile(small.nhanes.working$ipw_truncated_2,prob=c(0.10)))
        ipw_pupper<-as.numeric(quantile(small.nhanes.working$ipw_truncated_2,prob=c(0.90)))
        small.nhanes.working$ipw_truncated_3<-ifelse(small.nhanes.working$ipw_truncated_2>0 & small.nhanes.working$ipw_truncated_2<ipw_plower,ipw_plower,
                                                     ifelse(small.nhanes.working$ipw_truncated_2>ipw_pupper,ipw_pupper,small.nhanes.working$ipw_truncated_2))
        
        #display new IPW
        print("SUMMARIZE IPW:")
        head(small.nhanes.working)
        print(summary(small.nhanes.working))
        print(quantile(small.nhanes.working$ipw, prob=c(0.01,0.99)))
        print(quantile(small.nhanes.working$ipw_truncated_1, prob=c(0.05,0.95)))
        print(quantile(small.nhanes.working$ipw_truncated_2, prob=c(0.10,0.90)))
        print("MEAN, SD, MIN, MAX ORIGINAL")
        print(mean(small.nhanes.working$ipw))
        print(sd(small.nhanes.working$ipw))
        print(min(small.nhanes.working$ipw))
        print(max(small.nhanes.working$ipw))
        print("MEAN, SD, MIN, MAX ipw_truncated_1")
        print(mean(small.nhanes.working$ipw_truncated_1))
        print(sd(small.nhanes.working$ipw_truncated_1))
        print(min(small.nhanes.working$ipw_truncated_1))
        print(max(small.nhanes.working$ipw_truncated_1))
        print("MEAN, SD, MIN, MAX ipw_truncated_2")
        print(mean(small.nhanes.working$ipw_truncated_2))
        print(sd(small.nhanes.working$ipw_truncated_2))
        print(min(small.nhanes.working$ipw_truncated_2))
        print(max(small.nhanes.working$ipw_truncated_2))
        print("MEAN, SD, MIN, MAX ipw_truncated_3")
        print(mean(small.nhanes.working$ipw_truncated_3))
        print(sd(small.nhanes.working$ipw_truncated_3))
        print(min(small.nhanes.working$ipw_truncated_3))
        print(max(small.nhanes.working$ipw_truncated_3))
        
        #confirm weights work
        print("CONFIRM WEIGHTS WORK:")
        weighted <- svydesign(ids=~0, data=small.nhanes.working, weights=small.nhanes.working$ipw)
        print(svyCreateTableOne(vars="SmokeNow", data=weighted, smd=TRUE, test=TRUE))
        weighted <- svydesign(ids=~0, data=small.nhanes.working, weights=small.nhanes.working$ipw_truncated_2)
        print(svyCreateTableOne(vars="SmokeNow", data=weighted, smd=TRUE, test=TRUE))
        weighted <- svydesign(ids=~0, data=small.nhanes.working, weights=small.nhanes.working$ipw_truncated_3)
        print(svyCreateTableOne(vars="SmokeNow", data=weighted, smd=TRUE, test=TRUE))

      #means from IPW weighted population

        print("##################################################")
        print("##################################################")
        
        #est1<-mean(small.nhanes.working$smokeyn * small.nhanes.working$BPSysAve/small.nhanes.working$ps)
        #est0<-mean((1-small.nhanes.working$smokeyn) * small.nhanes.working$BPSysAve/(1-small.nhanes.working$ps))
        est1<-mean(small.nhanes.working$smokeyn * small.nhanes.working$BPSysAve*small.nhanes.working$ipw)
        est0<-mean((1-small.nhanes.working$smokeyn) * small.nhanes.working$BPSysAve*small.nhanes.working$ipw)
        print("")
        print("MEAN ESTIMATES USING ipw")
        print(paste("EST1: ", est1))
        print(paste("EST0: ", est0))
        print(paste("DIFF: ", est1-est0))
          
          #MARGINAL STRUCTURAL MODEL
          msm <- coef(glm(BPSysAve ~ smokeyn, weights=small.nhanes.working$ipw, data=small.nhanes.working, family=gaussian(link=identity)))
          print("MARGINAL STRUCTRAL MODEL ESTIMATES")
          print(msm)
          
        est1<-mean(small.nhanes.working$smokeyn * small.nhanes.working$BPSysAve*small.nhanes.working$ipw_truncated_1)
        est0<-mean((1-small.nhanes.working$smokeyn) * small.nhanes.working$BPSysAve*small.nhanes.working$ipw_truncated_1)
        print("")
        print("MEAN ESTIMATES USING ipw_truncated_1")
        print(paste("EST1: ", est1))
        print(paste("EST0: ", est0))
        print(paste("DIFF: ", est1-est0))
          
          #MARGINAL STRUCTURAL MODEL
          msm <- coef(glm(BPSysAve ~ smokeyn, weights=small.nhanes.working$ipw_truncated_1, data=small.nhanes.working, family=gaussian(link=identity)))
          print("MARGINAL STRUCTRAL MODEL ESTIMATES")
          print(msm)
          
        est1<-mean(small.nhanes.working$smokeyn * small.nhanes.working$BPSysAve*small.nhanes.working$ipw_truncated_2)
        est0<-mean((1-small.nhanes.working$smokeyn) * small.nhanes.working$BPSysAve*small.nhanes.working$ipw_truncated_2)
        print("")
        print("MEAN ESTIMATES USING ipw_truncated_2")
        print(paste("EST1: ", est1))
        print(paste("EST0: ", est0))
        print(paste("DIFF: ", est1-est0))
          
          #MARGINAL STRUCTURAL MODEL
          msm <- coef(glm(BPSysAve ~ smokeyn, weights=small.nhanes.working$ipw_truncated_2, data=small.nhanes.working, family=gaussian(link=identity)))
          print("MARGINAL STRUCTRAL MODEL ESTIMATES")
          print(msm)
          
        est1<-mean(small.nhanes.working$smokeyn * small.nhanes.working$BPSysAve*small.nhanes.working$ipw_truncated_3)
        est0<-mean((1-small.nhanes.working$smokeyn) * small.nhanes.working$BPSysAve*small.nhanes.working$ipw_truncated_3)
        print("")
        print("MEAN ESTIMATES USING ipw_truncated_3")
        print(paste("EST1: ", est1))
        print(paste("EST0: ", est0))
        print(paste("DIFF: ", est1-est0))
  
          #MARGINAL STRUCTURAL MODEL
          msm <- coef(glm(BPSysAve ~ smokeyn, weights=small.nhanes.working$ipw_truncated_3, data=small.nhanes.working, family=gaussian(link=identity)))
          print("MARGINAL STRUCTRAL MODEL ESTIMATES")
          print(msm)

      # Bootstrap standard error and confidence interval:
        
        print("##################################################")
        print("##################################################")
        
        nboot <- 1000
        nobs <- length(small.nhanes.working$BPSysAve)
        rds <- rep(NA, nboot)
        rrs <- rep(NA, nboot)
        rds1 <- rep(NA, nboot)
        rrs1 <- rep(NA, nboot)
        rds2 <- rep(NA, nboot)
        rrs2 <- rep(NA, nboot)
        rds3 <- rep(NA, nboot)
        rrs3 <- rep(NA, nboot)
        set.seed(1)
        for (i in 1:nboot) {
          
          bootidx <- sample(c(1:nobs), size=nobs, replace=TRUE) #sample from 1 to Nobs with replacement
          bootdata <- small.nhanes.working[bootidx,] #create bootstrap dataset with Nobs, including observations that match each bootidx
          psmodel <- glm(modelvars, family=binomial(link=logit), data=bootdata)
          bootdata$ps <- predict(psmodel, data=bootdata, type="response")

          #generate IPW weights
          
            #original IPW
            bootdata$ipw<-(bootdata$smokeyn/bootdata$ps) + (1-bootdata$smokeyn)/(1-bootdata$ps)  

            #1 and 99
            bootdata$ipw_truncated_1=bootdata$ipw
            ipw_plower<-as.numeric(quantile(bootdata$ipw,prob=c(0.01)))
            ipw_pupper<-as.numeric(quantile(bootdata$ipw,prob=c(0.99)))
            bootdata$ipw_truncated_1<-ifelse(bootdata$ipw>0 & bootdata$ipw<ipw_plower,ipw_plower,
                                                         ifelse(bootdata$ipw>ipw_pupper,ipw_pupper,bootdata$ipw))
            
            #5 and 95
            bootdata$ipw_truncated_2=bootdata$ipw_truncated_1
            ipw_plower<-as.numeric(quantile(bootdata$ipw_truncated_1,prob=c(0.05)))
            ipw_pupper<-as.numeric(quantile(bootdata$ipw_truncated_1,prob=c(0.95)))
            bootdata$ipw_truncated_2<-ifelse(bootdata$ipw_truncated_1>0 & bootdata$ipw_truncated_1<ipw_plower,ipw_plower,
                                                         ifelse(bootdata$ipw_truncated_1>ipw_pupper,ipw_pupper,bootdata$ipw_truncated_1))
            
            #10 and 90
            bootdata$ipw_truncated_3=bootdata$ipw_truncated_2
            ipw_plower<-as.numeric(quantile(bootdata$ipw_truncated_2,prob=c(0.10)))
            ipw_pupper<-as.numeric(quantile(bootdata$ipw_truncated_2,prob=c(0.90)))
            bootdata$ipw_truncated_3<-ifelse(bootdata$ipw_truncated_2>0 & bootdata$ipw_truncated_2<ipw_plower,ipw_plower,
                                                         ifelse(bootdata$ipw_truncated_2>ipw_pupper,ipw_pupper,bootdata$ipw_truncated_2))
            
          #estimates
            
            #original IPW
            #rds[i] <- mean(bootdata$smokeyn * bootdata$BPSysAve/bootdata$ps) - mean((1-bootdata$smokeyn) * bootdata$BPSysAve/(1-bootdata$ps))
            #rrs[i] <- mean(bootdata$smokeyn * bootdata$BPSysAve/bootdata$ps) / mean((1-bootdata$smokeyn) * bootdata$BPSysAve/(1-bootdata$ps))
            rds[i] <- mean(bootdata$smokeyn * bootdata$BPSysAve*bootdata$ipw) - mean((1-bootdata$smokeyn) * bootdata$BPSysAve*bootdata$ipw)
            rrs[i] <- mean(bootdata$smokeyn * bootdata$BPSysAve*bootdata$ipw) / mean((1-bootdata$smokeyn) * bootdata$BPSysAve*bootdata$ipw)
            
            #truncated p1/p99
            rds1[i] <- mean(bootdata$smokeyn * bootdata$BPSysAve*bootdata$ipw_truncated_1) - mean((1-bootdata$smokeyn) * bootdata$BPSysAve*bootdata$ipw_truncated_1)
            rrs1[i] <- mean(bootdata$smokeyn * bootdata$BPSysAve*bootdata$ipw_truncated_1) / mean((1-bootdata$smokeyn) * bootdata$BPSysAve*bootdata$ipw_truncated_1)
            
            #truncated p5/p95
            rds2[i] <- mean(bootdata$smokeyn * bootdata$BPSysAve*bootdata$ipw_truncated_2) - mean((1-bootdata$smokeyn) * bootdata$BPSysAve*bootdata$ipw_truncated_2)
            rrs2[i] <- mean(bootdata$smokeyn * bootdata$BPSysAve*bootdata$ipw_truncated_2) / mean((1-bootdata$smokeyn) * bootdata$BPSysAve*bootdata$ipw_truncated_2)
            
            #truncated p10/p90
            rds3[i] <- mean(bootdata$smokeyn * bootdata$BPSysAve*bootdata$ipw_truncated_3) - mean((1-bootdata$smokeyn) * bootdata$BPSysAve*bootdata$ipw_truncated_3)
            rrs3[i] <- mean(bootdata$smokeyn * bootdata$BPSysAve*bootdata$ipw_truncated_3) / mean((1-bootdata$smokeyn) * bootdata$BPSysAve*bootdata$ipw_truncated_3)
            
          #if (i %% 100 == 0)
          #print(i)
        }

        print("##################################################")
        print("##################################################")
        
        print("")
        print("BOOTSTRAP 95% CI OF RISK DIFFERENCE USING ipw")
        
        #risk difference
        print(sd(rds))
        print(mean(rds))
        print(quantile(rds, prob=c(0.025,0.975)))
        
        #risk ratio
        #sd(rrs)
        #mean(rrs)
        #print(quantile(rrs, prob=c(0.025,0.975)))
        
        print("")
        print("BOOTSTRAP 95% CI OF RISK DIFFERENCE USING ipw_truncated_1")
        
        #risk difference
        print(sd(rds1))
        print(mean(rds1))
        print(quantile(rds1, prob=c(0.025,0.975)))
        
        print("")
        print("BOOTSTRAP 95% CI OF RISK DIFFERENCE USING ipw_truncated_2")
        
        #risk difference
        print(sd(rds2))
        print(mean(rds2))
        print(quantile(rds2, prob=c(0.025,0.975)))
        
        print("")
        print("BOOTSTRAP 95% CI OF RISK DIFFERENCE USING ipw_truncated_3")
        
        #risk difference
        print(sd(rds3))
        print(mean(rds3))
        print(quantile(rds3, prob=c(0.025,0.975)))
        
  }


    
    
















##################################################
# FINAL PS MODELS TO GENERATE IPTW
##################################################

#set model variables here
modelvarlist <- c("smokeyn ~ Age + female + racecat + educat + marstat + incomecat")

for (modelvars in modelvarlist){
  
  print("##################################################")
  print("##################################################")
  print("##################################################")
  print("##################################################")
  
  print(modelvars)
  
  print("##################################################")
  print("##################################################")
  print("##################################################")
  print("##################################################")
  
  #model estimates
  
  psmodel <- glm(modelvars, data=small.nhanes.working, family=binomial(link=logit))
  print(summary(psmodel))
  small.nhanes.working$ps <- predict(psmodel, type="response")
  print(summary(small.nhanes.working$ps))
  tapply(small.nhanes.working$ps,small.nhanes.working$smokeyn,summary)
  
  #generate IPW weights and progressively truncate
  
  print("##################################################")
  print("##################################################")
  
  #original  
  small.nhanes.working$ipw<-(small.nhanes.working$smokeyn/small.nhanes.working$ps) + (1-small.nhanes.working$smokeyn)/(1-small.nhanes.working$ps)  
  
  #generate estimates using original IPW
  small.nhanes.working$est1<-small.nhanes.working$smokeyn * small.nhanes.working$BPSysAve/small.nhanes.working$ps
  small.nhanes.working$est0<-(1-small.nhanes.working$smokeyn) * small.nhanes.working$BPSysAve/(1-small.nhanes.working$ps)
  
  #1 and 99
  small.nhanes.working$ipw_truncated_1=small.nhanes.working$ipw
  ipw_plower<-as.numeric(quantile(small.nhanes.working$ipw,prob=c(0.01)))
  ipw_pupper<-as.numeric(quantile(small.nhanes.working$ipw,prob=c(0.99)))
  small.nhanes.working$ipw_truncated_1<-ifelse(small.nhanes.working$ipw>0 & small.nhanes.working$ipw<ipw_plower,ipw_plower,
                                               ifelse(small.nhanes.working$ipw>ipw_pupper,ipw_pupper,small.nhanes.working$ipw))
  
  #5 and 95
  small.nhanes.working$ipw_truncated_2=small.nhanes.working$ipw_truncated_1
  ipw_plower<-as.numeric(quantile(small.nhanes.working$ipw_truncated_1,prob=c(0.05)))
  ipw_pupper<-as.numeric(quantile(small.nhanes.working$ipw_truncated_1,prob=c(0.95)))
  small.nhanes.working$ipw_truncated_2<-ifelse(small.nhanes.working$ipw_truncated_1>0 & small.nhanes.working$ipw_truncated_1<ipw_plower,ipw_plower,
                                               ifelse(small.nhanes.working$ipw_truncated_1>ipw_pupper,ipw_pupper,small.nhanes.working$ipw_truncated_1))
  
  #10 and 90
  small.nhanes.working$ipw_truncated_3=small.nhanes.working$ipw_truncated_2
  ipw_plower<-as.numeric(quantile(small.nhanes.working$ipw_truncated_2,prob=c(0.10)))
  ipw_pupper<-as.numeric(quantile(small.nhanes.working$ipw_truncated_2,prob=c(0.90)))
  small.nhanes.working$ipw_truncated_3<-ifelse(small.nhanes.working$ipw_truncated_2>0 & small.nhanes.working$ipw_truncated_2<ipw_plower,ipw_plower,
                                               ifelse(small.nhanes.working$ipw_truncated_2>ipw_pupper,ipw_pupper,small.nhanes.working$ipw_truncated_2))
  
  #display new IPW
  print("SUMMARIZE IPW:")
  head(small.nhanes.working)
  print(summary(small.nhanes.working))
  print(quantile(small.nhanes.working$ipw, prob=c(0.01,0.99)))
  print(quantile(small.nhanes.working$ipw_truncated_1, prob=c(0.05,0.95)))
  print(quantile(small.nhanes.working$ipw_truncated_2, prob=c(0.10,0.90)))
  print("MEAN, SD, MIN, MAX ORIGINAL")
  print(mean(small.nhanes.working$ipw))
  print(sd(small.nhanes.working$ipw))
  print(min(small.nhanes.working$ipw))
  print(max(small.nhanes.working$ipw))
  print("MEAN, SD, MIN, MAX ipw_truncated_1")
  print(mean(small.nhanes.working$ipw_truncated_1))
  print(sd(small.nhanes.working$ipw_truncated_1))
  print(min(small.nhanes.working$ipw_truncated_1))
  print(max(small.nhanes.working$ipw_truncated_1))
  print("MEAN, SD, MIN, MAX ipw_truncated_2")
  print(mean(small.nhanes.working$ipw_truncated_2))
  print(sd(small.nhanes.working$ipw_truncated_2))
  print(min(small.nhanes.working$ipw_truncated_2))
  print(max(small.nhanes.working$ipw_truncated_2))
  print("MEAN, SD, MIN, MAX ipw_truncated_3")
  print(mean(small.nhanes.working$ipw_truncated_3))
  print(sd(small.nhanes.working$ipw_truncated_3))
  print(min(small.nhanes.working$ipw_truncated_3))
  print(max(small.nhanes.working$ipw_truncated_3))
  
  #confirm weights work
  print("CONFIRM WEIGHTS WORK:")
  weighted <- svydesign(ids=~0, data=small.nhanes.working, weights=small.nhanes.working$ipw)
  print(svyCreateTableOne(vars="SmokeNow", data=weighted, smd=TRUE, test=TRUE))
  weighted <- svydesign(ids=~0, data=small.nhanes.working, weights=small.nhanes.working$ipw_truncated_2)
  print(svyCreateTableOne(vars="SmokeNow", data=weighted, smd=TRUE, test=TRUE))
  weighted <- svydesign(ids=~0, data=small.nhanes.working, weights=small.nhanes.working$ipw_truncated_3)
  print(svyCreateTableOne(vars="SmokeNow", data=weighted, smd=TRUE, test=TRUE))
  
  #means from IPW weighted population
  
  print("##################################################")
  print("##################################################")
  
  #est1<-mean(small.nhanes.working$smokeyn * small.nhanes.working$BPSysAve/small.nhanes.working$ps)
  #est0<-mean((1-small.nhanes.working$smokeyn) * small.nhanes.working$BPSysAve/(1-small.nhanes.working$ps))
  est1<-mean(small.nhanes.working$smokeyn * small.nhanes.working$BPSysAve*small.nhanes.working$ipw)
  est0<-mean((1-small.nhanes.working$smokeyn) * small.nhanes.working$BPSysAve*small.nhanes.working$ipw)
  print("")
  print("MEAN ESTIMATES USING ipw")
  print(paste("EST1: ", est1))
  print(paste("EST0: ", est0))
  print(paste("DIFF: ", est1-est0))
  
  #MARGINAL STRUCTURAL MODEL
  msm <- coef(glm(BPSysAve ~ smokeyn, weights=small.nhanes.working$ipw, data=small.nhanes.working, family=gaussian(link=identity)))
  print("MARGINAL STRUCTRAL MODEL ESTIMATES")
  print(msm)
  
  est1<-mean(small.nhanes.working$smokeyn * small.nhanes.working$BPSysAve*small.nhanes.working$ipw_truncated_1)
  est0<-mean((1-small.nhanes.working$smokeyn) * small.nhanes.working$BPSysAve*small.nhanes.working$ipw_truncated_1)
  print("")
  print("MEAN ESTIMATES USING ipw_truncated_1")
  print(paste("EST1: ", est1))
  print(paste("EST0: ", est0))
  print(paste("DIFF: ", est1-est0))
  
  #MARGINAL STRUCTURAL MODEL
  msm <- glm(BPSysAve ~ smokeyn, weights=small.nhanes.working$ipw_truncated_1, data=small.nhanes.working, family=gaussian(link=identity))
  confint(msm)
  print("MARGINAL STRUCTRAL MODEL ESTIMATES - LINEAR")
  print(summary(msm))
  print(msm)
  msm <- coef(glm(BPSysAve ~ smokeyn, weights=small.nhanes.working$ipw_truncated_1, data=small.nhanes.working, family=poisson(link=log)))
  print("MARGINAL STRUCTRAL MODEL ESTIMATES - LOG-LINEAR")
  print(summary(msm))
  print(msm)
  
  est1<-mean(small.nhanes.working$smokeyn * small.nhanes.working$BPSysAve*small.nhanes.working$ipw_truncated_2)
  est0<-mean((1-small.nhanes.working$smokeyn) * small.nhanes.working$BPSysAve*small.nhanes.working$ipw_truncated_2)
  print("")
  print("MEAN ESTIMATES USING ipw_truncated_2")
  print(paste("EST1: ", est1))
  print(paste("EST0: ", est0))
  print(paste("DIFF: ", est1-est0))
  
  #MARGINAL STRUCTURAL MODEL
  msm <- coef(glm(BPSysAve ~ smokeyn, weights=small.nhanes.working$ipw_truncated_2, data=small.nhanes.working, family=gaussian(link=identity)))
  print("MARGINAL STRUCTRAL MODEL ESTIMATES")
  print(msm)
  
  est1<-mean(small.nhanes.working$smokeyn * small.nhanes.working$BPSysAve*small.nhanes.working$ipw_truncated_3)
  est0<-mean((1-small.nhanes.working$smokeyn) * small.nhanes.working$BPSysAve*small.nhanes.working$ipw_truncated_3)
  print("")
  print("MEAN ESTIMATES USING ipw_truncated_3")
  print(paste("EST1: ", est1))
  print(paste("EST0: ", est0))
  print(paste("DIFF: ", est1-est0))
  
  #MARGINAL STRUCTURAL MODEL
  msm <- coef(glm(BPSysAve ~ smokeyn, weights=small.nhanes.working$ipw_truncated_3, data=small.nhanes.working, family=gaussian(link=identity)))
  print("MARGINAL STRUCTRAL MODEL ESTIMATES")
  print(msm)
  
  # Bootstrap standard error and confidence interval:
  
  print("##################################################")
  print("##################################################")
  
  nboot <- 1000
  nobs <- length(small.nhanes.working$BPSysAve)
  rds <- rep(NA, nboot)
  rrs <- rep(NA, nboot)
  rds1 <- rep(NA, nboot)
  rrs1 <- rep(NA, nboot)
  rds2 <- rep(NA, nboot)
  rrs2 <- rep(NA, nboot)
  rds3 <- rep(NA, nboot)
  rrs3 <- rep(NA, nboot)
  set.seed(1)
  for (i in 1:nboot) {
    
    bootidx <- sample(c(1:nobs), size=nobs, replace=TRUE) #sample from 1 to Nobs with replacement
    bootdata <- small.nhanes.working[bootidx,] #create bootstrap dataset with Nobs, including observations that match each bootidx
    psmodel <- glm(modelvars, family=binomial(link=logit), data=bootdata)
    bootdata$ps <- predict(psmodel, data=bootdata, type="response")
    
    #generate IPW weights
    
    #original IPW
    bootdata$ipw<-(bootdata$smokeyn/bootdata$ps) + (1-bootdata$smokeyn)/(1-bootdata$ps)  
    
    #1 and 99
    bootdata$ipw_truncated_1=bootdata$ipw
    ipw_plower<-as.numeric(quantile(bootdata$ipw,prob=c(0.01)))
    ipw_pupper<-as.numeric(quantile(bootdata$ipw,prob=c(0.99)))
    bootdata$ipw_truncated_1<-ifelse(bootdata$ipw>0 & bootdata$ipw<ipw_plower,ipw_plower,
                                     ifelse(bootdata$ipw>ipw_pupper,ipw_pupper,bootdata$ipw))
    
    #5 and 95
    bootdata$ipw_truncated_2=bootdata$ipw_truncated_1
    ipw_plower<-as.numeric(quantile(bootdata$ipw_truncated_1,prob=c(0.05)))
    ipw_pupper<-as.numeric(quantile(bootdata$ipw_truncated_1,prob=c(0.95)))
    bootdata$ipw_truncated_2<-ifelse(bootdata$ipw_truncated_1>0 & bootdata$ipw_truncated_1<ipw_plower,ipw_plower,
                                     ifelse(bootdata$ipw_truncated_1>ipw_pupper,ipw_pupper,bootdata$ipw_truncated_1))
    
    #10 and 90
    bootdata$ipw_truncated_3=bootdata$ipw_truncated_2
    ipw_plower<-as.numeric(quantile(bootdata$ipw_truncated_2,prob=c(0.10)))
    ipw_pupper<-as.numeric(quantile(bootdata$ipw_truncated_2,prob=c(0.90)))
    bootdata$ipw_truncated_3<-ifelse(bootdata$ipw_truncated_2>0 & bootdata$ipw_truncated_2<ipw_plower,ipw_plower,
                                     ifelse(bootdata$ipw_truncated_2>ipw_pupper,ipw_pupper,bootdata$ipw_truncated_2))
    
    #estimates
    
    #original IPW
    #rds[i] <- mean(bootdata$smokeyn * bootdata$BPSysAve/bootdata$ps) - mean((1-bootdata$smokeyn) * bootdata$BPSysAve/(1-bootdata$ps))
    #rrs[i] <- mean(bootdata$smokeyn * bootdata$BPSysAve/bootdata$ps) / mean((1-bootdata$smokeyn) * bootdata$BPSysAve/(1-bootdata$ps))
    rds[i] <- mean(bootdata$smokeyn * bootdata$BPSysAve*bootdata$ipw) - mean((1-bootdata$smokeyn) * bootdata$BPSysAve*bootdata$ipw)
    rrs[i] <- mean(bootdata$smokeyn * bootdata$BPSysAve*bootdata$ipw) / mean((1-bootdata$smokeyn) * bootdata$BPSysAve*bootdata$ipw)
    
    #truncated p1/p99
    rds1[i] <- mean(bootdata$smokeyn * bootdata$BPSysAve*bootdata$ipw_truncated_1) - mean((1-bootdata$smokeyn) * bootdata$BPSysAve*bootdata$ipw_truncated_1)
    rrs1[i] <- mean(bootdata$smokeyn * bootdata$BPSysAve*bootdata$ipw_truncated_1) / mean((1-bootdata$smokeyn) * bootdata$BPSysAve*bootdata$ipw_truncated_1)
    
    #truncated p5/p95
    rds2[i] <- mean(bootdata$smokeyn * bootdata$BPSysAve*bootdata$ipw_truncated_2) - mean((1-bootdata$smokeyn) * bootdata$BPSysAve*bootdata$ipw_truncated_2)
    rrs2[i] <- mean(bootdata$smokeyn * bootdata$BPSysAve*bootdata$ipw_truncated_2) / mean((1-bootdata$smokeyn) * bootdata$BPSysAve*bootdata$ipw_truncated_2)
    
    #truncated p10/p90
    rds3[i] <- mean(bootdata$smokeyn * bootdata$BPSysAve*bootdata$ipw_truncated_3) - mean((1-bootdata$smokeyn) * bootdata$BPSysAve*bootdata$ipw_truncated_3)
    rrs3[i] <- mean(bootdata$smokeyn * bootdata$BPSysAve*bootdata$ipw_truncated_3) / mean((1-bootdata$smokeyn) * bootdata$BPSysAve*bootdata$ipw_truncated_3)
    
    #if (i %% 100 == 0)
    #print(i)
  }
  
  print("##################################################")
  print("##################################################")
  
  print("")
  print("BOOTSTRAP 95% CI OF RISK DIFFERENCE USING ipw")
  
  #risk difference
  print(sd(rds))
  print(mean(rds))
  print(quantile(rds, prob=c(0.025,0.975)))
  
  #risk ratio
  #sd(rrs)
  #mean(rrs)
  #print(quantile(rrs, prob=c(0.025,0.975)))
  
  print("")
  print("BOOTSTRAP 95% CI OF RISK DIFFERENCE USING ipw_truncated_1")
  
  #risk difference
  print(sd(rds1))
  print(mean(rds1))
  print(quantile(rds1, prob=c(0.025,0.975)))
  
  print("")
  print("BOOTSTRAP 95% CI OF RISK DIFFERENCE USING ipw_truncated_2")
  
  #risk difference
  print(sd(rds2))
  print(mean(rds2))
  print(quantile(rds2, prob=c(0.025,0.975)))
  
  print("")
  print("BOOTSTRAP 95% CI OF RISK DIFFERENCE USING ipw_truncated_3")
  
  #risk difference
  print(sd(rds3))
  print(mean(rds3))
  print(quantile(rds3, prob=c(0.025,0.975)))
  
}
    
#PROPENSITY SCORE DESCRIPTIVES
boxplot(ps~smokeyn,data=small.nhanes.working, main="Propensity scores",
        xlab="Current smoking status", ylab="Predicted probability") 

##################################################
# Covariate balance after weighting:
##################################################

library(survey)

weighted <- svydesign(ids=~0, data=small.nhanes.working, weights=small.nhanes.working$smokeyn/small.nhanes.working$ps + (1-small.nhanes.working$smokeyn)/(1-small.nhanes.working$ps))
table <- svyCreateTableOne(vars=c("female","age7cat","racecat","educat","marstat","incomecat","povcat","Age","Poverty"), strata="SmokeNow", data=weighted, smd=TRUE, test=TRUE)
print(table, smd=TRUE)


























    
##################################################
##################################################
# MODEL BUILDING
##################################################
##################################################

#original coding
#BPSysAve ~ smokeyn + Age + female + Race3 + Education + MaritalStatus + HHIncome + Poverty

#required for likelihood ratio test
#install.packages("lmtest")
library(lmtest)

#required for VIF
#install.packages("VIF")
library(VIF)

#required for VIF
#install.packages("car")
library(car)

#required for CPR plots
#install.packages("crPlots")
library(crPlots)

#test full model based on original versus new coding of variables, linear model
#INCLUDE AGESQ
iamodel1 <- glm(BPSysAve ~ smokeyn + Age + female + racecat + educat + marstat + incomecat + povcat, data=small.nhanes.working, family=gaussian(link=identity))
print(summary(iamodel1))
qqnorm(resid(iamodel1))
qqline(resid(iamodel1))
iamodel2 <- glm(BPSysAve ~ smokeyn + Age + female + Race3 + Education + MaritalStatus + HHIncome + Poverty, data=small.nhanes.working, family=gaussian(link=identity))
print(summary(iamodel2))
qqnorm(resid(iamodel2))
qqline(resid(iamodel2))

#test full model with and without agesq, and with and without age categorical, recoded variables, linear model
#INCLUDE AGESQ

  iamodel1 <- glm(BPSysAve ~ smokeyn + Age + female + racecat + educat + marstat + incomecat + povcat, data=small.nhanes.working, family=gaussian(link=identity))
    print(summary(iamodel1))
    qqnorm(resid(iamodel1))
    qqline(resid(iamodel1))
    
  iamodel2 <- glm(BPSysAve ~ smokeyn + Age + agesq + female + racecat + educat + marstat + incomecat + povcat, data=small.nhanes.working, family=gaussian(link=identity))
    print(summary(iamodel2))
    qqnorm(resid(iamodel2))
    qqline(resid(iamodel2))
    lrtest(iamodel1,iamodel2)
  
  #lrtest not significant, indicating that full model with categorical age is not favored over nested model with continuous age
  iamodel3 <- glm(BPSysAve ~ smokeyn + age7cat + female + racecat + educat + marstat + incomecat + povcat, data=small.nhanes.working, family=gaussian(link=identity))
    print(summary(iamodel3))
    qqnorm(resid(iamodel3))
    qqline(resid(iamodel3))
    lrtest(iamodel1,iamodel3)

  #compare age using poisson model, without poverty
    iamodel1 <- glm(BPSysAve ~ smokeyn + Age + female + racecat + educat + marstat + incomecat, data=small.nhanes.working, family=poisson(link=log))
    iamodel3 <- glm(BPSysAve ~ smokeyn + age7cat + female + racecat + educat + marstat + incomecat, data=small.nhanes.working, family=poisson(link=log))
    lrtest(iamodel1,iamodel3)
    
#test transformation of BP, with agesq
#normal plot is improved, consider transformation but with log link, test in next step
iamodel1 <- glm(BPSysAve ~ smokeyn + Age + agesq + female + racecat + educat + marstat + incomecat + povcat, data=small.nhanes.working, family=gaussian(link=identity))
print(summary(iamodel1))
qqnorm(resid(iamodel1))
qqline(resid(iamodel1))
iamodel2 <- glm(lnbp ~ smokeyn + Age + agesq + female + racecat + educat + marstat + incomecat + povcat, data=small.nhanes.working, family=gaussian(link=identity))
print(summary(iamodel2))
qqnorm(resid(iamodel2))
qqline(resid(iamodel2))
lrtest(iamodel1,iamodel2)

#test with log-linear model vs. linear
#USE LOG LINEAR MODEL
iamodel1 <- glm(BPSysAve ~ smokeyn + Age + agesq + female + racecat + educat + marstat + incomecat + povcat, data=small.nhanes.working, family=poisson(link=log))
print(summary(iamodel1))
qqnorm(resid(iamodel1))
qqline(resid(iamodel1))
iamodel2 <- glm(BPSysAve ~ smokeyn + Age + agesq + female + racecat + educat + marstat + incomecat + povcat, data=small.nhanes.working, family=gaussian(link=identity))
print(summary(iamodel2))
qqnorm(resid(iamodel2))
qqline(resid(iamodel2))

#test with log-linear model and interactions, removing agesq, removing poverty
#smoke*age significant
#smoke*gender significant
#smoke*income significant
#smoke*marital status significant
#smoke*education significant
#smoke*race significant
  
  iamodel1 <- glm(BPSysAve ~ smokeyn + Age + female + racecat + educat + marstat + incomecat, data=small.nhanes.working, family=poisson(link=log))
  print(summary(iamodel1))
  qqnorm(resid(iamodel1))
  qqline(resid(iamodel1))
  
  iamodel2 <- glm(BPSysAve ~ smokeyn*Age + female + racecat + educat + marstat + incomecat, data=small.nhanes.working, family=poisson(link=log))
  print(summary(iamodel2))
  qqnorm(resid(iamodel2))
  qqline(resid(iamodel2))
  lrtest(iamodel1,iamodel2)
  
  iamodel3 <- glm(BPSysAve ~ smokeyn*female + Age + racecat + educat + marstat + incomecat, data=small.nhanes.working, family=poisson(link=log))
  print(summary(iamodel3))
  qqnorm(resid(iamodel3))
  qqline(resid(iamodel3))
  lrtest(iamodel1,iamodel3)

  iamodel4 <- glm(BPSysAve ~ smokeyn*incomecat + Age + female + racecat + educat + marstat, data=small.nhanes.working, family=poisson(link=log))
  print(summary(iamodel4))
  qqnorm(resid(iamodel4))
  qqline(resid(iamodel4))
  lrtest(iamodel1,iamodel4)
  
  iamodel5 <- glm(BPSysAve ~ smokeyn*marstat + Age + female + racecat + educat + incomecat, data=small.nhanes.working, family=poisson(link=log))
  print(summary(iamodel5))
  qqnorm(resid(iamodel5))
  qqline(resid(iamodel5))
  lrtest(iamodel1,iamodel5)

  iamodel6 <- glm(BPSysAve ~ smokeyn*educat + Age + female + racecat + marstat + incomecat, data=small.nhanes.working, family=poisson(link=log))
  print(summary(iamodel6))
  qqnorm(resid(iamodel6))
  qqline(resid(iamodel6))
  lrtest(iamodel1,iamodel6)
  
  iamodel7 <- glm(BPSysAve ~ smokeyn*racecat + Age + female + educat + marstat + incomecat, data=small.nhanes.working, family=poisson(link=log))
  print(summary(iamodel7))
  qqnorm(resid(iamodel7))
  qqline(resid(iamodel7))
  lrtest(iamodel1,iamodel7)
  
  #test all interactions
  iamodel8 <- glm(BPSysAve ~ smokeyn + smokeyn*Age + smokeyn*female + smokeyn*racecat + smokeyn*educat + smokeyn*marstat + smokeyn*incomecat, data=small.nhanes.working, family=poisson(link=log))
  print(summary(iamodel8))
  qqnorm(resid(iamodel8))
  qqline(resid(iamodel8))
  
  #test all interactions
  iamodel8 <- glm(BPSysAve ~ smokeyn + smokeyn*Age + smokeyn*female + racecat + educat + marstat + incomecat, data=small.nhanes.working, family=poisson(link=log))
  print(summary(iamodel8))
  qqnorm(resid(iamodel8))
  qqline(resid(iamodel8))
  
#test with and without blocks of predictors, using log-linear model
#RETAIN ALL VARIABLES, STATISTICALLY SIGNIFICANT LR TEST FAVORING RETENTION

  iamodel1 <- glm(BPSysAve ~ smokeyn + Age + agesq + female + racecat + educat + marstat + incomecat, data=small.nhanes.working, family=poisson(link=log))
  print(summary(iamodel1))
  qqnorm(resid(iamodel1))
  qqline(resid(iamodel1))
  
  iamodel2 <- glm(BPSysAve ~ smokeyn + Age + agesq + female + racecat + educat + marstat + incomecat, data=small.nhanes.working, family=poisson(link=log))
  print(summary(iamodel2))
  qqnorm(resid(iamodel2))
  qqline(resid(iamodel2))
  lrtest(iamodel1,iamodel2)
  
  iamodel3 <- glm(BPSysAve ~ smokeyn + Age + agesq + female + racecat + educat + marstat + povcat, data=small.nhanes.working, family=poisson(link=log))
  print(summary(iamodel3))
  qqnorm(resid(iamodel3))
  qqline(resid(iamodel3))
  lrtest(iamodel1,iamodel3)
  
  iamodel4 <- glm(BPSysAve ~ smokeyn + Age + agesq + female + racecat + educat + incomecat + povcat, data=small.nhanes.working, family=poisson(link=log))
  print(summary(iamodel4))
  qqnorm(resid(iamodel4))
  qqline(resid(iamodel4))
  lrtest(iamodel1,iamodel4)
  
  iamodel5 <- glm(BPSysAve ~ smokeyn + Age + agesq + female + racecat + marstat + incomecat + povcat, data=small.nhanes.working, family=poisson(link=log))
  print(summary(iamodel5))
  qqnorm(resid(iamodel5))
  qqline(resid(iamodel5))
  lrtest(iamodel1,iamodel5)

#test VIF with full model, linear
#INCOME AND POVERTY HAVE INFLATED VIF, BUT LESS THAN 10
#REMOVE POVERTY, HIGHEST VIF
  
  iamodel1 <- lm(BPSysAve ~ smokeyn + Age + female + racecat + educat + marstat + incomecat + povcat, data=small.nhanes.working)
  print(summary(iamodel1))
  vif(iamodel1)
  
  iamodel2 <- lm(BPSysAve ~ smokeyn + Age + female + racecat + educat + marstat + incomecat, data=small.nhanes.working)
  print(summary(iamodel2))
  vif(iamodel2)

    lrtest(iamodel1,iamodel2)

    
    
    
    
    
    
    
    
    
#PROPENSITY SCORE MODEL, without age interaction
#test with and without blocks of predictors, using logistic model
    
    iamodel1 <- glm(smokeyn ~ Age + female + racecat + educat + marstat + incomecat, data=small.nhanes.working, family=binomial(link=logit))
    print(summary(iamodel1))
    qqnorm(resid(iamodel1))
    qqline(resid(iamodel1))
    
    #not significant, do not include age*sex
    iamodel2 <- glm(smokeyn ~ Age*female + racecat + educat + marstat + incomecat, data=small.nhanes.working, family=binomial(link=logit))
    print(summary(iamodel2))
    qqnorm(resid(iamodel2))
    qqline(resid(iamodel2))
    lrtest(iamodel1,iamodel2)
    
    #not significant, do not include age*sex
    iamodel3 <- glm(smokeyn ~ Age*racecat + female + educat + marstat + incomecat, data=small.nhanes.working, family=binomial(link=logit))
    print(summary(iamodel3))
    qqnorm(resid(iamodel3))
    qqline(resid(iamodel3))
    lrtest(iamodel1,iamodel3)
    
    #significant
    iamodel4 <- glm(smokeyn ~ Age*educat + female + racecat + marstat + incomecat, data=small.nhanes.working, family=binomial(link=logit))
    print(summary(iamodel4))
    qqnorm(resid(iamodel4))
    qqline(resid(iamodel4))
    lrtest(iamodel1,iamodel4)
    
    #not significant
    iamodel5 <- glm(smokeyn ~ Age*marstat + female + racecat + educat + incomecat, data=small.nhanes.working, family=binomial(link=logit))
    print(summary(iamodel5))
    qqnorm(resid(iamodel5))
    qqline(resid(iamodel5))
    lrtest(iamodel1,iamodel5)
    
    #significant
    iamodel6 <- glm(smokeyn ~ Age*incomecat + female + racecat + educat + marstat, data=small.nhanes.working, family=binomial(link=logit))
    print(summary(iamodel6))
    qqnorm(resid(iamodel6))
    qqline(resid(iamodel6))
    lrtest(iamodel1,iamodel6)
    
    #not significant
    iamodel7 <- glm(smokeyn ~ Age + female*racecat + educat + marstat + incomecat, data=small.nhanes.working, family=binomial(link=logit))
    print(summary(iamodel7))
    qqnorm(resid(iamodel7))
    qqline(resid(iamodel7))
    lrtest(iamodel1,iamodel7)
    
    #not significant
    iamodel8 <- glm(smokeyn ~ Age + female*educat + racecat + marstat + incomecat, data=small.nhanes.working, family=binomial(link=logit))
    print(summary(iamodel8))
    qqnorm(resid(iamodel8))
    qqline(resid(iamodel8))
    lrtest(iamodel1,iamodel8)
    
    #not significant
    iamodel9 <- glm(smokeyn ~ Age + female*incomecat + racecat + educat + marstat, data=small.nhanes.working, family=binomial(link=logit))
    print(summary(iamodel9))
    qqnorm(resid(iamodel9))
    qqline(resid(iamodel9))
    lrtest(iamodel1,iamodel9)
    
    #significant
    iamodel0 <- glm(smokeyn ~ Age + female + racecat + educat*incomecat + marstat, data=small.nhanes.working, family=binomial(link=logit))
    print(summary(iamodel0))
    qqnorm(resid(iamodel0))
    qqline(resid(iamodel0))
    lrtest(iamodel1,iamodel0)
    
    
plot(iamodel)
plot(fitted(iamodel), resid(iamodel)) #homoscedasticity
plot(small.nhanes.working$Age,resid(iamodel))
#abline(lm(resid(iamodel) ~small.nhanes.working$Age), col="red")
lines(lowess(small.nhanes.working$Age,resid(iamodel)), col="red")



##################################################
##################################################
# Direct standardization, saturated model:
##################################################
##################################################

  #set model variables here
  modelvarlist <- c("BPSysAve ~ smokeyn + Age + female + Race3 + Education + MaritalStatus + HHIncome + Poverty",
                    "BPSysAve ~ smokeyn + Age + female + racecat + educat + marstat + incomecat + povcat",
                    "BPSysAve ~ smokeyn + Age + female + racecat + educat + marstat + incomecat")
  
  for (modelvars in modelvarlist){
    
    print(modelvars)
    
    #model estimates
    
      iamodel <- glm(modelvars, data=small.nhanes.working, family=gaussian(link=identity))
      print(summary(iamodel))
    
    #create new data to score, with smoking status set to either 1 or 0, and all other covariates set to observed values
      
      small.nhanes.smoke1<-small.nhanes.working
      small.nhanes.smoke1$smokeyn<-1
      #print(table(small.nhanes.smoke1$smokeyn))
      
      small.nhanes.smoke0<-small.nhanes
      small.nhanes.smoke0$smokeyn<-0
      #print(table(small.nhanes.smoke0$smokeyn))
    
    # means of predicted means based on regression estimates but applied to synthetic dataset with exposure set to 1 and 0, and covariates set to observed values
    
      est1<-mean(predict(iamodel,small.nhanes.smoke1,type="response"))
      est0<-mean(predict(iamodel,small.nhanes.smoke0, type="response"))
    
      print(paste("EST1: ", est1))
      print(paste("EST0: ", est0))
      print(paste("DIFF: ", est1-est0))

    # Bootstrap standard error and confidence interval:
        
      nboot <- 1000
      nobs <- length(small.nhanes.working$BPSysAve)
      #print(nobs)
      rds <- rep(NA, nboot)
      rrs <- rep(NA, nboot)
      set.seed(1)
      for (i in 1:nboot) {
        bootidx <- sample(c(1:nobs), size=nobs, replace=TRUE) #sample from 1 to Nobs with replacement
        bootdata <- small.nhanes.working[bootidx,] #create bootstrap dataset with Nobs, including observations that match each bootidx
        iamodel <- glm(modelvars, data=bootdata, family=gaussian(link=identity))
        
        #create data to score
        bootdata.smoke1<-bootdata
        bootdata.smoke1$smokeyn<-1
        #print(table(bootdata.smoke1$smokeyn))
        
        bootdata.smoke0<-bootdata
        bootdata.smoke0$smokeyn<-0
        #print(table(bootdata.smoke0$smokeyn))
        
        # means of predicted means based on regression estimates but applied to synthetic dataset with exposure set to 1 and 0, and covariates set to observed values
        est1<-mean(predict(iamodel,bootdata.smoke1,type="response"))
        est0<-mean(predict(iamodel,bootdata.smoke0, type="response"))
        #print(est1)
        #print(est0)
        
        rds[i] <- est1-est0
        rrs[i] <- est1/est0
        #if (i %% 100 == 0)
          #print(i)
      }

      #risk difference
      sd(rds)
      mean(rds)
      print(quantile(rds, prob=c(0.025,0.975)))
      
      #risk ratio
      sd(rrs)
      mean(rrs)
      #print(quantile(rrs, prob=c(0.025,0.975)))
      
  }

