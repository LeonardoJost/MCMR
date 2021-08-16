### Corrected power estimation for number of blocks
#     Copyright (C) 2021  Leonardo Jost
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

###functions

#generate dataset
#n - number of participants
#N - number of measurement categories in total (this is currently always n*6)
#numberOfEachTrial - number of repetitions of each measurement for binomial distribution (note the hard coded outliers)
#standard deviation consists of two parts
#sqrt(0.66)*sd - standard deviation between participants
#sqrt(sd2)*sd - standard deviation within participants
#for sd2=0.34 the total standard deviation is sd
generateData=function(n,N,numberOfEachTrial,sd=0.73,sd2=0.34) {
  testdata=data.frame(ids=as.factor(rep(1:n,each=N/n)),
                      sex=factor(rep(c("m","f"),each=N/2)),
                      nStimuli=factor(rep(c(2,4,8),N/3)),
                      type=factor(rep(c("type1","type2"),N/2)))
  #get base probabilities for each combination of nStimuli and type
  # testdata$baseProb=ifelse(testdata$nStimuli==2 & testdata$type=="type1" & testdata$sex=="m",2.20,
  #                          ifelse(testdata$nStimuli==4 & testdata$type=="type1" & testdata$sex=="m",2.20,
  #                          ifelse(testdata$nStimuli==8 & testdata$type=="type1" & testdata$sex=="m",2.20,
  #                          ifelse(testdata$nStimuli==2 & testdata$type=="type2" & testdata$sex=="m",2.20,
  #                          ifelse(testdata$nStimuli==4 & testdata$type=="type2" & testdata$sex=="m",2.20,
  #                          ifelse(testdata$nStimuli==8 & testdata$type=="type2" & testdata$sex=="m",2.20,
  #                          ifelse(testdata$nStimuli==2 & testdata$type=="type1" & testdata$sex=="f",1.93,
  #                          ifelse(testdata$nStimuli==4 & testdata$type=="type1" & testdata$sex=="f",1.66,
  #                          ifelse(testdata$nStimuli==8 & testdata$type=="type1" & testdata$sex=="f",1.39,
  #                          ifelse(testdata$nStimuli==2 & testdata$type=="type2" & testdata$sex=="f",2.20,
  #                          ifelse(testdata$nStimuli==4 & testdata$type=="type2" & testdata$sex=="f",2.11,
  #                          ifelse(testdata$nStimuli==8 & testdata$type=="type2" & testdata$sex=="f",2.02,
  #                          0))))))))))))
  testdata$baseProb=ifelse(testdata$nStimuli==2 & testdata$type=="type1" & testdata$sex=="m",1.45,
                    ifelse(testdata$nStimuli==4 & testdata$type=="type1" & testdata$sex=="m",1.45,
                    ifelse(testdata$nStimuli==8 & testdata$type=="type1" & testdata$sex=="m",1.45,
                    ifelse(testdata$nStimuli==2 & testdata$type=="type2" & testdata$sex=="m",1.45,
                    ifelse(testdata$nStimuli==4 & testdata$type=="type2" & testdata$sex=="m",1.45,
                    ifelse(testdata$nStimuli==8 & testdata$type=="type2" & testdata$sex=="m",1.45,
                    ifelse(testdata$nStimuli==2 & testdata$type=="type1" & testdata$sex=="f",1.195,
                    ifelse(testdata$nStimuli==4 & testdata$type=="type1" & testdata$sex=="f",0.94,
                    ifelse(testdata$nStimuli==8 & testdata$type=="type1" & testdata$sex=="f",0.685,
                    ifelse(testdata$nStimuli==2 & testdata$type=="type2" & testdata$sex=="f",1.45,
                    ifelse(testdata$nStimuli==4 & testdata$type=="type2" & testdata$sex=="f",1.365,
                    ifelse(testdata$nStimuli==8 & testdata$type=="type2" & testdata$sex=="f",1.28,
                    0))))))))))))
  #convert to numeric to test for main effects as part of interactions
  testdata$sexNumeric=sapply(testdata$sex,function(i) contr.sum(2)[i,])
  testdata$typeNumeric=sapply(testdata$type,function(i) contr.sum(2)[i,])
  nStimuliNumeric=sapply(testdata$nStimuli,function(i) contr.sum(3)[i,])
  testdata$nStimuliNumeric1=nStimuliNumeric[1,]
  testdata$nStimuliNumeric2=nStimuliNumeric[2,]
  #get base probability for each participant, corr coeff of .66
  testdata$baseProbID=rep(rnorm(n),each=N/n)*sqrt(0.66)*sd
  #get probability from log odds
  testdata$logOdds=testdata$baseProb+testdata$baseProbID+rnorm(N)*sqrt(sd2)*sd
  #convert to probability
  testdata$prob=exp(testdata$logOdds)/(1+exp(testdata$logOdds))
  #generate number of correct responses
  #note that the binomial distribution adds additional variance of n*p*(1-p)
  #generate number of attempted tasks
  #not used, because unanswered tasks are already included in accuracy
  #testdata$weights=replicate(nrow(testdata),sum(runif(numberOfEachTrial)>0.3))
  testdata$weights=rep(numberOfEachTrial,N)
  #generate correct responses
  testdata$correctResponses=rbinom(N,size=testdata$weights,prob=testdata$prob)/testdata$weights
  #generate fully correct trials (4 items, 2 consecutively correct items as p^2 because there are no other assumptions)
  testdata$correctResponsesScoringSystem=rbinom(N,size=testdata$weights/2,prob=testdata$prob^2)/testdata$weights/2
  #use p^3 as base probability due to scoring approximation
  testdata$correctResponsesScoringSystem3=rbinom(N,size=testdata$weights/2,prob=testdata$prob^3)/testdata$weights/2
  return(testdata)
}

#get p values of effects of interest
getSignificant=function(testdata){
  glmerModel=glmer(correctResponses~sexNumeric*nStimuliNumeric1*typeNumeric+sexNumeric*nStimuliNumeric2*typeNumeric+(1|ids),
                    family=binomial(),data=testdata,weights=weights,
                    control = glmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
  glmerModel1=update(glmerModel,formula = ~. -sexNumeric:nStimuliNumeric1:typeNumeric-sexNumeric:nStimuliNumeric2:typeNumeric)
  glmerModel2=update(glmerModel,formula = ~. -sexNumeric:nStimuliNumeric1-sexNumeric:nStimuliNumeric2)
  glmerModel3=update(glmerModel,formula = ~. -sexNumeric:typeNumeric)
  return(c(anova(glmerModel,glmerModel1)$"Pr(>Chisq)"[2],
           anova(glmerModel,glmerModel2)$"Pr(>Chisq)"[2],
           anova(glmerModel,glmerModel3)$"Pr(>Chisq)"[2]))
}
getSignificantLmer=function(testdata){
  lmerModel=lmer(logOdds~sexNumeric*nStimuliNumeric1*typeNumeric+sexNumeric*nStimuliNumeric2*typeNumeric+(1|ids),
                   data=testdata,REML=FALSE,
                   control = lmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
  lmerModel1=update(lmerModel,formula = ~. -sexNumeric:nStimuliNumeric1:typeNumeric-sexNumeric:nStimuliNumeric2:typeNumeric)
  lmerModel2=update(lmerModel,formula = ~. -sexNumeric:nStimuliNumeric1-sexNumeric:nStimuliNumeric2)
  lmerModel3=update(lmerModel,formula = ~. -sexNumeric:typeNumeric)
  return(c(anova(lmerModel,lmerModel1)$"Pr(>Chisq)"[2],
           anova(lmerModel,lmerModel2)$"Pr(>Chisq)"[2],
           anova(lmerModel,lmerModel3)$"Pr(>Chisq)"[2]))
}
#iterate over random data
#ns - vector of number of participants
#linear - use of lmer or glmer
#numberOfEachTrials - vector of numbers of each trial (for binomial distribution in glmer)
#reps - number of simulations
#in case of lmer=TRUE, numberOfEachTrials is not meaningful
randSim=function(ns,linear=TRUE,numberOfEachTrials=c(1),reps=1000,sd=0.73,sd2=0.34){
  numTrials=length(numberOfEachTrials)
  #loop over number of participants
  significantDataFrame=data.frame(numberOfEachTrial=rep(numberOfEachTrials,each=3*length(ns)),
                                  n=rep(ns,each=3*numTrials),
                                  effects=rep(c("sex*nStimuli*type","sex*nStimuli","sex*type"),length(ns)*numTrials),
                                  propSignificant=rep(0,length(ns)*3*numTrials))
  for(numberOfEachTrial in numberOfEachTrials){
    for(n in ns) {
      #to get some sense of progress
      print(n)
      #every combination (3 nStimuli*2 types=6)
      N=n*6
      significant = matrix(nrow=reps, ncol=3)
      for(i in 1:reps){
        testdata=generateData(n,N,numberOfEachTrial,sd,sd2)
        if (linear) {
          significant[i,]=getSignificantLmer(testdata)<0.05
        } else {
          significant[i,]=getSignificant(testdata)<0.05
        }
      }
      #save proportion to data frame
      significantDataFrame$propSignificant[which(significantDataFrame$numberOfEachTrial==numberOfEachTrial & significantDataFrame$n==n & significantDataFrame$effects=="sex*nStimuli*type")]=
        sum(significant[,1])/reps
      significantDataFrame$propSignificant[which(significantDataFrame$numberOfEachTrial==numberOfEachTrial & significantDataFrame$n==n & significantDataFrame$effects=="sex*nStimuli")]=
        sum(significant[,2])/reps
      significantDataFrame$propSignificant[which(significantDataFrame$numberOfEachTrial==numberOfEachTrial & significantDataFrame$n==n & significantDataFrame$effects=="sex*type")]=
        sum(significant[,3])/reps
    }
  }
  return(significantDataFrame)
}

#calculate cohens d for logarithmic odds and simulated responses
getCohensDs=function(testdata,typ,nStim){
  #print(paste(typ,nStim,sep=","))
  #cohens d for log odds
  meanM=mean(testdata$logOdds[which(testdata$nStimuli==nStim & testdata$sex=="m" & testdata$type==typ)],na.rm=T)
  meanF=mean(testdata$logOdds[which(testdata$nStimuli==nStim & testdata$sex=="f" & testdata$type==typ)],na.rm=T)
  sdM=sd(testdata$logOdds[which(testdata$nStimuli==nStim & testdata$sex=="m" & testdata$type==typ)],na.rm=T)
  sdF=sd(testdata$logOdds[which(testdata$nStimuli==nStim & testdata$sex=="f" & testdata$type==typ)],na.rm=T)
  valueLogOdds=(meanM-meanF)/(sqrt((sdM^2+sdF^2)/2))
  #cohens d for correct responses
  meanM=mean(testdata$correctResponses[which(testdata$nStimuli==nStim & testdata$sex=="m" & testdata$type==typ)],na.rm=T)
  meanF=mean(testdata$correctResponses[which(testdata$nStimuli==nStim & testdata$sex=="f" & testdata$type==typ)],na.rm=T)
  sdM=sd(testdata$correctResponses[which(testdata$nStimuli==nStim & testdata$sex=="m" & testdata$type==typ)],na.rm=T)
  sdF=sd(testdata$correctResponses[which(testdata$nStimuli==nStim & testdata$sex=="f" & testdata$type==typ)],na.rm=T)
  valueResponses=(meanM-meanF)/(sqrt((sdM^2+sdF^2)/2))
  #cohens d for scoring system
  meanM=mean(testdata$correctResponsesScoringSystem[which(testdata$nStimuli==nStim & testdata$sex=="m" & testdata$type==typ)],na.rm=T)
  meanF=mean(testdata$correctResponsesScoringSystem[which(testdata$nStimuli==nStim & testdata$sex=="f" & testdata$type==typ)],na.rm=T)
  sdM=sd(testdata$correctResponsesScoringSystem[which(testdata$nStimuli==nStim & testdata$sex=="m" & testdata$type==typ)],na.rm=T)
  sdF=sd(testdata$correctResponsesScoringSystem[which(testdata$nStimuli==nStim & testdata$sex=="f" & testdata$type==typ)],na.rm=T)
  valueResponsesScoringSystem=(meanM-meanF)/(sqrt((sdM^2+sdF^2)/2))
  #cohens d for scoring system (p^3)
  meanM=mean(testdata$correctResponsesScoringSystem3[which(testdata$nStimuli==nStim & testdata$sex=="m" & testdata$type==typ)],na.rm=T)
  meanF=mean(testdata$correctResponsesScoringSystem3[which(testdata$nStimuli==nStim & testdata$sex=="f" & testdata$type==typ)],na.rm=T)
  sdM=sd(testdata$correctResponsesScoringSystem3[which(testdata$nStimuli==nStim & testdata$sex=="m" & testdata$type==typ)],na.rm=T)
  sdF=sd(testdata$correctResponsesScoringSystem3[which(testdata$nStimuli==nStim & testdata$sex=="f" & testdata$type==typ)],na.rm=T)
  valueResponsesScoringSystem3=(meanM-meanF)/(sqrt((sdM^2+sdF^2)/2))
return(c(valueLogOdds,valueResponses,valueResponsesScoringSystem,valueResponsesScoringSystem3))
}

#calculate mean cohensD for multiple randomly generated data
cohensDsim=function(ns,numberOfEachTrials=c(8,12,16),reps=100,sd=0.73,sd2=0.34){
  numTrials=length(numberOfEachTrials)
  #loop over number of participants
  dsDataFrame=data.frame(numberOfEachTrial=rep(numberOfEachTrials,each=4*length(ns)),
                                  n=rep(ns,each=4*numTrials),
                                  probType=rep(c("logOdds","responses","scoringSystem","scoringSystem3"),length(ns)*numTrials),
                                  value=rep(0,length(ns)*4*numTrials))
  for(numberOfEachTrial in numberOfEachTrials){
    for(n in ns) {
      #every combination (3 nStimuli*2 types=6)
      N=n*6
      cohensDs = matrix(nrow=reps, ncol=4)
      for(i in 1:reps){
        testdata=generateData(n,N,numberOfEachTrial,sd,sd2)
        cohensDs[i,]=getCohensDs(testdata,"type1",4)
      }
      #save means to data frame
      dsDataFrame$value[which(dsDataFrame$numberOfEachTrial==numberOfEachTrial & dsDataFrame$n==n & dsDataFrame$probType=="logOdds")]=
        mean(cohensDs[,1])
      dsDataFrame$value[which(dsDataFrame$numberOfEachTrial==numberOfEachTrial & dsDataFrame$n==n & dsDataFrame$probType=="responses")]=
        mean(cohensDs[,2])
      dsDataFrame$value[which(dsDataFrame$numberOfEachTrial==numberOfEachTrial & dsDataFrame$n==n & dsDataFrame$probType=="scoringSystem")]=
        mean(cohensDs[,3])
      dsDataFrame$value[which(dsDataFrame$numberOfEachTrial==numberOfEachTrial & dsDataFrame$n==n & dsDataFrame$probType=="scoringSystem3")]=
        mean(cohensDs[,4])
    }
  }
  return(dsDataFrame)
}

###script

#load libraries
library(lme4)
library(optimx)
library(ggplot2)

#generate random seed (this should be random enough)
#sample(0:100000,1)
#28329
#88735
#99261
set.seed(28329)
#show different powers for different numbers of participants
ns=c(14:26)*5
significantDataFrame=randSim(ns)
#plot by n
ggplot(significantDataFrame,aes(x=n,y=propSignificant,color=effects)) +
  geom_point() + geom_line() + labs(y="simulated power", x="N") +
  scale_x_continuous(breaks = ns) +
  theme_classic()
ggsave(paste("figs/SimulPowerN2.png",sep=""))
#save data
write.table(significantDataFrame,file="simulPowerN2.csv",sep=";", row.names = FALSE)

#get power for simulated binomial distributions
set.seed(88735)
numberOfEachTrials=c(12,16,24,32,48,64)
significantDataFrame=randSim(ns=c(100),F,numberOfEachTrials,1000,0.73,0)
#plot by number of trials
ggplot(significantDataFrame,aes(x=numberOfEachTrial,y=propSignificant,color=effects)) +
  geom_point() + geom_line() + labs(y="simulated power", x="trials per test") +
  scale_x_continuous(breaks = numberOfEachTrials) +
  theme_classic()
ggsave(paste("figs/SimulPowerTrials2.png",sep=""))
#save data
write.table(significantDataFrame,file="simulPowerTrials2.csv",sep=";", row.names = FALSE)
#calculate cohens d for different numbers of trials
set.seed(99261)
numberOfEachTrials=c(8,12,16,24,32,48)
cohensDsDataFrame=cohensDsim(ns=c(100),numberOfEachTrials,1000,0.73,0)
#rename
cohensDsDataFrame$values=ifelse(cohensDsDataFrame$probType=="logOdds","base probability(log odds)",
                                ifelse(cohensDsDataFrame$probType=="responses","binomially distributed responses",
                                ifelse(cohensDsDataFrame$probType=="scoringSystem","scoring system (p^2)","scoring system (p^3)")))
#plot by number of trials
ggplot(cohensDsDataFrame,aes(x=numberOfEachTrial,y=value,color=values,shape=values)) +
  geom_point() + geom_line() + labs(y="simulated Cohen's d", x="number of responses",color="",shape="") +
  scale_x_continuous(breaks = numberOfEachTrials) +
  theme_classic() +theme(legend.position = c(0.7,0.25),legend.background = element_rect(fill="transparent"))
ggsave(paste("figs/cohensD2.png",sep=""))

#save data
write.table(cohensDsDataFrame,file="cohensD2.csv",sep=";", row.names = FALSE)


##power analysis and effect sizes for anova
library(Superpower)
#mu <- c(2.20,2.20,2.20,2.20,2.20,2.20,1.93,1.66,1.39,2.20,2.11,2.02)
mu <- c(1.45,1.45,1.45,1.45,1.45,1.45,1.195,0.94,0.685,1.45,1.365,1.28)
n <- 50
sd <- 0.73
r <- 0.66
string = "2b*2w*3w"
alpha_level <- 0.05
labelnames = c("sex","m","f","type","mixed","pairwise","nStim","2","4","8")
design_result <- ANOVA_design(design = string,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r,
                              labelnames = labelnames,
                              plot=T)
#design_result
exact_result <- ANOVA_exact(design_result,
                            alpha_level = alpha_level,
                            verbose=F)
exact_result$main_results
simulation_result <- ANOVA_power(design_result, 
                                 alpha_level = alpha_level, 
                                 nsims = 500,
                                 verbose = F)
simulation_result$main_results

#effects on 2x2x2
#calculate effect size
library(Superpower)
mu <- c(2.20,2.20,2.20,2.20,1.93,1.66,2.20,2.11)
mu <- c(1.45,1.45,1.45,1.45,1.195,0.94,1.45,0.9)
n <- 50
sd <- 0.73
r <- 0.66
string = "2b*2w*2w"
alpha_level <- 0.05
labelnames = c("sex","m","f","type","mixed","pairwise","nStim","2","4")
design_result <- ANOVA_design(design = string,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r,
                              labelnames = labelnames,
                              plot=T)
#design_result
exact_result <- ANOVA_exact(design_result,
                            alpha_level = alpha_level,
                            verbose=F)
exact_result$main_results
exact_result$main_results$cohen_f[3]+exact_result$main_results$cohen_f[5]
