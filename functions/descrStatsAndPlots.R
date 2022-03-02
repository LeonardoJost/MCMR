### print descriptive statistics and plot dataset 
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

##create output directories, if they don't exist (outputs warnings otherwise)
dir.create("figs")
dir.create("figs/MR")

#load full dataset
datasetAnalysis=read.csv(file="dataset\\dataset.csv",sep=";")
#print descriptive statistics
outliers=unique(datasetAnalysis[which(datasetAnalysis$outlier),c("ID","STEM","Experience","sex")])
n_occur=data.frame(table(paste(outliers$STEM,paste(outliers$Experience,outliers$sex))))
print(n_occur)
##number of each combination of between factors
datasetBetweenFactors=unique(datasetAnalysis[,c("ID","sex","STEM","Experience")])
nrow(datasetBetweenFactors)
n_occur=data.frame(table(paste(datasetBetweenFactors$sex,paste(datasetBetweenFactors$STEM,datasetBetweenFactors$Experience))))
print(n_occur)

#number of each combination of between factors
datasetBetweenFactors=unique(datasetAnalysis[,c("ID","sex","Experience")])
nrow(datasetBetweenFactors)
n_occur=data.frame(table(paste(datasetBetweenFactors$sex,datasetBetweenFactors$Experience)))
print(n_occur)

#overall outliers
outlierIDs=unique(datasetAnalysis[,c("outlier","ID","sex")])
n_occur=data.frame(table(paste(outlierIDs$outlier,outlierIDs$sex)))
print(n_occur)

#remove outliers
datasetAnalysis=datasetAnalysis[which(!datasetAnalysis$outlier & !is.na(datasetAnalysis$sex)),]

#create summarized datasets
library(plyr)
#create dataset summarized by trials (over multiple items of trials)
datasetByIDandTrial=ddply(datasetAnalysis,
                          .(ID,block,Experience,STEM,sex,nStimuli,typeOfAlternatives,trialNumber),
                          summarize,
                          hits=sum((type=="hit")))
#create dataset summarized by blocks
datasetByIDandBlock=ddply(datasetByIDandTrial,
                          .(ID,block,Experience,STEM,sex,nStimuli,typeOfAlternatives),
                          summarize,
                          hitSum=sum(hits),
                          acc=hitSum/24)
#save full dataset to csv
library(ggplot2)
#plot data as line graph (mean Data by degree and condition)
ggplot(datasetByIDandBlock,aes(y=acc,x=nStimuli, fill=sex, shape=typeOfAlternatives,color=sex)) + 
  stat_summary(na.rm=TRUE, fun=mean, geom="line",aes(linetype=sex)) +
  stat_summary(na.rm=TRUE, fun=mean, geom="point", size=2) +
  stat_summary(fun.data=mean_se,geom="errorbar",position = "dodge") +
  scale_x_continuous(breaks=c(2,4,8))+
  labs(x="Number of alternatives",y="Proportion of correct items",color="Sex",linetype="Sex",shape="Type of alternatives") + 
  guides(fill="none") + 
  theme_classic() + theme(legend.position = "right")
ggsave("figs/MR/LinePlot.png")
#plot data as line graph separated by experience and stem
ns=ddply(datasetByIDandBlock,
         .(Experience,STEM),
         summarize,
         n=paste("n = ", length(ID)/6))
ggplot(datasetByIDandBlock,aes(y=acc,x=nStimuli, fill=sex, shape=typeOfAlternatives,color=sex)) + 
  stat_summary(na.rm=TRUE, fun=mean, geom="line",aes(linetype=sex)) +
  stat_summary(na.rm=TRUE, fun=mean, geom="point", size=2) +
  stat_summary(fun.data=mean_se,geom="errorbar",position = "dodge") +
  scale_x_continuous(breaks=c(2,4,8))+
  facet_grid(Experience ~ STEM)+
  labs(x="Number of alternatives",y="Proportion of correct items",color="Sex",linetype="Sex",shape="Type of alternatives") + 
  guides(fill="none") + 
  geom_text(data=plyr::count(datasetByIDandBlock, vars = c("Experience","STEM")), aes(x=6, y=0.5, label=paste0("n = ",freq/6)), colour="black", inherit.aes=FALSE, parse=FALSE)+
  theme_bw() + theme(legend.position = "right")
ggsave("figs/MR/LinePlotInteraction.png")

#effect of angle
#create dataset summarized by angle 
datasetByIdAndDeg=ddply(datasetAnalysis,
                        .(ID,Experience,STEM,sex,nStimuli,typeOfAlternatives,deg),
                        summarize,
                        hits=sum((type=="hit")),
                        misses=sum((type=="incorrect")),
                        acc=hits/(hits+misses))
ggplot(datasetByIdAndDeg,aes(y=acc,x=deg)) +
  stat_summary(na.rm=TRUE, fun=mean, geom="line") +
  stat_summary(na.rm=TRUE, fun=mean, geom="point", size=2) +
  stat_summary(fun.data=mean_se,geom="errorbar",position = "dodge") +
  guides(fill="none") + 
  theme_classic() + theme(legend.position = "right")
ggsave("figs/MR/Angle.png")
#overall cohens d
datasetByIDandBlockNonStemNoExp=datasetByIDandBlock[which(datasetByIDandBlock$Experience=="no" & datasetByIDandBlock$STEM=="nonSTEM"),]
getCohensD=function(testdata,nStim,typ,sdCalculated=FALSE){
  meanM=mean(testdata$acc[which(testdata$nStimuli==nStim & testdata$sex=="male" & testdata$typeOfAlternatives==typ)],na.rm=T)
  meanF=mean(testdata$acc[which(testdata$nStimuli==nStim & testdata$sex=="female" & testdata$typeOfAlternatives==typ)],na.rm=T)
  if(sdCalculated){
    sdM=testdata$accSd[which(testdata$nStimuli==nStim & testdata$sex=="male" & testdata$typeOfAlternatives==typ)]
    sdF=testdata$accSd[which(testdata$nStimuli==nStim & testdata$sex=="female" & testdata$typeOfAlternatives==typ)]
  } else {
    sdM=sd(testdata$acc[which(testdata$nStimuli==nStim & testdata$sex=="male" & testdata$typeOfAlternatives==typ)],na.rm=T)
    sdF=sd(testdata$acc[which(testdata$nStimuli==nStim & testdata$sex=="female" & testdata$typeOfAlternatives==typ)],na.rm=T)
  }
  return((meanM-meanF)/(sqrt((sdM^2+sdF^2)/2)))
}
getCohensD(datasetByIDandBlockNonStemNoExp,2,"paired")
getCohensD(datasetByIDandBlockNonStemNoExp,2,"mixed")
getCohensD(datasetByIDandBlockNonStemNoExp,4,"paired")
getCohensD(datasetByIDandBlockNonStemNoExp,4,"mixed")
getCohensD(datasetByIDandBlockNonStemNoExp,8,"paired")
getCohensD(datasetByIDandBlockNonStemNoExp,8,"mixed")

getCohensD(datasetByIDandBlock,2,"paired")
getCohensD(datasetByIDandBlock,2,"mixed")
getCohensD(datasetByIDandBlock,4,"paired")
getCohensD(datasetByIDandBlock,4,"mixed")
getCohensD(datasetByIDandBlock,8,"paired")
getCohensD(datasetByIDandBlock,8,"mixed")

source("functions/helpers.R")
datasetByType=ddply(datasetByIDandBlock,
                          .(Experience,STEM,sex,nStimuli,typeOfAlternatives),
                          summarize,
                          accSd=sd(acc),
                          acc=mean(acc),
                          accLogOdds=toLogOdds(acc))
datasetByTypeNonStemNoExp=datasetByType[which(datasetByType$Experience=="no" & datasetByType$STEM=="nonSTEM"),]

#within test correlation
mixed2=datasetByIDandBlock$acc[which(datasetByIDandBlock$typeOfAlternatives=="mixed" & datasetByIDandBlock$nStimuli==2)]
mixed4=datasetByIDandBlock$acc[which(datasetByIDandBlock$typeOfAlternatives=="mixed" & datasetByIDandBlock$nStimuli==4)]
mixed8=datasetByIDandBlock$acc[which(datasetByIDandBlock$typeOfAlternatives=="mixed" & datasetByIDandBlock$nStimuli==8)]
paired2=datasetByIDandBlock$acc[which(datasetByIDandBlock$typeOfAlternatives=="paired" & datasetByIDandBlock$nStimuli==2)]
paired4=datasetByIDandBlock$acc[which(datasetByIDandBlock$typeOfAlternatives=="paired" & datasetByIDandBlock$nStimuli==4)]
paired8=datasetByIDandBlock$acc[which(datasetByIDandBlock$typeOfAlternatives=="paired" & datasetByIDandBlock$nStimuli==8)]
cor(matrix(c(mixed2,mixed4,mixed8,paired2,paired4,paired8),ncol=6))

#anova
summary(aov(acc~nStimuli*typeOfAlternatives*sex+Error(ID/(nStimuli*typeOfAlternatives)),data=datasetByIDandBlock))
summary(aov(acc~nStimuli*typeOfAlternatives*sex+Error(ID/(nStimuli*typeOfAlternatives)),data=datasetByIDandBlockNonStemNoExp))

#analyse log odds of accuracy
datasetByIDandBlock$accLogOdds=toLogOdds(datasetByIDandBlock$acc)
datasetByIDandBlock$accLogOdds[which(datasetByIDandBlock$accLogOdds>=10)]=9
mean(datasetByIDandBlock$accLogOdds)
sd(datasetByIDandBlock$accLogOdds)
mean(datasetByIDandBlock$accLogOdds)+2*sd(datasetByIDandBlock$accLogOdds)
sum(datasetByIDandBlock$accLogOdds>=10)
sum(datasetByIDandBlock$accLogOdds<10)
