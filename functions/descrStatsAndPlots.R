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
datasetAnalysis=read.csv(file="output\\dataset.csv",sep=";")
dataset=read.csv(file="dataset\\dataset.csv",sep=";")
datasetByIDandBlock=read.csv(file="dataset\\datasetGrouped.csv",sep=";")

# #print descriptive statistics
# outliers=unique(datasetAnalysis[which(datasetAnalysis$outlier),c("ID","STEM","Experience","sex")])
# n_occur=data.frame(table(paste(outliers$STEM,paste(outliers$Experience,outliers$sex))))
# print(n_occur)
# ##number of each combination of between factors
# datasetBetweenFactors=unique(datasetAnalysis[,c("ID","sex","STEM","Experience")])
# nrow(datasetBetweenFactors)
# n_occur=data.frame(table(paste(datasetBetweenFactors$sex,paste(datasetBetweenFactors$STEM,datasetBetweenFactors$Experience))))
# print(n_occur)
# 
#number of each combination of between factors
datasetBetweenFactors=unique(datasetAnalysis[,c("ID","sex","education")])
nrow(datasetBetweenFactors)
n_occur=data.frame(table(paste(datasetBetweenFactors$sex,datasetBetweenFactors$education)))
print(n_occur)
# 
# #outliers by sex
# outlierIDs=unique(datasetAnalysis[,c("outlier","ID","sex")])
# n_occur=data.frame(table(paste(outlierIDs$outlier,outlierIDs$sex)))
# print(n_occur)
# 
# #outliers by experience
# outlierIDs=unique(datasetAnalysis[,c("outlier","ID","sex","experienceAcute","experienceChronic")])
# n_occur=data.frame(table(paste(outlierIDs$outlier,outlierIDs$sex,outlierIDs$experienceAcute,outlierIDs$experienceChronic)))
# print(n_occur)
# 
# #overall outliers
# outlierIDs=unique(datasetAnalysis[,c("outlier","ID")])
# n_occur=data.frame(table(paste(outlierIDs$outlier)))
# print(n_occur)
# 
# #remove outliers
# datasetAnalysis=datasetAnalysis[which(!datasetAnalysis$outlier & !is.na(datasetAnalysis$sex)),]
# #remove participants with experience
# datasetAnalysis=datasetAnalysis[which((datasetAnalysis$experienceAcute=="no" & datasetAnalysis$experienceChronic=="no") | datasetAnalysis$experience=="no"),]

# #create summarized datasets
# library(plyr)
# #create dataset summarized by trials (over multiple items of trials)
# datasetByIDandTrial=ddply(datasetAnalysis,
#                           .(ID,block,experience,sex,nStimuli,typeOfAlternatives,trialNumber),
#                           summarize,
#                           hits=sum((type=="hit")),
#                           incorrects=sum((type=="incorrect")))
# #create dataset summarized by blocks
# datasetByIDandBlock=ddply(datasetByIDandTrial,
#                           .(ID,block,experience,sex,nStimuli,typeOfAlternatives),
#                           summarize,
#                           hitSum=sum(hits),
#                           incorrectSum=sum(incorrects),
#                           acc=hitSum/24,
#                           accAttempts=hitSum/(hitSum+incorrectSum))
#save full dataset to csv
library(ggplot2)
#plot accuracy data as line graph (mean Data by degree and condition)
ggplot(datasetByIDandBlock,aes(y=acc,x=nStimuli, fill=sex, shape=typeOfAlternatives,color=sex,linetype=sex)) + 
  stat_summary(na.rm=TRUE, fun=mean, geom="line") +
  stat_summary(na.rm=TRUE, fun=mean, geom="point", size=2) +
  stat_summary(fun.data=mean_se,geom="errorbar",position = "dodge",aes(linetype=NULL)) +
  scale_x_continuous(breaks=c(2,8))+
  labs(x="Number of alternatives",y="Proportion of correct items",color="Sex",linetype="Sex",shape="Type of alternatives") + 
  guides(fill="none") + 
  theme_classic() + theme(legend.position = "right")
ggsave("figs/MR/LinePlot.png")
#plot attempted accuracy data as line graph (mean Data by degree and condition)
ggplot(datasetByIDandBlock,aes(y=accAttempts,x=nStimuli, fill=sex, shape=typeOfAlternatives,color=sex,linetype=sex)) + 
  stat_summary(na.rm=TRUE, fun=mean, geom="line") +
  stat_summary(na.rm=TRUE, fun=mean, geom="point", size=2) +
  stat_summary(fun.data=mean_se,geom="errorbar",position = "dodge",aes(linetype=NULL)) +
  scale_x_continuous(breaks=c(2,8))+
  labs(x="Number of alternatives",y="Proportion of correct attempted items",color="Sex",linetype="Sex",shape="Type of alternatives") + 
  guides(fill="none") + 
  theme_classic() + theme(legend.position = "right")
ggsave("figs/MR/LinePlotAttemptedAcc.png")
#plot attempted data as line graph (mean Data by degree and condition)
ggplot(datasetByIDandBlock,aes(y=attempts,x=nStimuli, fill=sex, shape=typeOfAlternatives,color=sex,linetype=sex)) + 
  stat_summary(na.rm=TRUE, fun=mean, geom="line") +
  stat_summary(na.rm=TRUE, fun=mean, geom="point", size=2) +
  stat_summary(fun.data=mean_se,geom="errorbar",position = "dodge",aes(linetype=NULL)) +
  scale_x_continuous(breaks=c(2,8))+
  labs(x="Number of alternatives",y="Attempted items",color="Sex",linetype="Sex",shape="Type of alternatives") + 
  guides(fill="none") + 
  theme_classic() + theme(legend.position = "right")
ggsave("figs/MR/LinePlotAttempts.png")
#plot fully correct trials as line graph (traditional scoring system)
ggplot(datasetByIDandBlock[which(datasetByIDandBlock$nStimuli==8),],aes(y=accScoringSystem,x=typeOfAlternatives, fill=sex,color=sex,linetype=sex)) + 
  #stat_summary(na.rm=TRUE, fun=mean, geom="line") +
  stat_summary(na.rm=TRUE, fun=mean, geom="point", size=2) +
  stat_summary(fun.data=mean_se,geom="errorbar",position = "dodge",aes(linetype=NULL)) +
  labs(x="Type of alternatives",y="Proportion of correct items",color="Sex",linetype="Sex",shape="Type of alternatives") + 
  guides(fill="none") + 
  theme_classic() + theme(legend.position = "right")
ggsave("figs/MR/LinePlotScoringSystem.png")
#plot data as line graph separated by experience and stem
ns=ddply(datasetByIDandBlock,
         .(experience),
         summarize,
         n=paste("n = ", length(ID)/4))
ggplot(datasetByIDandBlock,aes(y=acc,x=nStimuli, fill=sex, shape=typeOfAlternatives,color=sex,linetype=sex)) + 
  stat_summary(na.rm=TRUE, fun=mean, geom="line") +
  stat_summary(na.rm=TRUE, fun=mean, geom="point", size=2) +
  stat_summary(fun.data=mean_se,geom="errorbar",position = "dodge",aes(linetype=NULL)) +
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
                        .(ID,sex,nStimuli,typeOfAlternatives,deg),
                        summarize,
                        hits=sum((type=="hit")),
                        misses=sum((type=="incorrect")),
                        acc=hits/(hits+misses))
ggplot(datasetByIdAndDeg,aes(y=acc,x=deg)) +
  stat_summary(na.rm=TRUE, fun=mean, geom="line") +
  stat_summary(na.rm=TRUE, fun=mean, geom="point", size=2) +
  stat_summary(fun.data=mean_se,geom="errorbar",position = "dodge",aes(linetype=NULL)) +
  guides(fill="none") + 
  theme_classic() + theme(legend.position = "right")
ggsave("figs/MR/Angle.png")

#plot questionnairedata
#education
ggplot(questionnaireDataNoOutliers,aes(x=factor(education),color=sex, fill=sex)) +
  geom_bar(stat="count",position="dodge")  + 
  xlab("education") + ylab("count") +
  scale_x_discrete(guide = guide_axis(n.dodge=3),limits=c("Kein Schulabschluss", "Hauptschulabschluss", "Mittlere Reife" ,"Abitur","Studium","Bachelor","Master/Diplom","Promotion")) +
  theme_classic() + theme(legend.position = "right")
ggsave("figs/education.png")

