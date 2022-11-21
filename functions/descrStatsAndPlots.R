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


library(ggplot2)
#plot accuracy data as line graph (mean Data by degree and condition)
ggplot(datasetByIDandBlock,aes(y=acc,x=nStimuli, fill=sex, shape=typeOfAlternatives,color=sex,linetype=sex)) + 
  stat_summary(na.rm=TRUE, fun=mean, geom="line") +
  stat_summary(na.rm=TRUE, fun=mean, geom="point", size=2) +
  stat_summary(fun.data=mean_se,geom="errorbar",position = "dodge",aes(linetype=NULL)) +
  scale_x_continuous(breaks=c(2,8))+
  labs(x="Number of alternatives",y="Proportion of correct items",color="Sex",linetype="Sex",shape="Type of\nalternatives") + 
  guides(fill="none") + 
  theme_classic() + theme(text = element_text(size=15),legend.position = "right")
ggsave("figs/MR/LinePlot.png")
#plot attempted accuracy data as line graph (mean Data by degree and condition)
ggplot(datasetByIDandBlock,aes(y=accAttempts,x=nStimuli, fill=sex, shape=typeOfAlternatives,color=sex,linetype=sex)) + 
  stat_summary(na.rm=TRUE, fun=mean, geom="line") +
  stat_summary(na.rm=TRUE, fun=mean, geom="point", size=2) +
  stat_summary(fun.data=mean_se,geom="errorbar",position = "dodge",aes(linetype=NULL)) +
  scale_x_continuous(breaks=c(2,8))+
  labs(x="Number of alternatives",y="Proportion of correct attempted items",color="Sex",linetype="Sex",shape="Type of\nalternatives") + 
  guides(fill="none") + 
  theme_classic() + theme(text = element_text(size=15),legend.position = "right")
ggsave("figs/MR/LinePlotAttemptedAcc.png")
#plot attempted data as line graph (mean Data by degree and condition)
ggplot(datasetByIDandBlock,aes(y=attempts,x=nStimuli, fill=sex, shape=typeOfAlternatives,color=sex,linetype=sex)) + 
  stat_summary(na.rm=TRUE, fun=mean, geom="line") +
  stat_summary(na.rm=TRUE, fun=mean, geom="point", size=2) +
  stat_summary(fun.data=mean_se,geom="errorbar",position = "dodge",aes(linetype=NULL)) +
  scale_x_continuous(breaks=c(2,8))+
  labs(x="Number of alternatives",y="Attempted items",color="Sex",linetype="Sex",shape="Type of\nalternatives") + 
  guides(fill="none") + 
  theme_classic() + theme(text = element_text(size=15),legend.position = "right")
ggsave("figs/MR/LinePlotAttempts.png")
#plot fully correct trials as line graph (traditional scoring system)
ggplot(datasetByIDandBlock[which(datasetByIDandBlock$nStimuli==8),],aes(y=accScoringSystem,x=typeOfAlternatives, fill=sex,color=sex,linetype=sex)) + 
  #stat_summary(na.rm=TRUE, fun=mean, geom="line") +
  stat_summary(na.rm=TRUE, fun=mean, geom="point", size=2) +
  stat_summary(fun.data=mean_se,geom="errorbar",position = "dodge",aes(linetype=NULL)) +
  labs(x="Type of alternatives",y="Proportion of fully correct trials",color="Sex",linetype="Sex",shape="Type of alternatives") + 
  guides(fill="none") + 
  theme_classic() + theme(text = element_text(size=15),legend.position = "right")
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
  scale_x_continuous(breaks=c(45,90,135,180))+
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
#age
ggplot(questionnaireDataNoOutliers,aes(x=age,color=sex, fill=sex)) +
  geom_bar(stat="count",position="dodge")  + 
  xlab("age in years") + ylab("count") +
  theme_classic() + theme(legend.position = "right")
ggsave("figs/age.png")

combineImages(c("figs/age.png","figs/education.png"),1,2,"figs/ageEducation.png")



# more restrictive outlier detection
library(plyr)
#overall averages for each participant
datasetByID=ddply(datasetByIDandBlock,
                  .(ID,sex),
                  summarize,
                  hitsAvg=sum(hitSum)/4,
                  incorrectsAvg=sum(incorrectSum)/4,
                  attemptsAvg=sum(attempts)/4,
                  accAttemptsAvg=hitsAvg/(hitsAvg+incorrectsAvg),
                  accAvg=sum(hitsAvg/24))
#exclude those with attempted accuracy at or below 0.6
possibleOutliers1=datasetByID[which(datasetByID$accAttemptsAvg<=0.6),] #136 outliers
sum(possibleOutliers1$sex=="male") #65
sum(possibleOutliers1$sex=="female") #71
datasetByIDandBlockRestrictiveOutlier=datasetByIDandBlock[which(!(datasetByIDandBlock$ID %in% possibleOutliers1$ID)),]
#plot and save figure
ggplot(datasetByIDandBlockRestrictiveOutlier,aes(y=acc,x=nStimuli, fill=sex, shape=typeOfAlternatives,color=sex,linetype=sex)) + 
  stat_summary(na.rm=TRUE, fun=mean, geom="line") +
  stat_summary(na.rm=TRUE, fun=mean, geom="point", size=2) +
  stat_summary(fun.data=mean_se,geom="errorbar",position = "dodge",aes(linetype=NULL)) +
  scale_x_continuous(breaks=c(2,8))+
  labs(x="Number of alternatives",y="Proportion of correct items",color="Sex",linetype="Sex",shape="Type of\nalternatives") + 
  guides(fill="none") + 
  theme_classic() + theme(text = element_text(size=15),legend.position = "right")
ggsave("figs/restrictiveOutliers.png")
#save dataset
write.table(datasetByIDandBlockRestrictiveOutlier,file="output\\datasetGroupedRestrictiveOutliers.csv",sep=";", row.names = F)

combineImages(c("figs/restrictiveOutliers.png","figs/MR/LinePlotAttempts.png","figs/MR/LinePlotAttemptedAcc.png","figs/MR/LinePlotScoringSystem.png"),2,2,"figs/MR/combined.png")

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
getCohensD(datasetByIDandBlock,2,"paired")
getCohensD(datasetByIDandBlock,2,"mixed")
getCohensD(datasetByIDandBlock,8,"paired")
getCohensD(datasetByIDandBlock,8,"mixed")