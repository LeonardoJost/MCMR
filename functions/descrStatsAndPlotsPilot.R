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
ggsave("figs/MR/LinePlotPilot.png")
#plot attempted accuracy data as line graph (mean Data by degree and condition)
ggplot(datasetByIDandBlock,aes(y=accAttempts,x=nStimuli, fill=sex, shape=typeOfAlternatives,color=sex,linetype=sex)) + 
  stat_summary(na.rm=TRUE, fun=mean, geom="line") +
  stat_summary(na.rm=TRUE, fun=mean, geom="point", size=2) +
  stat_summary(fun.data=mean_se,geom="errorbar",position = "dodge",aes(linetype=NULL)) +
  scale_x_continuous(breaks=c(2,8))+
  labs(x="Number of alternatives",y="Proportion of correct attempted items",color="Sex",linetype="Sex",shape="Type of alternatives") + 
  guides(fill="none") + 
  theme_classic() + theme(legend.position = "right")
ggsave("figs/MR/LinePlotAttemptedAccPilot.png")
#plot attempted data as line graph (mean Data by degree and condition)
ggplot(datasetByIDandBlock,aes(y=attempts,x=nStimuli, fill=sex, shape=typeOfAlternatives,color=sex,linetype=sex)) + 
  stat_summary(na.rm=TRUE, fun=mean, geom="line") +
  stat_summary(na.rm=TRUE, fun=mean, geom="point", size=2) +
  stat_summary(fun.data=mean_se,geom="errorbar",position = "dodge",aes(linetype=NULL)) +
  scale_x_continuous(breaks=c(2,8))+
  labs(x="Number of alternatives",y="Attempted items",color="Sex",linetype="Sex",shape="Type of alternatives") + 
  guides(fill="none") + 
  theme_classic() + theme(legend.position = "right")
ggsave("figs/MR/LinePlotAttemptsPilot.png")
#plot fully correct trials as line graph (traditional scoring system)
ggplot(datasetByIDandBlock[which(datasetByIDandBlock$nStimuli==8),],aes(y=accScoringSystem,x=typeOfAlternatives, fill=sex,color=sex,linetype=sex)) + 
  #stat_summary(na.rm=TRUE, fun=mean, geom="line") +
  stat_summary(na.rm=TRUE, fun=mean, geom="point", size=2) +
  stat_summary(fun.data=mean_se,geom="errorbar",position = "dodge",aes(linetype=NULL)) +
  labs(x="Type of alternatives",y="Proportion of correct items",color="Sex",linetype="Sex",shape="Type of alternatives") + 
  guides(fill="none") + 
  theme_classic() + theme(legend.position = "right")
ggsave("figs/MR/LinePlotScoringSystemPilot.png")
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
  facet_grid(experience ~ STEM)+
  labs(x="Number of alternatives",y="Proportion of correct items",color="Sex",linetype="Sex",shape="Type of alternatives") + 
  guides(fill="none") + 
  geom_text(data=plyr::count(datasetByIDandBlock, vars = c("experience","STEM")), aes(x=6, y=0.5, label=paste0("n = ",freq/6)), colour="black", inherit.aes=FALSE, parse=FALSE)+
  theme_bw() + theme(legend.position = "right")
ggsave("figs/MR/LinePlotInteractionPilot.png")

#effect of angle
#create dataset summarized by angle 
datasetByIdAndDeg=ddply(datasetNoOutlier,
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
ggsave("figs/MR/AnglePilot.png")


combineImages(c("figs/MR/LinePlotAttemptsPilot.png","figs/MR/LinePlotAttemptedAccPilot.png","figs/MR/LinePlotScoringSystemPilot.png"),1,3,"figs/MR/combinedPilot.png")
