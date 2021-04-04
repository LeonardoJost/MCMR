### create and plot dataset for analysis
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

#load full dataset
datasetAnalysis=read.csv(file="dataset\\dataset.csv",sep=";")
#remove outliers
datasetAnalysis=datasetAnalysis[which(!datasetAnalysis$outlier & !is.na(datasetAnalysis$sex)),]

#scaling
datasetAnalysis$degScaled=datasetAnalysis$deg/100

#create summarized datasets
library(plyr)
#create dataset summarized by trials (over multiple items of trials)
datasetByIDandTrial=ddply(datasetAnalysis,
                          .(ID,block,Experience,STEM,sex,nStimuli,typeOfAlternatives,itemNumber),
                          summarize,
                          hits=sum((type=="hit")),
                          incorrects=sum((type=="incorrect")),
                          allCorrect=ifelse(incorrects==0 & hits>0,1,0))
#create dataset summarized by blocks
datasetByIDandBlock=ddply(datasetAnalysis,
                          .(ID,block,Experience,STEM,sex,nStimuli,typeOfAlternatives),
                          summarize,
                          time=sum(reactionTime*2/nStimuli,na.rm=T)/180000,
                          hits=sum((type=="hit")),
                          incorrects=sum((type=="incorrect")),
                          accAttempts=hits/(hits+incorrects),
                          acc=sum((type=="hit")/24))
library(ggplot2)
#plot data as line graph (mean Data by degree and condition)
ggplot(datasetByIDandBlock,aes(y=acc,x=nStimuli, fill=sex, shape=typeOfAlternatives,color=sex)) + 
  stat_summary(na.rm=TRUE, fun=mean, geom="line",aes(linetype=sex)) +
  stat_summary(na.rm=TRUE, fun=mean, geom="point", size=2) +
  stat_summary(fun.data=mean_se,geom="errorbar",position = "dodge") +
  scale_x_continuous(breaks=c(2,4,8))+
  labs(x="number of alternatives",y="Proportion of correct answers",color="sex",linetype="sex",shape="type of alternatives") + 
  guides(fill=FALSE) + 
  theme_classic() + theme(legend.position = "right")
ggsave("figs/MR/LinePlot.png")
ggplot(datasetByIDandTrial,aes(y=allCorrect,x=nStimuli, fill=sex, shape=typeOfAlternatives,color=sex)) + 
  stat_summary(na.rm=TRUE, fun=mean, geom="line",aes(linetype=sex)) +
  stat_summary(na.rm=TRUE, fun=mean, geom="point", size=2) +
  stat_summary(fun.data=mean_se,geom="errorbar",position = "dodge") +
  scale_x_continuous(breaks=c(2,4,8))+
  labs(x="number of alternatives",y="Proportion of correct answers",color="sex",linetype="sex",shape="type of alternatives") + 
  guides(fill=FALSE) + 
  theme_classic() + theme(legend.position = "right")

