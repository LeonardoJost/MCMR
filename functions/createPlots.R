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
                          acc=hitSum/24,
                          n=paste("n = ", length(ID)/6))
#save full dataset to csv
library(ggplot2)
#plot data as line graph (mean Data by degree and condition)
ggplot(datasetByIDandBlock,aes(y=acc,x=nStimuli, fill=sex, shape=typeOfAlternatives,color=sex)) + 
  stat_summary(na.rm=TRUE, fun=mean, geom="line",aes(linetype=sex)) +
  stat_summary(na.rm=TRUE, fun=mean, geom="point", size=2) +
  stat_summary(fun.data=mean_se,geom="errorbar",position = "dodge") +
  scale_x_continuous(breaks=c(2,4,8))+
  labs(x="number of alternatives",y="Proportion of correct trials",color="sex",linetype="sex",shape="type of alternatives") + 
  guides(fill=FALSE) + 
  theme_bw() + theme(legend.position = "right")
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
  labs(x="number of alternatives",y="Proportion of correct trials",color="sex",linetype="sex",shape="type of alternatives") + 
  guides(fill=FALSE) + 
  geom_text(data=plyr::count(datasetByIDandBlock, vars = c("Experience","STEM")), aes(x=6, y=0.5, label=paste0("n = ",freq/6)), colour="black", inherit.aes=FALSE, parse=FALSE)+
  theme_bw() + theme(legend.position = "right")
ggsave("figs/MR/LinePlotInteraction.png")


