### Script for detection of outliers
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

source("functions/helpers.R")

library(plyr)

markOutliers=function(dataset,verbose){
  #averages for each participant and condition
  datasetByIDandBlock=ddply(dataset,
                            .(ID,block,Experience,sex),
                            summarize,
                            time=sum(reactionTime*2/nStimuli,na.rm=T)/180000,
                            hits=sum((type=="hit")),
                            incorrects=sum((type=="incorrect")),
                            attempts=(hits+incorrects)/24,
                            accAttempts=hits/(hits+incorrects),
                            acc=sum((type=="hit")/24),
                            firstAnswerSelected=sum(firstAnswerSelected,na.rm=T)/24)
  #overall averages for each participant
  datasetByID=ddply(datasetByIDandBlock,
                    .(ID,Experience,sex),
                    summarize,
                    timeAvg=sum(time,na.rm=T)/6,
                    hitsAvg=sum(hits)/6,
                    incorrectsAvg=sum(incorrects)/6,
                    attemptsAvg=sum(attempts)/6,
                    accAttemptsAvg=hitsAvg/(hitsAvg+incorrectsAvg),
                    accAvg=sum(hitsAvg/24),
                    firstAnswerSelectedAvg=mean(abs(firstAnswerSelected-1/2)))
  #exclude those with attempted accuracy at or below chance
  possibleOutliers1=datasetByID[which(datasetByID$accAttemptsAvg<=1/2),]
  #preferred speed too much over accuracy (sum of accuracy and used time <1)
  possibleOutliers2=datasetByID[which(datasetByID$accAvg+datasetByID$timeAvg<1),]
  #chose the first answer too often or too rarely indicating answer patterns
  possibleOutliers3=datasetByID[which(datasetByID$accAttemptsAvg<2/3 &
                                        datasetByID$firstAnswerSelectedAvg>0.4),]
  #too few attempts
  possibleOutliers4=datasetByID[which(datasetByID$attemptsAvg<=0.5),]
  #is empty
  #inspect data of blocks with low accuracy (many wrong or few answers)
  possibleOutliers5=datasetByIDandBlock[which(datasetByIDandBlock$acc<=0.1),]
  #critical case is already included in other outliers
  #combine
  outliers=unique(rbind(possibleOutliers1,possibleOutliers2,possibleOutliers3,possibleOutliers4))
  #console output
  if(verbose>1){
    print(paste("overall ", nrow(outliers)," outliers detected",sep=""))
  }
  if(verbose>2){
    print("outliers by type:")
    print(paste(nrow(possibleOutliers1)," outliers with overall accuracy at or below chance level",sep=""))
    print(paste(nrow(possibleOutliers2)," outliers with too much emphasis on speed over accuracy",sep=""))
    print(paste(nrow(possibleOutliers3)," outliers with too many/few choices of the first alternative",sep=""))
    print(paste(nrow(possibleOutliers4)," outliers with too few overall attempts",sep=""))
  }
  #mark outliers in original dataset
  dataset$outlier=ifelse(dataset$ID %in% outliers$ID,TRUE,FALSE)
  return(dataset)
}

