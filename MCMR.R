### main program
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
source("functions/readData.R", encoding="utf-8")
source("functions/generateGraphsAndTables.R", encoding="utf-8")
source("functions/detectOutliers.R")

##create output directories, if they don't exist (outputs warnings otherwise)
dir.create("figs")
dir.create("output")
dir.create("figs/MR")

##options, parameters
options(digits=6)
software=("OSWeb") #OSWeb or OpenSesame (classic)
softwareQuestionnaire=("jQuery") #jquery, OSWeb, or OpenSesame (classic)
#set data folder
folder="logfiles\\"
verbose=2 #detail of output
questionnaireOutFile="output\\questionnaire" #.csv added at end, leave empty if no output desired
#outlierFactor=3 #factor of sd to define outliers in MR
block=c("main1","main2","main3","main4")#name of interesting block of data
questionnaireDataCols=c("ID","age","education","study","work","sex","experience","experienceAcute","experienceChronic") #which questionnaire columns shall be kept for merging/summarizing
questionnaireDataColsForAnalysis=c("ID","sex","education","experience","experienceAcute","experienceChronic") #which questionnaire columns shall be kept for analysis

##read and write data
#read data
questionnaireData=getQuestionnaireData(softwareQuestionnaire,verbose,folder)
MRData=getMRData(software,verbose,folder,block)
#modify/clean data 
MRData=modifyMRData(verbose,MRData)
#merge rows of questionnaireData (replace empty values)
questionnaireData=data.frame(do.call(rbind, lapply(split(questionnaireData, questionnaireData$ID), function(a) sapply(a, function(x) x[!(x=="" | is.na(x))][1]))))
#remove participants who did not finish
questionnaireData=questionnaireData[which(!is.na(questionnaireData$experience)),]
#trasnform age to numeric
questionnaireData$age=toNumeric(questionnaireData$age)

#remove not at least descriptively analyzed questionnaire data
questionnaireData=subset(questionnaireData,select=questionnaireDataCols)
#calculate means from questionnaire (and save to csv)
calculateMeansQuestionnaire(verbose,questionnaireData,questionnaireOutFile)
#unify data
dataset=merge(MRData,questionnaireData,by="ID")
#mark outliers
dataset=markOutliers(dataset,verbose)
#remove unnecessary column
dataset$firstAnswerSelected=NULL
#calculate means from questionnaire for outliers separately
questionnaireDataOutliers=unique(dataset[which(dataset$outlier),questionnaireDataCols])
questionnaireDataNoOutliers=unique(dataset[which(!dataset$outlier &
                                                   ((dataset$experienceAcute=="no" & dataset$experienceChronic=="no") | dataset$experience=="no")),questionnaireDataCols])
#calculate means from questionnaire for outliers and nonOutliers separately (and save to csv)
calculateMeansQuestionnaire(verbose,questionnaireDataOutliers,paste(questionnaireOutFile,"Outliers",sep=""))
calculateMeansQuestionnaire(verbose,questionnaireDataNoOutliers,paste(questionnaireOutFile,"NoOutliers",sep=""))
#remove not analyzed questionnaire data to protect participant identity
dataset[names(dataset)[names(dataset) %in% setdiff(questionnaireDataCols,questionnaireDataColsForAnalysis)]]=NULL
#anonymise IDs to protect participant identity
dataset$ID=as.factor(dataset$ID)
levels(dataset$ID)=paste("id",sample.int(length(levels(dataset$ID))),sep="")
#remove divers participants (because there is only one and outlier) for anonymisation
dataset=dataset[which(dataset$sex!="divers"),]

#save full dataset to csv
write.table(dataset,file="output\\datasetFull.csv",sep=";", row.names = F)

#remove outliers and persons with too much experience (dataset for analysis)
#remove outliers
datasetNoOutlier=dataset[which(!dataset$outlier),]
#remove participants with experience
datasetNoOutlier=datasetNoOutlier[which((datasetNoOutlier$experienceAcute=="no" & datasetNoOutlier$experienceChronic=="no") | datasetNoOutlier$experience=="no"),]

#add sum contrasted variables (divide by 2 for scaling)
datasetNoOutlier$sexContrasts=sapply(as.factor(datasetNoOutlier$sex),function(i) contr.sum(2)[i,])/2
datasetNoOutlier$typeContrasts=sapply(as.factor(datasetNoOutlier$typeOfAlternatives),function(i) contr.sum(2)[i,])/2
datasetNoOutlier$nStimuliContrasts=sapply(as.factor(datasetNoOutlier$nStimuli),function(i) contr.sum(2)[i,])/2
#add education in ascending order (from alphabetical factors)
datasetNoOutlier$educationNumeric=c(4,6,2,1,7,3,8,5)[as.numeric(as.factor(datasetNoOutlier$education))]


#save dataset to csv
write.table(datasetNoOutlier,file="output\\dataset.csv",sep=";", row.names = F)

#create summarized datasets
library(plyr)
#create dataset summarized by trials (over multiple items of trials)
datasetByIDandTrial=ddply(datasetNoOutlier,
                          .(ID,block,sexContrasts,nStimuliContrasts,typeContrasts,sex,nStimuli,typeOfAlternatives,trialNumber,attemptedItems,education,educationNumeric),
                          summarize,
                          hits=sum((type=="hit")),
                          incorrects=sum((type=="incorrect")))
#create dataset summarized by blocks
datasetByIDandBlock=ddply(datasetByIDandTrial,
                          .(ID,block,sexContrasts,nStimuliContrasts,typeContrasts,sex,nStimuli,typeOfAlternatives,education,educationNumeric),
                          summarize,
                          hitSum=sum(hits),
                          incorrectSum=sum(incorrects),
                          acc=hitSum/24,
                          accAttempts=hitSum/(hitSum+incorrectSum),
                          attempts=sum(attemptedItems,na.rm=T),
                          fullCorrectTrials=sum(hits==nStimuli/2),
                          accScoringSystem=nStimuli[1]*sum(hits==nStimuli/2)/24/2)
#save dataset to csv
write.table(datasetByIDandBlock,file="output\\datasetGrouped.csv",sep=";", row.names = F)

