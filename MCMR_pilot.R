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
#set data folder
folder="logfiles\\pilot\\"
verbose=3 #detail of output
questionnaireOutFile="output\\questionnaire" #.csv added at end, leave empty if no output desired
outlierFactor=3 #factor of sd to define outliers in MR
block=c("main1","main2","main3","main4","main5","main6")#name of interesting block of data
questionnaireDataCols=c("ID","Gender","STEM","Experience") #which questionnaire columns shall be kept for statistical analysis

##read and write data
#read data
questionnaireData=getQuestionnaireData(software,verbose,folder)
MRData=getMRData(software,verbose,folder,block)
#modify/clean data 
questionnaireData=modifyQuestionnaireData(questionnaireData,c("Gender"),c(),c())
MRData=modifyMRData(verbose,MRData)

#calculate means from questionnaire (and save to csv)
calculateMeansQuestionnaire(verbose,questionnaireData,questionnaireOutFile,"")
#remove not analyzed questionnaire data to protect participant identity
questionnaireData=subset(questionnaireData,select=questionnaireDataCols)
questionnaireData$STEM=ifelse(grepl("MINT",questionnaireData$STEM),"STEM",
                              ifelse(grepl("anderes",questionnaireData$STEM),"nonSTEM","none"))
#rename gender to sex
questionnaireData$sex=questionnaireData$Gender
questionnaireData$Gender=NULL
#capitalize experience
questionnaireData$experience=questionnaireData$Experience
questionnaireData$Experience=NULL
#unify data
dataset=merge(MRData,questionnaireData,by="ID")
#mark outliers
dataset=markOutliers(dataset,verbose,blocks=6)
#remove unnecessary column
dataset$firstAnswerSelected=NULL
#anonymise IDs to protect participant identity
dataset$ID=as.factor(dataset$ID)
levels(dataset$ID)=paste("id",sample.int(length(levels(dataset$ID))),sep="")

#add sum contrasted variables
dataset$sexContrasts=sapply(as.factor(dataset$sex),function(i) contr.sum(2)[i,])
dataset$typeContrasts=sapply(as.factor(dataset$typeOfAlternatives),function(i) contr.sum(2)[i,])
nStimuliContrasts=sapply(as.factor(dataset$nStimuli),function(i) contr.sum(3)[i,])
dataset$nStimuliContrasts1=nStimuliContrasts[1,]
dataset$nStimuliContrasts2=nStimuliContrasts[2,]
dataset$experienceContrasts=sapply(as.factor(dataset$experience),function(i) contr.sum(2)[i,])
STEMContrasts=sapply(as.factor(dataset$STEM),function(i) contr.sum(3)[i,])
dataset$STEMContrasts1=STEMContrasts[1,]
dataset$STEMContrasts2=STEMContrasts[2,]

#rename entries to english
dataset$sex=ifelse(dataset$sex=="m","male","female")
dataset$experience=ifelse(dataset$experience=="Ja","yes","no")


#save full dataset to csv
write.table(dataset,file="output\\datasetFullPilot.csv",sep=";", row.names = F)

#remove outliers
datasetNoOutlier=dataset[which(!dataset$outlier),]
datasetNoOutlier=datasetNoOutlier[which(!is.na(datasetNoOutlier$sex)),]

#save dataset to csv
write.table(datasetNoOutlier,file="output\\datasetPilot.csv",sep=";", row.names = F)

#create summarized datasets
library(plyr)
#create dataset summarized by trials (over multiple items of trials)
datasetByIDandTrial=ddply(datasetNoOutlier,
                          .(ID,block,experience,STEMContrasts1,STEMContrasts2,experienceContrasts,STEM,sexContrasts,nStimuliContrasts1,nStimuliContrasts2,typeContrasts,sex,nStimuli,typeOfAlternatives,trialNumber,attemptedItems),
                          summarize,
                          hits=sum((type=="hit")),
                          incorrects=sum((type=="incorrect")))
#create dataset summarized by blocks
datasetByIDandBlock=ddply(datasetByIDandTrial,
                          .(ID,block,experience,STEMContrasts1,STEMContrasts2,experienceContrasts,STEM,sexContrasts,nStimuliContrasts1,nStimuliContrasts2,typeContrasts,sex,nStimuli,typeOfAlternatives),
                          summarize,
                          hitSum=sum(hits),
                          incorrectSum=sum(incorrects),
                          acc=hitSum/24,
                          accAttempts=hitSum/(hitSum+incorrectSum),
                          attempts=sum(attemptedItems,na.rm=T),
                          fullCorrectTrials=sum(hits==nStimuli/2),
                          accScoringSystem=nStimuli[1]*sum(hits==nStimuli/2)/24/2)
#save dataset to csv
write.table(datasetByIDandBlock,file="output\\datasetGroupedPilot.csv",sep=";", row.names = F)
