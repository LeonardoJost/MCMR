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
softwareQuestionnaire=("jQuery") #jaquery, OSWeb, or OpenSesame (classic)
#set data folder
folder="logfiles\\"
verbose=3 #detail of output
questionnaireOutFile="output\\questionnaire" #.csv added at end, leave empty if no output desired
outlierFactor=3 #factor of sd to define outliers in MR
block=c("main1","main2","main3","main4")#name of interesting block of data
questionnaireDataCols=c("ID","age","tic","education","study","work","sex","experience","experienceAcute","experienceChronic") #which questionnaire columns shall be kept for analysis

##read and write data
#read data
questionnaireData=getQuestionnaireData(softwareQuestionnaire,verbose,folder)
MRData=getMRData(software,verbose,folder,block)
#modify/clean data 
MRData=modifyMRData(verbose,MRData)
#merge rows of questionnaireData (replace empty values)
questionnaireData=data.frame(do.call(rbind, lapply(split(questionnaireData, questionnaireData$ID), function(a) sapply(a, function(x) x[!(x=="")][1]))))


#calculate means from questionnaire (and save to csv)
calculateMeansQuestionnaire(verbose,questionnaireData,questionnaireOutFile,"")
#remove not analyzed questionnaire data to protect participant identity
questionnaireData=subset(questionnaireData,select=questionnaireDataCols)

#unify data
dataset=merge(MRData,questionnaireData,by="ID")
#mark outliers
dataset=markOutliers(dataset,verbose)
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
dataset$ExperienceContrasts=sapply(as.factor(dataset$Experience),function(i) contr.sum(2)[i,])
STEMContrasts=sapply(as.factor(dataset$STEM),function(i) contr.sum(3)[i,])
dataset$STEMContrasts1=STEMContrasts[1,]
dataset$STEMContrasts2=STEMContrasts[2,]


#save full dataset to csv
write.table(dataset,file="output\\dataset.csv",sep=";", row.names = F)
