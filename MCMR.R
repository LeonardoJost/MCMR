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
folder="data\\"
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
#unify data
dataset=merge(MRData,questionnaireData,by="ID")
#mark outliers
dataset=markOutliers(dataset)
#remove unnecessary column
dataset$firstAnswerSelected=NULL
#anonymise IDs to protect participant identity
dataset$ID=as.factor(dataset$ID)
levels(dataset$ID)=paste("id",sample.int(length(levels(dataset$ID))),sep="")

#save full dataset to csv
write.table(dataset,file="output\\dataset.csv",sep=";", row.names = F)

##create datasets for analysis and plot reaction time and accuracy by interesting conditions
source("functions/createDatasets.R", encoding="utf-8")