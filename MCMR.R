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

##create output directories, if they don't exist (outputs warnings otherwise)
dir.create("figs")
dir.create("output")
dir.create("figs/MR")

##options, parameters
options(digits=6)
#set data folder
folder="data\\test\\"
verbose=3 #detail of output
questionaireOutFile="output\\questionaire" #.csv added at end, leave empty if no output desired
outlierFactor=3 #factor of sd to define outliers in MR
block=c("main1","main2","main3","main4")#name of interesting block of data
questionaireDataCols=c("ID","Gender") #which questionaire columns shall be kept for statistical analysis

##read and write data
#read data
questionaireData=getQuestionaireData(verbose,folder)
MRData=getMRData(verbose,folder,block)
#modify data 
questionaireData=modifyQuestionaireData(questionaireData)
MRData=modifyMRData(verbose,MRData)

#calculate means from questionaire (and save to csv)
calculateMeansQuestionaire(verbose,questionaireData,questionaireOutFile,"")
#remove not analyzed questionaire data to protect participant identity
questionaireData=subset(questionaireData,select=questionaireDataCols)

#unify data
dataset=merge(MRData,questionaireData,by="ID")
#anonymise IDs to protect participant identity
dataset$ID=as.factor(dataset$ID)
levels(dataset$ID)=paste("id",sample.int(length(levels(dataset$ID))),sep="")
