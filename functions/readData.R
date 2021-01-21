### Read mental rotation data
#     Copyright (C) 2020  Leonardo Jost
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
source("functions/readDataOpenSesame.R", encoding="utf-8")
source("functions/calculateData.R", encoding="utf-8")

#get questionaireData
#verbose: detail of output
#folder: folder to search in for data
getQuestionaireData=function(verbose,folder){
  if (verbose>1) {
    print("Reading questionaire data from files ...")
  }
  questionaireData=getOpenSesameQuestionaireData(verbose,folder, preText="", part="questionaire",ending="csv")
  if (verbose>1) {
    print(paste("Questionaire data from",nrow(questionaireData),"participants was read."))
  }
  return(questionaireData)
}

#get mental rotation data
#verbose: detail of output
#folder: folder to search in for data
#block: name of block of interest
getMRData=function(verbose,folder,block="main"){
  if (verbose>1) {
    print(paste("Reading mental rotation data for block",block,"from files"))
  }
  MRData=getOpenSesameMRData(verbose,folder,part=block)
  if (verbose>1) {
    print(paste("Mental rotation data from",length(unique(MRData$ID)),"participants was read. (",nrow(MRData),"trials in total)"))
  }
  return(MRData)
}

#modifies the questionairedata, calculates some additional information
#questionaireData: dataset
modifyQuestionaireData=function(questionaireData) {
  if (verbose>1) {
    print("Doing calculations on questionaire data ...")
  }
  questionaireData=modifyOpenSesameQuestionaireData(questionaireData)
  if (verbose>1) {
    print("Calculations on questionaire data finished.")
  }
  return(questionaireData)
}

#modifies the mental rotation data, calculates some additional information
#verbose: detail of output
#MRData: dataset
modifyMRData=function(verbose,MRData) {
  if (verbose>1) {
    print("Transforming mental rotation data ...")
  }
  #convert array to numbers
  MRData$anglesArray=stringToNumArray(MRData$anglesArray)
  MRData$answers=stringToNumArray(MRData$answers)
  #drop trials in which more than nStimuli/2 answers are selected (possible at end of blocks)
  MRData=MRData[lapply(MRData$answers, function(x) sum(unlist(x)))<=MRData$nStimuli/2,]
  #trim angle arrays to number of stimuli (could this be vectorized?)
  for(i in 1:nrow(MRData)){
    MRData$anglesArray[i]=list(unlist(MRData$anglesArray[i])[1:MRData$nStimuli[i]])
  }
  #split arrays into rows
  #commented are unnecessary rows
  MRData=data.frame(angleStimulus=rep(MRData$angle,MRData$nStimuli),
                     model=rep(MRData$model,MRData$nStimuli),
                     nStimuli=rep(MRData$nStimuli,MRData$nStimuli),
                     #orientation=rep(MRData$orientation,MRData$nStimuli),
                     reactionTime=rep(MRData$reactionTime,MRData$nStimuli),
                     #startTimeOfTask=rep(MRData$startTimeOfTask,MRData$nStimuli),
                     #sumCorrect=rep(MRData$sumCorrect,MRData$nStimuli),
                     numberInBlock=rep(MRData$numberInBlock,MRData$nStimuli),
                     block=rep(MRData$block,MRData$nStimuli),
                     ID=rep(MRData$ID,MRData$nStimuli),
                     #arrays to unlist
                     angleAlternative=unlist(MRData$anglesArray),
                     #answer=unlist(stringToNumArray(MRData$answers)),
                     answerCorrect=unlist(stringToNumArray(MRData$correctAnswers)),
                     stimulusCorrect=unlist(stringToNumArray(MRData$correctStimuli)))
  #convert to numeric
  MRData$angleStimulus=toNumeric(MRData$angleStimulus)
  MRData$angleAlternative=toNumeric(MRData$angleAlternative)
  MRData$typeOfAlternatives=ifelse(MRData$angleStimulus %in% c(0,90,180,270),"mixed","paired")
  #calculate angle difference
  MRData$deg=abs(MRData$angleStimulus-MRData$angleAlternative)
  #modify angles to 360-angle if angle>180
  MRData$deg=ifelse(MRData$deg>180,360-MRData$deg,MRData$deg)
  #get type as string
  MRData$type=ifelse(MRData$answerCorrect==1,"hit","incorrect")
  #modelnumber as string
  MRData$modelNumber=paste("m",stringToNum(MRData$model),sep="")
  #drop unneeded columns
  MRData$model=NULL
  MRData$answerCorrect=NULL
  MRData$angleAlternative=NULL
  MRData$angleStimulus=NULL
  #drop mirrored stimuli
  MRData=MRData[MRData$stimulusCorrect==1,]
  return(MRData)
}