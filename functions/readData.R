### Functions to read and modify data
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

#get questionnaireData
#verbose: detail of output
#folder: folder to search in for data
getQuestionnaireData=function(software,verbose,folder){
  if (verbose>1) {
    print("Reading questionnaire data from files ...")
  }
  if (software=="OSWeb"){
    questionnaireData=getDataOSWeb(verbose,folder,part="questionnaire")
    questionnaireData=subset(questionnaireData,select=c("ID","questionID","answer"))
    questionnaireData=reshape(questionnaireData, idvar = "ID", timevar = "questionID", direction = "wide")
    names(questionnaireData)=gsub("answer.","",names(questionnaireData))
    
  }else if (software=="OpenSesame"){
    questionnaireData=getQuestionnaireDataOpenSesame(verbose,folder, preText="", c("questionaire","questionnaire"),ending="csv")
  }else if (software=="jQuery"){
    questionnaireData=getQuestionnaireDataJQuery(verbose,folder, preText="",ending="csv")
  }
  if (verbose>1) {
    print(paste("Questionnaire data from",nrow(questionnaireData),"participants was read."))
  }
  return(questionnaireData)
}

#get mental rotation data
#verbose: detail of output
#folder: folder to search in for data
#block: name of block of interest
getMRData=function(software,verbose,folder,block="main"){
  if (verbose>1) {
    print(paste("Reading mental rotation data for block",block,"from files"))
  }
  if (software=="OSWeb"){
    MRData=getDataOSWeb(verbose,folder,part=block)
  }else if (software=="OpenSesame"){
    MRData=getDataOpenSesame(verbose,folder,part=block)
  }
  if (verbose>1) {
    print(paste("Mental rotation data from",length(unique(MRData$ID)),"participants was read. (",nrow(MRData),"trials in total)"))
  }
  return(MRData)
}

#modifies the questionnairedata, calculates some additional information
#questionnaireData: dataset
modifyQuestionnaireData=function(questionnaireData,toFirstChars,toNums,cleanWhiteSpaces) {
  if (verbose>1) {
    print("Doing calculations on questionnaire data ...")
  }
  #transform values to numeric, remove white spaces, unify gender
  questionnaireData=cleanData(questionnaireData,toFirstChars,toNums,cleanWhiteSpaces)
  #rename columns to different names
  #colnames(questionnaireData) = make.unique(names(questionnaireData))
  if (verbose>1) {
    print("Calculations on questionnaire data finished.")
  }
  return(questionnaireData)
}

#modifies the mental rotation data, calculates some additional information
#verbose: detail of output
#MRData: dataset
modifyMRData=function(verbose,MRData) {
  if (verbose>1) {
    print("Transforming mental rotation data ...")
  }
  #remove IDs which restarted experiment
  n_occur=data.frame(table(MRData[MRData$block=="main1" & MRData$itemNumber==1,c("ID")]))
  MRData=MRData[!(MRData$ID %in% n_occur$Var1[which(n_occur$Freq>1)]),]
  #convert array to numbers
  MRData$answers=stringToNumArray(MRData$answers)
  #save first answer for possible answer patterns
  MRData$firstAnswerSelected=unlist(lapply(MRData$answers, function(x) x[[1]]))
  #drop trials in which more than nStimuli/2 answers are selected (possible at end of blocks)
  MRData=MRData[lapply(MRData$answers, function(x) sum(unlist(x)))<=MRData$nStimuli/2,]
  #split arrays into rows
  #commented are unnecessary rows
  MRData=data.frame(angleStimulus=rep(MRData$angle,MRData$nStimuli),
                    model=rep(MRData$model,MRData$nStimuli),
                    nStimuli=rep(MRData$nStimuli,MRData$nStimuli),
                    #orientation=rep(MRData$orientation,MRData$nStimuli),
                    reactionTime=rep(MRData$reactionTime,MRData$nStimuli),
                    #startTimeOfTask=rep(MRData$startTimeOfTask,MRData$nStimuli),
                    #sumCorrect=rep(MRData$sumCorrect,MRData$nStimuli),
                    itemNumber=rep(MRData$itemNumber,MRData$nStimuli),
                    block=rep(MRData$block,MRData$nStimuli),
                    ID=rep(MRData$ID,MRData$nStimuli),
                    firstAnswerSelected=rep(MRData$firstAnswerSelected,MRData$nStimuli),
                    #arrays to unlist
                    angleAlternative=unlist(stringToNumArray(MRData$anglesArray)),
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
  MRData$stimulusCorrect=NULL
  #add missing trials
  #find blocks for each ID for which less than 24 answers are recorded (timeout)
  library(plyr)
  missingAnswers=ddply(MRData,.(ID,block,nStimuli,typeOfAlternatives),
                       imputeMissingAnswers)
  if(nrow(missingAnswers)>0){
    missingAnswers$type="miss"
    #combine datasets
    MRData=rbind.fill(MRData,missingAnswers)
  }
  #scale trial numbers to 0..1
  trialNumbers=ddply(MRData,.(ID,block),summarize,
                     minItem=min(itemNumber),
                     maxItem=max(itemNumber))
  MRData=merge(MRData,trialNumbers,by=c("ID","block"))
  MRData$trialNumber=(MRData$itemNumber-MRData$minItem)/(MRData$maxItem-MRData$minItem)
  MRData$minItem=NULL
  MRData$maxItem=NULL
  MRData$itemNumber=NULL
  #add overall number of correct answers
  overallHits=ddply(MRData,
                    .(ID), summarize,
                    overallHits=sum((type=="hit")))
  MRData=merge(MRData,overallHits,by="ID")
  return(MRData)
}

#impute missing answers by mean of deg and itemNumber and random choice of modelNumber from previous data of block
imputeMissingAnswers=function(df){
  rows=24-nrow(df)
  deg=rep(mean(df$deg),rows)
  itemNumber=rep(mean(df$itemNumber),rows)
  modelNumber=sample(df$modelNumber,rows,replace=T)
  return(data.frame(deg,itemNumber,modelNumber))
}


#reads data from files
#verbose: detail of output
#folder: folder to search in for files
#preText: Filter, only get files which start with preText
#part: Filter, only get parts of data in blocks whose name matches part
#ending: filetype of files
getDataOpenSesame=function(verbose, folder, preText="", part="main",ending="csv") {
  #get files in folger (Reaction Time Data)
  fileNames=getFileNames(folder,preText,ending)
  if (verbose>2) {
    print("list of files:")
    print(fileNames)
  }
  #initialize empty dataframe
  dat=data.frame()
  #loop through all files
  for (fileIndex in 1:length(fileNames)) {
    fileName=fileNames[fileIndex]
    #read data in file as table
    rawData=read.csv(paste(folder,fileName,sep=""),header=TRUE,fill=TRUE, sep=",")
    #choose only specified block
    dataset=rawData[rawData$aaBlock %in% part,]
    dataset$numberInBlock=ave(dataset[,1],                 # Create numbering variable
                              dataset$aaBlock,
                              FUN = seq_along)
    if (verbose>3) {
      print(paste("read", nrow(dataset), "values from file:",fileName,"\n"))
    }
    #add to dataset
    dat=rbind(dat,dataset)
  }
  #change names
  dat$block=dat$aaBlock
  dat$ID=dat$aaID
  dat$aaBlock=NULL
  dat$aaID=NULL
  return(dat)
}

#reads data from OSWeb/JATOS files
#verbose: detail of output
#folder: folder to search in for files
#preText: Filter, only get files which start with preText
#part: Filter, only get parts of data in blocks whose name matches part
#ending: filetype of files
getDataOSWeb=function(verbose, folder, preText="", part="main",ending="csv") {
  #get files in folger (Reaction Time Data)
  fileNames=getFileNames(folder,preText,ending)
  if (verbose>2) {
    print("list of files:")
    print(fileNames)
  }
  #initialize empty dataframe
  dat=data.frame()
  #loop through all files
  for (fileIndex in 1:length(fileNames)) {
    fileName=fileNames[fileIndex]
    #read data in file as table
    rawData=read.csv(paste(folder,fileName,sep=""),header=TRUE,fill=TRUE, sep=",")
    #choose only specified block
    dataset=rawData[rawData$aaBlock %in% part,]
    if (verbose>3) {
      print(paste("read", nrow(dataset), "values from file:",fileName,"\n"))
    }
    #add to dataset
    dat=rbind(dat,dataset)
  }
  #change names
  dat$block=dat$aaBlock
  dat$ID=dat$workerId #aaId for old version
  dat$aaBlock=NULL
  dat$workerId=NULL
  return(dat)
}

#modified 
getQuestionnaireDataJQuery=function(verbose, folder, preText="",ending="csv") {
  #get files in folger (Reaction Time Data)
  fileNames=getFileNames(folder,preText,ending)
  if (verbose>2) {
    print("list of files:")
    print(fileNames)
  }
  #initialize empty dataframe
  dat=data.frame()
  #loop through all files
  for (fileIndex in 1:length(fileNames)) {
    fileName=fileNames[fileIndex]
    #read data in file as table
    rawData=read.csv(paste(folder,fileName,sep=""),header=TRUE,fill=TRUE, sep=",")
    #choose only specified block
    dataset=rawData[rawData$aaBlock=="",]
    if (verbose>3) {
      print(paste("read", nrow(dataset), "values from file:",fileName,"\n"))
    }
    #add to dataset
    dat=rbind(dat,dataset)
  }
  #change names
  #dat$block="questionnaire"
  dat$ID=dat$workerId #aaId for old version
  dat$aaBlock=NULL
  dat$workerId=NULL
  return(dat)
}

#reads data from files
#verbose: detail of output
#folder: folder to search in for files
#preText: Filter, only get files which start with preText
#part: Filter, only get parts of data in blocks whose name matches part
#ending: filetype of files
getQuestionnaireDataOpenSesame=function(verbose, folder, preText="", part=c("questionaire","questionnaire"),ending="csv") {
  #get files in folder
  fileNames=getFileNames(folder,preText,ending)
  if (verbose>2) {
    print("list of files:\n")
    print(fileNames)
  }
  #initialize empty dataframe
  dat=data.frame()
  #loop through all files
  for (fileIndex in 1:length(fileNames)) {
    fileName=fileNames[fileIndex]
    #read data in file as table
    rawData=read.csv(paste(folder,fileName,sep=""),header=TRUE,fill=TRUE, sep=",")
    #choose only specified block
    dataset=rawData[rawData$aaBlock %in% part,]
    #add interesting data to vector 
    values=append(toChar(dataset[,3]),dataset$aaID[1])
    if (verbose>3) {
      print(paste("read values for file:",fileName,"\n"))
      print(values)
    }
    #add to dataset
    dat=rbind(dat,values,stringsAsFactors = FALSE)
    #set names according to questionIDs
    if (fileIndex==1) {
      names(dat)=append(toChar(dataset[,4]),"ID")
    }
  }
  return(dat)
}