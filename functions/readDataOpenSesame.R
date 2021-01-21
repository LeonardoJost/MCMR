### Read data from OpenSesame
#     Copyright (C) 2019  Leonardo Jost
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

#get all questionaire data from OpenSesame
getOpenSesameQuestionaireData=function(verbose,folder, preText="", part="questionaire",ending="csv") {
  ##get questionaire data
  questionaireData=getQuestionaireDataOpenSesame(verbose,folder, preText, part,ending)
  return(questionaireData)
}

#get all MR data from OpenSesame
getOpenSesameMRData=function(verbose,folder, preText="", part="main",ending="csv") {
  #get MR data
  MRData=getDataOpenSesame(verbose,folder, preText, part,ending)
  return(MRData)
}

#modify questionaire Data from OpenSesame
modifyOpenSesameQuestionaireData=function(questionaireData) {
  #transform values to numeric, remove white spaces, unify gender
  questionaireData=cleanData(questionaireData,c("Gender"),c("Age"),c())
  #rename columns to different names
  #colnames(questionaireData) = make.unique(names(questionaireData))
  return(questionaireData)
}

#reads data from files
#verbose: detail of output
#folder: folder to search in for files
#preText: Filter, only get files which start with preText
#part: Filter, only get part of data in block, that contains part in the name
#ending: filetype of files
getDataOpenSesame=function(verbose, folder, preText="", part="main",ending="csv") {
  #get files in folger (Reaction Time Data)
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

#reads data from files
#verbose: detail of output
#folder: folder to search in for files
#preText: Filter, only get files which start with preText
#part: Filter, only get part of data in block, that contains part in the name
#ending: filetype of files
getQuestionaireDataOpenSesame=function(verbose, folder, preText="", part="questionaire",ending="csv") {
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
    dataset=rawData[grepl(part,rawData$aaBlock),]
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
