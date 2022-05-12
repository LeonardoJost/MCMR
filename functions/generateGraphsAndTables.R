### generate graph and table output
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

#calculate means and mode for questionnaire data and save to csv
calculateMeansQuestionnaire=function(verbose,questionnaireData,questionnaireOutFile="",handednessGraphFile=""){
  #calculate means and modes by sex and save to csv
  questionnaireDataMeansBysex=data.frame(lapply(questionnaireData[which(questionnaireData$sex==levels(as.factor(questionnaireData$sex))[1]),],meanMode),stringsAsFactors = FALSE)
  for (sexNumber in 1:length(levels(as.factor(questionnaireData$sex))))
    questionnaireDataMeansBysex[sexNumber,]=lapply(questionnaireData[which(questionnaireData$sex==levels(as.factor(questionnaireData$sex))[sexNumber]),],meanMode)
  questionnaireDataMeansBysex$ID=levels(as.factor(questionnaireData$sex))
  #means overall
  questionnaireDataMeans=data.frame(lapply(questionnaireData,meanMode),stringsAsFactors = FALSE)
  questionnaireDataMeans$ID=NULL
  #save to csv
  if (questionnaireOutFile!="") {
    if(verbose>1){
      print(paste("Writing mean and mode data for questionnaires (by sex) to file",paste(questionnaireOutFile,"MeansBysex.csv", sep="")))
      print(paste("Writing mean and mode data for questionnaires to file",paste(questionnaireOutFile,"Means.csv", sep="")))
    }
    if(verbose>2){
      print("Means by sex:")
      print(questionnaireDataMeansBysex)
      print("Overall means:")
      print(questionnaireDataMeans)
    }
    write.table(questionnaireDataMeansBysex,file=paste(questionnaireOutFile,"MeansBysex.csv", sep=""),sep=";", col.names=NA)
    write.table(questionnaireDataMeans,file=paste(questionnaireOutFile,"Means.csv", sep=""),sep=";", col.names=NA)
    #write.table(questionnaireData,file=paste(questionnaireOutFile,".csv", sep=""),sep=";", col.names=NA)
  }
  if (handednessGraphFile!="") {
    if(verbose>1){
      print(paste("Writing handedness graph (by sex) to file",handednessGraphFile))
    }
    #plot handedness
    library(ggplot2)
    if(length(levels(as.factor(questionnaireData$sex)))>1)
      ggplot(questionnaireData,aes(hand)) + geom_histogram(binwidth=0.5,aes(fill=sex)) +xlab("Handedness") + ylab("Count") + theme_bw()
    else
      ggplot(questionnaireData,aes(hand)) + geom_histogram(binwidth=0.5) +xlab("Handedness") + ylab("Count") + theme_bw()
    
    ggsave(handednessGraphFile)
  }
}

#combine multiple images into one
combineImages=function(imagesList,rows,columns,outputFile,outputWidth=1028){
  library(magick)
  initImage=image_read(imagesList[1])
  #each row contains columns images
  imageRows=rep(initImage,columns)
  imageColumns=rep(initImage,rows)
  #combine images in rows and columns
  counter=1
  for(i in 1:rows){
    for(j in 1:columns){
      imageRows[j]=image_read(imagesList[counter])
      counter=counter+1
    }
    #append horizontally
    imageRow=image_append(imageRows)
    imageColumns[i]=imageRow
  }
  #append vertically
  image=image_append(imageColumns,stack=TRUE)
  #save
  image_write(image, path = outputFile)
  gc()
}
