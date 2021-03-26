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
calculateMeansQuestionnaire=function(verbose,questionnaireData,questionnaireOutFile,handednessGraphFile){
  #calculate means and modes by gender and save to csv
  questionnaireDataMeansByGender=data.frame(lapply(questionnaireData[which(questionnaireData$Gender==levels(as.factor(questionnaireData$Gender))[1]),],meanMode),stringsAsFactors = FALSE)
  for (genderNumber in 1:length(levels(as.factor(questionnaireData$Gender))))
    questionnaireDataMeansByGender[genderNumber,]=lapply(questionnaireData[which(questionnaireData$Gender==levels(as.factor(questionnaireData$Gender))[genderNumber]),],meanMode)
  questionnaireDataMeansByGender$ID=levels(as.factor(questionnaireData$Gender))
  #means overall
  questionnaireDataMeans=data.frame(lapply(questionnaireData,meanMode),stringsAsFactors = FALSE)
  
  #save to csv
  if (questionnaireOutFile!="") {
    if(verbose>1){
      print(paste("Writing mean and mode data for questionnaires (by gender) to file",paste(questionnaireOutFile,"MeansByGender.csv", sep="")))
      print(paste("Writing mean and mode data for questionnaires to file",paste(questionnaireOutFile,"Means.csv", sep="")))
    }
    if(verbose>2){
      print("Means by gender:")
      print(questionnaireDataMeansByGender)
      print("Overall means:")
      print(subset(questionnaireDataMeans,select=-c(ID)))
    }
    write.table(questionnaireDataMeansByGender,file=paste(questionnaireOutFile,"MeansByGender.csv", sep=""),sep=";", col.names=NA)
    write.table(questionnaireDataMeans,file=paste(questionnaireOutFile,"Means.csv", sep=""),sep=";", col.names=NA)
    #write.table(questionnaireData,file=paste(questionnaireOutFile,".csv", sep=""),sep=";", col.names=NA)
  }
  if (handednessGraphFile!="") {
    if(verbose>1){
      print(paste("Writing handedness graph (by gender) to file",handednessGraphFile))
    }
    #plot handedness
    library(ggplot2)
    if(length(levels(as.factor(questionnaireData$Gender)))>1)
      ggplot(questionnaireData,aes(hand)) + geom_histogram(binwidth=0.5,aes(fill=Gender)) +xlab("Handedness") + ylab("Count") + theme_bw()
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
