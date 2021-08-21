### create custom images using MRlibrary
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

##LOAD THESE FROM MRLIBRARY
#load all supporting functions
source("functions/helpers.R")
source("functions/mainLoops.R")
source("functions/imageManipulation.R")
source("functions/petersBattistaModels.R")
source("functions/customModels.R")

#set parameters
source("functions/parameters.R")
#change some parameters
baseFolder="figs\\"
centering="optical"
backgroundColor='black'
modelNumber=2
pixelDiameter=20
backgroundImage='data\\background.png'
fileFormat="tiff"
imageSize=1024
rotationAngleXBase=-15*pi/180
rotationAngleYBase=0*pi/180
rotationAngleZBase=15*pi/180
usePetersBattistaModels=TRUE
colors=c('white')

#load blank image
backgroundFromFile=image_read(backgroundImage)
background=image_scale(backgroundFromFile,imageSize)

### comapre designs
#positions:
pos=matrix(c(
  #sm test
  c(-2,0,3),
  c(0,0,3),
  #vk test
  c(-2,0,2),
  c(0,0,2),
  c(1,0,2),
  c(2,0,2),
  c(3,0,2),
  #jj test
  c(-2,0,1),
  c(0,0,1),
  c(-1,0,0),
  #jj horizontal
  c(-2,0,-1),
  c(0,0,-1),
  c(1,0,-1),
  ##jj rotated/vk2
  c(-2,0,-2),
  c(0,0,-2),
  c(1,0,-2)),
  nrow=3)
#multiply
pos=pos*7
#shift
pos=pos+c(0,0,0)
pos=t(pos)
angles=t(matrix(c(c(0,0,0),c(0,45,0),
                  c(0,0,0),c(0,45,0),c(0,-45,0),c(0,45,0),c(0,-45,0),
                  c(0,0,0),c(0,0,0),c(0,45,0),
                  c(0,45,0),c(0,0,0),c(0,0,0),
                  c(0,0,0),c(0,45,0),c(0,45,0)),
                nrow=3))
orientations=c("a","a",
               "a","a","a","b","b",
               "a","b","a",
               "a","a","b",
               "a","a","b")
img=showImages()
#show figure
#print(img)
#trim and add border and separating lines
img=image_draw(image_border(image_trim(img),"black","20x20"))
abline(h=150,col="white")
abline(h=290,col="white")
abline(h=570,col="white")
abline(h=710,col="white")
#save image
image_write(img, path = paste(baseFolder,"DesignComparison",".", fileFormat,sep=""), format = fileFormat)
dev.off()
gc()

### compare sm and vk test
#positions:
pos=matrix(c(
  #sm test
  c(-0.5,0,3),
  c(1.5,0,3),
  #vk test
  c(-2,0,2),
  c(0,0,2),
  c(1,0,2),
  c(2,0,2),
  c(3,0,2)),
  nrow=3)
#multiply
pos=pos*7
#shift
pos=pos+c(0,0,0)
pos=t(pos)
angles=t(matrix(c(c(0,0,0),c(0,45,0),
                  c(0,0,0),c(0,45,0),c(0,-45,0),c(0,45,0),c(0,-45,0)),
                nrow=3))
orientations=c("a","a",
               "a","a","a","b","b")
img=showImages()
#show figure
#print(img)
#trim and add border and separating lines
img=image_draw(image_border(image_trim(img),"black","20x20"))
abline(h=150,col="white")
#save image
image_write(img, path = paste(baseFolder,"SmVkComparison",".", fileFormat,sep=""), format = fileFormat)
dev.off()
gc()

###general design
#positions:
pos=matrix(c(
  #targets
  c(-2,0,3),
  c(-2,0,2),
  c(-2,0,1),
  c(-2,0,0),
  c(-2,0,-1),
  c(-2,0,-2),
  c(-2,0,-3),
  #alternatives
  c(0,0,3),
  c(0,0,2),
  c(0,0,1),
  c(0,0,0),
  c(0,0,-1),
  c(0,0,-2),
  c(0,0,-3),
  #second row of alternatives
  c(1,0,3),
  c(1,0,2),
  c(1,0,1),
  c(1,0,0),
  c(1,0,-1),
  c(1,0,-2),
  c(1,0,-3)),
  nrow=3)
#multiply
pos=pos*7
#shift
pos=pos+c(0,0,0)
pos=t(pos)
angles=t(matrix(c(c(0,0,0),c(0,0,0),c(0,0,45),c(45,0,0),c(0,-45,0),c(-45,0,0),c(0,0,0),
                  c(0,45,0),c(0,-45,0),c(0,0,-45),c(-45,0,0),c(0,45,0),c(45,0,0),c(0,0,0),
                  c(0,45,0),c(0,-45,0),c(0,0,45),c(45,0,0),c(0,-45,0),c(-45,0,0),c(0,0,0)),
                nrow=3))
orientations=c("a","b","a","a","a","a","a",
               "a","a","a","a","a","a","a",
               "b","b","a","a","b","b","a")
multipleModelsNumber=c(2,2,4,5,6,7,8,
                       2,2,4,4,6,7,10,
                       2,2,5,5,6,7,11)
img=showImages(multipleModels=TRUE)
#trim and add border
img=image_draw(img)
#add text
for(i in 1:nrow(pos)){
  x=pos[i,1]*pixelDiameter+imageSize/2+pixelDiameter*3
  y=-pos[i,3]*pixelDiameter+imageSize/2-pixelDiameter*3
  text(x, y, paste(multipleModelsNumber[i],orientations[i]),col="white",cex=2)
}
#trim and add border
img=image_border(image_trim(img),"black","20x20")
#show figure
#print(img)
#save image
image_write(img, path = paste(baseFolder,"GeneralDesign",".", fileFormat,sep=""), format = fileFormat)
dev.off()
gc()

###trials for experiment (use rotation order z,x,y)
#positions:
pos=matrix(c(
  #2 paired
  c(-1.5,0,3),
  c(0,0,3),
  c(1,0,3),
  #4 unpaired
  c(-1.5,0,2),
  c(0,0,2),
  c(1,0,2),
  c(2,0,2),
  c(3,0,2),
  #8 paired
  c(-1.5,0,0.5),
  c(0,0,1),
  c(1,0,1),
  c(2,0,1),
  c(3,0,1),
  c(0,0,0),
  c(1,0,0),
  c(2,0,0),
  c(3,0,0)),
  nrow=3)
#multiply
pos=pos*7
#shift
pos=pos+c(0,0,0)
pos=t(pos)
angles=t(matrix(c(c(0,0,0),c(0,0,45),c(0,0,-45),
                  c(0,0,45),c(0,0,0),c(0,0,-30),c(0,0,90),c(0,0,-120),
                  c(0,0,0),c(0,0,45),c(0,0,105),c(0,0,-210),c(0,0,-165),
                  c(0,0,90),c(0,0,-75),c(0,0,180),c(0,0,315)),
                nrow=3))
orientations=c("a","a","b",
               "a","a","b","a","b",
               "a","a","b","b","b",
               "a","b","a","a")
img=showImages()
#show figure
#print(img)
#trim and add border and separating lines
img=image_draw(image_border(image_trim(img),"black","20x20"))
abline(h=150,col="white")
abline(h=290,col="white")
#save image
image_write(img, path = paste(baseFolder,"Trials",".", fileFormat,sep=""), format = fileFormat)
dev.off()
gc()
