### get significance of fixed effects for subgroups of STEM*Experience
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

#load packages
using MixedModels
using Random
using DataFrames
using DataFramesMeta
using Gadfly
using CSV

#read data
dataset=CSV.read("dataset\\dataset.csv", DataFrame)
#inspect data
show(first(dataset,6))
show(names(dataset))
#show(IOContext(stdout, :limit=>false),dataset[115:1000,[:type, :deg]])
show(eltype.(eachcol(dataset)))
#remove outliers
dataset=dataset[.!dataset.outlier,:]
dataset=dataset[dataset[!,:sex].!="NA",:]
#convert types to numeric
for i in [:sexContrasts]
  dataset[!,i]=tryparse.(Int,dataset[!,i])
end
#add block as numeric variable
dataset.blockNumeric=tryparse.(Int,chop.(dataset.block,head=4,tail=0))
dataset.nStimuliFactor=string.(dataset.nStimuli)
#scaling deg and block to 0..1
dataset.deg=dataset.deg.-minimum(dataset.deg)
dataset.blockNumeric=dataset.blockNumeric.-minimum(dataset.blockNumeric)
dataset.deg=dataset.deg./maximum(dataset.deg)
dataset.blockNumeric=dataset.blockNumeric./maximum(dataset.blockNumeric)

dataset.responseCorrect=dataset.type.=="hit"

datasetNoStem=dataset[dataset[!,:STEM].=="nonSTEM",:]
datasetNoExpNoSTEM=datasetNoStem[datasetNoStem[!,:Experience].=="no",:]
datasetExpNoSTEM=datasetNoStem[datasetNoStem[!,:Experience].=="yes",:]

#check for overall four way interaction in nonStem dataset
modelFormula=@formula(responseCorrect~Experience*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed noStem1=fit(MixedModel,modelFormula,datasetNoStem,Binomial())
#reduced
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliFactor*typeContrasts+
              Experience*nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed noStem11=fit(MixedModel,modelFormula,datasetNoStem,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(noStem1,noStem11))

#check for random slopes (use random slopes of full dataset for start)
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpNoStem0=fit(MixedModel,modelFormula,datasetNoExpNoSTEM,Binomial())
#reduce random slopes
#nStimuliFactor
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (typeContrasts+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpNoStem01=fit(MixedModel,modelFormula,datasetNoExpNoSTEM,Binomial())
#typeContrasts
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpNoStem02=fit(MixedModel,modelFormula,datasetNoExpNoSTEM,Binomial())
#blockNumeric
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpNoStem03=fit(MixedModel,modelFormula,datasetNoExpNoSTEM,Binomial())
#trialNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric|ID)+
              (1|modelNumber))
@elapsed noExpNoStem04=fit(MixedModel,modelFormula,datasetNoExpNoSTEM,Binomial())
#(1|modelNumber)
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID))
@elapsed noExpNoStem05=fit(MixedModel,modelFormula,datasetNoExpNoSTEM,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(noExpNoStem0,noExpNoStem01))
show(MixedModels.likelihoodratiotest(noExpNoStem0,noExpNoStem02))
show(MixedModels.likelihoodratiotest(noExpNoStem0,noExpNoStem03))
show(MixedModels.likelihoodratiotest(noExpNoStem0,noExpNoStem04))
show(MixedModels.likelihoodratiotest(noExpNoStem0,noExpNoStem05))
#all significant
#check order
modelFormula=@formula(responseCorrect~nStimuliFactor*typeContrasts*sexContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpNoStem06=fit(MixedModel,modelFormula,datasetNoExpNoSTEM,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(noExpNoStem0,noExpNoStem06))
#ok

#check for overall main effects of interest
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpNoStem1=fit(MixedModel,modelFormula,datasetNoExpNoSTEM,Binomial())
#sexContrasts*nStimuliFactor*typeContrasts
modelFormula=@formula(responseCorrect~nStimuliFactor*typeContrasts+
              sexContrasts*typeContrasts+
              sexContrasts*nStimuliFactor+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpNoStem11=fit(MixedModel,modelFormula,datasetNoExpNoSTEM,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(noExpNoStem1,noExpNoStem11))
#n.s.
noExpNoStem2=noExpNoStem11
#reduce model
#nStimuliFactor*typeContrasts
modelFormula=@formula(responseCorrect~
              sexContrasts*typeContrasts+
              sexContrasts*nStimuliFactor+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpNoStem21=fit(MixedModel,modelFormula,datasetNoExpNoSTEM,Binomial())
#sexContrasts*typeContrasts
modelFormula=@formula(responseCorrect~nStimuliFactor*typeContrasts+
              sexContrasts*nStimuliFactor+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpNoStem22=fit(MixedModel,modelFormula,datasetNoExpNoSTEM,Binomial())
#sexContrasts*nStimuliFactor
modelFormula=@formula(responseCorrect~nStimuliFactor*typeContrasts+
              sexContrasts*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpNoStem23=fit(MixedModel,modelFormula,datasetNoExpNoSTEM,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(noExpNoStem2,noExpNoStem21))
show(MixedModels.likelihoodratiotest(noExpNoStem2,noExpNoStem22))
show(MixedModels.likelihoodratiotest(noExpNoStem2,noExpNoStem23))
#largest p for noExpNoStem23
noExpNoStem3=noExpNoStem23
#reduce model
#nStimuliFactor*typeContrasts
modelFormula=@formula(responseCorrect~nStimuliFactor+
              sexContrasts*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpNoStem31=fit(MixedModel,modelFormula,datasetNoExpNoSTEM,Binomial())
#sexContrasts*typeContrasts
modelFormula=@formula(responseCorrect~nStimuliFactor*typeContrasts+
              sexContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpNoStem32=fit(MixedModel,modelFormula,datasetNoExpNoSTEM,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(noExpNoStem3,noExpNoStem31))
show(MixedModels.likelihoodratiotest(noExpNoStem3,noExpNoStem32))
