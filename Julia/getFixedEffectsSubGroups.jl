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
using Statistics

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
#sclaing, centering function
function centerNormalize(vector)
  vector=vector.-minimum(vector)
  vector=vector./maximum(vector)
  vector=vector.-mean(vector)
end

#scaling numerical values to 0..1 (centered)
dataset.deg=centerNormalize(dataset.deg)
dataset.blockNumeric=centerNormalize(dataset.blockNumeric)
dataset.trialNumber=centerNormalize(dataset.trialNumber)
dataset.nStimuli=centerNormalize(dataset.nStimuli)
#center uneven STEM distributions (get average effects instead of main effects)
dataset.STEMContrasts1=centerNormalize(dataset.STEMContrasts1)
dataset.STEMContrasts2=centerNormalize(dataset.STEMContrasts2)


dataset.responseCorrect=dataset.type.=="hit"

datasetNoStem=dataset[dataset[!,:STEM].=="nonSTEM",:]
datasetNoExp=dataset[dataset[!,:Experience].=="no",:]
datasetNoExpNoSTEM=datasetNoStem[datasetNoStem[!,:Experience].=="no",:]
datasetExpNoSTEM=datasetNoStem[datasetNoStem[!,:Experience].=="yes",:]

#subsets with only one type of alternatives
datasetNoExpTypePaired=datasetNoExp[datasetNoExp[!,:typeOfAlternatives].=="paired",:]
datasetNoExpTypeMixed=datasetNoExp[datasetNoExp[!,:typeOfAlternatives].=="mixed",:]
datasetNoExpNoSTEMTypePaired=datasetNoExpNoSTEM[datasetNoExpNoSTEM[!,:typeOfAlternatives].=="paired",:]
datasetNoExpNoSTEMTypeMixed=datasetNoExpNoSTEM[datasetNoExpNoSTEM[!,:typeOfAlternatives].=="mixed",:]

#check for overall four way interaction in nonStem dataset
modelFormula=@formula(responseCorrect~STEMContrasts1*sexContrasts*nStimuli*typeContrasts+
              STEMContrasts2*sexContrasts*nStimuli*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExp1=fit(MixedModel,modelFormula,datasetNoExp,Binomial())
#reduced
modelFormula=@formula(responseCorrect~sexContrasts*nStimuli*typeContrasts+
              STEMContrasts1*nStimuli*typeContrasts+
              STEMContrasts1*sexContrasts*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuli+
              STEMContrasts2*nStimuli*typeContrasts+
              STEMContrasts2*sexContrasts*typeContrasts+
              STEMContrasts2*sexContrasts*nStimuli+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExp11=fit(MixedModel,modelFormula,datasetNoExp,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(noExp1,noExp11))
#significant
#main effects
#sexContrasts & nStimuli & typeContrasts
modelFormula=@formula(responseCorrect ~ 1 + STEMContrasts1 + sexContrasts + nStimuli + typeContrasts + STEMContrasts2 + blockNumeric + deg + trialNumber +
STEMContrasts1 & sexContrasts + STEMContrasts1 & nStimuli + sexContrasts & nStimuli + STEMContrasts1 & typeContrasts + sexContrasts & typeContrasts + nStimuli & typeContrasts + STEMContrasts2 & sexContrasts + STEMContrasts2 & nStimuli + STEMContrasts2 & typeContrasts +
STEMContrasts1 & sexContrasts & nStimuli + STEMContrasts1 & sexContrasts & typeContrasts + STEMContrasts1 & nStimuli & typeContrasts +
#sexContrasts & nStimuli & typeContrasts +
STEMContrasts2 & sexContrasts & nStimuli + STEMContrasts2 & sexContrasts & typeContrasts + STEMContrasts2 & nStimuli & typeContrasts +
STEMContrasts1 & sexContrasts & nStimuli & typeContrasts + STEMContrasts2 & sexContrasts & nStimuli & typeContrasts +
(1 + nStimuliFactor + typeContrasts + blockNumeric + trialNumber | ID) + (1 | modelNumber)
)
@elapsed noExp12=fit(MixedModel,modelFormula,datasetNoExp,Binomial())
#sexContrasts & nStimuli
modelFormula=@formula(responseCorrect ~ 1 + STEMContrasts1 + sexContrasts + nStimuli + typeContrasts + STEMContrasts2 + blockNumeric + deg + trialNumber +
STEMContrasts1 & sexContrasts + STEMContrasts1 & nStimuli +
#sexContrasts & nStimuli +
STEMContrasts1 & typeContrasts + sexContrasts & typeContrasts + nStimuli & typeContrasts + STEMContrasts2 & sexContrasts + STEMContrasts2 & nStimuli + STEMContrasts2 & typeContrasts +
STEMContrasts1 & sexContrasts & nStimuli + STEMContrasts1 & sexContrasts & typeContrasts + STEMContrasts1 & nStimuli & typeContrasts + sexContrasts & nStimuli & typeContrasts + STEMContrasts2 & sexContrasts & nStimuli + STEMContrasts2 & sexContrasts & typeContrasts + STEMContrasts2 & nStimuli & typeContrasts +
STEMContrasts1 & sexContrasts & nStimuli & typeContrasts + STEMContrasts2 & sexContrasts & nStimuli & typeContrasts +
(1 + nStimuliFactor + typeContrasts + blockNumeric + trialNumber | ID) + (1 | modelNumber)
)
@elapsed noExp13=fit(MixedModel,modelFormula,datasetNoExp,Binomial())
#sexContrasts & typeContrasts
modelFormula=@formula(responseCorrect ~ 1 + STEMContrasts1 + sexContrasts + nStimuli + typeContrasts + STEMContrasts2 + blockNumeric + deg + trialNumber +
STEMContrasts1 & sexContrasts + STEMContrasts1 & nStimuli + sexContrasts & nStimuli + STEMContrasts1 & typeContrasts +
#sexContrasts & typeContrasts +
nStimuli & typeContrasts + STEMContrasts2 & sexContrasts + STEMContrasts2 & nStimuli + STEMContrasts2 & typeContrasts +
STEMContrasts1 & sexContrasts & nStimuli + STEMContrasts1 & sexContrasts & typeContrasts + STEMContrasts1 & nStimuli & typeContrasts + sexContrasts & nStimuli & typeContrasts + STEMContrasts2 & sexContrasts & nStimuli + STEMContrasts2 & sexContrasts & typeContrasts + STEMContrasts2 & nStimuli & typeContrasts +
STEMContrasts1 & sexContrasts & nStimuli & typeContrasts + STEMContrasts2 & sexContrasts & nStimuli & typeContrasts +
(1 + nStimuliFactor + typeContrasts + blockNumeric + trialNumber | ID) + (1 | modelNumber)
)
@elapsed noExp14=fit(MixedModel,modelFormula,datasetNoExp,Binomial())
#nStimuli & typeContrasts
modelFormula=@formula(responseCorrect ~ 1 + STEMContrasts1 + sexContrasts + nStimuli + typeContrasts + STEMContrasts2 + blockNumeric + deg + trialNumber +
STEMContrasts1 & sexContrasts + STEMContrasts1 & nStimuli + sexContrasts & nStimuli + STEMContrasts1 & typeContrasts + sexContrasts & typeContrasts +
#nStimuli & typeContrasts +
STEMContrasts2 & sexContrasts + STEMContrasts2 & nStimuli + STEMContrasts2 & typeContrasts +
STEMContrasts1 & sexContrasts & nStimuli + STEMContrasts1 & sexContrasts & typeContrasts + STEMContrasts1 & nStimuli & typeContrasts + sexContrasts & nStimuli & typeContrasts + STEMContrasts2 & sexContrasts & nStimuli + STEMContrasts2 & sexContrasts & typeContrasts + STEMContrasts2 & nStimuli & typeContrasts +
STEMContrasts1 & sexContrasts & nStimuli & typeContrasts + STEMContrasts2 & sexContrasts & nStimuli & typeContrasts +
(1 + nStimuliFactor + typeContrasts + blockNumeric + trialNumber | ID) + (1 | modelNumber)
)
@elapsed noExp15=fit(MixedModel,modelFormula,datasetNoExp,Binomial())
#sexContrasts
modelFormula=@formula(responseCorrect ~ 1 + STEMContrasts1 +  nStimuli + typeContrasts + STEMContrasts2 + blockNumeric + deg + trialNumber +
STEMContrasts1 & sexContrasts + STEMContrasts1 & nStimuli + sexContrasts & nStimuli + STEMContrasts1 & typeContrasts + sexContrasts & typeContrasts + nStimuli & typeContrasts + STEMContrasts2 & sexContrasts + STEMContrasts2 & nStimuli + STEMContrasts2 & typeContrasts +
STEMContrasts1 & sexContrasts & nStimuli + STEMContrasts1 & sexContrasts & typeContrasts + STEMContrasts1 & nStimuli & typeContrasts + sexContrasts & nStimuli & typeContrasts + STEMContrasts2 & sexContrasts & nStimuli + STEMContrasts2 & sexContrasts & typeContrasts + STEMContrasts2 & nStimuli & typeContrasts +
STEMContrasts1 & sexContrasts & nStimuli & typeContrasts + STEMContrasts2 & sexContrasts & nStimuli & typeContrasts +
(1 + nStimuliFactor + typeContrasts + blockNumeric + trialNumber | ID) + (1 | modelNumber)
)
@elapsed noExp16=fit(MixedModel,modelFormula,datasetNoExp,Binomial())
#nStimuli
modelFormula=@formula(responseCorrect ~ 1 + STEMContrasts1 + sexContrasts +  typeContrasts + STEMContrasts2 + blockNumeric + deg + trialNumber +
STEMContrasts1 & sexContrasts + STEMContrasts1 & nStimuli + sexContrasts & nStimuli + STEMContrasts1 & typeContrasts + sexContrasts & typeContrasts + nStimuli & typeContrasts + STEMContrasts2 & sexContrasts + STEMContrasts2 & nStimuli + STEMContrasts2 & typeContrasts +
STEMContrasts1 & sexContrasts & nStimuli + STEMContrasts1 & sexContrasts & typeContrasts + STEMContrasts1 & nStimuli & typeContrasts + sexContrasts & nStimuli & typeContrasts + STEMContrasts2 & sexContrasts & nStimuli + STEMContrasts2 & sexContrasts & typeContrasts + STEMContrasts2 & nStimuli & typeContrasts +
STEMContrasts1 & sexContrasts & nStimuli & typeContrasts + STEMContrasts2 & sexContrasts & nStimuli & typeContrasts +
(1 + nStimuliFactor + typeContrasts + blockNumeric + trialNumber | ID) + (1 | modelNumber)
)
@elapsed noExp17=fit(MixedModel,modelFormula,datasetNoExp,Binomial())
#typeContrasts
modelFormula=@formula(responseCorrect ~ 1 + STEMContrasts1 + sexContrasts + nStimuli +  STEMContrasts2 + blockNumeric + deg + trialNumber +
STEMContrasts1 & sexContrasts + STEMContrasts1 & nStimuli + sexContrasts & nStimuli + STEMContrasts1 & typeContrasts + sexContrasts & typeContrasts + nStimuli & typeContrasts + STEMContrasts2 & sexContrasts + STEMContrasts2 & nStimuli + STEMContrasts2 & typeContrasts +
STEMContrasts1 & sexContrasts & nStimuli + STEMContrasts1 & sexContrasts & typeContrasts + STEMContrasts1 & nStimuli & typeContrasts + sexContrasts & nStimuli & typeContrasts + STEMContrasts2 & sexContrasts & nStimuli + STEMContrasts2 & sexContrasts & typeContrasts + STEMContrasts2 & nStimuli & typeContrasts +
STEMContrasts1 & sexContrasts & nStimuli & typeContrasts + STEMContrasts2 & sexContrasts & nStimuli & typeContrasts +
(1 + nStimuliFactor + typeContrasts + blockNumeric + trialNumber | ID) + (1 | modelNumber)
)
@elapsed noExp18=fit(MixedModel,modelFormula,datasetNoExp,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(noExp1,noExp12))
show(MixedModels.likelihoodratiotest(noExp1,noExp13))
show(MixedModels.likelihoodratiotest(noExp1,noExp14))
show(MixedModels.likelihoodratiotest(noExp1,noExp15))
show(MixedModels.likelihoodratiotest(noExp1,noExp16))
show(MixedModels.likelihoodratiotest(noExp1,noExp17))
show(MixedModels.likelihoodratiotest(noExp1,noExp18))

#check for overall main effects of interest in NoExpNoStem
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
#nStimuli as numeric
modelFormula=@formula(responseCorrect~sexContrasts*nStimuli*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpNoStem12=fit(MixedModel,modelFormula,datasetNoExpNoSTEM,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(noExpNoStem1,noExpNoStem11))
show(MixedModels.likelihoodratiotest(noExpNoStem1,noExpNoStem12))
#largest p for 12
noExpNoStem2=noExpNoStem12
#reduce model
modelFormula=@formula(responseCorrect~nStimuli*typeContrasts+
              sexContrasts*typeContrasts+
              sexContrasts*nStimuli+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpNoStem21=fit(MixedModel,modelFormula,datasetNoExpNoSTEM,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(noExpNoStem2,noExpNoStem21))
#n.s.
noExpNoStem3=noExpNoStem21
#reduce model
#nStimuli*typeContrasts
modelFormula=@formula(responseCorrect~
              sexContrasts*typeContrasts+
              sexContrasts*nStimuli+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpNoStem31=fit(MixedModel,modelFormula,datasetNoExpNoSTEM,Binomial())
#sexContrasts*typeContrasts
modelFormula=@formula(responseCorrect~nStimuli*typeContrasts+
              sexContrasts*nStimuli+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpNoStem32=fit(MixedModel,modelFormula,datasetNoExpNoSTEM,Binomial())
#sexContrasts*nStimuli
modelFormula=@formula(responseCorrect~nStimuli*typeContrasts+
              sexContrasts*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpNoStem33=fit(MixedModel,modelFormula,datasetNoExpNoSTEM,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(noExpNoStem3,noExpNoStem31))
show(MixedModels.likelihoodratiotest(noExpNoStem3,noExpNoStem32))
show(MixedModels.likelihoodratiotest(noExpNoStem3,noExpNoStem33))
#33 n.s.
noExpNoStem4=noExpNoStem33
#reduce
#nStimuli*typeContrasts
modelFormula=@formula(responseCorrect~nStimuli+
              sexContrasts*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpNoStem41=fit(MixedModel,modelFormula,datasetNoExpNoSTEM,Binomial())
#sexContrasts*typeContrasts
modelFormula=@formula(responseCorrect~nStimuli*typeContrasts+
              sexContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpNoStem42=fit(MixedModel,modelFormula,datasetNoExpNoSTEM,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(noExpNoStem4,noExpNoStem41))
show(MixedModels.likelihoodratiotest(noExpNoStem4,noExpNoStem42))
#both significant
#main effects
#nStimuli
modelFormula=@formula(responseCorrect ~ 1  + typeContrasts + sexContrasts +
blockNumeric + deg + trialNumber + nStimuli & typeContrasts + sexContrasts & typeContrasts +
(1 + nStimuliFactor + typeContrasts + blockNumeric + trialNumber | ID) + (1 | modelNumber))
@elapsed noExpNoStem43=fit(MixedModel,modelFormula,datasetNoExpNoSTEM,Binomial())
#typeContrasts
modelFormula=@formula(responseCorrect ~ 1 + nStimuli  + sexContrasts +
blockNumeric + deg + trialNumber + nStimuli & typeContrasts + sexContrasts & typeContrasts +
(1 + nStimuliFactor + typeContrasts + blockNumeric + trialNumber | ID) + (1 | modelNumber))
@elapsed noExpNoStem44=fit(MixedModel,modelFormula,datasetNoExpNoSTEM,Binomial())
#sexContrasts
modelFormula=@formula(responseCorrect ~ 1 + nStimuli + typeContrasts  +
blockNumeric + deg + trialNumber + nStimuli & typeContrasts + sexContrasts & typeContrasts +
(1 + nStimuliFactor + typeContrasts + blockNumeric + trialNumber | ID) + (1 | modelNumber))
@elapsed noExpNoStem45=fit(MixedModel,modelFormula,datasetNoExpNoSTEM,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(noExpNoStem4,noExpNoStem43))
show(MixedModels.likelihoodratiotest(noExpNoStem4,noExpNoStem44))
show(MixedModels.likelihoodratiotest(noExpNoStem4,noExpNoStem45))

#sex differences separated by type for noExp dataset
#paired
modelFormula=@formula(responseCorrect~STEMContrasts1*sexContrasts*nStimuli+
              STEMContrasts2*sexContrasts*nStimuli+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpTypePaired1=fit(MixedModel,modelFormula,datasetNoExpTypePaired,Binomial())
#reduced
modelFormula=@formula(responseCorrect~sexContrasts*nStimuli+
              STEMContrasts1*nStimuli+
              STEMContrasts1*sexContrasts+
              STEMContrasts2*nStimuli+
              STEMContrasts2*sexContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpTypePaired11=fit(MixedModel,modelFormula,datasetNoExpTypePaired,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(noExpTypePaired1,noExpTypePaired11))
#n.s.
noExpTypePaired2=noExpTypePaired11
#reduce
#sexContrasts*nStimuli
modelFormula=@formula(responseCorrect~STEMContrasts1*nStimuli+
              STEMContrasts1*sexContrasts+
              STEMContrasts2*nStimuli+
              STEMContrasts2*sexContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpTypePaired21=fit(MixedModel,modelFormula,datasetNoExpTypePaired,Binomial())
#STEMContrasts*nStimuli (both)
modelFormula=@formula(responseCorrect~sexContrasts*nStimuli+
              STEMContrasts1*sexContrasts+
              STEMContrasts2*sexContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpTypePaired22=fit(MixedModel,modelFormula,datasetNoExpTypePaired,Binomial())
#TEMContrasts*sexContrasts (both)
modelFormula=@formula(responseCorrect~sexContrasts*nStimuli+
              STEMContrasts1*nStimuli+
              STEMContrasts2*nStimuli+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpTypePaired23=fit(MixedModel,modelFormula,datasetNoExpTypePaired,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(noExpTypePaired2,noExpTypePaired21))
show(MixedModels.likelihoodratiotest(noExpTypePaired2,noExpTypePaired22))
show(MixedModels.likelihoodratiotest(noExpTypePaired2,noExpTypePaired23))
#largest p for 21
noExpTypePaired3=noExpTypePaired21
#reduce
#STEMContrasts*nStimuli
modelFormula=@formula(responseCorrect~STEMContrasts1*sexContrasts+
              STEMContrasts2*sexContrasts+
              nStimuli+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpTypePaired31=fit(MixedModel,modelFormula,datasetNoExpTypePaired,Binomial())
#STEMContrasts*sexContrasts
modelFormula=@formula(responseCorrect~STEMContrasts1*nStimuli+
              STEMContrasts2*nStimuli+
              sexContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpTypePaired32=fit(MixedModel,modelFormula,datasetNoExpTypePaired,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(noExpTypePaired3,noExpTypePaired31))
show(MixedModels.likelihoodratiotest(noExpTypePaired3,noExpTypePaired32))
#largest p for 31
noExpTypePaired4=noExpTypePaired31
#reduce
#STEMContrasts*sexContrasts
modelFormula=@formula(responseCorrect~STEMContrasts1+
              STEMContrasts2+sexContrasts+
              nStimuli+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpTypePaired41=fit(MixedModel,modelFormula,datasetNoExpTypePaired,Binomial())
#nStimuli
modelFormula=@formula(responseCorrect~STEMContrasts1*sexContrasts+
              STEMContrasts2*sexContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpTypePaired42=fit(MixedModel,modelFormula,datasetNoExpTypePaired,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(noExpTypePaired4,noExpTypePaired41))
show(MixedModels.likelihoodratiotest(noExpTypePaired4,noExpTypePaired42))
#largest p for 42
noExpTypePaired5=noExpTypePaired42
#reduce
#STEMContrasts*sexContrasts
modelFormula=@formula(responseCorrect~STEMContrasts1+
              STEMContrasts2+sexContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpTypePaired51=fit(MixedModel,modelFormula,datasetNoExpTypePaired,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(noExpTypePaired5,noExpTypePaired51))
#n.s.
noExpTypePaired6=noExpTypePaired51
#reduce
#STEM
modelFormula=@formula(responseCorrect~
              sexContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpTypePaired61=fit(MixedModel,modelFormula,datasetNoExpTypePaired,Binomial())
#sex
modelFormula=@formula(responseCorrect~STEMContrasts1+
              STEMContrasts2+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpTypePaired62=fit(MixedModel,modelFormula,datasetNoExpTypePaired,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(noExpTypePaired6,noExpTypePaired61))
show(MixedModels.likelihoodratiotest(noExpTypePaired6,noExpTypePaired62))
#62 n.s.
noExpTypePaired7=noExpTypePaired62
#reduce
#STEM
modelFormula=@formula(responseCorrect~
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpTypePaired71=fit(MixedModel,modelFormula,datasetNoExpTypePaired,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(noExpTypePaired7,noExpTypePaired71))
#significant

#mixed type
modelFormula=@formula(responseCorrect~STEMContrasts1*sexContrasts*nStimuli+
              STEMContrasts2*sexContrasts*nStimuli+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpTypeMixed1=fit(MixedModel,modelFormula,datasetNoExpTypeMixed,Binomial())
#reduced
modelFormula=@formula(responseCorrect~sexContrasts*nStimuli+
              STEMContrasts1*nStimuli+
              STEMContrasts1*sexContrasts+
              STEMContrasts2*nStimuli+
              STEMContrasts2*sexContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpTypeMixed11=fit(MixedModel,modelFormula,datasetNoExpTypeMixed,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(noExpTypeMixed1,noExpTypeMixed11))
#significant
#effect of sex
modelFormula=@formula(responseCorrect ~ 1 + STEMContrasts1  + nStimuli + STEMContrasts2 + blockNumeric + deg + trialNumber + STEMContrasts1 & sexContrasts + STEMContrasts1 & nStimuli + sexContrasts & nStimuli + STEMContrasts2 & sexContrasts + STEMContrasts2 & nStimuli + STEMContrasts1 & sexContrasts & nStimuli + STEMContrasts2 & sexContrasts & nStimuli + (1 + nStimuliFactor + blockNumeric
+ trialNumber | ID) + (1 | modelNumber)
)
@elapsed noExpTypeMixed12=fit(MixedModel,modelFormula,datasetNoExpTypeMixed,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(noExpTypeMixed1,noExpTypeMixed12))


#sex differences separated by type for noExpNoStem dataset
#paired type
modelFormula=@formula(responseCorrect~nStimuli+
              sexContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpNoStemTypePaired1=fit(MixedModel,modelFormula,datasetNoExpNoSTEMTypePaired,Binomial())
#reduce
#nStimuli
modelFormula=@formula(responseCorrect~
              sexContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpNoStemTypePaired11=fit(MixedModel,modelFormula,datasetNoExpNoSTEMTypePaired,Binomial())
#sexContrasts
modelFormula=@formula(responseCorrect~nStimuli+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpNoStemTypePaired12=fit(MixedModel,modelFormula,datasetNoExpNoSTEMTypePaired,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(noExpNoStemTypePaired1,noExpNoStemTypePaired11))
show(MixedModels.likelihoodratiotest(noExpNoStemTypePaired1,noExpNoStemTypePaired12))
#largest p for 11
noExpNoStemTypePaired2=noExpNoStemTypePaired11
#reduce
#sexContrasts
modelFormula=@formula(responseCorrect~
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpNoStemTypePaired21=fit(MixedModel,modelFormula,datasetNoExpNoSTEMTypePaired,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(noExpNoStemTypePaired2,noExpNoStemTypePaired21))
#n.s.

#mixed type
modelFormula=@formula(responseCorrect~nStimuli+
              sexContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpNoStemTypeMixed1=fit(MixedModel,modelFormula,datasetNoExpNoSTEMTypeMixed,Binomial())
#reduce
#nStimuli
modelFormula=@formula(responseCorrect~
              sexContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpNoStemTypeMixed11=fit(MixedModel,modelFormula,datasetNoExpNoSTEMTypeMixed,Binomial())
#sexContrasts
modelFormula=@formula(responseCorrect~nStimuli+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed noExpNoStemTypeMixed12=fit(MixedModel,modelFormula,datasetNoExpNoSTEMTypeMixed,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(noExpNoStemTypeMixed1,noExpNoStemTypeMixed11))
show(MixedModels.likelihoodratiotest(noExpNoStemTypeMixed1,noExpNoStemTypeMixed12))
#both significant