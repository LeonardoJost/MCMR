### get significance of fixed effects
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

#model after random slope selection
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm0=fit(MixedModel,modelFormula,dataset,Binomial())
##reduce fixed effects by nonsignificant effects
#reduce STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts
modelFormula=@formula(responseCorrect~ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts2*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts2*ExperienceContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts2*ExperienceContrasts*sexContrasts*typeContrasts+
              STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm01=fit(MixedModel,modelFormula,dataset,Binomial())
#reduce STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm02=fit(MixedModel,modelFormula,dataset,Binomial())
#blockNumeric
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm03=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm04=fit(MixedModel,modelFormula,dataset,Binomial())
#trialNumber
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm05=fit(MixedModel,modelFormula,dataset,Binomial())
#reduce nStimuli to numeric
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuli*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuli*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm06=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm0,gm01))
show(MixedModels.likelihoodratiotest(gm0,gm02))
show(MixedModels.likelihoodratiotest(gm0,gm03))
show(MixedModels.likelihoodratiotest(gm0,gm04))
show(MixedModels.likelihoodratiotest(gm0,gm05))
show(MixedModels.likelihoodratiotest(gm0,gm06))
#gm02 n.s. and gm06 n.s. (gm02 is part of five-way interaction -> keep)
gm1=gm06


##reduce gm1 by all possible fixed effects
#STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuli*typeContrasts+
modelFormula=@formula(responseCorrect~ExperienceContrasts*sexContrasts*nStimuli*typeContrasts+
              STEMContrasts2*sexContrasts*nStimuli*typeContrasts+
              STEMContrasts2*ExperienceContrasts*nStimuli*typeContrasts+
              STEMContrasts2*ExperienceContrasts*sexContrasts*typeContrasts+
              STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuli+
              STEMContrasts1*sexContrasts*nStimuli*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm11=fit(MixedModel,modelFormula,dataset,Binomial())
#blockNumeric
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuli*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuli*typeContrasts+
              deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm12=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuli*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuli*typeContrasts+
              blockNumeric+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm13=fit(MixedModel,modelFormula,dataset,Binomial())
#trialNumber
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuli*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuli*typeContrasts+
              blockNumeric+deg+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm14=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm1,gm11))
show(MixedModels.likelihoodratiotest(gm1,gm12))
show(MixedModels.likelihoodratiotest(gm1,gm13))
show(MixedModels.likelihoodratiotest(gm1,gm14))
#all significant
#fully written
modelFormula=@formula(responseCorrect ~ 1 + STEMContrasts2 + ExperienceContrasts + sexContrasts + nStimuli + typeContrasts + STEMContrasts1 + blockNumeric + deg + trialNumber + STEMContrasts2 & ExperienceContrasts + STEMContrasts2 & sexContrasts + ExperienceContrasts & sexContrasts + STEMContrasts2 & nStimuli + ExperienceContrasts & nStimuli + sexContrasts & nStimuli + STEMContrasts2 & typeContrasts + ExperienceContrasts & typeContrasts + sexContrasts & typeContrasts + nStimuli & typeContrasts + STEMContrasts1 & sexContrasts + STEMContrasts1 & nStimuli + STEMContrasts1 & typeContrasts + STEMContrasts2 & ExperienceContrasts & sexContrasts + STEMContrasts2 & ExperienceContrasts & nStimuli + STEMContrasts2 & sexContrasts & nStimuli + ExperienceContrasts & sexContrasts & nStimuli + STEMContrasts2 & ExperienceContrasts & typeContrasts + STEMContrasts2 & sexContrasts & typeContrasts + ExperienceContrasts & sexContrasts & typeContrasts + STEMContrasts2 & nStimuli & typeContrasts + ExperienceContrasts & nStimuli & typeContrasts + sexContrasts & nStimuli & typeContrasts + STEMContrasts1 & sexContrasts & nStimuli + STEMContrasts1 & sexContrasts & typeContrasts + STEMContrasts1 & nStimuli & typeContrasts + STEMContrasts2 & ExperienceContrasts & sexContrasts & nStimuli + STEMContrasts2 & ExperienceContrasts & sexContrasts & typeContrasts + STEMContrasts2 & ExperienceContrasts & nStimuli & typeContrasts + STEMContrasts2 & sexContrasts & nStimuli & typeContrasts + ExperienceContrasts & sexContrasts & nStimuli & typeContrasts + STEMContrasts1 & sexContrasts & nStimuli & typeContrasts + STEMContrasts2 & ExperienceContrasts & sexContrasts & nStimuli & typeContrasts + (1 + nStimuliFactor + typeContrasts + blockNumeric + trialNumber | ID) + (1 +
Experience | modelNumber))
#check for overall main effects of interest
modelFormula=@formula(responseCorrect ~ 1 + STEMContrasts2 + ExperienceContrasts + sexContrasts + nStimuli + typeContrasts + STEMContrasts1 + blockNumeric + deg + trialNumber +
STEMContrasts2 & ExperienceContrasts + STEMContrasts2 & sexContrasts + ExperienceContrasts & sexContrasts + STEMContrasts2 & nStimuli + ExperienceContrasts & nStimuli + sexContrasts & nStimuli + STEMContrasts2 & typeContrasts + ExperienceContrasts & typeContrasts + sexContrasts & typeContrasts + nStimuli & typeContrasts + STEMContrasts1 & sexContrasts + STEMContrasts1 & nStimuli + STEMContrasts1 & typeContrasts +
STEMContrasts2 & ExperienceContrasts & sexContrasts + STEMContrasts2 & ExperienceContrasts & nStimuli + STEMContrasts2 & sexContrasts & nStimuli + ExperienceContrasts & sexContrasts & nStimuli + STEMContrasts2 & ExperienceContrasts & typeContrasts + STEMContrasts2 & sexContrasts & typeContrasts + ExperienceContrasts & sexContrasts & typeContrasts + STEMContrasts2 & nStimuli & typeContrasts + ExperienceContrasts & nStimuli & typeContrasts +
#sexContrasts & nStimuli & typeContrasts +
STEMContrasts1 & sexContrasts & nStimuli + STEMContrasts1 & sexContrasts & typeContrasts + STEMContrasts1 & nStimuli & typeContrasts +
STEMContrasts2 & ExperienceContrasts & sexContrasts & nStimuli + STEMContrasts2 & ExperienceContrasts & sexContrasts & typeContrasts + STEMContrasts2 & ExperienceContrasts & nStimuli & typeContrasts + STEMContrasts2 & sexContrasts & nStimuli & typeContrasts + ExperienceContrasts & sexContrasts & nStimuli & typeContrasts + STEMContrasts1 & sexContrasts & nStimuli & typeContrasts +
STEMContrasts2 & ExperienceContrasts & sexContrasts & nStimuli & typeContrasts +
(1 + nStimuliFactor + typeContrasts + blockNumeric + trialNumber | ID) + (1 + Experience | modelNumber))
@elapsed gm15=fit(MixedModel,modelFormula,dataset,Binomial())
#sex*type
modelFormula=@formula(responseCorrect ~ 1 + STEMContrasts2 + ExperienceContrasts + sexContrasts + nStimuli + typeContrasts + STEMContrasts1 + blockNumeric + deg + trialNumber +
STEMContrasts2 & ExperienceContrasts + STEMContrasts2 & sexContrasts + ExperienceContrasts & sexContrasts + STEMContrasts2 & nStimuli + ExperienceContrasts & nStimuli + sexContrasts & nStimuli + STEMContrasts2 & typeContrasts + ExperienceContrasts & typeContrasts +
#sexContrasts & typeContrasts +
nStimuli & typeContrasts + STEMContrasts1 & sexContrasts + STEMContrasts1 & nStimuli + STEMContrasts1 & typeContrasts +
STEMContrasts2 & ExperienceContrasts & sexContrasts + STEMContrasts2 & ExperienceContrasts & nStimuli + STEMContrasts2 & sexContrasts & nStimuli + ExperienceContrasts & sexContrasts & nStimuli + STEMContrasts2 & ExperienceContrasts & typeContrasts + STEMContrasts2 & sexContrasts & typeContrasts + ExperienceContrasts & sexContrasts & typeContrasts + STEMContrasts2 & nStimuli & typeContrasts + ExperienceContrasts & nStimuli & typeContrasts +
sexContrasts & nStimuli & typeContrasts +
STEMContrasts1 & sexContrasts & nStimuli + STEMContrasts1 & sexContrasts & typeContrasts + STEMContrasts1 & nStimuli & typeContrasts +
STEMContrasts2 & ExperienceContrasts & sexContrasts & nStimuli + STEMContrasts2 & ExperienceContrasts & sexContrasts & typeContrasts + STEMContrasts2 & ExperienceContrasts & nStimuli & typeContrasts + STEMContrasts2 & sexContrasts & nStimuli & typeContrasts + ExperienceContrasts & sexContrasts & nStimuli & typeContrasts + STEMContrasts1 & sexContrasts & nStimuli & typeContrasts +
STEMContrasts2 & ExperienceContrasts & sexContrasts & nStimuli & typeContrasts +
(1 + nStimuliFactor + typeContrasts + blockNumeric + trialNumber | ID) + (1 + Experience | modelNumber))
@elapsed gm16=fit(MixedModel,modelFormula,dataset,Binomial())
#sex*nStimuli
modelFormula=@formula(responseCorrect ~ 1 + STEMContrasts2 + ExperienceContrasts + sexContrasts + nStimuli + typeContrasts + STEMContrasts1 + blockNumeric + deg + trialNumber +
STEMContrasts2 & ExperienceContrasts + STEMContrasts2 & sexContrasts + ExperienceContrasts & sexContrasts + STEMContrasts2 & nStimuli + ExperienceContrasts & nStimuli +
#sexContrasts & nStimuli +
STEMContrasts2 & typeContrasts + ExperienceContrasts & typeContrasts + sexContrasts & typeContrasts + nStimuli & typeContrasts + STEMContrasts1 & sexContrasts + STEMContrasts1 & nStimuli + STEMContrasts1 & typeContrasts +
STEMContrasts2 & ExperienceContrasts & sexContrasts + STEMContrasts2 & ExperienceContrasts & nStimuli + STEMContrasts2 & sexContrasts & nStimuli + ExperienceContrasts & sexContrasts & nStimuli + STEMContrasts2 & ExperienceContrasts & typeContrasts + STEMContrasts2 & sexContrasts & typeContrasts + ExperienceContrasts & sexContrasts & typeContrasts + STEMContrasts2 & nStimuli & typeContrasts + ExperienceContrasts & nStimuli & typeContrasts +
sexContrasts & nStimuli & typeContrasts +
STEMContrasts1 & sexContrasts & nStimuli + STEMContrasts1 & sexContrasts & typeContrasts + STEMContrasts1 & nStimuli & typeContrasts +
STEMContrasts2 & ExperienceContrasts & sexContrasts & nStimuli + STEMContrasts2 & ExperienceContrasts & sexContrasts & typeContrasts + STEMContrasts2 & ExperienceContrasts & nStimuli & typeContrasts + STEMContrasts2 & sexContrasts & nStimuli & typeContrasts + ExperienceContrasts & sexContrasts & nStimuli & typeContrasts + STEMContrasts1 & sexContrasts & nStimuli & typeContrasts +
STEMContrasts2 & ExperienceContrasts & sexContrasts & nStimuli & typeContrasts +
(1 + nStimuliFactor + typeContrasts + blockNumeric + trialNumber | ID) + (1 + Experience | modelNumber))
@elapsed gm17=fit(MixedModel,modelFormula,dataset,Binomial())
#sex
modelFormula=@formula(responseCorrect ~ 1 + STEMContrasts2 + ExperienceContrasts +
#sexContrasts +
nStimuli + typeContrasts + STEMContrasts1 + blockNumeric + deg + trialNumber +
STEMContrasts2 & ExperienceContrasts + STEMContrasts2 & sexContrasts + ExperienceContrasts & sexContrasts + STEMContrasts2 & nStimuli + ExperienceContrasts & nStimuli + sexContrasts & nStimuli + STEMContrasts2 & typeContrasts + ExperienceContrasts & typeContrasts + sexContrasts & typeContrasts + nStimuli & typeContrasts + STEMContrasts1 & sexContrasts + STEMContrasts1 & nStimuli + STEMContrasts1 & typeContrasts +
STEMContrasts2 & ExperienceContrasts & sexContrasts + STEMContrasts2 & ExperienceContrasts & nStimuli + STEMContrasts2 & sexContrasts & nStimuli + ExperienceContrasts & sexContrasts & nStimuli + STEMContrasts2 & ExperienceContrasts & typeContrasts + STEMContrasts2 & sexContrasts & typeContrasts + ExperienceContrasts & sexContrasts & typeContrasts + STEMContrasts2 & nStimuli & typeContrasts + ExperienceContrasts & nStimuli & typeContrasts +
sexContrasts & nStimuli & typeContrasts +
STEMContrasts1 & sexContrasts & nStimuli + STEMContrasts1 & sexContrasts & typeContrasts + STEMContrasts1 & nStimuli & typeContrasts +
STEMContrasts2 & ExperienceContrasts & sexContrasts & nStimuli + STEMContrasts2 & ExperienceContrasts & sexContrasts & typeContrasts + STEMContrasts2 & ExperienceContrasts & nStimuli & typeContrasts + STEMContrasts2 & sexContrasts & nStimuli & typeContrasts + ExperienceContrasts & sexContrasts & nStimuli & typeContrasts + STEMContrasts1 & sexContrasts & nStimuli & typeContrasts +
STEMContrasts2 & ExperienceContrasts & sexContrasts & nStimuli & typeContrasts +
(1 + nStimuliFactor + typeContrasts + blockNumeric + trialNumber | ID) + (1 + Experience | modelNumber))
@elapsed gm18=fit(MixedModel,modelFormula,dataset,Binomial())
#nStimuli*type
modelFormula=@formula(responseCorrect ~ 1 + STEMContrasts2 + ExperienceContrasts +
sexContrasts + nStimuli + typeContrasts + STEMContrasts1 + blockNumeric + deg + trialNumber +
STEMContrasts2 & ExperienceContrasts + STEMContrasts2 & sexContrasts + ExperienceContrasts & sexContrasts + STEMContrasts2 & nStimuli + ExperienceContrasts & nStimuli + sexContrasts & nStimuli + STEMContrasts2 & typeContrasts + ExperienceContrasts & typeContrasts + sexContrasts & typeContrasts +
#nStimuli & typeContrasts +
STEMContrasts1 & sexContrasts + STEMContrasts1 & nStimuli + STEMContrasts1 & typeContrasts +
STEMContrasts2 & ExperienceContrasts & sexContrasts + STEMContrasts2 & ExperienceContrasts & nStimuli + STEMContrasts2 & sexContrasts & nStimuli + ExperienceContrasts & sexContrasts & nStimuli + STEMContrasts2 & ExperienceContrasts & typeContrasts + STEMContrasts2 & sexContrasts & typeContrasts + ExperienceContrasts & sexContrasts & typeContrasts + STEMContrasts2 & nStimuli & typeContrasts + ExperienceContrasts & nStimuli & typeContrasts +
sexContrasts & nStimuli & typeContrasts +
STEMContrasts1 & sexContrasts & nStimuli + STEMContrasts1 & sexContrasts & typeContrasts + STEMContrasts1 & nStimuli & typeContrasts +
STEMContrasts2 & ExperienceContrasts & sexContrasts & nStimuli + STEMContrasts2 & ExperienceContrasts & sexContrasts & typeContrasts + STEMContrasts2 & ExperienceContrasts & nStimuli & typeContrasts + STEMContrasts2 & sexContrasts & nStimuli & typeContrasts + ExperienceContrasts & sexContrasts & nStimuli & typeContrasts + STEMContrasts1 & sexContrasts & nStimuli & typeContrasts +
STEMContrasts2 & ExperienceContrasts & sexContrasts & nStimuli & typeContrasts +
(1 + nStimuliFactor + typeContrasts + blockNumeric + trialNumber | ID) + (1 + Experience | modelNumber))
@elapsed gm19=fit(MixedModel,modelFormula,dataset,Binomial())
#nStimuli
modelFormula=@formula(responseCorrect ~ 1 + STEMContrasts2 + ExperienceContrasts + sexContrasts +
#nStimuli +
typeContrasts + STEMContrasts1 + blockNumeric + deg + trialNumber +
STEMContrasts2 & ExperienceContrasts + STEMContrasts2 & sexContrasts + ExperienceContrasts & sexContrasts + STEMContrasts2 & nStimuli + ExperienceContrasts & nStimuli + sexContrasts & nStimuli + STEMContrasts2 & typeContrasts + ExperienceContrasts & typeContrasts + sexContrasts & typeContrasts + nStimuli & typeContrasts + STEMContrasts1 & sexContrasts + STEMContrasts1 & nStimuli + STEMContrasts1 & typeContrasts +
STEMContrasts2 & ExperienceContrasts & sexContrasts + STEMContrasts2 & ExperienceContrasts & nStimuli + STEMContrasts2 & sexContrasts & nStimuli + ExperienceContrasts & sexContrasts & nStimuli + STEMContrasts2 & ExperienceContrasts & typeContrasts + STEMContrasts2 & sexContrasts & typeContrasts + ExperienceContrasts & sexContrasts & typeContrasts + STEMContrasts2 & nStimuli & typeContrasts + ExperienceContrasts & nStimuli & typeContrasts +
sexContrasts & nStimuli & typeContrasts +
STEMContrasts1 & sexContrasts & nStimuli + STEMContrasts1 & sexContrasts & typeContrasts + STEMContrasts1 & nStimuli & typeContrasts +
STEMContrasts2 & ExperienceContrasts & sexContrasts & nStimuli + STEMContrasts2 & ExperienceContrasts & sexContrasts & typeContrasts + STEMContrasts2 & ExperienceContrasts & nStimuli & typeContrasts + STEMContrasts2 & sexContrasts & nStimuli & typeContrasts + ExperienceContrasts & sexContrasts & nStimuli & typeContrasts + STEMContrasts1 & sexContrasts & nStimuli & typeContrasts +
STEMContrasts2 & ExperienceContrasts & sexContrasts & nStimuli & typeContrasts +
(1 + nStimuliFactor + typeContrasts + blockNumeric + trialNumber | ID) + (1 + Experience | modelNumber))
@elapsed gm110=fit(MixedModel,modelFormula,dataset,Binomial())
#type
modelFormula=@formula(responseCorrect ~ 1 + STEMContrasts2 + ExperienceContrasts + sexContrasts + nStimuli +
#typeContrasts +
STEMContrasts1 + blockNumeric + deg + trialNumber +
STEMContrasts2 & ExperienceContrasts + STEMContrasts2 & sexContrasts + ExperienceContrasts & sexContrasts + STEMContrasts2 & nStimuli + ExperienceContrasts & nStimuli + sexContrasts & nStimuli + STEMContrasts2 & typeContrasts + ExperienceContrasts & typeContrasts + sexContrasts & typeContrasts + nStimuli & typeContrasts + STEMContrasts1 & sexContrasts + STEMContrasts1 & nStimuli + STEMContrasts1 & typeContrasts +
STEMContrasts2 & ExperienceContrasts & sexContrasts + STEMContrasts2 & ExperienceContrasts & nStimuli + STEMContrasts2 & sexContrasts & nStimuli + ExperienceContrasts & sexContrasts & nStimuli + STEMContrasts2 & ExperienceContrasts & typeContrasts + STEMContrasts2 & sexContrasts & typeContrasts + ExperienceContrasts & sexContrasts & typeContrasts + STEMContrasts2 & nStimuli & typeContrasts + ExperienceContrasts & nStimuli & typeContrasts +
sexContrasts & nStimuli & typeContrasts +
STEMContrasts1 & sexContrasts & nStimuli + STEMContrasts1 & sexContrasts & typeContrasts + STEMContrasts1 & nStimuli & typeContrasts +
STEMContrasts2 & ExperienceContrasts & sexContrasts & nStimuli + STEMContrasts2 & ExperienceContrasts & sexContrasts & typeContrasts + STEMContrasts2 & ExperienceContrasts & nStimuli & typeContrasts + STEMContrasts2 & sexContrasts & nStimuli & typeContrasts + ExperienceContrasts & sexContrasts & nStimuli & typeContrasts + STEMContrasts1 & sexContrasts & nStimuli & typeContrasts +
STEMContrasts2 & ExperienceContrasts & sexContrasts & nStimuli & typeContrasts +
(1 + nStimuliFactor + typeContrasts + blockNumeric + trialNumber | ID) + (1 + Experience | modelNumber))
@elapsed gm111=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm1,gm15))
show(MixedModels.likelihoodratiotest(gm1,gm16))
show(MixedModels.likelihoodratiotest(gm1,gm17))
show(MixedModels.likelihoodratiotest(gm1,gm18))
show(MixedModels.likelihoodratiotest(gm1,gm19))
show(MixedModels.likelihoodratiotest(gm1,gm110))
show(MixedModels.likelihoodratiotest(gm1,gm111))

#check for overall main effects of interest
#disregard higher order interaction because of unequal distribution (average effects instead of main effects)
# modelFormula=@formula(responseCorrect~sexContrasts*nStimuli*typeContrasts+
#               blockNumeric+deg+trialNumber+
#               (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
#               (Experience|modelNumber))
# @elapsed gm5=fit(MixedModel,modelFormula,dataset,Binomial())
# #without triple interaction
# modelFormula=@formula(responseCorrect~nStimuli*typeContrasts+
#               sexContrasts*typeContrasts+
#               sexContrasts*nStimuli+
#               blockNumeric+deg+trialNumber+
#               (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
#               (Experience|modelNumber))
# @elapsed gm51=fit(MixedModel,modelFormula,dataset,Binomial())
# #comparison
# show(MixedModels.likelihoodratiotest(gm5,gm51))
# #n.s.
# gm6=gm51
# #reduce gm6
# #nStimuliFactor*typeContrasts
# modelFormula=@formula(responseCorrect~
#               sexContrasts*typeContrasts+
#               sexContrasts*nStimuli+
#               blockNumeric+deg+trialNumber+
#               (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
#               (Experience|modelNumber))
# @elapsed gm61=fit(MixedModel,modelFormula,dataset,Binomial())
# #sexContrasts*typeContrasts
# modelFormula=@formula(responseCorrect~nStimuli*typeContrasts+
#               sexContrasts*nStimuli+
#               blockNumeric+deg+trialNumber+
#               (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
#               (Experience|modelNumber))
# @elapsed gm62=fit(MixedModel,modelFormula,dataset,Binomial())
# #sexContrasts*nStimuliFactor
# modelFormula=@formula(responseCorrect~nStimuli*typeContrasts+
#               sexContrasts*typeContrasts+
#               blockNumeric+deg+trialNumber+
#               (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
#               (Experience|modelNumber))
# @elapsed gm63=fit(MixedModel,modelFormula,dataset,Binomial())
# #comparison
# show(MixedModels.likelihoodratiotest(gm6,gm61))
# show(MixedModels.likelihoodratiotest(gm6,gm62))
# show(MixedModels.likelihoodratiotest(gm6,gm63))
# #largest p for gm62
# gm7=gm62
# #nStimuliFactor*typeContrasts
# modelFormula=@formula(responseCorrect~typeContrasts+
#               sexContrasts*nStimuli+
#               blockNumeric+deg+trialNumber+
#               (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
#               (Experience|modelNumber))
# @elapsed gm71=fit(MixedModel,modelFormula,dataset,Binomial())
# #sexContrasts*nStimuliFactor
# modelFormula=@formula(responseCorrect~nStimuli*typeContrasts+
#               sexContrasts+
#               blockNumeric+deg+trialNumber+
#               (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
#               (Experience|modelNumber))
# @elapsed gm72=fit(MixedModel,modelFormula,dataset,Binomial())
# #comparison
# show(MixedModels.likelihoodratiotest(gm7,gm71))
# show(MixedModels.likelihoodratiotest(gm7,gm72))
# #gm72 n.s.
# gm8=gm72
# #nStimuliFactor*typeContrasts
# modelFormula=@formula(responseCorrect~nStimuli+typeContrasts+
#               sexContrasts+
#               blockNumeric+deg+trialNumber+
#               (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
#               (Experience|modelNumber))
# @elapsed gm81=fit(MixedModel,modelFormula,dataset,Binomial())
# #sexContrasts
# modelFormula=@formula(responseCorrect~nStimuli*typeContrasts+
#               blockNumeric+deg+trialNumber+
#               (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
#               (Experience|modelNumber))
# @elapsed gm82=fit(MixedModel,modelFormula,dataset,Binomial())
# #comparison
# show(MixedModels.likelihoodratiotest(gm8,gm81))
# show(MixedModels.likelihoodratiotest(gm8,gm82))
# #both significant
# #effect of type*sex
# modelFormula=@formula(responseCorrect~nStimuli*typeContrasts+
#               sexContrasts*typeContrasts+
#               blockNumeric+deg+trialNumber+
#               (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
#               (Experience|modelNumber))
# @elapsed gm83=fit(MixedModel,modelFormula,dataset,Binomial())
# #comparison
# show(MixedModels.likelihoodratiotest(gm8,gm83))
