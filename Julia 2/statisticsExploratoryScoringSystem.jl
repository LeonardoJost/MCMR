### exploratory analyses
#     Copyright (C) 2022  Leonardo Jost
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
dataset=CSV.read("dataset\\datasetGrouped.csv", DataFrame)
#inspect data
show(first(dataset,6))
show(names(dataset))
#show(IOContext(stdout, :limit=>false),dataset[115:1000,[:type, :deg]])
show(eltype.(eachcol(dataset)))
#add block as numeric variable
dataset.blockNumeric=tryparse.(Int,chop.(dataset.block,head=4,tail=0))
#scaling, centering function
function centerNormalize(vector)
  vector=vector.-minimum(vector)
  vector=vector./maximum(vector)
  vector=vector.-mean(vector)
end
#scaling block to range 0..1 and center
dataset.blockNumeric=centerNormalize(dataset.blockNumeric)
dataset.educationNumeric=centerNormalize(dataset.educationNumeric)

#dataset with only 8 alternatives
dataset8=dataset[dataset[:,:nStimuli] .== 8, :]

#scoring system
#start with maximal model
modelFormula=@formula(accScoringSystem~sexContrasts*typeContrasts+blockNumeric+
              (typeContrasts+blockNumeric|ID))
@elapsed slopesModel1=fit(LinearMixedModel,modelFormula, dataset8,REML=false)
show(slopesModel1.optsum)
show(slopesModel1)
#test removal of random correlation and slopes
#remove random correlation
modelFormula=@formula(accScoringSystem~sexContrasts*typeContrasts+blockNumeric+
              zerocorr(typeContrasts+blockNumeric|ID))
@elapsed slopesModel11=fit(LinearMixedModel,modelFormula, dataset8,REML=false)
#typeContrasts
modelFormula=@formula(accScoringSystem~sexContrasts*typeContrasts+blockNumeric+
              (blockNumeric|ID))
@elapsed slopesModel12=fit(LinearMixedModel,modelFormula, dataset8,REML=false)
#blockNumeric
modelFormula=@formula(accScoringSystem~sexContrasts*typeContrasts+blockNumeric+
              (typeContrasts|ID))
@elapsed slopesModel13=fit(LinearMixedModel,modelFormula, dataset8,REML=false)
#comparison
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel11))
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel12))
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel13))
#all significant at .2


#get significance of fixed effects
fixedEffects1=slopesModel1
#stepwise reduction
#start with maximal model
#sexContrasts*typeContrasts
modelFormula=@formula(accScoringSystem~sexContrasts+typeContrasts+blockNumeric+
              (typeContrasts+blockNumeric|ID))
@elapsed fixedEffects11=fit(LinearMixedModel,modelFormula, dataset8,REML=false)
#blockNumeric
modelFormula=@formula(accScoringSystem~sexContrasts*typeContrasts+
              (typeContrasts+blockNumeric|ID))
@elapsed fixedEffects12=fit(LinearMixedModel,modelFormula, dataset8,REML=false)
#comparison
show(MixedModels.likelihoodratiotest(fixedEffects1,fixedEffects11))
show(MixedModels.likelihoodratiotest(fixedEffects1,fixedEffects12))
#11 n.s.
fixedEffects2=fixedEffects11
#sexContrasts
modelFormula=@formula(accScoringSystem~typeContrasts+blockNumeric+
              (typeContrasts+blockNumeric|ID))
@elapsed fixedEffects21=fit(LinearMixedModel,modelFormula, dataset8,REML=false)
#typeContrasts
modelFormula=@formula(accScoringSystem~sexContrasts+blockNumeric+
              (typeContrasts+blockNumeric|ID))
@elapsed fixedEffects22=fit(LinearMixedModel,modelFormula, dataset8,REML=false)
#blockNumeric
modelFormula=@formula(accScoringSystem~sexContrasts+typeContrasts+
              (typeContrasts+blockNumeric|ID))
@elapsed fixedEffects23=fit(LinearMixedModel,modelFormula, dataset8,REML=false)
#comparison
show(MixedModels.likelihoodratiotest(fixedEffects2,fixedEffects21))
show(MixedModels.likelihoodratiotest(fixedEffects2,fixedEffects22))
show(MixedModels.likelihoodratiotest(fixedEffects2,fixedEffects23))
#21 n.s.
fixedEffects3=fixedEffects21
#typeContrasts
modelFormula=@formula(accScoringSystem~blockNumeric+
              (typeContrasts+blockNumeric|ID))
@elapsed fixedEffects31=fit(LinearMixedModel,modelFormula, dataset8,REML=false)
#blockNumeric
modelFormula=@formula(accScoringSystem~typeContrasts+
              (typeContrasts+blockNumeric|ID))
@elapsed fixedEffects32=fit(LinearMixedModel,modelFormula, dataset8,REML=false)
#comparison
show(MixedModels.likelihoodratiotest(fixedEffects3,fixedEffects31))
show(MixedModels.likelihoodratiotest(fixedEffects3,fixedEffects32))
#all significant
#n.s. effects
#sex
show(MixedModels.likelihoodratiotest(fixedEffects2,fixedEffects21))
#sex*type
modelFormula=@formula(accScoringSystem~typeContrasts+blockNumeric+sexContrasts&typeContrasts+
              (typeContrasts+blockNumeric|ID))
@elapsed fixedEffects33=fit(LinearMixedModel,modelFormula, dataset8,REML=false)
show(MixedModels.likelihoodratiotest(fixedEffects3,fixedEffects33))
