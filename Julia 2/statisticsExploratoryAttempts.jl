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


##number of attempted trials
#random slopes
#start with maximal model
modelFormula=@formula(attempts~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed slopesModel1=fit(LinearMixedModel,modelFormula, dataset,REML=false)
show(slopesModel1.optsum)
show(slopesModel1)
#test removal of random correlation and slopes
#remove random correlation
modelFormula=@formula(attempts~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+
              zerocorr(nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed slopesModel11=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#nStimuliContrasts*typeContrasts
modelFormula=@formula(attempts~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+
              (nStimuliContrasts+typeContrasts+blockNumeric|ID))
@elapsed slopesModel12=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#blockNumeric
modelFormula=@formula(attempts~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+
              (nStimuliContrasts*typeContrasts|ID))
@elapsed slopesModel13=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#comparison
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel11))
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel12))
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel13))
#all significant at .2

#get significance of fixed effects
fixedEffects1=slopesModel1
#stepwise reduction
#sexContrasts*nStimuliContrasts*typeContrasts
modelFormula=@formula(attempts~nStimuliContrasts*typeContrasts+
              sexContrasts*typeContrasts+
              sexContrasts*nStimuliContrasts+
              blockNumeric+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed fixedEffects11=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#blockNumeric
modelFormula=@formula(attempts~sexContrasts*nStimuliContrasts*typeContrasts+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed fixedEffects12=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#comparison
show(MixedModels.likelihoodratiotest(fixedEffects1,fixedEffects11))
show(MixedModels.likelihoodratiotest(fixedEffects1,fixedEffects12))
#sexContrasts*nStimuliContrasts*typeContrasts n.s.
fixedEffects2=fixedEffects11
#nStimuliContrasts*typeContrasts
modelFormula=@formula(attempts~sexContrasts*typeContrasts+
              sexContrasts*nStimuliContrasts+
              blockNumeric+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed fixedEffects21=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#sexContrasts*typeContrasts
modelFormula=@formula(attempts~nStimuliContrasts*typeContrasts+
              sexContrasts*nStimuliContrasts+
              blockNumeric+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed fixedEffects22=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#sexContrasts*nStimuliContrasts
modelFormula=@formula(attempts~nStimuliContrasts*typeContrasts+
              sexContrasts*typeContrasts+
              blockNumeric+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed fixedEffects23=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#blockNumeric
modelFormula=@formula(attempts~nStimuliContrasts*typeContrasts+
              sexContrasts*typeContrasts+
              sexContrasts*nStimuliContrasts+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed fixedEffects24=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#comparison
show(MixedModels.likelihoodratiotest(fixedEffects2,fixedEffects21))
show(MixedModels.likelihoodratiotest(fixedEffects2,fixedEffects22))
show(MixedModels.likelihoodratiotest(fixedEffects2,fixedEffects23))
show(MixedModels.likelihoodratiotest(fixedEffects2,fixedEffects24))
#largest p for 21
fixedEffects3=fixedEffects21
#sexContrasts*typeContrasts
modelFormula=@formula(attempts~typeContrasts+
              sexContrasts*nStimuliContrasts+
              blockNumeric+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed fixedEffects31=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#sexContrasts*nStimuliContrasts
modelFormula=@formula(attempts~sexContrasts*typeContrasts+
              nStimuliContrasts+
              blockNumeric+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed fixedEffects32=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#comparison
show(MixedModels.likelihoodratiotest(fixedEffects3,fixedEffects31))
show(MixedModels.likelihoodratiotest(fixedEffects3,fixedEffects32))
#largest p for 31
fixedEffects4=fixedEffects31
#typeContrasts
modelFormula=@formula(attempts~
              sexContrasts*nStimuliContrasts+
              blockNumeric+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed fixedEffects41=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#sexContrasts*nStimuliContrasts
modelFormula=@formula(attempts~typeContrasts+
              sexContrasts+nStimuliContrasts+
              blockNumeric+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed fixedEffects42=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#comparison
show(MixedModels.likelihoodratiotest(fixedEffects4,fixedEffects41))
show(MixedModels.likelihoodratiotest(fixedEffects4,fixedEffects42))
#42 n.s.
fixedEffects5=fixedEffects42
#typeContrasts
modelFormula=@formula(attempts~
              sexContrasts+nStimuliContrasts+
              blockNumeric+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed fixedEffects51=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#sexContrasts
modelFormula=@formula(attempts~typeContrasts+
              nStimuliContrasts+
              blockNumeric+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed fixedEffects52=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#nStimuliContrasts
modelFormula=@formula(attempts~typeContrasts+
              sexContrasts+
              blockNumeric+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed fixedEffects53=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#comparison
show(MixedModels.likelihoodratiotest(fixedEffects5,fixedEffects51))
show(MixedModels.likelihoodratiotest(fixedEffects5,fixedEffects52))
show(MixedModels.likelihoodratiotest(fixedEffects5,fixedEffects53))
#largest p for 52
fixedEffects6=fixedEffects52
#typeContrasts
modelFormula=@formula(attempts~
              nStimuliContrasts+
              blockNumeric+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed fixedEffects61=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#nStimuliContrasts
modelFormula=@formula(attempts~typeContrasts+
              blockNumeric+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed fixedEffects62=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#comparison
show(MixedModels.likelihoodratiotest(fixedEffects6,fixedEffects61))
show(MixedModels.likelihoodratiotest(fixedEffects6,fixedEffects62))
#62 n.s.
fixedEffects7=fixedEffects62
#typeContrasts
modelFormula=@formula(attempts~
              blockNumeric+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed fixedEffects71=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#blockNumeric
modelFormula=@formula(attempts~typeContrasts+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed fixedEffects72=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#comparison
show(MixedModels.likelihoodratiotest(fixedEffects7,fixedEffects71))
show(MixedModels.likelihoodratiotest(fixedEffects7,fixedEffects72))
#both significant
#test n.s. effects
#n alternatives
show(MixedModels.likelihoodratiotest(fixedEffects6,fixedEffects62))
#sex
modelFormula=@formula(attempts~typeContrasts+
              blockNumeric+sexContrasts+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed fixedEffects73=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#n*sex
modelFormula=@formula(attempts~typeContrasts+
              blockNumeric+nStimuliContrasts&sexContrasts+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed fixedEffects74=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#n*type
modelFormula=@formula(attempts~typeContrasts+
              blockNumeric+nStimuliContrasts&typeContrasts+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed fixedEffects75=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#sex*type
modelFormula=@formula(attempts~typeContrasts+
              blockNumeric+sexContrasts&typeContrasts+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed fixedEffects76=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#n*sex*type
modelFormula=@formula(attempts~typeContrasts+
              blockNumeric+nStimuliContrasts&sexContrasts&typeContrasts+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed fixedEffects77=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#comparison
show(MixedModels.likelihoodratiotest(fixedEffects7,fixedEffects73))
show(MixedModels.likelihoodratiotest(fixedEffects7,fixedEffects74))
show(MixedModels.likelihoodratiotest(fixedEffects7,fixedEffects75))
show(MixedModels.likelihoodratiotest(fixedEffects7,fixedEffects76))
show(MixedModels.likelihoodratiotest(fixedEffects7,fixedEffects77))
