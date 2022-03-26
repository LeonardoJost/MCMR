### test selection of random slopes
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
dataset=CSV.read("dataset\\dataset.csv", DataFrame)
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
#scaling block, deg, and trialNumber to range 0..1 and center
dataset.blockNumeric=centerNormalize(dataset.blockNumeric)
dataset.deg=centerNormalize(dataset.deg)
dataset.trialNumber=centerNormalize(dataset.trialNumber)


dataset.responseCorrect=dataset.type.=="hit"

##random slopes
#start with maximal model
#(do not use interactions to avoid overparametrization)
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (sexContrasts+nStimuliContrasts+typeContrasts+blockNumeric+deg+trialNumber|modelNumber))
@elapsed slopesModel1=fit(MixedModel,modelFormula,dataset,Binomial())
show(slopesModel1.optsum)
show(slopesModel1)
#test removal of individual effects
#remove random correlation by id
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              zerocorr(nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (sexContrasts+nStimuliContrasts+typeContrasts+blockNumeric+deg+trialNumber|modelNumber))
@elapsed slopesModel12=fit(MixedModel,modelFormula,dataset,Binomial())
#remove random correlation by id
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              zerocorr(sexContrasts+nStimuliContrasts+typeContrasts+blockNumeric+deg+trialNumber|modelNumber))
@elapsed slopesModel13=fit(MixedModel,modelFormula,dataset,Binomial())
#nStimuliContrasts*typeContrasts|ID
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts+typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (sexContrasts+nStimuliContrasts+typeContrasts+blockNumeric+deg+trialNumber|modelNumber))
@elapsed slopesModel14=fit(MixedModel,modelFormula,dataset,Binomial())
#blockNumeric|ID
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+deg+trialNumber|ID)+
              (sexContrasts+nStimuliContrasts+typeContrasts+blockNumeric+deg+trialNumber|modelNumber))
@elapsed slopesModel15=fit(MixedModel,modelFormula,dataset,Binomial())
#deg|ID
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+trialNumber|ID)+
              (sexContrasts+nStimuliContrasts+typeContrasts+blockNumeric+deg+trialNumber|modelNumber))
@elapsed slopesModel16=fit(MixedModel,modelFormula,dataset,Binomial())
#trialNumber|ID
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg|ID)+
              (sexContrasts+nStimuliContrasts+typeContrasts+blockNumeric+deg+trialNumber|modelNumber))
@elapsed slopesModel17=fit(MixedModel,modelFormula,dataset,Binomial())

#sexContrasts|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (nStimuliContrasts+typeContrasts+blockNumeric+deg+trialNumber|modelNumber))
@elapsed slopesModel18=fit(MixedModel,modelFormula,dataset,Binomial())
#nStimuliContrasts|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (sexContrasts+typeContrasts+blockNumeric+deg+trialNumber|modelNumber))
@elapsed slopesModel19=fit(MixedModel,modelFormula,dataset,Binomial())
#typeContrasts|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (sexContrasts+nStimuliContrasts+blockNumeric+deg+trialNumber|modelNumber))
@elapsed slopesModel110=fit(MixedModel,modelFormula,dataset,Binomial())
#blockNumeric|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (sexContrasts+nStimuliContrasts+typeContrasts+deg+trialNumber|modelNumber))
@elapsed slopesModel111=fit(MixedModel,modelFormula,dataset,Binomial())
#deg|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (sexContrasts+nStimuliContrasts+typeContrasts+blockNumeric+trialNumber|modelNumber))
@elapsed slopesModel112=fit(MixedModel,modelFormula,dataset,Binomial())
#trialNumber|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (sexContrasts+nStimuliContrasts+typeContrasts+blockNumeric+deg|modelNumber))
@elapsed slopesModel113=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel12))
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel13))
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel14))
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel15))
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel16))
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel17))
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel18))
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel19))
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel110))
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel111))
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel112))
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel113))
#largest p for slopesModel110
slopesModel2=slopesModel110
#test large p-values again
#zerocorr|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              zerocorr(sexContrasts+nStimuliContrasts+blockNumeric+deg+trialNumber|modelNumber))
@elapsed slopesModel21=fit(MixedModel,modelFormula,dataset,Binomial())
#sexContrasts|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (nStimuliContrasts+blockNumeric+deg+trialNumber|modelNumber))
@elapsed slopesModel22=fit(MixedModel,modelFormula,dataset,Binomial())
#nStimuliContrasts|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (sexContrasts+blockNumeric+deg+trialNumber|modelNumber))
@elapsed slopesModel23=fit(MixedModel,modelFormula,dataset,Binomial())
#blockNumeric|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (sexContrasts+nStimuliContrasts+deg+trialNumber|modelNumber))
@elapsed slopesModel24=fit(MixedModel,modelFormula,dataset,Binomial())
#deg|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (sexContrasts+nStimuliContrasts+blockNumeric+trialNumber|modelNumber))
@elapsed slopesModel25=fit(MixedModel,modelFormula,dataset,Binomial())
#trialNumber|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (sexContrasts+nStimuliContrasts+blockNumeric+deg|modelNumber))
@elapsed slopesModel26=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(slopesModel2,slopesModel21))
show(MixedModels.likelihoodratiotest(slopesModel2,slopesModel22))
show(MixedModels.likelihoodratiotest(slopesModel2,slopesModel23))
show(MixedModels.likelihoodratiotest(slopesModel2,slopesModel24))
show(MixedModels.likelihoodratiotest(slopesModel2,slopesModel25))
show(MixedModels.likelihoodratiotest(slopesModel2,slopesModel26))
#largest p for slopesModel25
slopesModel3=slopesModel25
#test large p-values again
#zerocorr|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              zerocorr(sexContrasts+nStimuliContrasts+blockNumeric+trialNumber|modelNumber))
@elapsed slopesModel31=fit(MixedModel,modelFormula,dataset,Binomial())
#sexContrasts|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (nStimuliContrasts+blockNumeric+trialNumber|modelNumber))
@elapsed slopesModel32=fit(MixedModel,modelFormula,dataset,Binomial())
#nStimuliContrasts|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (sexContrasts+blockNumeric+trialNumber|modelNumber))
@elapsed slopesModel33=fit(MixedModel,modelFormula,dataset,Binomial())
#blockNumeric|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (sexContrasts+nStimuliContrasts+trialNumber|modelNumber))
@elapsed slopesModel34=fit(MixedModel,modelFormula,dataset,Binomial())
#trialNumber|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (sexContrasts+nStimuliContrasts+blockNumeric|modelNumber))
@elapsed slopesModel35=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(slopesModel3,slopesModel31))
show(MixedModels.likelihoodratiotest(slopesModel3,slopesModel32))
show(MixedModels.likelihoodratiotest(slopesModel3,slopesModel33))
show(MixedModels.likelihoodratiotest(slopesModel3,slopesModel34))
show(MixedModels.likelihoodratiotest(slopesModel3,slopesModel35))
#largest p for slopesModel34
slopesModel4=slopesModel34
#test large p-values again
#zerocorr|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              zerocorr(sexContrasts+nStimuliContrasts+trialNumber|modelNumber))
@elapsed slopesModel41=fit(MixedModel,modelFormula,dataset,Binomial())
#sexContrasts|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (nStimuliContrasts+trialNumber|modelNumber))
@elapsed slopesModel42=fit(MixedModel,modelFormula,dataset,Binomial())
#nStimuliContrasts|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (sexContrasts+trialNumber|modelNumber))
@elapsed slopesModel43=fit(MixedModel,modelFormula,dataset,Binomial())
#trialNumber|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (sexContrasts+nStimuliContrasts|modelNumber))
@elapsed slopesModel44=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(slopesModel4,slopesModel41))
show(MixedModels.likelihoodratiotest(slopesModel4,slopesModel42))
show(MixedModels.likelihoodratiotest(slopesModel4,slopesModel43))
show(MixedModels.likelihoodratiotest(slopesModel4,slopesModel44))
#largest p for slopesModel44
slopesModel5=slopesModel44
#test large p-values again
#zerocorr|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              zerocorr(sexContrasts+nStimuliContrasts|modelNumber))
@elapsed slopesModel51=fit(MixedModel,modelFormula,dataset,Binomial())
#sexContrasts|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (nStimuliContrasts|modelNumber))
@elapsed slopesModel52=fit(MixedModel,modelFormula,dataset,Binomial())
#nStimuliContrasts|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (sexContrasts|modelNumber))
@elapsed slopesModel53=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(slopesModel5,slopesModel51))
show(MixedModels.likelihoodratiotest(slopesModel5,slopesModel52))
show(MixedModels.likelihoodratiotest(slopesModel5,slopesModel53))
#largest p for slopesModel52
slopesModel6=slopesModel52
#reduce
#zerocorr|ID
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              zerocorr(nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (nStimuliContrasts|modelNumber))
@elapsed slopesModel61=fit(MixedModel,modelFormula,dataset,Binomial())
#nStimuliContrasts*typeContrasts|ID
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts+typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (nStimuliContrasts|modelNumber))
@elapsed slopesModel62=fit(MixedModel,modelFormula,dataset,Binomial())
#blockNumeric|ID
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+deg+trialNumber|ID)+
              (nStimuliContrasts|modelNumber))
@elapsed slopesModel63=fit(MixedModel,modelFormula,dataset,Binomial())
#deg|ID
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+trialNumber|ID)+
              (nStimuliContrasts|modelNumber))
@elapsed slopesModel64=fit(MixedModel,modelFormula,dataset,Binomial())
#trialNumber|ID
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg|ID)+
              (nStimuliContrasts|modelNumber))
@elapsed slopesModel65=fit(MixedModel,modelFormula,dataset,Binomial())
#zerocorr|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              zerocorr(nStimuliContrasts|modelNumber))
@elapsed slopesModel66=fit(MixedModel,modelFormula,dataset,Binomial())
#nStimuliContrasts|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed slopesModel67=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(slopesModel6,slopesModel61))
show(MixedModels.likelihoodratiotest(slopesModel6,slopesModel62))
show(MixedModels.likelihoodratiotest(slopesModel6,slopesModel63))
show(MixedModels.likelihoodratiotest(slopesModel6,slopesModel64))
show(MixedModels.likelihoodratiotest(slopesModel6,slopesModel65))
show(MixedModels.likelihoodratiotest(slopesModel6,slopesModel66))
show(MixedModels.likelihoodratiotest(slopesModel6,slopesModel67))
#all <.2
#test effect of order?
modelFormula=@formula(responseCorrect~typeContrasts*sexContrasts*nStimuliContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (nStimuliContrasts|modelNumber))
@elapsed slopesModel68=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(slopesModel6,slopesModel68))
#good

#get significance of fixed effects
fixedEffects1=slopesModel6
#sexContrasts*nStimuliContrasts*typeContrasts
modelFormula=@formula(responseCorrect~nStimuliContrasts*typeContrasts+
              sexContrasts*typeContrasts+
              sexContrasts*nStimuliContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (nStimuliContrasts|modelNumber))
@elapsed fixedEffects11=fit(MixedModel,modelFormula,dataset,Binomial())
#blockNumeric
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (nStimuliContrasts|modelNumber))
@elapsed fixedEffects12=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (nStimuliContrasts|modelNumber))
@elapsed fixedEffects13=fit(MixedModel,modelFormula,dataset,Binomial())
#trialNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (nStimuliContrasts|modelNumber))
@elapsed fixedEffects14=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(fixedEffects1,fixedEffects11))
show(MixedModels.likelihoodratiotest(fixedEffects1,fixedEffects12))
show(MixedModels.likelihoodratiotest(fixedEffects1,fixedEffects13))
show(MixedModels.likelihoodratiotest(fixedEffects1,fixedEffects14))
#only 11 n.s.
fixedEffects2=fixedEffects11
#reduce
#nStimuliContrasts*typeContrasts
modelFormula=@formula(responseCorrect~
              sexContrasts*typeContrasts+
              sexContrasts*nStimuliContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (nStimuliContrasts|modelNumber))
@elapsed fixedEffects21=fit(MixedModel,modelFormula,dataset,Binomial())
#sexContrasts*typeContrasts
modelFormula=@formula(responseCorrect~nStimuliContrasts*typeContrasts+
              sexContrasts*nStimuliContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (nStimuliContrasts|modelNumber))
@elapsed fixedEffects22=fit(MixedModel,modelFormula,dataset,Binomial())
#sexContrasts*nStimuliContrasts+
modelFormula=@formula(responseCorrect~nStimuliContrasts*typeContrasts+
              sexContrasts*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (nStimuliContrasts|modelNumber))
@elapsed fixedEffects23=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(fixedEffects2,fixedEffects21))
show(MixedModels.likelihoodratiotest(fixedEffects2,fixedEffects22))
show(MixedModels.likelihoodratiotest(fixedEffects2,fixedEffects23))
#lowest lrt for 22 (negative?)
fixedEffects3=fixedEffects22
#nStimuliContrasts*typeContrasts+
modelFormula=@formula(responseCorrect~typeContrasts+
              sexContrasts*nStimuliContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (nStimuliContrasts|modelNumber))
@elapsed fixedEffects31=fit(MixedModel,modelFormula,dataset,Binomial())
#sexContrasts*nStimuliContrasts
modelFormula=@formula(responseCorrect~nStimuliContrasts*typeContrasts+
              sexContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (nStimuliContrasts|modelNumber))
@elapsed fixedEffects32=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(fixedEffects3,fixedEffects31))
show(MixedModels.likelihoodratiotest(fixedEffects3,fixedEffects32))
#larger p for 31
fixedEffects4=fixedEffects31
#typeContrasts
modelFormula=@formula(responseCorrect~
              sexContrasts*nStimuliContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (nStimuliContrasts|modelNumber))
@elapsed fixedEffects41=fit(MixedModel,modelFormula,dataset,Binomial())
#sexContrasts*nStimuliContrasts
modelFormula=@formula(responseCorrect~typeContrasts+
              sexContrasts+nStimuliContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (nStimuliContrasts|modelNumber))
@elapsed fixedEffects42=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(fixedEffects4,fixedEffects41))
show(MixedModels.likelihoodratiotest(fixedEffects4,fixedEffects42))
