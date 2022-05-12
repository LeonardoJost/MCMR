### test selection of random slopes and significance of fixed effects using binomial distribution
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
#remove random correlation by modelNumber
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
#all<.2
slopesModel6=slopesModel5
#test other
#zerocorr|ID
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              zerocorr(nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (sexContrasts+nStimuliContrasts|modelNumber))
@elapsed slopesModel61=fit(MixedModel,modelFormula,dataset,Binomial())
#nStimuliContrasts*typeContrasts|ID
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts+typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (sexContrasts+nStimuliContrasts|modelNumber))
@elapsed slopesModel62=fit(MixedModel,modelFormula,dataset,Binomial())
#blockNumeric|ID
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+deg+trialNumber|ID)+
              (sexContrasts+nStimuliContrasts|modelNumber))
@elapsed slopesModel63=fit(MixedModel,modelFormula,dataset,Binomial())
#deg|ID
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+trialNumber|ID)+
              (sexContrasts+nStimuliContrasts|modelNumber))
@elapsed slopesModel64=fit(MixedModel,modelFormula,dataset,Binomial())
#trialNumber|ID
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg|ID)+
              (sexContrasts+nStimuliContrasts|modelNumber))
@elapsed slopesModel65=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(slopesModel6,slopesModel61))
show(MixedModels.likelihoodratiotest(slopesModel6,slopesModel62))
show(MixedModels.likelihoodratiotest(slopesModel6,slopesModel63))
show(MixedModels.likelihoodratiotest(slopesModel6,slopesModel64))
show(MixedModels.likelihoodratiotest(slopesModel6,slopesModel65))
#all <.2
#test effect of order?
modelFormula=@formula(responseCorrect~typeContrasts*sexContrasts*nStimuliContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (sexContrasts+nStimuliContrasts|modelNumber))
@elapsed slopesModel66=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(slopesModel6,slopesModel66))
#further reduction necessary
#lowest variance and largest p for nStimuliContrasts|modelNumber
slopesModel7=slopesModel53
#test effect of order?
modelFormula=@formula(responseCorrect~typeContrasts*sexContrasts*nStimuliContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (sexContrasts|modelNumber))
@elapsed slopesModel71=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(slopesModel7,slopesModel71))
#further reduction necessary?
#lowest variance and largest p for sexContrasts|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed slopesModel8=fit(MixedModel,modelFormula,dataset,Binomial())
#different order
modelFormula=@formula(responseCorrect~typeContrasts*sexContrasts*nStimuliContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed slopesModel81=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(slopesModel81,slopesModel8))
#good
#test with added nStimuliContrasts|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (nStimuliContrasts|modelNumber))
@elapsed slopesModel9=fit(MixedModel,modelFormula,dataset,Binomial())
#different order
modelFormula=@formula(responseCorrect~typeContrasts*sexContrasts*nStimuliContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (nStimuliContrasts|modelNumber))
@elapsed slopesModel91=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(slopesModel9,slopesModel91))
#keep nStimuliContrasts|modelNumberout
#test removal of 1|modelNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID))
@elapsed slopesModel82=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(slopesModel8,slopesModel82))
#significant

#get significance of fixed effects
fixedEffects1=slopesModel8
#sexContrasts*nStimuliContrasts*typeContrasts
modelFormula=@formula(responseCorrect~nStimuliContrasts*typeContrasts+
              sexContrasts*typeContrasts+
              sexContrasts*nStimuliContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed fixedEffects11=fit(MixedModel,modelFormula,dataset,Binomial())
#blockNumeric
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed fixedEffects12=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed fixedEffects13=fit(MixedModel,modelFormula,dataset,Binomial())
#trialNumber
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliContrasts*typeContrasts+blockNumeric+deg+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
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
              (1|modelNumber))
@elapsed fixedEffects21=fit(MixedModel,modelFormula,dataset,Binomial())
#sexContrasts*typeContrasts
modelFormula=@formula(responseCorrect~nStimuliContrasts*typeContrasts+
              sexContrasts*nStimuliContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed fixedEffects22=fit(MixedModel,modelFormula,dataset,Binomial())
#sexContrasts*nStimuliContrasts+
modelFormula=@formula(responseCorrect~nStimuliContrasts*typeContrasts+
              sexContrasts*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed fixedEffects23=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(fixedEffects2,fixedEffects21))
show(MixedModels.likelihoodratiotest(fixedEffects2,fixedEffects22))
show(MixedModels.likelihoodratiotest(fixedEffects2,fixedEffects23))
#largest p for 22
fixedEffects3=fixedEffects22
#nStimuliContrasts*typeContrasts
modelFormula=@formula(responseCorrect~typeContrasts+
              sexContrasts*nStimuliContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed fixedEffects31=fit(MixedModel,modelFormula,dataset,Binomial())
#sexContrasts*nStimuliContrasts
modelFormula=@formula(responseCorrect~nStimuliContrasts*typeContrasts+
              sexContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
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
              (1|modelNumber))
@elapsed fixedEffects41=fit(MixedModel,modelFormula,dataset,Binomial())
#sexContrasts*nStimuliContrasts
modelFormula=@formula(responseCorrect~typeContrasts+
              sexContrasts+nStimuliContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed fixedEffects42=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(fixedEffects4,fixedEffects41))
show(MixedModels.likelihoodratiotest(fixedEffects4,fixedEffects42))
#42 n.s.
fixedEffects5=fixedEffects42
#typeContrasts
modelFormula=@formula(responseCorrect~
              sexContrasts+nStimuliContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed fixedEffects51=fit(MixedModel,modelFormula,dataset,Binomial())
#sexContrasts
modelFormula=@formula(responseCorrect~typeContrasts+
              nStimuliContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed fixedEffects52=fit(MixedModel,modelFormula,dataset,Binomial())
#nStimuliContrasts
modelFormula=@formula(responseCorrect~typeContrasts+
              sexContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed fixedEffects53=fit(MixedModel,modelFormula,dataset,Binomial())
#blockNumeric
modelFormula=@formula(responseCorrect~typeContrasts+
              sexContrasts+nStimuliContrasts+
              deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed fixedEffects54=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~typeContrasts+
              sexContrasts+nStimuliContrasts+
              blockNumeric+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed fixedEffects55=fit(MixedModel,modelFormula,dataset,Binomial())
#trialNumber
modelFormula=@formula(responseCorrect~typeContrasts+
              sexContrasts+nStimuliContrasts+
              blockNumeric+deg+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed fixedEffects56=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(fixedEffects5,fixedEffects51))
show(MixedModels.likelihoodratiotest(fixedEffects5,fixedEffects52))
show(MixedModels.likelihoodratiotest(fixedEffects5,fixedEffects53))
show(MixedModels.likelihoodratiotest(fixedEffects5,fixedEffects54))
show(MixedModels.likelihoodratiotest(fixedEffects5,fixedEffects55))
show(MixedModels.likelihoodratiotest(fixedEffects5,fixedEffects56))
#52 n.s.
fixedEffects6=fixedEffects52
#typeContrasts
modelFormula=@formula(responseCorrect~
              nStimuliContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed fixedEffects61=fit(MixedModel,modelFormula,dataset,Binomial())
#nStimuliContrasts
modelFormula=@formula(responseCorrect~typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed fixedEffects62=fit(MixedModel,modelFormula,dataset,Binomial())
#blockNumeric
modelFormula=@formula(responseCorrect~typeContrasts+
              nStimuliContrasts+
              deg+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed fixedEffects63=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~typeContrasts+
              nStimuliContrasts+
              blockNumeric+trialNumber+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed fixedEffects64=fit(MixedModel,modelFormula,dataset,Binomial())
#trialNumber
modelFormula=@formula(responseCorrect~typeContrasts+
              nStimuliContrasts+
              blockNumeric+deg+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed fixedEffects65=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(fixedEffects6,fixedEffects61))
show(MixedModels.likelihoodratiotest(fixedEffects6,fixedEffects62))
show(MixedModels.likelihoodratiotest(fixedEffects6,fixedEffects63))
show(MixedModels.likelihoodratiotest(fixedEffects6,fixedEffects64))
show(MixedModels.likelihoodratiotest(fixedEffects6,fixedEffects65))
#all significant
#test n.s. effects
#sex
show(MixedModels.likelihoodratiotest(fixedEffects6,fixedEffects5))
#nStimuli*sex
modelFormula=@formula(responseCorrect~typeContrasts+
              nStimuliContrasts+
              blockNumeric+deg+trialNumber+nStimuliContrasts&sexContrasts+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed fixedEffects66=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(fixedEffects6,fixedEffects66))
#nStimuli*type
modelFormula=@formula(responseCorrect~typeContrasts+
              nStimuliContrasts+
              blockNumeric+deg+trialNumber+nStimuliContrasts&typeContrasts+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed fixedEffects67=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(fixedEffects6,fixedEffects67))
#sex*type
modelFormula=@formula(responseCorrect~typeContrasts+
              nStimuliContrasts+
              blockNumeric+deg+trialNumber+sexContrasts&typeContrasts+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed fixedEffects68=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(fixedEffects6,fixedEffects68))
#sex*type*nStimuli
modelFormula=@formula(responseCorrect~typeContrasts+
              nStimuliContrasts+
              blockNumeric+deg+trialNumber+sexContrasts&typeContrasts&nStimuliContrasts+
              (nStimuliContrasts*typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed fixedEffects69=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(fixedEffects6,fixedEffects69))
