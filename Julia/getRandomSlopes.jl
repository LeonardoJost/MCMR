### test selection of random slopes
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

##random slopes
#start with maximal model
#(do not use interactions to avoid overparametrization, check later for interactions of interest)
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (sexContrasts+nStimuliFactor+typeContrasts+blockNumeric+STEM+Experience+deg+trialNumber|modelNumber))
@elapsed gm1=fit(MixedModel,modelFormula,dataset,Binomial())
show(gm1.optsum)
#remove random correlation
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              zerocorr(nStimuliFactor+typeContrasts+blockNumeric+deg+trialNumber|ID)+
              zerocorr(sexContrasts+nStimuliFactor+typeContrasts+blockNumeric+STEM+Experience+deg+trialNumber|modelNumber))
@elapsed gm2=fit(MixedModel,modelFormula,dataset,Binomial())
show(VarCorr(gm2))
show(MixedModels.likelihoodratiotest(gm1,gm2))
#gm2 is better than gm1
#remove (typeContrasts+STEM|modelNumber) (variance <.001)
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              zerocorr(nStimuliFactor+typeContrasts+blockNumeric+deg+trialNumber|ID)+
              zerocorr(sexContrasts+nStimuliFactor+blockNumeric+Experience+deg+trialNumber|modelNumber))
@elapsed gm3=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm2,gm3))
show(VarCorr(gm3))
#gm3 is better
#remove (sexContrasts+nStimuliFactor+deg|modelNumber) (variance <.01)
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              zerocorr(nStimuliFactor+typeContrasts+blockNumeric+deg+trialNumber|ID)+
              zerocorr(blockNumeric+Experience+trialNumber|modelNumber))
@elapsed gm4=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm3,gm4))
show(VarCorr(gm4))
#gm4 is better
#test removal of low variance components
#(blockNumeric|modelNumber)
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              zerocorr(nStimuliFactor+typeContrasts+blockNumeric+deg+trialNumber|ID)+
              zerocorr(Experience+trialNumber|modelNumber))
@elapsed gm5=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm4,gm5))
show(VarCorr(gm5))
#gm5 is better
#test removal of (trialNumber|modelNumber)
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              zerocorr(nStimuliFactor+typeContrasts+blockNumeric+deg+trialNumber|ID)+
              zerocorr(Experience|modelNumber))
@elapsed gm6=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm5,gm6))
show(VarCorr(gm6))
#gm6 is better
#test removal of (typeContrasts|ID)
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              zerocorr(nStimuliFactor+blockNumeric+deg+trialNumber|ID)+
              zerocorr(Experience|modelNumber))
@elapsed gm7=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm6,gm7))
#gm6 is better
#test removal of (Experience|modelNumber)
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              zerocorr(nStimuliFactor+typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed gm8=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm6,gm8))
show(VarCorr(gm8))
#gm6 is better

#readd random correlation
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm9=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm8,gm9))
show(gm9)
#gm9 is good
#test for removal of individual random slopes
#nStimuliFactor|ID
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm10=fit(MixedModel,modelFormula,dataset,Binomial())
#typeContrasts|ID
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+deg+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm11=fit(MixedModel,modelFormula,dataset,Binomial())
#blockNumeric|ID
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+deg+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm12=fit(MixedModel,modelFormula,dataset,Binomial())
#deg|ID
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm13=fit(MixedModel,modelFormula,dataset,Binomial())
#trialNumber|ID
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+deg|ID)+
              (Experience|modelNumber))
@elapsed gm14=fit(MixedModel,modelFormula,dataset,Binomial())
#Experience|modelNumber
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed gm15=fit(MixedModel,modelFormula,dataset,Binomial())

#comparison
show(MixedModels.likelihoodratiotest(gm9,gm10))
show(MixedModels.likelihoodratiotest(gm9,gm11))
show(MixedModels.likelihoodratiotest(gm9,gm12))
show(MixedModels.likelihoodratiotest(gm9,gm13))
show(MixedModels.likelihoodratiotest(gm9,gm14))
show(MixedModels.likelihoodratiotest(gm9,gm15))
#all significant

#comparison model with different order
modelFormula=@formula(responseCorrect~
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              (nStimuliFactor+typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm91=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm9,gm91))
#difference in deviance of .8, reduce model by deg (best model before)
modelFormula=@formula(responseCorrect~
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm131=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm13,gm131))
#much better difference <.2
#test reducing other random slopes
gm16=gm13
#nStimuliFactor|ID
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm17=fit(MixedModel,modelFormula,dataset,Binomial())
#typeContrasts|ID
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm18=fit(MixedModel,modelFormula,dataset,Binomial())
#blockNumeric|ID
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm19=fit(MixedModel,modelFormula,dataset,Binomial())
#trialNumber|ID
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric|ID)+
              (Experience|modelNumber))
@elapsed gm20=fit(MixedModel,modelFormula,dataset,Binomial())
#Experience|modelNumber
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed gm21=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm16,gm17))
show(MixedModels.likelihoodratiotest(gm16,gm18))
show(MixedModels.likelihoodratiotest(gm16,gm19))
show(MixedModels.likelihoodratiotest(gm16,gm20))
show(MixedModels.likelihoodratiotest(gm16,gm21))
#keep gm16
