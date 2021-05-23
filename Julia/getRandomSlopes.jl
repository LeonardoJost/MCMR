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

dataset.responseCorrect=dataset.type.=="hit"

##random slopes
#start with maximal model
#(do not use interactions to avoid overparametrization, check later for interactions of interest)
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              block+deg+itemNumber+
              (nStimuliFactor+typeContrasts+block+deg+itemNumber|ID)+
              (sexContrasts+nStimuliFactor+typeContrasts+block+STEM+Experience+deg+itemNumber|modelNumber))
@elapsed gm1=fit(MixedModel,modelFormula,dataset,Binomial())
show(gm1.optsum)
#remove random correlation
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              block+deg+itemNumber+
              zerocorr(nStimuliFactor+typeContrasts+block+deg+itemNumber|ID)+
              zerocorr(sexContrasts+nStimuliFactor+typeContrasts+block+STEM+Experience+deg+itemNumber|modelNumber))
@elapsed gm2=fit(MixedModel,modelFormula,dataset,Binomial())
show(VarCorr(gm2))
show(MixedModels.likelihoodratiotest(gm1,gm2))
#gm2 is better than gm1
#remove (deg+itemNumber|ID)+(deg+type+itemNumber+sexContrasts|modelNumber) (variance <.001)
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              block+deg+itemNumber+
              zerocorr(nStimuliFactor+typeContrasts+block|ID)+
              zerocorr(sexContrasts+nStimuliFactor+block+STEM+Experience|modelNumber))
@elapsed gm3=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm2,gm3))
show(VarCorr(gm3))
#gm3 is better
#remove (sexContrasts+STEM+nStimuliFactor+block|modelNumber) (variance/components <.002)
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              block+deg+itemNumber+
              zerocorr(nStimuliFactor+typeContrasts+block|ID)+
              zerocorr(Experience|modelNumber))
@elapsed gm4=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm3,gm4))
show(VarCorr(gm4))
#gm4 is better
#test removal of (block|ID) (lowest variance component)
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              block+deg+itemNumber+
              zerocorr(nStimuliFactor+typeContrasts|ID)+
              zerocorr(Experience|modelNumber))
@elapsed gm5=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm4,gm5))
#gm4 is better
#test removal (typeContrasts|ID) (lowest variance)
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              block+deg+itemNumber+
              zerocorr(nStimuliFactor+block|ID)+
              zerocorr(Experience|modelNumber))
@elapsed gm6=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm4,gm6))
#gm4 is better

#readd random correlation
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              block+deg+itemNumber+
              (nStimuliFactor+typeContrasts+block|ID)+
              (Experience|modelNumber))
@elapsed gm7=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm4,gm7))
#gm7 is good
#test for removal of individual random slopes
#nStimuliFactor|ID
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              block+deg+itemNumber+
              (typeContrasts+block|ID)+
              (Experience|modelNumber))
@elapsed gm8=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm7,gm8))
#typeContrasts|ID
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              block+deg+itemNumber+
              (nStimuliFactor+block|ID)+
              (Experience|modelNumber))
@elapsed gm9=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm7,gm9))
#block|ID
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              block+deg+itemNumber+
              (nStimuliFactor+typeContrasts|ID)+
              (Experience|modelNumber))
@elapsed gm10=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm7,gm10))
#Experience|modelNumber
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              block+deg+itemNumber+
              (nStimuliFactor+typeContrasts+block|ID)+
              (1|modelNumber))
@elapsed gm11=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm7,gm11))
#gm7 is best model

#reduce nStimuli and block to numeric instead of factors
#nStimuli
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              block+deg+itemNumber+
              (nStimuli+typeContrasts+block|ID)+
              (Experience|modelNumber))
@elapsed gm12=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm7,gm12))
#block
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              block+deg+itemNumber+
              (nStimuliFactor+typeContrasts+blockNumeric|ID)+
              (Experience|modelNumber))
@elapsed gm13=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm7,gm13))
#gm7 is best
#check for random slope of interaction
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              block+deg+itemNumber+
              (nStimuliFactor*typeContrasts+block|ID)+
              (Experience|modelNumber))
@elapsed gm14=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm7,gm14))
#gm7 is best
