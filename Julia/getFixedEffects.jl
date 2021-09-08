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
#comparison
show(MixedModels.likelihoodratiotest(gm0,gm01))
show(MixedModels.likelihoodratiotest(gm0,gm02))
show(MixedModels.likelihoodratiotest(gm0,gm03))
show(MixedModels.likelihoodratiotest(gm0,gm04))
show(MixedModels.likelihoodratiotest(gm0,gm05))
#only gm02 n.s.
gm1=gm02


##reduce gm1 by all possible fixed effects
#STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts
modelFormula=@formula(responseCorrect~ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts2*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts2*ExperienceContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts2*ExperienceContrasts*sexContrasts*typeContrasts+
              STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor+
              STEMContrasts1*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm11=fit(MixedModel,modelFormula,dataset,Binomial())
#STEMContrasts1*nStimuliFactor*typeContrasts
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm12=fit(MixedModel,modelFormula,dataset,Binomial())
#STEMContrasts1*sexContrasts*typeContrasts
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm13=fit(MixedModel,modelFormula,dataset,Binomial())
#STEMContrasts1*sexContrasts*nStimuliFactor
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm14=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm1,gm11))
show(MixedModels.likelihoodratiotest(gm1,gm12))
show(MixedModels.likelihoodratiotest(gm1,gm13))
show(MixedModels.likelihoodratiotest(gm1,gm14))
#largest p for gm12
gm2=gm12
##reduce gm2 by all possible fixed effects
#STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts
modelFormula=@formula(responseCorrect~ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts2*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts2*ExperienceContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts2*ExperienceContrasts*sexContrasts*typeContrasts+
              STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor+
              STEMContrasts1*sexContrasts*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm21=fit(MixedModel,modelFormula,dataset,Binomial())
#STEMContrasts1*sexContrasts*typeContrasts
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm22=fit(MixedModel,modelFormula,dataset,Binomial())
#STEMContrasts1*sexContrasts*nStimuliFactor
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*typeContrasts+
              STEMContrasts1*nStimuliFactor+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm23=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm2,gm21))
show(MixedModels.likelihoodratiotest(gm2,gm22))
show(MixedModels.likelihoodratiotest(gm2,gm23))
#only gm22 n.s.
gm3=gm22
##reduce gm3 by all possible fixed effects
#STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts
modelFormula=@formula(responseCorrect~ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts2*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts2*ExperienceContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts2*ExperienceContrasts*sexContrasts*typeContrasts+
              STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor+
              STEMContrasts1*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm31=fit(MixedModel,modelFormula,dataset,Binomial())
#STEMContrasts1*typeContrasts
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm32=fit(MixedModel,modelFormula,dataset,Binomial())
#STEMContrasts1*sexContrasts*nStimuliFactor
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*typeContrasts+
              STEMContrasts1*nStimuliFactor+
              STEMContrasts1*sexContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm33=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm3,gm31))
show(MixedModels.likelihoodratiotest(gm3,gm32))
show(MixedModels.likelihoodratiotest(gm3,gm33))
#only gm32 n.s.
gm4=gm32
#STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts
modelFormula=@formula(responseCorrect~ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts2*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts2*ExperienceContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts2*ExperienceContrasts*sexContrasts*typeContrasts+
              STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor+
              STEMContrasts1*sexContrasts*nStimuliFactor+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm41=fit(MixedModel,modelFormula,dataset,Binomial())
#STEMContrasts1*sexContrasts*nStimuliFactor
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*nStimuliFactor+
              STEMContrasts1*sexContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm42=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm4,gm41))
show(MixedModels.likelihoodratiotest(gm4,gm42))
#all significant
#values for covariates
#blockNumeric
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor+
              deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm43=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor+
              blockNumeric+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm44=fit(MixedModel,modelFormula,dataset,Binomial())
#trialNumber
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor+
              blockNumeric+deg+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm45=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm4,gm43))
show(MixedModels.likelihoodratiotest(gm4,gm44))
show(MixedModels.likelihoodratiotest(gm4,gm45))


#check for overall main effects of interest
modelFormula=@formula(responseCorrect~sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm5=fit(MixedModel,modelFormula,dataset,Binomial())
#without triple interaction
modelFormula=@formula(responseCorrect~nStimuliFactor*typeContrasts+
              sexContrasts*typeContrasts+
              sexContrasts*nStimuliFactor+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm51=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm5,gm51))
#n.s.
gm6=gm51
#reduce gm6
#nStimuliFactor*typeContrasts
modelFormula=@formula(responseCorrect~
              sexContrasts*typeContrasts+
              sexContrasts*nStimuliFactor+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm61=fit(MixedModel,modelFormula,dataset,Binomial())
#sexContrasts*typeContrasts
modelFormula=@formula(responseCorrect~nStimuliFactor*typeContrasts+
              sexContrasts*nStimuliFactor+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm62=fit(MixedModel,modelFormula,dataset,Binomial())
#sexContrasts*nStimuliFactor
modelFormula=@formula(responseCorrect~nStimuliFactor*typeContrasts+
              sexContrasts*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm63=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm6,gm61))
show(MixedModels.likelihoodratiotest(gm6,gm62))
show(MixedModels.likelihoodratiotest(gm6,gm63))
#largest p for gm62
gm7=gm62
#nStimuliFactor*typeContrasts
modelFormula=@formula(responseCorrect~typeContrasts+
              sexContrasts*nStimuliFactor+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm71=fit(MixedModel,modelFormula,dataset,Binomial())
#sexContrasts*nStimuliFactor
modelFormula=@formula(responseCorrect~nStimuliFactor*typeContrasts+
              sexContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm72=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm7,gm71))
show(MixedModels.likelihoodratiotest(gm7,gm72))
#gm72 n.s.
gm8=gm72
#nStimuliFactor*typeContrasts
modelFormula=@formula(responseCorrect~nStimuliFactor+typeContrasts+
              sexContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm81=fit(MixedModel,modelFormula,dataset,Binomial())
#sexContrasts
modelFormula=@formula(responseCorrect~nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm82=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm8,gm81))
show(MixedModels.likelihoodratiotest(gm8,gm82))
#both significant
#effect of type*sex
modelFormula=@formula(responseCorrect~nStimuliFactor*typeContrasts+
              sexContrasts*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed gm83=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm8,gm83))
