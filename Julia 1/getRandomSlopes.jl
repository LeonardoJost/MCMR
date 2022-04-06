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
dataset.nStimuli=dataset.nStimuli.-minimum(dataset.nStimuli)
dataset.deg=dataset.deg./maximum(dataset.deg)
dataset.blockNumeric=dataset.blockNumeric./maximum(dataset.blockNumeric)
dataset.nStimuli=dataset.nStimuli./maximum(dataset.nStimuli)

dataset.responseCorrect=dataset.type.=="hit"

##random slopes
#start with maximal model
#(do not use interactions to avoid overparametrization, check later for interactions of interest)
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (sexContrasts+nStimuliFactor+typeContrasts+blockNumeric+STEM+Experience+deg+trialNumber|modelNumber))
@elapsed slopesModel1=fit(MixedModel,modelFormula,dataset,Binomial())
show(slopesModel1.optsum)
#remove random correlation
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              zerocorr(nStimuliFactor+typeContrasts+blockNumeric+deg+trialNumber|ID)+
              zerocorr(sexContrasts+nStimuliFactor+typeContrasts+blockNumeric+STEM+Experience+deg+trialNumber|modelNumber))
@elapsed slopesModel2=fit(MixedModel,modelFormula,dataset,Binomial())
show(VarCorr(slopesModel2))
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel2))
#slopesModel2 is better than slopesModel1
#remove (typeContrasts+STEM|modelNumber) (variance <.001)
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              zerocorr(nStimuliFactor+typeContrasts+blockNumeric+deg+trialNumber|ID)+
              zerocorr(sexContrasts+nStimuliFactor+blockNumeric+Experience+deg+trialNumber|modelNumber))
@elapsed slopesModel3=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(slopesModel2,slopesModel3))
show(VarCorr(slopesModel3))
#slopesModel3 is better
#remove (sexContrasts+nStimuliFactor+deg|modelNumber) (variance <.01)
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              zerocorr(nStimuliFactor+typeContrasts+blockNumeric+deg+trialNumber|ID)+
              zerocorr(blockNumeric+Experience+trialNumber|modelNumber))
@elapsed slopesModel4=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(slopesModel3,slopesModel4))
show(VarCorr(slopesModel4))
#slopesModel4 is better
#test removal of low variance components
#(blockNumeric|modelNumber)
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              zerocorr(nStimuliFactor+typeContrasts+blockNumeric+deg+trialNumber|ID)+
              zerocorr(Experience+trialNumber|modelNumber))
@elapsed slopesModel5=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(slopesModel4,slopesModel5))
show(VarCorr(slopesModel5))
#slopesModel5 is better
#test removal of (trialNumber|modelNumber)
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              zerocorr(nStimuliFactor+typeContrasts+blockNumeric+deg+trialNumber|ID)+
              zerocorr(Experience|modelNumber))
@elapsed slopesModel6=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(slopesModel5,slopesModel6))
show(VarCorr(slopesModel6))
#slopesModel6 is better
#test removal of (typeContrasts|ID)
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              zerocorr(nStimuliFactor+blockNumeric+deg+trialNumber|ID)+
              zerocorr(Experience|modelNumber))
@elapsed slopesModel7=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(slopesModel6,slopesModel7))
#slopesModel6 is better
#test removal of (Experience|modelNumber)
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              zerocorr(nStimuliFactor+typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed slopesModel8=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(slopesModel6,slopesModel8))
show(VarCorr(slopesModel8))
#slopesModel6 is better

#readd random correlation
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed slopesModel9=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(slopesModel8,slopesModel9))
show(slopesModel9)
#slopesModel9 is good
#test for removal of individual random slopes
#nStimuliFactor|ID
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed slopesModel10=fit(MixedModel,modelFormula,dataset,Binomial())
#typeContrasts|ID
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+deg+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed slopesModel11=fit(MixedModel,modelFormula,dataset,Binomial())
#blockNumeric|ID
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+deg+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed slopesModel12=fit(MixedModel,modelFormula,dataset,Binomial())
#deg|ID
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed slopesModel13=fit(MixedModel,modelFormula,dataset,Binomial())
#trialNumber|ID
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+deg|ID)+
              (Experience|modelNumber))
@elapsed slopesModel14=fit(MixedModel,modelFormula,dataset,Binomial())
#Experience|modelNumber
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (1|modelNumber))
@elapsed slopesModel15=fit(MixedModel,modelFormula,dataset,Binomial())

#comparison
show(MixedModels.likelihoodratiotest(slopesModel9,slopesModel10))
show(MixedModels.likelihoodratiotest(slopesModel9,slopesModel11))
show(MixedModels.likelihoodratiotest(slopesModel9,slopesModel12))
show(MixedModels.likelihoodratiotest(slopesModel9,slopesModel13))
show(MixedModels.likelihoodratiotest(slopesModel9,slopesModel14))
show(MixedModels.likelihoodratiotest(slopesModel9,slopesModel15))
#all significant

#comparison model with different order
modelFormula=@formula(responseCorrect~
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              (nStimuliFactor+typeContrasts+blockNumeric+deg+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed slopesModel91=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(slopesModel9,slopesModel91))
#difference in deviance of .8, reduce model by deg (best model before)
modelFormula=@formula(responseCorrect~
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed slopesModel131=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(slopesModel13,slopesModel131))
#much better difference <.2
#test reducing other random slopes
slopesModel16=slopesModel13
#nStimuliFactor|ID
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed slopesModel17=fit(MixedModel,modelFormula,dataset,Binomial())
#typeContrasts|ID
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed slopesModel18=fit(MixedModel,modelFormula,dataset,Binomial())
#blockNumeric|ID
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed slopesModel19=fit(MixedModel,modelFormula,dataset,Binomial())
#trialNumber|ID
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric|ID)+
              (Experience|modelNumber))
@elapsed slopesModel20=fit(MixedModel,modelFormula,dataset,Binomial())
#Experience|modelNumber
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (1|modelNumber))
@elapsed slopesModel21=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(slopesModel16,slopesModel17))
show(MixedModels.likelihoodratiotest(slopesModel16,slopesModel18))
show(MixedModels.likelihoodratiotest(slopesModel16,slopesModel19))
show(MixedModels.likelihoodratiotest(slopesModel16,slopesModel20))
show(MixedModels.likelihoodratiotest(slopesModel16,slopesModel21))
#keep slopesModel16
#test reduction of nStimuli to numeric
modelFormula=@formula(responseCorrect~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuliFactor*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuliFactor*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuli+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber))
@elapsed slopesModel22=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(slopesModel16,slopesModel22))
