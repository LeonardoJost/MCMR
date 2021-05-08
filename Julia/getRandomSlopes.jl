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
show(IOContext(stdout, :limit=>false),dataset[115:1000,[:type, :deg]])
show(eltype.(eachcol(dataset)))
#remove outliers
dataset=dataset[.!dataset.outlier,:]
dataset=dataset[dataset[!,:sex].!="NA",:]
#convert types to numeric
for i in [:sexNumeric]
  dataset[!,i]=tryparse.(Int,dataset[!,i])
end
#add block as numeric variable
dataset.blockNumeric=tryparse.(Int,chop.(dataset.block,head=4,tail=0))
dataset.nStimuliFactor=string.(dataset.nStimuli)

dataset.responseCorrect=dataset.type.=="hit"

##random slopes
#start with maximal model
#(do not use interactions to avoid overparametrization, check later for interactions of interest)
modelFormula=@formula(responseCorrect~STEM*sexNumeric*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*nStimuliFactor*typeNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block+deg+itemNumber|ID)+
              (sexNumeric+nStimuliFactor+typeNumeric+block+STEM+Experience+deg+itemNumber|modelNumber))
@elapsed gm1=fit(MixedModel,modelFormula,dataset,Binomial())
show(gm1.optsum)
#remove random correlation
modelFormula=@formula(responseCorrect~STEM*sexNumeric*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*nStimuliFactor*typeNumeric+
              block+deg+itemNumber+
              zerocorr(nStimuliFactor+typeNumeric+block+deg+itemNumber|ID)+
              zerocorr(sexNumeric+nStimuliFactor+typeNumeric+block+STEM+Experience+deg+itemNumber|modelNumber))
@elapsed gm2=fit(MixedModel,modelFormula,dataset,Binomial())
show(VarCorr(gm2))
show(MixedModels.likelihoodratiotest(gm1,gm2))
#gm2 is better than gm1
#remove (deg+type+itemNumber+sexNumeric|modelNumber) (variance <.001)
modelFormula=@formula(responseCorrect~STEM*sexNumeric*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*nStimuliFactor*typeNumeric+
              block+deg+itemNumber+
              zerocorr(nStimuliFactor+typeNumeric+block+deg+itemNumber|ID)+
              zerocorr(nStimuliFactor+block+STEM+Experience|modelNumber))
@elapsed gm3=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm2,gm3))
show(VarCorr(gm3))
#gm3 is better
#remove (deg+itemNumber|ID) (variance <.001)
modelFormula=@formula(responseCorrect~STEM*sexNumeric*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*nStimuliFactor*typeNumeric+
              block+deg+itemNumber+
              zerocorr(nStimuliFactor+typeNumeric+block|ID)+
              zerocorr(nStimuliFactor+block+STEM+Experience|modelNumber))
@elapsed gm4=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm3,gm4))
show(VarCorr(gm4))
#gm4 is better
#remove (block+STEM|modelNumber) (variance components <.001)
modelFormula=@formula(responseCorrect~STEM*sexNumeric*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*nStimuliFactor*typeNumeric+
              block+deg+itemNumber+
              zerocorr(nStimuliFactor+typeNumeric+block|ID)+
              zerocorr(nStimuliFactor+Experience|modelNumber))
@elapsed gm5=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm4,gm5))
show(VarCorr(gm5))
#gm5 is better
#test removal (nStimuliFactor|modelNumber) (lowest variance components)
modelFormula=@formula(responseCorrect~STEM*sexNumeric*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*nStimuliFactor*typeNumeric+
              block+deg+itemNumber+
              zerocorr(nStimuliFactor+typeNumeric+block|ID)+
              zerocorr(Experience|modelNumber))
@elapsed gm6=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm5,gm6))
#gm5 is better
#test removal (Experience|modelNumber) (low variance components)
modelFormula=@formula(responseCorrect~STEM*sexNumeric*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*nStimuliFactor*typeNumeric+
              block+deg+itemNumber+
              zerocorr(nStimuliFactor+typeNumeric+block|ID)+
              zerocorr(nStimuliFactor|modelNumber))
@elapsed gm6a=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm5,gm6a))
#gm5 is better
#test removal (typeNumeric|ID) (low variance components)
modelFormula=@formula(responseCorrect~STEM*sexNumeric*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*nStimuliFactor*typeNumeric+
              block+deg+itemNumber+
              zerocorr(nStimuliFactor+block|ID)+
              zerocorr(nStimuliFactor+Experience|modelNumber))
@elapsed gm7=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm5,gm7))
#gm5 is better
#test removal (nStimuliFactor|ID)
modelFormula=@formula(responseCorrect~STEM*sexNumeric*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*nStimuliFactor*typeNumeric+
              block+deg+itemNumber+
              zerocorr(typeNumeric+block|ID)+
              zerocorr(nStimuliFactor+Experience|modelNumber))
@elapsed gm8=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm5,gm8))
#gm5 is better
#test removal (block|ID)
modelFormula=@formula(responseCorrect~STEM*sexNumeric*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*nStimuliFactor*typeNumeric+
              block+deg+itemNumber+
              zerocorr(nStimuliFactor+typeNumeric|ID)+
              zerocorr(nStimuliFactor+Experience|modelNumber))
@elapsed gm9=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm5,gm9))
#gm5 is better
#readd random correlation
modelFormula=@formula(responseCorrect~STEM*sexNumeric*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*nStimuliFactor*typeNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (nStimuliFactor+Experience|modelNumber))
@elapsed gm10=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm5,gm10))
#gm10 is good
#test for removal of individual random slopes
#nStimuliFactor|ID
modelFormula=@formula(responseCorrect~STEM*sexNumeric*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*nStimuliFactor*typeNumeric+
              block+deg+itemNumber+
              (typeNumeric+block|ID)+
              (nStimuliFactor+Experience|modelNumber))
@elapsed gm11=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm10,gm11))
#typeNumeric|ID
modelFormula=@formula(responseCorrect~STEM*sexNumeric*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*nStimuliFactor*typeNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+block|ID)+
              (nStimuliFactor+Experience|modelNumber))
@elapsed gm12=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm10,gm12))
#block|ID
modelFormula=@formula(responseCorrect~STEM*sexNumeric*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*nStimuliFactor*typeNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric|ID)+
              (nStimuliFactor+Experience|modelNumber))
@elapsed gm13=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm10,gm13))
#Experience|modelNumber
modelFormula=@formula(responseCorrect~STEM*sexNumeric*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*nStimuliFactor*typeNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (nStimuliFactor|modelNumber))
@elapsed gm14=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm10,gm14))
#nStimuliFactor|modelNumber
modelFormula=@formula(responseCorrect~STEM*sexNumeric*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*nStimuliFactor*typeNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm15=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm10,gm15))
#gm15 is best model

#reduce nStimuli and block to numeric instead of factors
#nStimuli
modelFormula=@formula(responseCorrect~STEM*sexNumeric*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*nStimuliFactor*typeNumeric+
              block+deg+itemNumber+
              (nStimuli+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm16=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm15,gm16))
#block
modelFormula=@formula(responseCorrect~STEM*sexNumeric*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*nStimuliFactor*typeNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+blockNumeric|ID)+
              (Experience|modelNumber))
@elapsed gm17=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm15,gm17))
#gm15 is best
#check for random slope of interaction
modelFormula=@formula(responseCorrect~STEM*sexNumeric*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*nStimuliFactor*typeNumeric+
              block+deg+itemNumber+
              (nStimuliFactor*typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm18=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm15,gm18))
#gm15 is better
