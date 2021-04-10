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
using StatsModels

#read data
dataset=CSV.read("dataset\\dataset.csv", DataFrame)
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

#model after random slope selection
modelFormula=@formula(responseCorrect~sexNumeric*nStimuliFactor*typeNumeric+
              block+STEM+Experience+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm0=fit(MixedModel,modelFormula,dataset,Binomial())
#test for additional interaction with STEM and Experience
#STEM
modelFormula=@formula(responseCorrect~sexNumeric*nStimuliFactor*typeNumeric*STEM+
              block+Experience+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm1=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm0,gm1))
#Experience
modelFormula=@formula(responseCorrect~sexNumeric*nStimuliFactor*typeNumeric*Experience+
              block+STEM+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm2=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(gm0,gm2))
#gm0 is best
##reduce fixed effects by nonsignificant effects
#reduce triple interaction
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              sexNumeric*typeNumeric+
              sexNumeric*nStimuliFactor+
              block+STEM+Experience+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm01=fit(MixedModel,modelFormula,dataset,Binomial())
#block
modelFormula=@formula(responseCorrect~sexNumeric*nStimuliFactor*typeNumeric+
              STEM+Experience+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm02=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM
modelFormula=@formula(responseCorrect~sexNumeric*nStimuliFactor*typeNumeric+
              block+Experience+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm03=fit(MixedModel,modelFormula,dataset,Binomial())
#Experience
modelFormula=@formula(responseCorrect~sexNumeric*nStimuliFactor*typeNumeric+
              block+STEM+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm04=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~sexNumeric*nStimuliFactor*typeNumeric+
              block+STEM+Experience+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm05=fit(MixedModel,modelFormula,dataset,Binomial())
#itemNumber
modelFormula=@formula(responseCorrect~sexNumeric*nStimuliFactor*typeNumeric+
              block+STEM+Experience+deg+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm06=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm0,gm01))
show(MixedModels.likelihoodratiotest(gm0,gm02))
show(MixedModels.likelihoodratiotest(gm0,gm03))
show(MixedModels.likelihoodratiotest(gm0,gm04))
show(MixedModels.likelihoodratiotest(gm0,gm05))
show(MixedModels.likelihoodratiotest(gm0,gm06))
#only gm01 n.s.
gm1=gm01
##reduce gm1 by all possible fixed effects
#nStimuliFactor*typeNumeric
modelFormula=@formula(responseCorrect~sexNumeric*typeNumeric+
              sexNumeric*nStimuliFactor+
              block+STEM+Experience+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm11=fit(MixedModel,modelFormula,dataset,Binomial())
#sexNumeric*typeNumeric
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              sexNumeric*nStimuliFactor+
              block+STEM+Experience+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm12=fit(MixedModel,modelFormula,dataset,Binomial())
#sexNumeric*nStimuliFactor
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              sexNumeric*typeNumeric+
              block+STEM+Experience+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm13=fit(MixedModel,modelFormula,dataset,Binomial())
#block
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              sexNumeric*typeNumeric+
              sexNumeric*nStimuliFactor+
              STEM+Experience+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm14=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              sexNumeric*typeNumeric+
              sexNumeric*nStimuliFactor+
              block+Experience+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm15=fit(MixedModel,modelFormula,dataset,Binomial())
#Experience
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              sexNumeric*typeNumeric+
              sexNumeric*nStimuliFactor+
              block+STEM+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm16=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              sexNumeric*typeNumeric+
              sexNumeric*nStimuliFactor+
              block+STEM+Experience+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm17=fit(MixedModel,modelFormula,dataset,Binomial())
#itemNumber
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              sexNumeric*typeNumeric+
              sexNumeric*nStimuliFactor+
              block+STEM+Experience+deg+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm18=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm1,gm11))
show(MixedModels.likelihoodratiotest(gm1,gm12))
show(MixedModels.likelihoodratiotest(gm1,gm13))
show(MixedModels.likelihoodratiotest(gm1,gm14))
show(MixedModels.likelihoodratiotest(gm1,gm15))
show(MixedModels.likelihoodratiotest(gm1,gm16))
show(MixedModels.likelihoodratiotest(gm1,gm17))
show(MixedModels.likelihoodratiotest(gm1,gm18))
#lowest deviance for gm12 (but negative?)
gm2=gm12
##reduce gm1 by all possible fixed effects
#nStimuliFactor*typeNumeric
modelFormula=@formula(responseCorrect~typeNumeric+
              sexNumeric*nStimuliFactor+
              block+STEM+Experience+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm21=fit(MixedModel,modelFormula,dataset,Binomial())
#sexNumeric*nStimuliFactor
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              sexNumeric+
              block+STEM+Experience+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm22=fit(MixedModel,modelFormula,dataset,Binomial())
#block
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              sexNumeric*nStimuliFactor+
              STEM+Experience+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm23=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              sexNumeric*nStimuliFactor+
              block+Experience+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm24=fit(MixedModel,modelFormula,dataset,Binomial())
#Experience
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              sexNumeric*nStimuliFactor+
              block+STEM+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm25=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              sexNumeric*nStimuliFactor+
              block+STEM+Experience+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm26=fit(MixedModel,modelFormula,dataset,Binomial())
#itemNumber
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              sexNumeric*nStimuliFactor+
              block+STEM+Experience+deg+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm27=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm2,gm21))
show(MixedModels.likelihoodratiotest(gm2,gm22))
show(MixedModels.likelihoodratiotest(gm2,gm23))
show(MixedModels.likelihoodratiotest(gm2,gm24))
show(MixedModels.likelihoodratiotest(gm2,gm25))
show(MixedModels.likelihoodratiotest(gm2,gm26))
show(MixedModels.likelihoodratiotest(gm2,gm27))
#lowest deviance for gm22
gm3=gm22
##reduce gm1 by all possible fixed effects
#nStimuliFactor*typeNumeric
modelFormula=@formula(responseCorrect~nStimuliFactor+typeNumeric+
              sexNumeric+
              block+STEM+Experience+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm31=fit(MixedModel,modelFormula,dataset,Binomial())
#sexNumeric
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              block+STEM+Experience+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm32=fit(MixedModel,modelFormula,dataset,Binomial())
#block
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              sexNumeric+
              STEM+Experience+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm33=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              sexNumeric+
              block+Experience+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm34=fit(MixedModel,modelFormula,dataset,Binomial())
#Experience
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              sexNumeric+
              block+STEM+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm35=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              sexNumeric+
              block+STEM+Experience+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm36=fit(MixedModel,modelFormula,dataset,Binomial())
#itemNumber
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              sexNumeric+
              block+STEM+Experience+deg+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm37=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm3,gm31))
show(MixedModels.likelihoodratiotest(gm3,gm32))
show(MixedModels.likelihoodratiotest(gm3,gm33))
show(MixedModels.likelihoodratiotest(gm3,gm34))
show(MixedModels.likelihoodratiotest(gm3,gm35))
show(MixedModels.likelihoodratiotest(gm3,gm36))
show(MixedModels.likelihoodratiotest(gm3,gm37))
#lowest deviance for gm35
gm4=gm35
##reduce gm1 by all possible fixed effects
#nStimuliFactor*typeNumeric
modelFormula=@formula(responseCorrect~nStimuliFactor+typeNumeric+
              sexNumeric+
              block+STEM+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm41=fit(MixedModel,modelFormula,dataset,Binomial())
#sexNumeric
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              block+STEM+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm42=fit(MixedModel,modelFormula,dataset,Binomial())
#block
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              sexNumeric+
              STEM+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm43=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm44=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              sexNumeric+
              block+STEM+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm45=fit(MixedModel,modelFormula,dataset,Binomial())
#itemNumber
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              sexNumeric+
              block+STEM+deg+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm46=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm4,gm41))
show(MixedModels.likelihoodratiotest(gm4,gm42))
show(MixedModels.likelihoodratiotest(gm4,gm43))
show(MixedModels.likelihoodratiotest(gm4,gm44))
show(MixedModels.likelihoodratiotest(gm4,gm45))
show(MixedModels.likelihoodratiotest(gm4,gm46))
#lowest deviance for gm42
gm5=gm42
##reduce gm1 by all possible fixed effects
#nStimuliFactor*typeNumeric
modelFormula=@formula(responseCorrect~nStimuliFactor+typeNumeric+
              block+STEM+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm51=fit(MixedModel,modelFormula,dataset,Binomial())
#block
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              STEM+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm52=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm53=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              block+STEM+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm54=fit(MixedModel,modelFormula,dataset,Binomial())
#itemNumber
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              block+STEM+deg+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm55=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm5,gm51))
show(MixedModels.likelihoodratiotest(gm5,gm52))
show(MixedModels.likelihoodratiotest(gm5,gm53))
show(MixedModels.likelihoodratiotest(gm5,gm54))
show(MixedModels.likelihoodratiotest(gm5,gm55))
#all significant
#test for reduction of nStimuli and block to numeric variables
#nStimuli
modelFormula=@formula(responseCorrect~nStimuli*typeNumeric+
              block+STEM+deg+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm56=fit(MixedModel,modelFormula,dataset,Binomial())
#block
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              blockNumeric+STEM+deg+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm57=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm5,gm56))
show(MixedModels.likelihoodratiotest(gm5,gm57))
##nonsignificant comparisons
#sexNumeric
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              block+STEM+deg+itemNumber+
              sexNumeric+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm58=fit(MixedModel,modelFormula,dataset,Binomial())
#sexNumeric*typeNumeric
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              block+STEM+deg+itemNumber+
              sexNumeric*typeNumeric+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm59=fit(MixedModel,modelFormula,dataset,Binomial())
#sexNumeric*nStimuliFactor
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              block+STEM+deg+itemNumber+
              sexNumeric*nStimuliFactor+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm510=fit(MixedModel,modelFormula,dataset,Binomial())
#sexNumeric*nStimuliFactor*typeNumeric
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              block+STEM+deg+itemNumber+
              sexNumeric*nStimuliFactor*typeNumeric+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm511=fit(MixedModel,modelFormula,dataset,Binomial())
#Experience
modelFormula=@formula(responseCorrect~nStimuliFactor*typeNumeric+
              block+STEM+deg+itemNumber+
              Experience+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm512=fit(MixedModel,modelFormula,dataset,Binomial())
#interactions of STEM and Experience with sex
