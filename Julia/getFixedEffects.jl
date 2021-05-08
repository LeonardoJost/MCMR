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
modelFormula=@formula(responseCorrect~STEM*sexNumeric*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*nStimuliFactor*typeNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm0=fit(MixedModel,modelFormula,dataset,Binomial())
##reduce fixed effects by nonsignificant effects
#reduce STEM*sexNumeric*nStimuliFactor*typeNumeric
modelFormula=@formula(responseCorrect~Experience*sexNumeric*nStimuliFactor*typeNumeric+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm01=fit(MixedModel,modelFormula,dataset,Binomial())
#reduce Experience*sexNumeric*nStimuliFactor*typeNumeric
modelFormula=@formula(responseCorrect~Experience*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*typeNumeric+
              Experience*sexNumeric*nStimuliFactor+
              STEM*sexNumeric*nStimuliFactor*typeNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm02=fit(MixedModel,modelFormula,dataset,Binomial())
#block
modelFormula=@formula(responseCorrect~STEM*sexNumeric*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*nStimuliFactor*typeNumeric+
              deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm03=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~STEM*sexNumeric*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*nStimuliFactor*typeNumeric+
              block+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm04=fit(MixedModel,modelFormula,dataset,Binomial())
#itemNumber
modelFormula=@formula(responseCorrect~STEM*sexNumeric*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*nStimuliFactor*typeNumeric+
              block+deg+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm05=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm0,gm01))
show(MixedModels.likelihoodratiotest(gm0,gm02))
show(MixedModels.likelihoodratiotest(gm0,gm03))
show(MixedModels.likelihoodratiotest(gm0,gm04))
show(MixedModels.likelihoodratiotest(gm0,gm05))
#only gm01 n.s.
gm1=gm01
##reduce gm1 by all possible fixed effects
#Experience*sexNumeric*nStimuliFactor*typeNumeric
modelFormula=@formula(responseCorrect~sexNumeric*nStimuliFactor*typeNumeric+
              Experience*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*typeNumeric+
              Experience*sexNumeric*nStimuliFactor+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm11=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*nStimuliFactor*typeNumeric
modelFormula=@formula(responseCorrect~Experience*sexNumeric*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm12=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*sexNumeric*typeNumeric
modelFormula=@formula(responseCorrect~Experience*sexNumeric*nStimuliFactor*typeNumeric+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm13=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*sexNumeric*nStimuliFactor
modelFormula=@formula(responseCorrect~Experience*sexNumeric*nStimuliFactor*typeNumeric+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*typeNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm14=fit(MixedModel,modelFormula,dataset,Binomial())
#block
modelFormula=@formula(responseCorrect~Experience*sexNumeric*nStimuliFactor*typeNumeric+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm15=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~Experience*sexNumeric*nStimuliFactor*typeNumeric+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm16=fit(MixedModel,modelFormula,dataset,Binomial())
#itemNumber
modelFormula=@formula(responseCorrect~Experience*sexNumeric*nStimuliFactor*typeNumeric+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm17=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm1,gm11))
show(MixedModels.likelihoodratiotest(gm1,gm12))
show(MixedModels.likelihoodratiotest(gm1,gm13))
show(MixedModels.likelihoodratiotest(gm1,gm14))
show(MixedModels.likelihoodratiotest(gm1,gm15))
show(MixedModels.likelihoodratiotest(gm1,gm16))
show(MixedModels.likelihoodratiotest(gm1,gm17))
#lowest deviance for gm13
gm2=gm13
##reduce gm1 by all possible fixed effects
#Experience*sexNumeric*nStimuliFactor*typeNumeric
modelFormula=@formula(responseCorrect~sexNumeric*nStimuliFactor*typeNumeric+
              Experience*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*typeNumeric+
              Experience*sexNumeric*nStimuliFactor+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm21=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*nStimuliFactor*typeNumeric
modelFormula=@formula(responseCorrect~Experience*sexNumeric*nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm22=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*sexNumeric*nStimuliFactor
modelFormula=@formula(responseCorrect~Experience*sexNumeric*nStimuliFactor*typeNumeric+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm23=fit(MixedModel,modelFormula,dataset,Binomial())
#block
modelFormula=@formula(responseCorrect~Experience*sexNumeric*nStimuliFactor*typeNumeric+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm24=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~Experience*sexNumeric*nStimuliFactor*typeNumeric+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm25=fit(MixedModel,modelFormula,dataset,Binomial())
#itemNumber
modelFormula=@formula(responseCorrect~Experience*sexNumeric*nStimuliFactor*typeNumeric+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm26=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm2,gm21))
show(MixedModels.likelihoodratiotest(gm2,gm22))
show(MixedModels.likelihoodratiotest(gm2,gm23))
show(MixedModels.likelihoodratiotest(gm2,gm24))
show(MixedModels.likelihoodratiotest(gm2,gm25))
show(MixedModels.likelihoodratiotest(gm2,gm26))
#lowest deviance for gm21
gm3=gm21
##reduce gm1 by all possible fixed effects
#sexNumeric*nStimuliFactor*typeNumeric
modelFormula=@formula(responseCorrect~Experience*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*typeNumeric+
              Experience*sexNumeric*nStimuliFactor+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm31=fit(MixedModel,modelFormula,dataset,Binomial())
#Experience*nStimuliFactor*typeNumeric
modelFormula=@formula(responseCorrect~sexNumeric*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*typeNumeric+
              Experience*sexNumeric*nStimuliFactor+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm32=fit(MixedModel,modelFormula,dataset,Binomial())
#Experience*sexNumeric*typeNumeric
modelFormula=@formula(responseCorrect~sexNumeric*nStimuliFactor*typeNumeric+
              Experience*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*nStimuliFactor+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm33=fit(MixedModel,modelFormula,dataset,Binomial())
#Experience*sexNumeric*nStimuliFactor
modelFormula=@formula(responseCorrect~sexNumeric*nStimuliFactor*typeNumeric+
              Experience*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*typeNumeric+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm34=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*nStimuliFactor*typeNumeric
modelFormula=@formula(responseCorrect~sexNumeric*nStimuliFactor*typeNumeric+
              Experience*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*typeNumeric+
              Experience*sexNumeric*nStimuliFactor+
              STEM*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm35=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*sexNumeric*nStimuliFactor
modelFormula=@formula(responseCorrect~sexNumeric*nStimuliFactor*typeNumeric+
              Experience*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*typeNumeric+
              Experience*sexNumeric*nStimuliFactor+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm36=fit(MixedModel,modelFormula,dataset,Binomial())
#block
modelFormula=@formula(responseCorrect~sexNumeric*nStimuliFactor*typeNumeric+
              Experience*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*typeNumeric+
              Experience*sexNumeric*nStimuliFactor+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm37=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~sexNumeric*nStimuliFactor*typeNumeric+
              Experience*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*typeNumeric+
              Experience*sexNumeric*nStimuliFactor+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm38=fit(MixedModel,modelFormula,dataset,Binomial())
#itemNumber
modelFormula=@formula(responseCorrect~sexNumeric*nStimuliFactor*typeNumeric+
              Experience*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*typeNumeric+
              Experience*sexNumeric*nStimuliFactor+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm39=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm3,gm31))
show(MixedModels.likelihoodratiotest(gm3,gm32))
show(MixedModels.likelihoodratiotest(gm3,gm33))
show(MixedModels.likelihoodratiotest(gm3,gm34))
show(MixedModels.likelihoodratiotest(gm3,gm35))
show(MixedModels.likelihoodratiotest(gm3,gm36))
show(MixedModels.likelihoodratiotest(gm3,gm37))
show(MixedModels.likelihoodratiotest(gm3,gm38))
show(MixedModels.likelihoodratiotest(gm3,gm39))
#lowest deviance for gm31
gm4=gm31
##reduce gm1 by all possible fixed effects
#Experience*nStimuliFactor*typeNumeric
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              Experience*sexNumeric*nStimuliFactor+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm41=fit(MixedModel,modelFormula,dataset,Binomial())
#Experience*sexNumeric*typeNumeric
modelFormula=@formula(responseCorrect~Experience*nStimuliFactor*typeNumeric+
              sexNumeric*typeNumeric+
              Experience*sexNumeric*nStimuliFactor+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm42=fit(MixedModel,modelFormula,dataset,Binomial())
#Experience*sexNumeric*nStimuliFactor
modelFormula=@formula(responseCorrect~Experience*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*typeNumeric+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm43=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*nStimuliFactor*typeNumeric
modelFormula=@formula(responseCorrect~Experience*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*typeNumeric+
              Experience*sexNumeric*nStimuliFactor+
              STEM*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm44=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*sexNumeric*nStimuliFactor
modelFormula=@formula(responseCorrect~Experience*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*typeNumeric+
              Experience*sexNumeric*nStimuliFactor+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm45=fit(MixedModel,modelFormula,dataset,Binomial())
#block
modelFormula=@formula(responseCorrect~Experience*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*typeNumeric+
              Experience*sexNumeric*nStimuliFactor+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm46=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~Experience*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*typeNumeric+
              Experience*sexNumeric*nStimuliFactor+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm47=fit(MixedModel,modelFormula,dataset,Binomial())
#itemNumber
modelFormula=@formula(responseCorrect~Experience*nStimuliFactor*typeNumeric+
              Experience*sexNumeric*typeNumeric+
              Experience*sexNumeric*nStimuliFactor+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm48=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm4,gm41))
show(MixedModels.likelihoodratiotest(gm4,gm42))
show(MixedModels.likelihoodratiotest(gm4,gm43))
show(MixedModels.likelihoodratiotest(gm4,gm44))
show(MixedModels.likelihoodratiotest(gm4,gm45))
show(MixedModels.likelihoodratiotest(gm4,gm46))
show(MixedModels.likelihoodratiotest(gm4,gm47))
show(MixedModels.likelihoodratiotest(gm4,gm48))
#lowest deviance for gm41
gm5=gm41
##reduce gm1 by all possible fixed effects
#Experience*sexNumeric*typeNumeric
modelFormula=@formula(responseCorrect~sexNumeric*typeNumeric+
              Experience*typeNumeric+
              Experience*sexNumeric*nStimuliFactor+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm51=fit(MixedModel,modelFormula,dataset,Binomial())
#Experience*sexNumeric*nStimuliFactor
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              Experience*nStimuliFactor+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm52=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*nStimuliFactor*typeNumeric
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              Experience*sexNumeric*nStimuliFactor+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm53=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*sexNumeric*nStimuliFactor+
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              Experience*sexNumeric*nStimuliFactor+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm54=fit(MixedModel,modelFormula,dataset,Binomial())
#block
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              Experience*sexNumeric*nStimuliFactor+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm55=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              Experience*sexNumeric*nStimuliFactor+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm56=fit(MixedModel,modelFormula,dataset,Binomial())
#itemNumber
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              Experience*sexNumeric*nStimuliFactor+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm57=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm5,gm51))
show(MixedModels.likelihoodratiotest(gm5,gm52))
show(MixedModels.likelihoodratiotest(gm5,gm53))
show(MixedModels.likelihoodratiotest(gm5,gm54))
show(MixedModels.likelihoodratiotest(gm5,gm55))
show(MixedModels.likelihoodratiotest(gm5,gm56))
show(MixedModels.likelihoodratiotest(gm5,gm57))
#lowest deviance for gm52
gm6=gm52
##reduce gm6 by all possible fixed effects
#Experience*sexNumeric*typeNumeric
modelFormula=@formula(responseCorrect~sexNumeric*typeNumeric+
              Experience*typeNumeric+
              Experience*sexNumeric+
              Experience*nStimuliFactor+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm61=fit(MixedModel,modelFormula,dataset,Binomial())
#Experience*nStimuliFactor
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm62=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*nStimuliFactor*typeNumeric
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              Experience*nStimuliFactor+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm63=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*sexNumeric*nStimuliFactor
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              Experience*nStimuliFactor+
              STEM*nStimuliFactor*typeNumeric+
              sexNumeric*nStimuliFactor+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm64=fit(MixedModel,modelFormula,dataset,Binomial())
#block
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              Experience*nStimuliFactor+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm65=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              Experience*nStimuliFactor+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm66=fit(MixedModel,modelFormula,dataset,Binomial())
#itemNumber
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              Experience*nStimuliFactor+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm67=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm6,gm61))
show(MixedModels.likelihoodratiotest(gm6,gm62))
show(MixedModels.likelihoodratiotest(gm6,gm63))
show(MixedModels.likelihoodratiotest(gm6,gm64))
show(MixedModels.likelihoodratiotest(gm6,gm65))
show(MixedModels.likelihoodratiotest(gm6,gm66))
show(MixedModels.likelihoodratiotest(gm6,gm67))
#lowest deviance for gm62
gm7=gm62
##reduce gm7 by all possible fixed effects
#Experience*sexNumeric*typeNumeric
modelFormula=@formula(responseCorrect~sexNumeric*typeNumeric+
              Experience*typeNumeric+
              Experience*sexNumeric+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm71=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*nStimuliFactor*typeNumeric
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm72=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*sexNumeric*nStimuliFactor
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              STEM*nStimuliFactor*typeNumeric+
              sexNumeric*nStimuliFactor+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm73=fit(MixedModel,modelFormula,dataset,Binomial())
#block
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm74=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm75=fit(MixedModel,modelFormula,dataset,Binomial())
#itemNumber
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric*nStimuliFactor+
              block+deg+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm76=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm7,gm71))
show(MixedModels.likelihoodratiotest(gm7,gm72))
show(MixedModels.likelihoodratiotest(gm7,gm73))
show(MixedModels.likelihoodratiotest(gm7,gm74))
show(MixedModels.likelihoodratiotest(gm7,gm75))
show(MixedModels.likelihoodratiotest(gm7,gm76))
#largest p for gm73
gm8=gm73
##reduce gm8 by all possible fixed effects
#Experience*sexNumeric*typeNumeric
modelFormula=@formula(responseCorrect~sexNumeric*typeNumeric+
              Experience*typeNumeric+
              Experience*sexNumeric+
              STEM*nStimuliFactor*typeNumeric+
              sexNumeric*nStimuliFactor+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm81=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*nStimuliFactor*typeNumeric
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*nStimuliFactor+
              sexNumeric*nStimuliFactor+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm82=fit(MixedModel,modelFormula,dataset,Binomial())
#sexNumeric*nStimuliFactor
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm83=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*sexNumeric
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              STEM*nStimuliFactor*typeNumeric+
              sexNumeric*nStimuliFactor+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm84=fit(MixedModel,modelFormula,dataset,Binomial())
#block
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              STEM*nStimuliFactor*typeNumeric+
              sexNumeric*nStimuliFactor+
              STEM*sexNumeric+
              deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm85=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              STEM*nStimuliFactor*typeNumeric+
              sexNumeric*nStimuliFactor+
              STEM*sexNumeric+
              block+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm86=fit(MixedModel,modelFormula,dataset,Binomial())
#itemNumber
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              STEM*nStimuliFactor*typeNumeric+
              sexNumeric*nStimuliFactor+
              STEM*sexNumeric+
              block+deg+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm87=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm8,gm81))
show(MixedModels.likelihoodratiotest(gm8,gm82))
show(MixedModels.likelihoodratiotest(gm8,gm83))
show(MixedModels.likelihoodratiotest(gm8,gm84))
show(MixedModels.likelihoodratiotest(gm8,gm85))
show(MixedModels.likelihoodratiotest(gm8,gm86))
show(MixedModels.likelihoodratiotest(gm8,gm87))
#largest p for gm83
gm9=gm83
##reduce gm9 by all possible fixed effects
#Experience*sexNumeric*typeNumeric
modelFormula=@formula(responseCorrect~sexNumeric*typeNumeric+
              Experience*typeNumeric+
              Experience*sexNumeric+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm91=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*nStimuliFactor*typeNumeric
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*nStimuliFactor+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm92=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*sexNumeric+
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              STEM*nStimuliFactor*typeNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm93=fit(MixedModel,modelFormula,dataset,Binomial())
#block
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric+
              deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm94=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric+
              block+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm95=fit(MixedModel,modelFormula,dataset,Binomial())
#itemNumber
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              STEM*nStimuliFactor*typeNumeric+
              STEM*sexNumeric+
              block+deg+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm96=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm9,gm91))
show(MixedModels.likelihoodratiotest(gm9,gm92))
show(MixedModels.likelihoodratiotest(gm9,gm93))
show(MixedModels.likelihoodratiotest(gm9,gm94))
show(MixedModels.likelihoodratiotest(gm9,gm95))
show(MixedModels.likelihoodratiotest(gm9,gm96))
#largest p for gm92
gm10=gm92
##reduce gm10 by all possible fixed effects
#Experience*sexNumeric*typeNumeric
modelFormula=@formula(responseCorrect~sexNumeric*typeNumeric+
              Experience*typeNumeric+
              Experience*sexNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*nStimuliFactor+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm101=fit(MixedModel,modelFormula,dataset,Binomial())
#nStimuliFactor*typeNumeric
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              STEM*typeNumeric+
              STEM*nStimuliFactor+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm102=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*typeNumeric
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              nStimuliFactor*typeNumeric+
              STEM*nStimuliFactor+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm103=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*nStimuliFactor
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm104=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*sexNumeric
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*nStimuliFactor+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm105=fit(MixedModel,modelFormula,dataset,Binomial())
#block
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*nStimuliFactor+
              STEM*sexNumeric+
              deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm106=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*nStimuliFactor+
              STEM*sexNumeric+
              block+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm107=fit(MixedModel,modelFormula,dataset,Binomial())
#itemNumber
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*nStimuliFactor+
              STEM*sexNumeric+
              block+deg+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm108=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm10,gm101))
show(MixedModels.likelihoodratiotest(gm10,gm102))
show(MixedModels.likelihoodratiotest(gm10,gm103))
show(MixedModels.likelihoodratiotest(gm10,gm104))
show(MixedModels.likelihoodratiotest(gm10,gm105))
show(MixedModels.likelihoodratiotest(gm10,gm106))
show(MixedModels.likelihoodratiotest(gm10,gm107))
show(MixedModels.likelihoodratiotest(gm10,gm108))
#largest p for gm104
gm11=gm104
##reduce gm11 by all possible fixed effects
#Experience*sexNumeric*typeNumeric
modelFormula=@formula(responseCorrect~sexNumeric*typeNumeric+
              Experience*typeNumeric+
              Experience*sexNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm111=fit(MixedModel,modelFormula,dataset,Binomial())
#nStimuliFactor*typeNumeric+
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              nStimuliFactor+
              STEM*typeNumeric+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm112=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*typeNumeric
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              nStimuliFactor*typeNumeric+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm113=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*sexNumeric
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm114=fit(MixedModel,modelFormula,dataset,Binomial())
#block
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*sexNumeric+
              deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm115=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*sexNumeric+
              block+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm116=fit(MixedModel,modelFormula,dataset,Binomial())
#itemNumber
modelFormula=@formula(responseCorrect~Experience*sexNumeric*typeNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*sexNumeric+
              block+deg+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm117=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm11,gm111))
show(MixedModels.likelihoodratiotest(gm11,gm112))
show(MixedModels.likelihoodratiotest(gm11,gm113))
show(MixedModels.likelihoodratiotest(gm11,gm114))
show(MixedModels.likelihoodratiotest(gm11,gm115))
show(MixedModels.likelihoodratiotest(gm11,gm116))
show(MixedModels.likelihoodratiotest(gm11,gm117))
#largest p for gm111
gm12=gm111
##reduce gm12 by all possible fixed effects
#sexNumeric*typeNumeric
modelFormula=@formula(responseCorrect~Experience*typeNumeric+
              Experience*sexNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm121=fit(MixedModel,modelFormula,dataset,Binomial())
#Experience*typeNumeric
modelFormula=@formula(responseCorrect~sexNumeric*typeNumeric+
              Experience*sexNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm122=fit(MixedModel,modelFormula,dataset,Binomial())
#Experience*sexNumeric
modelFormula=@formula(responseCorrect~sexNumeric*typeNumeric+
              Experience*typeNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm123=fit(MixedModel,modelFormula,dataset,Binomial())
#nStimuliFactor*typeNumeric
modelFormula=@formula(responseCorrect~sexNumeric*typeNumeric+
              Experience*typeNumeric+
              Experience*sexNumeric+
              nStimuliFactor+
              STEM*typeNumeric+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm124=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*typeNumeric
modelFormula=@formula(responseCorrect~sexNumeric*typeNumeric+
              Experience*typeNumeric+
              Experience*sexNumeric+
              nStimuliFactor*typeNumeric+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm125=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*sexNumeric
modelFormula=@formula(responseCorrect~sexNumeric*typeNumeric+
              Experience*typeNumeric+
              Experience*sexNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm126=fit(MixedModel,modelFormula,dataset,Binomial())
#block
modelFormula=@formula(responseCorrect~sexNumeric*typeNumeric+
              Experience*typeNumeric+
              Experience*sexNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*sexNumeric+
              deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm127=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~sexNumeric*typeNumeric+
              Experience*typeNumeric+
              Experience*sexNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*sexNumeric+
              block+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm128=fit(MixedModel,modelFormula,dataset,Binomial())
#itemNumber
modelFormula=@formula(responseCorrect~sexNumeric*typeNumeric+
              Experience*typeNumeric+
              Experience*sexNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*sexNumeric+
              block+deg+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm129=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm12,gm121))
show(MixedModels.likelihoodratiotest(gm12,gm122))
show(MixedModels.likelihoodratiotest(gm12,gm123))
show(MixedModels.likelihoodratiotest(gm12,gm124))
show(MixedModels.likelihoodratiotest(gm12,gm125))
show(MixedModels.likelihoodratiotest(gm12,gm126))
show(MixedModels.likelihoodratiotest(gm12,gm127))
show(MixedModels.likelihoodratiotest(gm12,gm128))
show(MixedModels.likelihoodratiotest(gm12,gm129))
#lowest deviance for gm121
gm13=gm121
##reduce gm13 by all possible fixed effects
#Experience*typeNumeric
modelFormula=@formula(responseCorrect~Experience*sexNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm131=fit(MixedModel,modelFormula,dataset,Binomial())
#Experience*sexNumeric
modelFormula=@formula(responseCorrect~Experience*typeNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm132=fit(MixedModel,modelFormula,dataset,Binomial())
#nStimuliFactor*typeNumeric+
modelFormula=@formula(responseCorrect~Experience*typeNumeric+
              Experience*sexNumeric+
              nStimuliFactor+
              STEM*typeNumeric+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm133=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*typeNumeric+
modelFormula=@formula(responseCorrect~Experience*typeNumeric+
              Experience*sexNumeric+
              nStimuliFactor*typeNumeric+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm134=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*sexNumeric
modelFormula=@formula(responseCorrect~Experience*typeNumeric+
              Experience*sexNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm135=fit(MixedModel,modelFormula,dataset,Binomial())
#block
modelFormula=@formula(responseCorrect~Experience*typeNumeric+
              Experience*sexNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*sexNumeric+
              deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm136=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~Experience*typeNumeric+
              Experience*sexNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*sexNumeric+
              block+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm137=fit(MixedModel,modelFormula,dataset,Binomial())
#itemNumber
modelFormula=@formula(responseCorrect~Experience*typeNumeric+
              Experience*sexNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*sexNumeric+
              block+deg+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm138=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm13,gm131))
show(MixedModels.likelihoodratiotest(gm13,gm132))
show(MixedModels.likelihoodratiotest(gm13,gm133))
show(MixedModels.likelihoodratiotest(gm13,gm134))
show(MixedModels.likelihoodratiotest(gm13,gm135))
show(MixedModels.likelihoodratiotest(gm13,gm136))
show(MixedModels.likelihoodratiotest(gm13,gm137))
show(MixedModels.likelihoodratiotest(gm13,gm138))
#largest p for gm131
gm14=gm131
##reduce gm14 by all possible fixed effects
#Experience*sexNumeric
modelFormula=@formula(responseCorrect~Experience+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm141=fit(MixedModel,modelFormula,dataset,Binomial())
#nStimuliFactor*typeNumeric
modelFormula=@formula(responseCorrect~Experience*sexNumeric+
              nStimuliFactor+
              STEM*typeNumeric+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm142=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*typeNumeric
modelFormula=@formula(responseCorrect~Experience*sexNumeric+
              nStimuliFactor*typeNumeric+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm143=fit(MixedModel,modelFormula,dataset,Binomial())
#STEM*sexNumeric
modelFormula=@formula(responseCorrect~Experience*sexNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm144=fit(MixedModel,modelFormula,dataset,Binomial())
#block
modelFormula=@formula(responseCorrect~Experience*sexNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*sexNumeric+
              deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm145=fit(MixedModel,modelFormula,dataset,Binomial())
#deg
modelFormula=@formula(responseCorrect~Experience*sexNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*sexNumeric+
              block+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm146=fit(MixedModel,modelFormula,dataset,Binomial())
#itemNumber
modelFormula=@formula(responseCorrect~Experience*sexNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*sexNumeric+
              block+deg+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm147=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm14,gm141))
show(MixedModels.likelihoodratiotest(gm14,gm142))
show(MixedModels.likelihoodratiotest(gm14,gm143))
show(MixedModels.likelihoodratiotest(gm14,gm144))
show(MixedModels.likelihoodratiotest(gm14,gm145))
show(MixedModels.likelihoodratiotest(gm14,gm146))
show(MixedModels.likelihoodratiotest(gm14,gm147))
#all significant
#test for reduction of nStimuli and block to numeric variables
#nStimuli
modelFormula=@formula(responseCorrect~Experience*sexNumeric+
              nStimuli*typeNumeric+
              STEM*typeNumeric+
              STEM*sexNumeric+
              block+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm148=fit(MixedModel,modelFormula,dataset,Binomial())
#block
modelFormula=@formula(responseCorrect~Experience*sexNumeric+
              nStimuliFactor*typeNumeric+
              STEM*typeNumeric+
              STEM*sexNumeric+
              blockNumeric+deg+itemNumber+
              (nStimuliFactor+typeNumeric+block|ID)+
              (Experience|modelNumber))
@elapsed gm149=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(gm14,gm148))
show(MixedModels.likelihoodratiotest(gm14,gm149))
##nonsignificant comparisons
