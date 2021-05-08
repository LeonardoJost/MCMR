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
dataset=CSV.read("output\\datasetByBlock.csv", DataFrame)
#inspect data
show(first(dataset,6))
show(names(dataset))
show(eltype.(eachcol(dataset)))
#add block as numeric variable
dataset.blockNumeric=tryparse.(Int,chop.(dataset.block,head=4,tail=0))
dataset.nStimuliFactor=string.(dataset.nStimuli)
dataset.wts=48 ./ dataset.nStimuli

#model after random slope selection
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*nStimuliFactor*typeContrasts+
              Experience*sexContrasts*nStimuliFactor*typeContrasts+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm0=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
##reduce fixed effects by nonsignificant effects
#STEM*sexContrasts*nStimuliFactor*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*nStimuliFactor*typeContrasts+
              STEM*sexContrasts*typeContrasts+
              STEM*sexContrasts*nStimuliFactor+
              Experience*sexContrasts*nStimuliFactor*typeContrasts+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm01=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#Experience*sexContrasts*nStimuliFactor*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*nStimuliFactor*typeContrasts+
              Experience*nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm02=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#block
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*nStimuliFactor*typeContrasts+
              Experience*sexContrasts*nStimuliFactor*typeContrasts+
              (nStimuliFactor+block|ID))
@elapsed gm03=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#comparison
show(MixedModels.likelihoodratiotest(gm0,gm01))
show(MixedModels.likelihoodratiotest(gm0,gm02))
show(MixedModels.likelihoodratiotest(gm0,gm03))
#largest p for gm01
gm1=gm01
##reduce gm1 by all possible fixed effects
#STEM*nStimuliFactor*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              STEM*sexContrasts*nStimuliFactor+
              Experience*sexContrasts*nStimuliFactor*typeContrasts+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm11=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#STEM*sexContrasts*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*nStimuliFactor*typeContrasts+
              STEM*sexContrasts*nStimuliFactor+
              Experience*sexContrasts*nStimuliFactor*typeContrasts+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm12=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#STEM*sexContrasts*nStimuliFactor
modelFormula=@formula(allCorrectSum/wts~STEM*nStimuliFactor*typeContrasts+
              STEM*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor*typeContrasts+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm13=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#Experience*sexContrasts*nStimuliFactor*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*nStimuliFactor*typeContrasts+
              STEM*sexContrasts*typeContrasts+
              STEM*sexContrasts*nStimuliFactor+
              sexContrasts*nStimuliFactor*typeContrasts+
              Experience*nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm14=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#block
modelFormula=@formula(allCorrectSum/wts~STEM*nStimuliFactor*typeContrasts+
              STEM*sexContrasts*typeContrasts+
              STEM*sexContrasts*nStimuliFactor+
              Experience*sexContrasts*nStimuliFactor*typeContrasts+
              (nStimuliFactor+block|ID))
@elapsed gm15=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#comparison
show(MixedModels.likelihoodratiotest(gm1,gm11))
show(MixedModels.likelihoodratiotest(gm1,gm12))
show(MixedModels.likelihoodratiotest(gm1,gm13))
show(MixedModels.likelihoodratiotest(gm1,gm14))
show(MixedModels.likelihoodratiotest(gm1,gm15))
#largest p for gm11
gm2=gm11
##reduce gm2 by all possible fixed effects
#STEM*sexContrasts*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              STEM*sexContrasts*nStimuliFactor+
              Experience*sexContrasts*nStimuliFactor*typeContrasts+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm21=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#STEM*sexContrasts*nStimuliFactor
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              STEM*nStimuliFactor+
              Experience*sexContrasts*nStimuliFactor*typeContrasts+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm22=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#Experience*sexContrasts*nStimuliFactor*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              STEM*sexContrasts*nStimuliFactor+
              sexContrasts*nStimuliFactor*typeContrasts+
              Experience*nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm23=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#bloc
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              STEM*sexContrasts*nStimuliFactor+
              Experience*sexContrasts*nStimuliFactor*typeContrasts+
              (nStimuliFactor+block|ID))
@elapsed gm24=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#comparison
show(MixedModels.likelihoodratiotest(gm2,gm21))
show(MixedModels.likelihoodratiotest(gm2,gm22))
show(MixedModels.likelihoodratiotest(gm2,gm23))
show(MixedModels.likelihoodratiotest(gm2,gm24))
#largest p for gm23
gm3=gm23
##reduce gm3 by all possible fixed effects
#STEM*sexContrasts*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              STEM*sexContrasts*nStimuliFactor+
              sexContrasts*nStimuliFactor*typeContrasts+
              Experience*nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm31=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#STEM*sexContrasts*nStimuliFactor
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              STEM*nStimuliFactor+
              sexContrasts*nStimuliFactor*typeContrasts+
              Experience*nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm32=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#sexContrasts*nStimuliFactor*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              STEM*sexContrasts*nStimuliFactor+
              Experience*nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm33=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#Experience*nStimuliFactor*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              STEM*sexContrasts*nStimuliFactor+
              sexContrasts*nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm34=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#Experience*sexContrasts*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              STEM*sexContrasts*nStimuliFactor+
              sexContrasts*nStimuliFactor*typeContrasts+
              Experience*nStimuliFactor*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm35=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#Experience*sexContrasts*nStimuliFactor
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              STEM*sexContrasts*nStimuliFactor+
              sexContrasts*nStimuliFactor*typeContrasts+
              Experience*nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm36=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#block
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              STEM*sexContrasts*nStimuliFactor+
              sexContrasts*nStimuliFactor*typeContrasts+
              Experience*nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              (nStimuliFactor+block|ID))
@elapsed gm37=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#comparison
show(MixedModels.likelihoodratiotest(gm3,gm31))
show(MixedModels.likelihoodratiotest(gm3,gm32))
show(MixedModels.likelihoodratiotest(gm3,gm33))
show(MixedModels.likelihoodratiotest(gm3,gm34))
show(MixedModels.likelihoodratiotest(gm3,gm35))
show(MixedModels.likelihoodratiotest(gm3,gm36))
show(MixedModels.likelihoodratiotest(gm3,gm37))
#largest p for gm33
gm4=gm33
##reduce gm4 by all possible fixed effects
#STEM*sexContrasts*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              STEM*sexContrasts*nStimuliFactor+
              Experience*nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm41=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#STEM*sexContrasts*nStimuliFactor
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              STEM*nStimuliFactor+
              Experience*nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm42=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#Experience*nStimuliFactor*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              STEM*sexContrasts*nStimuliFactor+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm43=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#Experience*sexContrasts*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              STEM*sexContrasts*nStimuliFactor+
              Experience*nStimuliFactor*typeContrasts+
              sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm44=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#Experience*sexContrasts*nStimuliFactor
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              STEM*sexContrasts*nStimuliFactor+
              Experience*nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm45=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#block
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              STEM*sexContrasts*nStimuliFactor+
              Experience*nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              (nStimuliFactor+block|ID))
@elapsed gm46=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#comparison
show(MixedModels.likelihoodratiotest(gm4,gm41))
show(MixedModels.likelihoodratiotest(gm4,gm42))
show(MixedModels.likelihoodratiotest(gm4,gm43))
show(MixedModels.likelihoodratiotest(gm4,gm44))
show(MixedModels.likelihoodratiotest(gm4,gm45))
show(MixedModels.likelihoodratiotest(gm4,gm46))
#largest p for gm43
gm5=gm43
##reduce gm5 by all possible fixed effects
#STEM*sexContrasts*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              STEM*sexContrasts*nStimuliFactor+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm51=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#STEM*sexContrasts*nStimuliFactor
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              STEM*nStimuliFactor+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm52=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#nStimuliFactor*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              STEM*sexContrasts*nStimuliFactor+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm53=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#Experience*sexContrasts*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              STEM*sexContrasts*nStimuliFactor+
              nStimuliFactor*typeContrasts+
              Experience*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm54=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#Experience*sexContrasts*nStimuliFactor
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              STEM*sexContrasts*nStimuliFactor+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm55=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#block
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              STEM*sexContrasts*nStimuliFactor+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              (nStimuliFactor+block|ID))
@elapsed gm56=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#comparison
show(MixedModels.likelihoodratiotest(gm5,gm51))
show(MixedModels.likelihoodratiotest(gm5,gm52))
show(MixedModels.likelihoodratiotest(gm5,gm53))
show(MixedModels.likelihoodratiotest(gm5,gm54))
show(MixedModels.likelihoodratiotest(gm5,gm55))
show(MixedModels.likelihoodratiotest(gm5,gm56))
#largest p for gm52
gm6=gm52
##reduce gm6 by all possible fixed effects
#STEM*sexContrasts*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              STEM*sexContrasts+
              STEM*nStimuliFactor+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm61=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#STEM*nStimuliFactor
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm62=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#nStimuliFactor*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              STEM*nStimuliFactor+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm63=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#Experience*sexContrasts*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              STEM*nStimuliFactor+
              nStimuliFactor*typeContrasts+
              Experience*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm64=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#Experience*sexContrasts*nStimuliFactor
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              STEM*nStimuliFactor+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              sexContrasts*nStimuliFactor+
              Experience*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm65=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#block
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              STEM*nStimuliFactor+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              (nStimuliFactor+block|ID))
@elapsed gm66=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#comparison
show(MixedModels.likelihoodratiotest(gm6,gm61))
show(MixedModels.likelihoodratiotest(gm6,gm62))
show(MixedModels.likelihoodratiotest(gm6,gm63))
show(MixedModels.likelihoodratiotest(gm6,gm64))
show(MixedModels.likelihoodratiotest(gm6,gm65))
show(MixedModels.likelihoodratiotest(gm6,gm66))
#largest p for gm62
gm7=gm62
##reduce gm7 by all possible fixed effects
#STEM*sexContrasts*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              STEM*sexContrasts+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm71=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#nStimuliFactor*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm72=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#Experience*sexContrasts*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              nStimuliFactor*typeContrasts+
              Experience*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm73=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#Experience*sexContrasts*nStimuliFactor
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              sexContrasts*nStimuliFactor+
              Experience*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm74=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#block
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts*typeContrasts+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              (nStimuliFactor+block|ID))
@elapsed gm75=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#comparison
show(MixedModels.likelihoodratiotest(gm7,gm71))
show(MixedModels.likelihoodratiotest(gm7,gm72))
show(MixedModels.likelihoodratiotest(gm7,gm73))
show(MixedModels.likelihoodratiotest(gm7,gm74))
show(MixedModels.likelihoodratiotest(gm7,gm75))
#largest p for gm71
gm8=gm71
##reduce gm8 by all possible fixed effects
#STEM*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm81=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#STEM*sexContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm82=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#nStimuliFactor*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              STEM*sexContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm83=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#Experience*sexContrasts*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              STEM*sexContrasts+
              nStimuliFactor*typeContrasts+
              sexContrasts*typeContrasts+
              Experience*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm84=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#Experience*sexContrasts*nStimuliFactor
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              STEM*sexContrasts+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              sexContrasts*nStimuliFactor+
              Experience*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm85=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#block
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              STEM*sexContrasts+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*sexContrasts*nStimuliFactor+
              (nStimuliFactor+block|ID))
@elapsed gm86=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#comparison
show(MixedModels.likelihoodratiotest(gm8,gm81))
show(MixedModels.likelihoodratiotest(gm8,gm82))
show(MixedModels.likelihoodratiotest(gm8,gm83))
show(MixedModels.likelihoodratiotest(gm8,gm84))
show(MixedModels.likelihoodratiotest(gm8,gm85))
show(MixedModels.likelihoodratiotest(gm8,gm86))
#largest p for gm85
gm9=gm85
##reduce gm9 by all possible fixed effects
#STEM*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              sexContrasts*nStimuliFactor+
              Experience*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm91=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#STEM*sexContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              sexContrasts*nStimuliFactor+
              Experience*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm92=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#nStimuliFactor*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              STEM*sexContrasts+
              Experience*sexContrasts*typeContrasts+
              sexContrasts*nStimuliFactor+
              Experience*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm93=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#Experience*sexContrasts*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              STEM*sexContrasts+
              nStimuliFactor*typeContrasts+
              sexContrasts*typeContrasts+
              Experience*typeContrasts+
              Experience*sexContrasts+
              sexContrasts*nStimuliFactor+
              Experience*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm94=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#sexContrasts*nStimuliFactor
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              STEM*sexContrasts+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              Experience*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm95=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#Experience*nStimuliFactor
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              STEM*sexContrasts+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm96=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#block
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              STEM*sexContrasts+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              sexContrasts*nStimuliFactor+
              Experience*nStimuliFactor+
              (nStimuliFactor+block|ID))
@elapsed gm97=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#comparison
show(MixedModels.likelihoodratiotest(gm9,gm91))
show(MixedModels.likelihoodratiotest(gm9,gm92))
show(MixedModels.likelihoodratiotest(gm9,gm93))
show(MixedModels.likelihoodratiotest(gm9,gm94))
show(MixedModels.likelihoodratiotest(gm9,gm95))
show(MixedModels.likelihoodratiotest(gm9,gm96))
show(MixedModels.likelihoodratiotest(gm9,gm97))
#largest p for gm96
gm10=gm96
##reduce gm10 by all possible fixed effects
#STEM*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm101=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#STEM*sexContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm102=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#nStimuliFactor*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              STEM*sexContrasts+
              Experience*sexContrasts*typeContrasts+
              sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm103=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#Experience*sexContrasts*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              STEM*sexContrasts+
              nStimuliFactor*typeContrasts+
              sexContrasts*typeContrasts+
              Experience*typeContrasts+
              Experience*sexContrasts+
              sexContrasts*nStimuliFactor+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm104=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#sexContrasts*nStimuliFactor
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              STEM*sexContrasts+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm105=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#block
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              STEM*sexContrasts+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              sexContrasts*nStimuliFactor+
              (nStimuliFactor+block|ID))
@elapsed gm106=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#comparison
show(MixedModels.likelihoodratiotest(gm10,gm101))
show(MixedModels.likelihoodratiotest(gm10,gm102))
show(MixedModels.likelihoodratiotest(gm10,gm103))
show(MixedModels.likelihoodratiotest(gm10,gm104))
show(MixedModels.likelihoodratiotest(gm10,gm105))
show(MixedModels.likelihoodratiotest(gm10,gm106))
#largest p for gm105
gm11=gm105
##reduce gm11 by all possible fixed effects
#STEM*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*sexContrasts+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm111=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#STEM*sexContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm112=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#nStimuliFactor*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              STEM*sexContrasts+
              Experience*sexContrasts*typeContrasts+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm113=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#Experience*sexContrasts*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              STEM*sexContrasts+
              nStimuliFactor*typeContrasts+
              sexContrasts*typeContrasts+
              Experience*typeContrasts+
              Experience*sexContrasts+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm114=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#block
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              STEM*sexContrasts+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              (nStimuliFactor+block|ID))
@elapsed gm115=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#comparison
show(MixedModels.likelihoodratiotest(gm11,gm111))
show(MixedModels.likelihoodratiotest(gm11,gm112))
show(MixedModels.likelihoodratiotest(gm11,gm113))
show(MixedModels.likelihoodratiotest(gm11,gm114))
show(MixedModels.likelihoodratiotest(gm11,gm115))
#largest p for gm112
gm12=gm112
##reduce gm12 by all possible fixed effects
#STEM*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm121=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#nStimuliFactor*typeContrasts+
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              nStimuliFactor+
              Experience*sexContrasts*typeContrasts+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm122=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#Experience*sexContrasts*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              nStimuliFactor*typeContrasts+
              sexContrasts*typeContrasts+
              Experience*typeContrasts+
              Experience*sexContrasts+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm123=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#block
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              (nStimuliFactor+block|ID))
@elapsed gm124=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#comparison
show(MixedModels.likelihoodratiotest(gm12,gm121))
show(MixedModels.likelihoodratiotest(gm12,gm122))
show(MixedModels.likelihoodratiotest(gm12,gm123))
show(MixedModels.likelihoodratiotest(gm12,gm124))
#all significant
#test reduction of block and nStimuli to numeric
#nStimuli
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              nStimuli*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              block+
              (nStimuliFactor+block|ID))
@elapsed gm125=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#block
modelFormula=@formula(allCorrectSum/wts~STEM*typeContrasts+
              nStimuliFactor*typeContrasts+
              Experience*sexContrasts*typeContrasts+
              blockNumeric+
              (nStimuliFactor+block|ID))
@elapsed gm126=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#comparison
show(MixedModels.likelihoodratiotest(gm12,gm125))
show(MixedModels.likelihoodratiotest(gm12,gm126))
#both significant
gmBase=gm12
#check contrasts
modelFormula=@formula(allCorrectSum/wts~STEMContrasts1*typeContrasts+STEMContrasts2*typeContrasts+
              nStimuliContrasts1*typeContrasts+nStimuliContrasts2*typeContrasts+
              ExperienceContrasts*sexContrasts*typeContrasts+
              block+
              (nStimuliContrasts1+nStimuliContrasts2+block|ID))
@elapsed gmBase=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
show(MixedModels.likelihoodratiotest(gm12,gmBase))
#parametric bootstrap
gmBaseBstp = parametricbootstrap(1000, gmBase)
intervals=DataFrame(shortestcovint(gmBaseBstp))
CSV.write("output\\bootstraps.csv",intervals)

#nonsignificant effects
#sexContrasts*nStimuliContrasts*typeContrasts
modelFormula=@formula(allCorrectSum/wts~STEMContrasts1*typeContrasts+STEMContrasts2*typeContrasts+
              nStimuliContrasts1*typeContrasts+nStimuliContrasts2*typeContrasts+
              ExperienceContrasts*sexContrasts*typeContrasts+
              block+
              sexContrasts&nStimuliContrasts1&typeContrasts+
              sexContrasts&nStimuliContrasts2&typeContrasts+
              (nStimuliContrasts1+nStimuliContrasts2+block|ID))
@elapsed gmBase1=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#sexContrasts*nStimuliContrasts
modelFormula=@formula(allCorrectSum/wts~STEMContrasts1*typeContrasts+STEMContrasts2*typeContrasts+
              nStimuliContrasts1*typeContrasts+nStimuliContrasts2*typeContrasts+
              ExperienceContrasts*sexContrasts*typeContrasts+
              block+
              sexContrasts&nStimuliContrasts1+sexContrasts&nStimuliContrasts2+
              (nStimuliContrasts1+nStimuliContrasts2+block|ID))
@elapsed gmBase2=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)
#sexContrasts*STEM
modelFormula=@formula(allCorrectSum/wts~STEMContrasts1*typeContrasts+STEMContrasts2*typeContrasts+
              nStimuliContrasts1*typeContrasts+nStimuliContrasts2*typeContrasts+
              ExperienceContrasts*sexContrasts*typeContrasts+
              block+
              sexContrasts&STEMContrasts1+sexContrasts&STEMContrasts2+
              (nStimuliContrasts1+nStimuliContrasts2+block|ID))
@elapsed gmBase3=fit(MixedModel,modelFormula,dataset,Binomial(),wts=dataset.wts)

#comparison
show(MixedModels.likelihoodratiotest(gmBase,gmBase1))
show(MixedModels.likelihoodratiotest(gmBase,gmBase2))
show(MixedModels.likelihoodratiotest(gmBase,gmBase3))
