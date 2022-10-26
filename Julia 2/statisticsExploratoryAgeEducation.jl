### exploratory analyses using age and education
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
dataset=CSV.read("output\\datasetGrouped.csv", DataFrame)
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
#scaling block to range 0..1 and center
dataset.blockNumeric=centerNormalize(dataset.blockNumeric)
dataset.educationNumeric=centerNormalize(dataset.educationNumeric)
dataset.age=centerNormalize(dataset.age)

#additional covariate of education for sex differences
modelFormula=@formula(acc~nStimuliContrasts+typeContrasts+sexContrasts+educationNumeric+age+
              blockNumeric+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed education1=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#without education
modelFormula=@formula(acc~nStimuliContrasts+typeContrasts+sexContrasts+age+
              blockNumeric+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed education11=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#without sex
modelFormula=@formula(acc~nStimuliContrasts+typeContrasts+educationNumeric+age+
              blockNumeric+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed education12=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#without age
modelFormula=@formula(acc~nStimuliContrasts+typeContrasts+sexContrasts+educationNumeric+
              blockNumeric+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed education13=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#comparison
show(MixedModels.likelihoodratiotest(education1,education11))
show(MixedModels.likelihoodratiotest(education1,education12))
show(MixedModels.likelihoodratiotest(education1,education13))
#education11 n.s.
education2=education11
#without sex
modelFormula=@formula(acc~nStimuliContrasts+typeContrasts+age+
              blockNumeric+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed education21=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#without age
modelFormula=@formula(acc~nStimuliContrasts+typeContrasts+sexContrasts+
              blockNumeric+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed education22=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#comparison
show(MixedModels.likelihoodratiotest(education2,education21))
show(MixedModels.likelihoodratiotest(education2,education22))


#education without age
modelFormula=@formula(acc~nStimuliContrasts+typeContrasts+sexContrasts+educationNumeric+
              blockNumeric+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed education3=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#without education
modelFormula=@formula(acc~nStimuliContrasts+typeContrasts+sexContrasts+
              blockNumeric+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed education31=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#comparison
show(MixedModels.likelihoodratiotest(education3,education31))
