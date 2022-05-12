### exploratory analyses
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
dataset=CSV.read("dataset\\datasetGrouped.csv", DataFrame)
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

#additional covariate of education for sex differences
modelFormula=@formula(acc~nStimuliContrasts+typeContrasts+sexContrasts+educationNumeric+
              blockNumeric+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed education1=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#without education
modelFormula=@formula(acc~nStimuliContrasts+typeContrasts+sexContrasts+
              blockNumeric+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed education2=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#without sex
modelFormula=@formula(acc~nStimuliContrasts+typeContrasts+educationNumeric+
              blockNumeric+
              (nStimuliContrasts*typeContrasts+blockNumeric|ID))
@elapsed education3=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#comparison
show(MixedModels.likelihoodratiotest(education1,education2))
show(MixedModels.likelihoodratiotest(education1,education3))
