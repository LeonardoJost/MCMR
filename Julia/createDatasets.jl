### create datasets for analysis
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

##test code for bootstrapping
@elapsed mBase=fit(LinearMixedModel,@formula(reactionTime~deg+
           (deg|ID)+(1|modelNumber)), dataset,REML=false)
issingular(mBase)
mBaseBstp = parametricbootstrap(1000, mBase)
df=DataFrame(mBaseBstp.allpars)
σres = @where(df, :type .== "σ", :group .== "residual").value
plot(x = σres, Geom.density, Guide.xlabel("Parametric bootstrap estimates of σ"))
df2=groupby(df, [:type, :group, :names])
combine(df2, :value => (x -> shortestcovint(x,0.95)) => :interval)

##test code for programatic formula generation

myTerms=[Term(:block), Term(:STEM), Term(:deg), Term(:itemNumber), Term(:Experience),
          Term(:sexNumeric)]
f=Term(:responseCorrect)~sum(myTerms)+Term(:"(1|a)")
#cannot get random effects in
