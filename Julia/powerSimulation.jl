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
using StatsBase
using Bootstrap


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


noExpNoStemTypeMixed1Bootstrap=parametricbootstrap(1000, noExpNoStemTypeMixed1)
df=DataFrame(noExpNoStemTypeMixed1Bootstrap.allpars)
df2=groupby(df, [:type, :group, :names])
print(combine(df2, :value => (x -> shortestcovint(x,0.95)) => :interval))

#random sampling
randomSampleIDs=StatsBase.sample(unique(dataset.ID),200;replace=true)
show(MixedModels.likelihoodratiotest(noExpNoStem4,noExpNoStem41).tests.pvalues[1])

df=dataset
#create empty dataframe to save values
ns=(20:30)*10
pValues=DataFrame())
for n in ns
    #create random sampled dataframe
    randomSampleIDs=StatsBase.sample(unique(df.ID),n;replace=true)
    df2=DataFrame()
    for idNumber in 1:n
        thisID=randomSampleIDs[idNumber]
        append!(df2,df[df[!,:ID].==thisID,:])
        #assign new ID
    end
end
randomSampleIDs=StatsBase.sample(unique(df.ID),200;replace=true)
df2=DataFrame()
for thisID in randomSampleIDs
    append!(df2,df[df[!,:ID].==thisID,:])
    #assign new ID
end

show(groupby(df,:ID)[randomSampleIDs])
df2=vcat(df,df,cols=:union)

vec=Vector{Float16}(undef,100)
vec=vec*10
