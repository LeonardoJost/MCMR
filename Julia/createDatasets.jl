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
using CSV
#read data
dataset=CSV.read("dataset\\dataset.csv")
show(first(dataset,6))
show(names(dataset))
show(dataset)
#scaling
dataset.deg=dataset.deg/100
#accuracy dataset
datasetAcc=dataset
#drop rows for reaction time dataset
datasetRt=select(dataset,Not([:deg, :type]))
#make unique
unique!(datasetRt)

#analysis
@elapsed mBase=fit(LinearMixedModel,@formula(reactionTime~deg+
           (deg|ID)+(1|modelNumber)), dataset,REML=false)

mBaseBstp = parametricbootstrap(MersenneTwister(42), 1000, mBase)
df=DataFrame(mBaseBstp.allpars)
df2=groupby(df, [:type, :group, :names])
combine(df2, :value => (x -> shortestcovint(x,0.95)) => :interval)
shortestcovint(df.value,0.9)
