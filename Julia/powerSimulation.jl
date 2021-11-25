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
using Plots


#select dataset to sample from
df=datasetNoExpNoSTEM[datasetNoExpNoSTEM[!,:nStimuliFactor].!="4",:]
df.nStimuli=centerNormalize(df.nStimuli)
#modelFormulas for effects of interest
modelFormula=@formula(responseCorrect ~ 1 + sexContrasts + nStimuli + typeContrasts + blockNumeric + deg + trialNumber +
    sexContrasts & nStimuli + sexContrasts & typeContrasts + nStimuli & typeContrasts +
    sexContrasts & nStimuli & typeContrasts +
    (nStimuli+typeContrasts+blockNumeric+trialNumber|newID) + (1 | modelNumber))
#3-way interaction
modelFormula3=@formula(responseCorrect ~ 1 + sexContrasts + nStimuli + typeContrasts + blockNumeric + deg + trialNumber +
    sexContrasts & nStimuli + sexContrasts & typeContrasts + nStimuli & typeContrasts +
    (nStimuli+typeContrasts+blockNumeric+trialNumber|newID) + (1 | modelNumber))
#nStimuli*typeContrasts
modelFormulaNType=@formula(responseCorrect ~ 1 + sexContrasts + nStimuli + typeContrasts + blockNumeric + deg + trialNumber +
    sexContrasts & nStimuli + sexContrasts & typeContrasts +
    sexContrasts & nStimuli & typeContrasts +
    (nStimuli+typeContrasts+blockNumeric+trialNumber|newID) + (1 | modelNumber))
#sexContrasts*typeContrasts+
modelFormulaSexType=@formula(responseCorrect ~ 1 + sexContrasts + nStimuli + typeContrasts + blockNumeric + deg + trialNumber +
    sexContrasts & nStimuli + nStimuli & typeContrasts +
    sexContrasts & nStimuli & typeContrasts +
    (nStimuli+typeContrasts+blockNumeric+trialNumber|newID) + (1 | modelNumber))
#sexContrasts*nStimuli
modelFormulaSexN=@formula(responseCorrect ~ 1 + sexContrasts + nStimuli + typeContrasts + blockNumeric + deg + trialNumber +
    sexContrasts & typeContrasts + nStimuli & typeContrasts +
    sexContrasts & nStimuli & typeContrasts +
    (nStimuli+typeContrasts+blockNumeric+trialNumber|newID) + (1 | modelNumber))

#numbers of participants
ns=(300:50:600)
#number of simulations per n
repeats=100
#create vector to save values
pValues3=Vector{Float32}(undef,repeats)
pValuesSexType=Vector{Float32}(undef,repeats)
pValuesSexN=Vector{Float32}(undef,repeats)
#create empty dataframe to save values
power=DataFrame(n=Int64[],ps3=Vector{Float32}[],proportionSignificant3=Float32[],
    psSexType=Vector{Float32}[],proportionSignificantSexType=Float32[],
    psSexN=Vector{Float32}[],proportionSignificantSexN=Float32[])
#for each number of participants, do the number of simulations
for n in ns
    @show n
    for thisRepeat in 1:repeats
        @show thisRepeat
        #create random sampled dataframe
        randomSampleIDs=StatsBase.sample(unique(df.ID),n;replace=true)
        #create new dataframe
        df2=DataFrame()
        #populate data with random sampled IDs
        for idNumber in 1:n
            thisID=randomSampleIDs[idNumber]
            newID=string("id",idNumber)
            #append to data with new (unique) ID
            append!(df2,insertcols!(df[df[!,:ID].==thisID,:],1,:newID => newID))
        end
        #calculate statistics
        modelBase=fit(MixedModel,modelFormula,df2,Binomial())
        model3=fit(MixedModel,modelFormula3,df2,Binomial())
        modelSexType=fit(MixedModel,modelFormulaSexType,df2,Binomial())
        modelSexN=fit(MixedModel,modelFormulaSexN,df2,Binomial())
        thisP3=MixedModels.likelihoodratiotest(modelBase,model3).tests.pvalues[1]
        pValues3[thisRepeat]=thisP3
        thisPSexType=MixedModels.likelihoodratiotest(modelBase,modelSexType).tests.pvalues[1]
        pValuesSexType[thisRepeat]=thisPSexType
        thisPSexN=MixedModels.likelihoodratiotest(modelBase,modelSexN).tests.pvalues[1]
        pValuesSexN[thisRepeat]=thisPSexN
    end
    proportionSignificant3=sum(pValues3.<=.05)/repeats
    proportionSignificantSexType=sum(pValuesSexType.<=.05)/repeats
    proportionSignificantSexN=sum(pValuesSexN.<=.05)/repeats
    push!(power,[n,copy(pValues3),proportionSignificant3,
        copy(pValuesSexType),proportionSignificantSexType,
        copy(pValuesSexN),proportionSignificantSexN])
end

Plots.plot(power.n,power.proportionSignificant3)
Plots.plot!(power.n,power.proportionSignificantSexN)
Plots.plot!(power.n,power.proportionSignificantSexType)

### testing



modelFormula=@formula(responseCorrect~nStimuli*typeContrasts+
              sexContrasts*typeContrasts+
              (1|ID))
@elapsed noExpNoStem4OnlyIntercept=fit(MixedModel,modelFormula,datasetNoExpNoSTEM,Binomial())
modelFormula=@formula(responseCorrect~1+
              (1|ID))
@elapsed noExpNoStem4OnlyInterceptNoFixed=fit(MixedModel,modelFormula,datasetNoExpNoSTEM,Binomial())
