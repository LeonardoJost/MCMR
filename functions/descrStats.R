### print descriptive statistics
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

#number of each combination of between factors
datasetBetweenFactors=unique(datasetAnalysis[,c("ID","sex","STEM","Experience")])
nrow(datasetBetweenFactors)
n_occur=data.frame(table(paste(datasetBetweenFactors$sex,paste(datasetBetweenFactors$STEM,datasetBetweenFactors$Experience))))
print(n_occur)

#number of each combination of between factors
datasetBetweenFactors=unique(datasetAnalysis[,c("ID","sex","Experience")])
nrow(datasetBetweenFactors)
n_occur=data.frame(table(paste(datasetBetweenFactors$sex,datasetBetweenFactors$Experience)))
print(n_occur)

#overall outliers
outlierIDs=unique(datasetAnalysis[,c("outlier","ID","sex")])
n_occur=data.frame(table(paste(outlierIDs$outlier,outlierIDs$sex)))
print(n_occur)
