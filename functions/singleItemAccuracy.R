### Estimate single item accuracy from multiple scores
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

#parameters
numberOfTrials=c(12,12,
                 1,1,
                 24,24,
                 1,1,
                 1,1,
                 24,24,
                 24,24,
                 24,24,
                 1,1,
                 24,24,
                 1,1)
correctTrials=c(6.74,3.85,
                0.71,0.48,
                20.76,18.91,
                0.65,0.81,
                0.47,0.66,
                12.34,9.57,
                12.83,8.15,
                12.6,8.8,
                0.739,0.602,
                18.0,14.4,
                0.776,0.642)
#calculate and print estimates
method1=3*correctTrials/5/numberOfTrials+0.4
method2=(correctTrials/numberOfTrials)^(1/3)
for(i in 1:length(method1)){
  print(paste(round(100*method1[i])/100,round(100*method2[i])/100))
}
