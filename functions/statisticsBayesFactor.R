### Approximation of Bayes factors according to Wagenmakers (2007)
#     Copyright (C) 2020  Leonardo Jost
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

#compute BIC difference from LRT
#chiSq=2*(logLik(m1)-logLik(m2))
#BIC(m1)=-2*logLik(m1)+log(n)*df(m1)
#delBIC=BIC(m1)-BIC(m2)
#      =-2*(logLik(m1)-logLik(m2))+log(n)*(df(m1)-df(m2))
#      =-chiSq(m1,m2)+log(n)*delDf
computeDelBICfromLRT=function(chiSq,n,delDf){
  return(-chiSq+log(n)*delDf)
}
#returns bayes factor computed from the difference in BIC
computeBFfromDelBIC=function(delBIC){
  return(exp(delBIC/2))
}
#returns bayes factor in favor of the less complex model (null hypothesis)
computeBFfromLRT=function(chiSq,n,delDf){
  return(computeBFfromDelBIC(computeDelBICfromLRT(chiSq,n,delDf)))
}
#returns bayes factor in favor of the less complex model (null hypothesis)
#m1 should be the alternative, m2 should be the null hypothesis
computeBFfromTwoModels=function(m1,m2,n,delDf){
  return(computeBFfromLRT(-2*(logLik(m2)-logLik(m1)),n,delDf))
}

round(computeBFfromLRT(c(8.51,0.21,26.40,1.26,0.48,3.75,41.62,33.98),234,1),2)
round(computeBFfromLRT(c(0.2,2.11,1.28,2.37),234,1),2)
round(computeBFfromLRT(c(4.66,11.76),234,2),2)
computeBFfromLRT(12.68,116,2)
round(computeBFfromLRT(c(0.01,21.54,0.19,3.85,4.28,12.81,9.07),116,1),2)
round(computeBFfromLRT(c(1.63,27.47,0.37,4.62,4.03,14.64,4.26),87,1),2)

round(computeBFfromLRT(c(8.16,1.27),116,1),2)
round(computeBFfromLRT(c(8.05,1.34),87,1),2)
