#test same analysis as julia with lme4 to compare results
library(lme4)
library(optimx)

dataset=datasetAnalysis


dataset$sexContrasts=toNumeric(dataset$sexContrasts)
#add block as numeric variable
dataset$blockNumeric=stringToNum(dataset$block)
dataset$nStimuliFactor=toChar(dataset$nStimuli)
#sclaing, centering function
centerNormalize=function(vector) {
  vector=vector-min(vector)
  vector=vector/max(vector)
  vector=vector-mean(vector)
  return(vector)
}

#scaling numerical values to 0..1 (centered)
dataset$deg=centerNormalize(dataset$deg)
dataset$blockNumeric=centerNormalize(dataset$blockNumeric)
dataset$trialNumber=centerNormalize(dataset$trialNumber)
dataset$nStimuli=centerNormalize(dataset$nStimuli)
#center uneven STEM distributions (get average effects instead of main effects)
dataset$STEMContrasts1=centerNormalize(dataset$STEMContrasts1)
dataset$STEMContrasts2=centerNormalize(dataset$STEMContrasts2)



aBase=glmer((type=="hit")~STEMContrasts2*ExperienceContrasts*sexContrasts*nStimuli*typeContrasts+
              STEMContrasts1*sexContrasts*nStimuli*typeContrasts+
              blockNumeric+deg+trialNumber+
              (nStimuliFactor+typeContrasts+blockNumeric+trialNumber|ID)+
              (Experience|modelNumber)
            ,family=binomial(),data=dataset,control = glmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
aBase.summary=modelSummary(aBase)
