# conditional logit model

# load packages
library(rstan)
library(loo)

# set the directory where "supplemental_getModelFit.R" and .stan  files are located
setwd("")




####################################
###             DATA            ####
####################################

#load data
data <- read.csv("simulated.fight.data.csv", stringsAsFactors = FALSE)

#if you need to simulate a dataset:

# number of individuals in the dataset
nInd = 20

# individual indices
individuals = 1:nInd

# create a trait value for each individual
traitA = rnorm(nInd)
traitB = rnorm(nInd)


#Center and standardize
traitA = (traitA - mean(traitA))/sd(traitA)
traitB = (traitB - mean(traitB))/sd(traitB)

#Effect for traits
bA = rep(c(-1, 1), each = nInd/2)
bB = rep(c(-0.5, 0.5), each = nInd/2)
bAxB = rep(c(-0.1, 0.1), each = nInd/2) 

#compute scores
scores = traitA*bA + traitB*bB + traitA*traitB*bAxB


softmax = function(vect) {
  vect = vect - max(vect) #To get rid of overflow issues.
  return(exp(vect)/sum(exp(vect)))
}

# focal indices
focals = 1:20

generateFight = function(fightId) {
  
  nAvail = floor(runif(1, min=3, max=7+1)) #There will be between 3 and 8 individuals within each fight.
  id = sample(individuals, 1)
  focal = focals[id]
  groupMembers = sample(individuals, size = nAvail)
  
  
  fightScores = scores[groupMembers]
  fightScores = softmax(fightScores)
  memberChoice = sample(1:nAvail, 1, prob = fightScores)
  
  #fightScores = traitA[groupMembers] * bA[id]
  #fightScores = softmax(fightScores)
  #memberChoice = sample(1:nAvail, 1, prob = fightScores)
  
  solicited = rep(0, nAvail)
  solicited[memberChoice] = 1
  
  # the dataset
  fightData = data.frame(fight.num = fightId, focal.id = focal, aud.id = groupMembers, 
                         solicited = solicited, traitA = traitA[groupMembers], traitB = traitB[groupMembers], 
                         stringsAsFactors = FALSE)
  return(fightData)
}

#Generate data for a large number of fights.
fights = lapply( 1:150, generateFight)
data = do.call("rbind", fights)

#######################################
######          models           ######
#######################################

rankCol = "traitA"
relCol = "traitB"

source( paste0("supplemental_getModelFit.R") )

# one predictor
m1 = getModelFit("traitA", NULL, interaction=F, useRE=T, hyper=T)

# two predictors
m2 = getModelFit("traitA", "traitB", interaction=F, useRE=T, hyper=T)

# interaction
m3 = getModelFit("traitA", "traitB", interaction=T, useRE=T, hyper=T)


# extract samples from the posterior and compute median for m1:
mainEffect = median( extract(m1$stanfit, "b_rank")$b_rank )
variance = median( extract(m1$stanfit, "var_rank")$var_rank )

# compute median for random effects 
randomEffects =  extract(m1$stanfit, "b_rank_re")$b_rank_re
median_randomEffects <- sapply(1:nInd, function(x) median( randomEffects[,x] ) )


#######################################
###  coefficient of determination  ####
#######################################

# for model m3 

# from the simulated data extract trait A and B values
i = data$traitA
j = data$traitB

# extract posterior samples from m3
samplesA <- extract(m3$stanfit, "b_rank")$b_rank
samplesB <- extract(m3$stanfit, "b_rel")$b_rel
samplesAxB <- extract(m3$stanfit, "b_int")$b_int

# empty lists to store the results
valsFull <- list()
valsA <- list()
valsB<- list()
valsAxB <- list()

# compute values
for( x in 1:length(samplesA)){
  
  valsFull[[x]] = samplesA[x]*i + samplesB[x]*j + samplesAxB[x]*i*j
  valsA[[x]] = samplesA[x]*i
  valsB[[x]] = samplesB[x]*j
  valsAxB[[x]] = samplesAxB[x]*i*j
  
}

# empty vectors to store the results
coefA <- numeric()
coefB <- numeric()
coefAxB <- numeric()

for( z in 1:length(samplesA)){
  
  coefA[z] <-  cor(valsr[[z]], valsFull[[z]])**2
  coefB[z] <- cor(valsq[[z]], valsFull[[z]])**2
  coefAxB[z] <-  cor(valsrq[[z]], valsFull[[z]])**2 
  
}

median(coefA) 

median(coefB) 

median(coefAxB) 

#summary(lm(valsFull ~ i * j))


