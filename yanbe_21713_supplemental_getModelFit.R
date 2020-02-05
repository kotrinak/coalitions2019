
# function to run Stan models: 
getModelFit = function(rankCol, relCol, interaction, useRE = FALSE, hyper = FALSE) {
  
  data$numeric_id = as.numeric(as.factor(data$aud.id))
  data$focal_id = as.numeric(as.factor(data$focal.id))
  data$selected = data$solicited == 1 
  
  nInd = max(data$numeric_id)
  nFights = max(data$fight.num)
  nFocal = max(data$focal_id)
  
  selected = rep(0, nFights)
  focal = rep(0, nFights)
  include = matrix(0, nrow = nFights, ncol = nInd)
  
  #Each of these values default to zero if they are not included in the model, this means they will have no effect in the model fitting process since they will not contribute to an audience member score.
  ind_rank = matrix(0, nrow = nFights, ncol = nInd)
  ind_rel = matrix(0, nrow = nFights, ncol = nInd)
  ind_int = matrix(0, nrow = nFights, ncol = nInd)
  
  for(i in 1:nrow(data)) {
    fightNum = data[i, "fight.num"]
    ind = data[i, "numeric_id"]
    include[fightNum, ind] = 1
    
    if(!is.null(rankCol)) {
      ind_rank[fightNum, ind] = data[i, rankCol]
    }
    if(!is.null(relCol)){
      ind_rel[fightNum, ind] = data[i, relCol]
    }
    if(interaction) {
      ind_int[fightNum, ind] = data[i, relCol]*data[i, rankCol] 
    }
    
    if(data[i, "selected"]) {
      selected[fightNum] = ind
    }
    focal[fightNum] = data[i, "focal_id"]
  }
  
  tmpRank <<- ind_rank
  tmpSelected <<-selected
  tmpInclude <<-include
  data = list(nInd = nInd, nFocal = nFocal, nFights = nFights, selected = selected, include = include, focal = focal, ind_rank = ind_rank, ind__rel = ind_rel, ind_int = ind_int)
  
  #We default to a no random effect model for the case of the random effect model. There are no paramaters in this model, and there's nothing to fit, so it doesn't matter if random effects are included or not.
  if(!useRE | (is.null(rankCol) & is.null(relCol))) {
    
    fit <- stan(file = '1.fullModel-noRE.stan', data = data, pars = c("ind_score", "sum_scores", "firstInd", "fight_scores"), include = FALSE, iter = 4000) # warmup=1000, control = list(max_treedepth = 20))
  
  }else if(!hyper){
    
    fit <- stan(file = '2.fullModel-noHyperPrior.stan', data = data, pars = c("ind_score", "sum_scores", "firstInd", "fight_scores"), include = FALSE, iter = 4000) # warmup=1000, control = list(max_treedepth = 20))
  
  }else{
    
    #This is a fancy way of removing terms that don't influence the model.
    
    lines = readLines("3.fullModel.stan")
    
    if(is.null(rankCol)) {
      rankLines = grepl("//RANK", lines)
      lines = lines[!rankLines] #Removes lines that deal with rank.
    }
    if(is.null(relCol)) {
      relLines = grepl("//REL", lines)
      lines = lines[!relLines] #Removes lines that deal with rank.
    }
    if(!interaction) {
      intLines = grepl("//INT", lines)
      lines = lines[!intLines] #Removes lines that deal with rank.
    }
    model_code = paste(lines, collapse = "\n")
    fit <- stan(model_code = model_code, data = data, pars = c("ind_score", "sum_scores", "firstInd", "fight_scores"), include = FALSE, iter = 4000, control = list(adapt_delta = 0.99) )
    #fit <- stan(file = 'fullModel.stan', data = data, pars = c("ind_score", "sum_scores", "firstInd", "fight_scores"), include = FALSE, iter = 4000)
    
  }
  
  ##Don't use this. It gives the Maximum a posteriori estimate (MAP)
  # fit_2 <- optimizing(fit@stanmodel, data = data)
  
  
  log_lik <- extract (fit, "log_lik")$log_lik
  
  waic = waic(log_lik)
  return(list(stanfit = fit, waic = waic))
}
