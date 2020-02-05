
data { 
  int nInd;
  int nFights;
  int selected[nFights];
  int include[nFights,nInd];
  real ind_rank[nFights,nInd]; //RANK
  real ind_rel[nFights,nInd]; //REL
  real ind_int[nFights,nInd]; //INT

  int nFocal;
  int focal[nFights];

}
parameters {
  real b_rank; //RANK
  real b_rel; //REL
  real b_int; //INT
  real raw_rank_re[nFocal]; //RANK
  real raw_rel_re[nFocal]; //REL
  real raw_int_re[nFocal]; //INT

  real<lower=0> var_rank; //RANK
  real<lower=0> var_rel; //REL
  real<lower=0> var_int; //INT
} 
transformed parameters {

  real b_rank_re[nFocal]; //RANK
  real b_rel_re[nFocal]; //REL
  real b_int_re[nFocal]; //INT

  real b_ind_rank[nFocal]; //RANK
  real b_ind_rel[nFocal]; //REL
  real b_ind_int[nFocal]; //INT

  for (i in 1:nFocal) {
    b_rank_re[i] = raw_rank_re[i]*sqrt(var_rank); //RANK
    b_rel_re[i] = raw_rel_re[i]*sqrt(var_rel); //REL
    b_int_re[i] = raw_int_re[i]*sqrt(var_int); //INT
  }
  for (i in 1:nFocal) {
    b_ind_rank[i] = b_rank + b_rank_re[i]; //RANK
    b_ind_rel[i] = b_rel + b_rel_re[i]; //REL
    b_ind_int[i] = b_int + b_int_re[i]; //INT
  }

}
model {
  int firstInd;
  int focalId;
  real sum_scores;
  real fight_scores[nFights];
  real ind_score[nFights,nInd];

  for (fightNum in 1:nFights) {
    focalId = focal[fightNum];
    for (i in 1:nInd) {
      ind_score[fightNum, i] = 0;
      ind_score[fightNum, i] += ind_rank[fightNum,i] * b_ind_rank[focalId]; //RANK
      ind_score[fightNum, i] += ind_rel[fightNum,i] * b_ind_rel[focalId]; //REL
      ind_score[fightNum, i] += ind_int[fightNum,i] * b_ind_int[focalId]; //INT
    }
  }

  for (fightNum in 1:nFights){
    sum_scores = 0;
    firstInd = 1;
    for (i in 1:nInd){    
      if (include[fightNum, i]){
        if (firstInd == 1) {
          sum_scores = ind_score[fightNum,i];
          firstInd = 0;
        } else{
          sum_scores = log_sum_exp(sum_scores, ind_score[fightNum,i]);
        }
      }
    }
    fight_scores[fightNum] = ind_score[fightNum,selected[fightNum]] - sum_scores;
  }
  for (fightNum in 1:nFights){
    target += fight_scores[fightNum];
  }

  b_rank ~ normal(0,100); //RANK
  b_rel ~ normal(0,100); //REL
  b_int ~ normal(0,100); //INT

  var_rank ~ inv_gamma(0.001, 0.001); //RANK
  var_rel~ inv_gamma(0.001, 0.001); //REL
  var_int~ inv_gamma(0.001, 0.001); //INT

  for(i in 1:nFocal) {
    //Re-scaled rank is Normal(0,sd). Can also do as normal(0,1) on non-rescaled, and add the extra -log(sd) term. The /2 is for the fact that sd is the sqrt of variance.
    raw_rank_re[i] ~ normal(0,1); //RANK
    raw_rel_re[i] ~ normal(0,1); //REL
    raw_int_re[i] ~ normal(0,1); //INT
  }
}

generated quantities{

  real log_lik[nFights];

  int firstInd;
  int focalId;
  real sum_scores;
  real fight_scores[nFights];
  real ind_score[nFights,nInd];


  for (fightNum in 1:nFights) {
    focalId = focal[fightNum];
    for (i in 1:nInd) {
      ind_score[fightNum, i] = 0;
      ind_score[fightNum, i] += ind_rank[fightNum,i] * b_ind_rank[focalId]; //RANK
      ind_score[fightNum, i] += ind_rel[fightNum,i] * b_ind_rel[focalId]; //REL
      ind_score[fightNum, i] += ind_int[fightNum,i] * b_ind_int[focalId]; //INT
    }
  }

  for (fightNum in 1:nFights){
    sum_scores = 0;
    firstInd = 1;
    for (i in 1:nInd){    
      if (include[fightNum, i]){
        if (firstInd == 1) {
          sum_scores = ind_score[fightNum,i];
          firstInd = 0;
        } else{
          sum_scores = log_sum_exp(sum_scores, ind_score[fightNum,i]);
        }
      }
    }
    fight_scores[fightNum] = ind_score[fightNum,selected[fightNum]] - sum_scores;
  }

  for (fightNum in 1:nFights){
    log_lik[fightNum] = fight_scores[fightNum];
  }

}
