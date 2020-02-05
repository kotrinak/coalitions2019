
data { 
  int nInd;
  int nFights;
  int selected[nFights];
  int include[nFights,nInd];
  real ind_rank[nFights,nInd];
  real ind_rel[nFights,nInd];
  real ind_int[nFights,nInd];

  int nFocal;
  int focal[nFights];

}
parameters {
  real b_rank;
  real b_rel;
  real b_int;
} 
transformed parameters {


  real b_ind_rank[nFocal];
  real b_ind_rel[nFocal];
  real b_ind_int[nFocal];

  for (i in 1:nFocal) {
    b_ind_rank[i] = b_rank;
    b_ind_rel[i] = b_rel;
    b_ind_int[i] = b_int;
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
      ind_score[fightNum, i] = ind_rank[fightNum,i] * b_ind_rank[focalId];
      ind_score[fightNum, i] += ind_rel[fightNum,i] * b_ind_rel[focalId];
      ind_score[fightNum, i] += ind_int[fightNum,i] * b_ind_int[focalId];
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

  b_rank ~ normal(0,100);
  b_rel ~ normal(0,100);
  b_int ~ normal(0,100);

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
      ind_score[fightNum, i] = ind_rank[fightNum,i] * b_ind_rank[focalId];
      ind_score[fightNum, i] += ind_rel[fightNum,i] * b_ind_rel[focalId];
      ind_score[fightNum, i] += ind_int[fightNum,i] * b_ind_int[focalId];
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
