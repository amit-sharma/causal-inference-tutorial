
N = 1000
NUM_LOCATIONS=10
NUM_CATEGORIES=10
NUM_VISITS=100
PROB_XY_SAME_CATEGORY=0.8

Main <- function () {
  sex = rbinom(N, 1, 0.5)
  location = rmultinom(N, 1, rep(1/NUM_LOCATIONS,NUM_LOCATIONS))
  category = rmultinom(N, 1, rep(1/NUM_CATEGORIES, NUM_CATEGORIES))
  user_history = rmultinom(N, NUM_VISITS, rep(1/NUM_CATEGORIES, NUM_CATEGORIES))
  
  category_x = apply(category, 2, function(x){which(x==1)})
  category_y = ifelse(runif(N, min=0, max=1) < PROB_XY_SAME_CATEGORY, category_x, min(category_x +1, NUM_CATEGORIES))
  i=0
  visit_x = apply(user_history, 2, function(z) {i<<-i+1; return(z[category_x[i]] )})
}