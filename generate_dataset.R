library(dplyr)

NUM_USERS = 10000
NUM_PRODUCTS = 1000
NUM_LOCATIONS=4
NUM_CATEGORIES=10
NUM_VISITS=100
PROB_XY_SAME_CATEGORY=0.8
REC_VISITS_BASERATE = 0.1
MAX_SHOWN_RECS = 3

Main <- function () {
  sex = rbinom(NUM_USERS, 1, 0.5)
  # NUM_LOCATIONS*N matrix: each column is a user
  location = rmultinom(NUM_USERS, 1, rep(1/NUM_LOCATIONS,NUM_LOCATIONS))
  location_u = apply(location, 2, function(x){which(x==1)})
  # NUM_CATEGORIES*NUM_PRODUCTS matrix: each column is a user
  #category = rmultinom(NUM_PRODUCTS, 1, rep(1/NUM_CATEGORIES, NUM_CATEGORIES))
  # A vector with the category number for focal product
  #category_x = apply(category, 2, function(x){which(x==1)})
  
  # NUM_CATEGORIES*N matrix: each column is a user, each row is a category and 
  # contains the number of visits by user to that category
  user_history = rmultinom(NUM_USERS, NUM_VISITS, rep(1/NUM_CATEGORIES, NUM_CATEGORIES))
  cate=0
  user_visits_list = apply(user_history, 2, function(user_col){
    cate<<-0; 
    sapply(user_col, 
          function(cate_col){
            cate<<-cate+1;
            rep(cate, cate_col)
          })
    })
  user_visit_categories = unlist(user_visits_list)
  user_id=0
  user_ids_list = apply(user_history, 2, 
                        function(user_col){
                          user_id<<-user_id+1;
                          list(rep(user_id,sum(user_col)))
                        })
           
  user_ids = unlist(user_ids_list)
  visits_df = data.frame(user_id=user_ids, category=user_visit_categories)
  locations_df = data.frame(user_id=1:NUM_USERS, location=location_u)
  categories_df = data.frame(category=1:10, start=seq(1, 901,100), end=seq(100,1000,100))
  temp_user_visits = 
    inner_join(visits_df, locations_df, by="user_id") %>%
    inner_join(categories_df, by="category") %>%
    mutate(product_id = floor(start + runif(length(start))*(end-start))
    )
  
  user_visits = 
    mutate(temp_user_visits,
           is_rec_visit = 
             ifelse(runif(length(start)) <= REC_VISITS_BASERATE*(location + (category %% 4)),1,0),
           rec_rank = 
             ifelse(is_rec_visit == 1, 
                    floor(runif(length(start),min=1,max=MAX_SHOWN_RECS+1)),
                    ifelse(runif(length(start)) <= 0.1,
                           floor(runif(length(start), min=MAX_SHOWN_RECS+1, max=2*MAX_SHOWN_RECS+1)),
                           -1
                    )
             )
    ) %>% 
    select(-start, -end)
  # A vector with the category number for recommended product
  category_y = ifelse(runif(N, min=0, max=1) < PROB_XY_SAME_CATEGORY, category_x, min(category_x +1, NUM_CATEGORIES))
  i=0
  visit_x = apply(user_history, 2, function(z) {i<<-i+1; return(z[category_x[i]] )})
}