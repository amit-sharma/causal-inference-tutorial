## CAUSAL INFERENCE TUTORIAL
## Author: Amit Sharma
## Script to generate user visits data to an app store.
## Simulates a data-generating process and returns two csv files.

## DO NOT RUN THIS UNLESS YOU WISH TO GENERATE CUSTOM DATA.
## Use datasets "user_app_visits_A.csv" and "user_app_visits_B.csv" instead.

# Library for easy manipulation of data frames.
library(dplyr)

NUM_USERS = 10000 # Number of users 
NUM_PRODUCTS = 1000 # Number of apps
NUM_ACTIVITY_LEVELS=4 # Different activity levels. 1=Lowest, 4= Highest
NUM_CATEGORIES=10 # Different app categories. E.g. productivity, game, music, etc.
NUM_VISITS=100 # We sample equal number of visits by each user
REC_VISITS_BASERATE = 0.05 # The lowest click-through rate for a recommendation
MAX_SHOWN_RECS = 3 # Maximum number of recommendations shown in the Store interface.
ALGORITHM = "A" # Possible values: A or B (corresponding to "A/B" test)

generate_activity_levels <- function(num_users, num_activity_levels, algorithm="A"){
  ret = NULL
  if (algorithm=="A"){
    ret = rmultinom(num_users, 1, 
                    rep(1/num_activity_levels,num_activity_levels))
  } else if (algorithm=="B") {
    ret = rmultinom(num_users, 1,
                    seq(1:num_activity_levels)/sum(seq(1:num_activity_levels)))
  }
                    
  return(ret)
}

generate_user_visits_dataset <- function () {
  ## GENERATING USER ATTRIBUTES: gender and level of activity.
  # Vector containing gender data (Male=1, Female=0) for each user.
  gender_u = rbinom(NUM_USERS, 1, 0.5)
  
  # Generating NUM_ACTIVITY_LEVELS*NUM_USERS matrix: each column is a user. 
  # For each user/column, only one of the rows is non-zero. 
  activity = generate_activity_levels(NUM_USERS, 
                                      NUM_ACTIVITY_LEVELS, 
                                      algorithm=ALGORITHM)
  # Vector containing activity levels (1=Lowest, NUM_ACTIVITY_LEVELS=Highest) for each user
  activity_u = apply(activity, 2, function(x){which(x==1)})
  
  # NUM_CATEGORIES*NUM_PRODUCTS matrix: each column is a user
  #category = rmultinom(NUM_PRODUCTS, 1, rep(1/NUM_CATEGORIES, NUM_CATEGORIES))
  
  # NUM_CATEGORIES*N matrix: each column is a user, each row is a category and 
  # contains the number of visits by user to that category
  
  ## GENERATING DISTRIBUTION OF USER VISITS TO DIFFERENT APP CATEGORIES.
  # Assume we have data for 100 visits for each user.
  user_history = rmultinom(NUM_USERS, NUM_VISITS, 
                           rep(1/NUM_CATEGORIES, NUM_CATEGORIES))
  # Converting user_history matrix to a vector
  # Each element corresponds to a page visit. 
  # The value of the element is the category of the app visited.
  # The first 100 elements correspond to user 1, the next 100 to user 2, and so on.
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

  # Also creating a corresponding vector of user_ids for joining later.
  user_id=0
  user_ids_list = apply(user_history, 2, 
                        function(user_col){
                          user_id<<-user_id+1;
                          list(rep(user_id,sum(user_col)))
                        })
           
  user_ids = unlist(user_ids_list)
  
  # Joining user_id and visit vectors
  visits_df = data.frame(user_id=user_ids, 
                         category=user_visit_categories)
  # Joining user_id and activity level vectors
  activity_levels_df = data.frame(user_id=1:NUM_USERS, 
                                  activity_level=activity_u)
  # Joining user_id and gender vectors
  gender_df = data.frame(user_id=1:NUM_USERS, 
                         gender=gender_u)
  # Without loss of generality, assuming that app ids are ordered by category
  # Thus, apps 1:100 belong to Category 1, 101:200 belong to Category 2, and so on.
  categories_df = data.frame(category=1:10, 
                             start=seq(1, 901,100), 
                             end=seq(100,1000,100))
  
  # Joining visits, activity level and gender data of each user.
  joined_data = 
    inner_join(visits_df, activity_levels_df, by="user_id") %>%
    inner_join(gender_df, by="user_id") %>%
    inner_join(categories_df, by="category")
    
  # Adding app id to each visit, based on the app category.
  product_joined_data = 
    mutate(joined_data, 
           product_id = floor(start + runif(length(start))*(end-start))
    )
  
  # NOW ADDING RECOMMENDATION SYSTEM DATA: which of the visits came from recommendation click-throughs.
  # Key function: encodes many of the causal assumptions. 
  # is_rec_visit is more likely for higher activity users and some categories.
  # rec_rank is between 1:MAX_SHOWN_RECS for visits that came from recommendation.
  # Otherwise, if a user visited an app that was also highly ranked for recommendation
  # (within 2*MAX_SHOWN_RECS, but not shown), we record its rank too. 
  user_visits = 
    mutate(product_joined_data,
           is_rec_visit = 
             ifelse(runif(length(start)) <= REC_VISITS_BASERATE*(activity_level + (category %% 4)),
                    1,
                    0),
           rec_rank = 
             ifelse(is_rec_visit == 1, 
                    floor(runif(length(start),min=1,max=MAX_SHOWN_RECS+1)),
                    ifelse(runif(length(start)) <= 0.1,
                           floor(runif(length(start), min=MAX_SHOWN_RECS+1, max=2*MAX_SHOWN_RECS+1)),
                           -1
                    )
             )
    )
    
    # Removing irrelevant columns.
    rel_user_visits = select(user_visits, 
                             user_id, -gender, activity_level,
                             product_id, category, is_rec_visit, rec_rank,
                             -start, -end)
    write.csv(rel_user_visits, 
              file=paste("user_app_visits_", ALGORITHM, ".csv", sep=""), 
              row.names=FALSE)
}