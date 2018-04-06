## CAUSAL INFERENCE TUTORIAL
## Author: Amit Sharma
## Script to generate user visits data to an app store.

# Library for easy manipulation of data frames.
library(dplyr)

MAX_SHOWN_RECS = 3
naive_observational_estimate <- function(user_visits){
  # Naive observational estimate
  # Simply the fraction of visits that resulted in a recommendation click-through.
  est = 
    summarise(user_visits, naive_estimate=sum(is_rec_visit)/length(is_rec_visit))
  return(est)
}

stratified_by_activity_estimate <- function(user_visits) {
  # Stratified observational estimate by activity level of each user.
  est = 
    group_by(user_visits, activity_level) %>% 
    summarise(stratified_estimate=sum(is_rec_visit)/length(is_rec_visit))
  return(est)
}

stratified_by_category_estimate <- function(user_visits) {
  # Stratified observational estimate by app category 
  est = 
    group_by(user_visits, category) %>% 
    summarise(stratified_estimate=sum(is_rec_visit)/length(is_rec_visit))
  return(est)
}

fully_conditioned_estimate <- function(user_visits) {
  # Stratified observational estimate by both user activity level and app category.
  est = group_by(user_visits, location, category) %>% 
    summarise(stratified_estimate=sum(is_rec_visit)/length(is_rec_visit))
}

ranking_discontinuity_estimate <- function(user_visits) {
  # Regression discontinuity estimate for the causal effect of recommendation system.
  ctr_by_rank = group_by(user_visits, rec_rank) %>%
    summarise(num_clicks_by_rank = length(product_id)) %>%
    mutate(ctr_estimate_by_rank = num_clicks_by_rank/sum(num_clicks_by_rank))
  
  # Comparing the last shown recommendation and the first not-shown recommendation.
  # Assuming that there are no position order effects in recommendation click-throughs.
  est = 
    filter(ctr_by_rank,rec_rank==MAX_SHOWN_RECS)$ctr_estimate_by_rank - 
    filter(ctr_by_rank,rec_rank==MAX_SHOWN_RECS+1)$ctr_estimate_by_rank
  
  upper_bound_est = est*MAX_SHOWN_RECS
  return(upper_bound_est)
}

Main <- function() {
  # Reading app visit logs for two different algorithms.
  user_app_visits_A = read.csv("user_app_visits_A.csv")
  user_app_visits_B = read.csv("user_app_visits_B.csv")
  
  # GOAL 1: COMPARE ALGORITHM A VERSUS B
  # Naive estimate
  naive_observational_estimate(user_app_visits_A)
  naive_observational_estimate(user_app_visits_B)
  
  # Stratified estimate (by user activity level)
  stratified_by_activity_estimate(user_app_visits_A)
  stratified_by_activity_estimate(user_app_visits_B)
  
  # Stratified estimate (by app category)
  stratified_by_category_estimate(user_app_visits_A)
  stratified_by_category_estimate(user_app_visits_B)
  
  # GOAL 2: FIND THE CAUSAL EFFECT OF SHOWING RECOMMENDATIONS
  # Regression discontinuity estimate for Algorithm A
  naive_observational_estimate(user_app_visits_A)
  ranking_discontinuity_estimate(user_app_visits_A)
  
}