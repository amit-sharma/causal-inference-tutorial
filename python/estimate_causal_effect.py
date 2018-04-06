## CAUSAL INFERENCE TUTORIAL
## Author: Amit Sharma
## Script to estimate causal effect of recommendations on a website.

# Library for easy manipulation of data frames.
import numpy as np
import pandas as pd


MAX_SHOWN_RECS = 3
def naive_observational_estimate(user_visits):
  # Naive observational estimate
  # Simply the fraction of visits that resulted in a recommendation click-through.
  est = np.sum(user_visits["is_rec_visit"])/len(user_visits["is_rec_visit"])
  print("Mean estimate: {0}".format(est))
  return(est)

def stratified_by_activity_estimate(user_visits):
  # Stratified observational estimate by activity level of each user.
  grouped = user_visits.groupby('activity_level')
  grouped = grouped.agg({'is_rec_visit': lambda x: np.sum(x)/x.size })
  est = grouped.rename(columns= {'is_rec_visit':'stratified_estimate'})
  print("Mean estimate: {0}".format(np.mean(est['stratified_estimate'])))
  return(est)

def stratified_by_category_estimate(user_visits):
  # Stratified observational estimate by app category 
  grouped = user_visits.groupby('category')
  grouped = grouped.agg({'is_rec_visit': lambda x: np.sum(x)/x.size })
  est = grouped.rename(columns= {'is_rec_visit':'stratified_estimate'})
  print("Mean estimate: {0}".format(np.mean(est['stratified_estimate'])))
  return(est)

def fully_conditioned_estimate(user_visits):
  # Stratified observational estimate by both user activity level and app category.
  grouped = user_visits.groupby(['activity_level', 'category'])
  grouped = grouped.agg({'is_rec_visit': lambda x: np.sum(x)/x.size })
  est = grouped.rename(columns= {'is_rec_visit':'stratified_estimate'})
  print("Mean estimate: {0}".format(np.mean(est['stratified_estimate'])))
  return(est)

def ranking_discontinuity_estimate(user_visits):
  # Regression discontinuity estimate for the causal effect of recommendation system.
  ctr_by_rank = user_visits.groupby('rec_rank')
  ctr_by_rank = ctr_by_rank.agg({'product_id': lambda x: x.size})
  ctr_by_rank = ctr_by_rank.rename(columns={'product_id':'num_clicks_by_rank'})
  ctr_by_rank = ctr_by_rank.reset_index()
  sum_num_clicks = np.sum(ctr_by_rank['num_clicks_by_rank'])
  ctr_by_rank['ctr_estimate_by_rank'] = ctr_by_rank['num_clicks_by_rank']/sum_num_clicks

  
  # Comparing the last shown recommendation and the first not-shown recommendation.
  # Assuming that there are no position order effects in recommendation click-throughs.
  print(ctr_by_rank)
  est1 = ctr_by_rank.loc[ctr_by_rank.rec_rank==MAX_SHOWN_RECS, 'ctr_estimate_by_rank']
  est2 = ctr_by_rank.loc[ctr_by_rank.rec_rank==(MAX_SHOWN_RECS+1), 'ctr_estimate_by_rank']
  est = est1.iloc[0] -est2.iloc[0]
  upper_bound_est = est*MAX_SHOWN_RECS
  print("Mean estimate: {0}".format(upper_bound_est))
  return(upper_bound_est)

if __name__=="__main__":
  # Reading app visit logs for two different algorithms.
  user_app_visits_A = pd.read_csv("../datasets/user_app_visits_A.csv")
  user_app_visits_B = pd.read_csv("../datasets/user_app_visits_B.csv")
  
  # GOAL 1: COMPARE ALGORITHM A VERSUS B
  # Naive estimate
  naive_observational_estimate(user_app_visits_A)
  naive_observational_estimate(user_app_visits_B)
 
  
  # Stratified estimate (by user activity level)
  print(stratified_by_activity_estimate(user_app_visits_A))
  print(stratified_by_activity_estimate(user_app_visits_B))
  
  # Stratified estimate (by app category)
  print(stratified_by_category_estimate(user_app_visits_A))
  print(stratified_by_category_estimate(user_app_visits_B))

  # Fully conditioned estimate
  print(fully_conditioned_estimate(user_app_visits_A))
  print(fully_conditioned_estimate(user_app_visits_B))

  # GOAL 2: FIND THE CAUSAL EFFECT OF SHOWING RECOMMENDATIONS
  # Regression discontinuity estimate for Algorithm A
  print(naive_observational_estimate(user_app_visits_A))
  ranking_discontinuity_estimate(user_app_visits_A)
