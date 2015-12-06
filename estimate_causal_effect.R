naive_observational_estimate <- function(user_visits){
  est = summarise(user_visits, naive_estimate=sum(is_rec_visit)/length(is_rec_visit))
  return(est)
}

stratified_by_location_estimate <- function(user_visits) {
  est = group_by(user_visits, location) %>% summarise(stratified_estimate=sum(is_rec_visit)/length(is_rec_visit))
}

stratified_by_category_estimate <- function(user_visits) {
  est = group_by(user_visits, category) %>% summarise(stratified_estimate=sum(is_rec_visit)/length(is_rec_visit))
}

fully_conditioned_estimate <- function(user_visits) {
  est = group_by(user_visits, location, category) %>% 
    summarise(stratified_estimate=sum(is_rec_visit)/length(is_rec_visit))
}

ranking_discontinuity_estimate <- function(user_visits) {
  ctr_by_rank = group_by(user_visits, rec_rank) %>%
    summarise(num_clicks_by_rank = length(product_id)) %>%
    mutate(ctr_estimate_by_rank = num_clicks_by_rank/sum(num_clicks_by_rank))
  est = 
    filter(ctr_by_rank,rec_rank==MAX_SHOWN_RECS)$ctr_estimate_by_rank - 
    filter(ctr_by_rank,rec_rank==MAX_SHOWN_RECS+1)$ctr_estimate_by_rank
  upper_bound_est = est*MAX_SHOWN_RECS
  return(upper_bound_est)
}