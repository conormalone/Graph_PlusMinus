#putting "Data-Prep.R" into graph label format, 
#each graph will have an ordered categorical value for the home team score on a stint
graph_y_function <- function(df){
  stints_grouped <- df %>% dplyr::group_by(all_lineups, game_id) %>% 
    dplyr::summarize(score_per_poss = sum(shot_pts_home+shot_pts_away)/ sum(poss_home+poss_away)) %>% 
    replace(is.na(.), 0) %>% dplyr::select(score_per_poss)
 return(stints_grouped$score_per_poss) 
}