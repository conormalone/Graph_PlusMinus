library(tidyverse)
library(reticulate)

pbp2021 <-readRDS("pbp20-21.rds")
pbp2122 <-readRDS("pbp21-22.rds")
#function to clean and edit the dfs
pbp_clean_function <-function(df){
  #leave out garbage time reps
df <- df %>% dplyr::filter(garbage_time == 0) %>% 
  #select just the key cols
  dplyr::select(c("game_id", "hs","vs", "lineup_home", "lineup_away","poss_home", "poss_away", "team_home","team_away"))
df$game_id <- as.factor(df$game_id)
#split each 
df$home_split_lineup <-strsplit(df$lineup_home, ",")
df$away_split_lineup <-strsplit(df$lineup_away, ",")
return(df)
}
pbp2021_clean <- pbp_clean_function(pbp2021)
pbp2122_clean <- pbp_clean_function(pbp2122)

#take all names from data, remove white space, get unique and sort alphabetically.
#will be used to get list of all players as node feature list
player_names <- sort(unique(trimws(unlist(c(pbp2021_clean$home_split_lineup, pbp2021_clean$away_split_lineup)))))
