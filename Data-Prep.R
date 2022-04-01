library(tidyverse)
library(reticulate)
#negate %in% to get all players not on replacement level list "later"
`%ni%` <- Negate(`%in%`)
pbp2021 <-readRDS("pbp20-21.rds")
pbp2122 <-readRDS("pbp21-22.rds")
#function to clean and edit the dfs
pbp_clean_function <-function(df){
  #leave out garbage time reps
  df <- df %>% dplyr::filter(garbage_time == 0 & (poss_home == 1|  poss_away == 1)) %>% 
    dplyr::mutate(off_lineup = ifelse(poss_home ==1,lineup_home,lineup_away), def_lineup = ifelse(poss_home ==1,lineup_away, lineup_home)) %>% 
    #select just the key cols
    dplyr::select(c("game_id", "hs","vs","secs_played", "shot_pts_home", "shot_pts_away", "poss_home", "poss_away","off_lineup", "def_lineup","poss_home", "poss_away", "team_home","team_away"))
  df$game_id <- as.factor(df$game_id)
  #split each lineup
  df$off_split_lineup <-strsplit(df$off_lineup, ",")
  df$def_split_lineup <-strsplit(df$def_lineup, ",")
  df <- df %>% 
    #give each player in the lineup a column
    tidyr::unnest_wider(c(off_split_lineup, def_split_lineup), names_sep = "_") %>% 
    tidyr::unite(all_lineups, off_lineup, def_lineup, remove = F, sep = ", ")
  df$all_lineups <-as.factor(df$all_lineups)
  return(df)
}
pbp2021_clean <- pbp_clean_function(pbp2021)
pbp2122_clean <- pbp_clean_function(pbp2122)

#remove frames where knicks have 6 on the court
pbp2021_clean <- pbp2021_clean %>%  dplyr::filter(is.na(def_split_lineup_6)| is.na(off_split_lineup_6)) %>% 
  dplyr::select(-def_split_lineup_6, -off_split_lineup_6)
#isolate the game in question and remove the errant player (Kevon Looney, not a Knick)
messy_game <- pbp2021_clean %>% dplyr::filter(game_id == "22000483") %>% 
  mutate(off_split_lineup_3 = replace(off_split_lineup_3, off_split_lineup_3 == " Kevon Looney", "Taj Gibson"),
         off_split_lineup_4 = replace(off_split_lineup_4, off_split_lineup_4 == " Kevon Looney", "Taj Gibson"),
         def_split_lineup_3 = replace(def_split_lineup_3, def_split_lineup_3 == " Kevon Looney", "Taj Gibson"),
         def_split_lineup_4 = replace(def_split_lineup_4, def_split_lineup_4 == " Kevon Looney", "Taj Gibson"))
#remove the game from the dataset and add back the cleaned game
pbp2021_clean <- pbp2021_clean %>% dplyr::filter(game_id != "22000483")
pbp2021_clean <- rbind(pbp2021_clean,messy_game)
pbp_clean <-rbind(pbp2021_clean, pbp2122_clean)
#get players by mins played, will take top x number of players
long_player_time <- as.data.frame(rbind(cbind(pbp_clean$secs_played, pbp_clean$off_split_lineup_1),cbind(pbp_clean$secs_played, pbp_clean$off_split_lineup_2),
                   cbind(pbp_clean$secs_played, pbp_clean$off_split_lineup_3),cbind(pbp_clean$secs_played, pbp_clean$off_split_lineup_4),
                   cbind(pbp_clean$secs_played, pbp_clean$off_split_lineup_5),cbind(pbp_clean$secs_played, pbp_clean$def_split_lineup_1),
                   cbind(pbp_clean$secs_played, pbp_clean$def_split_lineup_2),cbind(pbp_clean$secs_played, pbp_clean$def_split_lineup_3),
                   cbind(pbp_clean$secs_played, pbp_clean$def_split_lineup_4),cbind(pbp_clean$secs_played, pbp_clean$def_split_lineup_5)))
#add column names
colnames(long_player_time) <- c("seconds", "name")
#trim leading whitespace
long_player_time$name <- as.factor(trimws(long_player_time$name))
#set seconds as numeric
long_player_time$seconds <-as.numeric(long_player_time$seconds)
#get players ordered by time played
player_time_grouped <- long_player_time %>% dplyr::group_by(name) %>% dplyr::summarise(playing_time_mins = sum(seconds)/60) %>% arrange(desc(playing_time_mins))
#cleaned list of all players
all_player_list <- unique(trimws(c(pbp_clean$off_split_lineup_1, pbp_clean$off_split_lineup_2,pbp_clean$off_split_lineup_3,pbp_clean$off_split_lineup_4,pbp_clean$off_split_lineup_5,
                                   pbp_clean$def_split_lineup_1, pbp_clean$def_split_lineup_2,pbp_clean$def_split_lineup_3,pbp_clean$def_split_lineup_4,pbp_clean$def_split_lineup_5)))

#get players in 401st for mins played onwards
rep_level_players <- as.character(unique(player_time_grouped$name[401:nrow(player_time_grouped)]))
