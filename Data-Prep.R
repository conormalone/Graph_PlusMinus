library(tidyverse)
library(reticulate)

pbp2021 <-readRDS("pbp20-21.rds")
pbp2122 <-readRDS("pbp21-22.rds")
#function to clean and edit the dfs
pbp_clean_function <-function(df){
  #leave out garbage time reps
  df <- df %>% dplyr::filter(garbage_time == 0 & (poss_home == 1|  poss_away == 1)) %>% 
    #add column turning home and away scores into one value where away points are negative
    #dplyr::mutate(score = shot_pts_home - shot_pts_away, poss_count = poss_home + poss_away) %>% 
    #select just the key cols
    dplyr::select(c("game_id", "hs","vs","secs_played", "shot_pts_home", "shot_pts_away", "poss_home", "poss_away","lineup_home", "lineup_away","poss_home", "poss_away", "team_home","team_away"))
  df$game_id <- as.factor(df$game_id)
  #split each lineup
  df$home_split_lineup <-strsplit(df$lineup_home, ",")
  df$away_split_lineup <-strsplit(df$lineup_away, ",")
  df <- df %>% 
    #give each player in the lineup a column
    tidyr::unnest_wider(c(home_split_lineup, away_split_lineup), names_sep = "_") %>% 
    tidyr::unite(all_lineups, lineup_home, lineup_away, poss_home, remove = F, sep = ", ")
  df$all_lineups <-as.factor(df$all_lineups)
  return(df)
}
pbp2021_clean <- pbp_clean_function(pbp2021)
pbp2122_clean <- pbp_clean_function(pbp2122)

#remove frames where knicks have 6 on the court
pbp2021_clean <- pbp2021_clean %>%  dplyr::filter(is.na(home_split_lineup_6)) %>% 
  dplyr::select(-home_split_lineup_6)
#isolate the game in question and remove the errant player (Kevon Looney, not a Knick)
messy_game <- pbp2021_clean %>% dplyr::filter(game_id == "22000483") %>% 
  mutate(home_split_lineup_3 = replace(home_split_lineup_3, home_split_lineup_3 == " Kevon Looney", "Taj Gibson"),
         home_split_lineup_4 = replace(home_split_lineup_4, home_split_lineup_4 == " Kevon Looney", "Taj Gibson"))
#remove the game from the dataset and add back the cleaned game
pbp2021_clean <- pbp2021_clean %>% dplyr::filter(game_id != "22000483")
pbp2021_clean <- rbind(pbp2021_clean,messy_game)
pbp_clean <-rbind(pbp2021_clean, pbp2122_clean)
#get players by mins played, will take top x number of players
long_player_time <- as.data.frame(rbind(cbind(pbp_clean$secs_played, pbp_clean$home_split_lineup_1),cbind(pbp_clean$secs_played, pbp_clean$home_split_lineup_2),
                   cbind(pbp_clean$secs_played, pbp_clean$home_split_lineup_3),cbind(pbp_clean$secs_played, pbp_clean$home_split_lineup_4),
                   cbind(pbp_clean$secs_played, pbp_clean$home_split_lineup_5),cbind(pbp_clean$secs_played, pbp_clean$away_split_lineup_1),
                   cbind(pbp_clean$secs_played, pbp_clean$away_split_lineup_2),cbind(pbp_clean$secs_played, pbp_clean$away_split_lineup_3),
                   cbind(pbp_clean$secs_played, pbp_clean$away_split_lineup_4),cbind(pbp_clean$secs_played, pbp_clean$away_split_lineup_5)))
#add column names
colnames(long_player_time) <- c("seconds", "name")
#trim leading whitespace
long_player_time$name <- as.factor(trimws(long_player_time$name))
#set seconds as numeric
long_player_time$seconds <-as.numeric(long_player_time$seconds)
player_time_grouped <- long_player_time %>% dplyr::group_by(name) %>% dplyr::summarise(playing_time_mins = sum(seconds)/60) %>% arrange(desc(playing_time_mins))

head(player_time_grouped)
tail(player_time_grouped)
plot(density(player_time_grouped$playing_time_mins[350:537]))
mean(player_time_grouped$playing_time_mins[350:537])
sum(player_time_grouped$playing_time_mins[350:537])



#need
#per stint
#score
#on court on bench

#todo
#get stint regardless of player being 1 2 3 on list
#get lineups per game
#lineups per stint
