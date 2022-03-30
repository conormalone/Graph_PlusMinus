library(tidyverse)
#function formatting pbp data from "Data-Prep.R" into adjacency matrices
#where every player in a game is featured, players on a stint have a value 1 to each other
graph_a_function <-function(df){
matrix_list = list()
k = 1
for(i in 1:length(levels(df$game_id))){
  #get just the game
  game_subset <- df %>%  dplyr::filter(game_id == levels(df$game_id)[i])
  game_subset<-droplevels(game_subset)
  #get players from this game
  game_player_list <- unique(trimws(c(game_subset$home_split_lineup_1, game_subset$home_split_lineup_2,game_subset$home_split_lineup_3,game_subset$home_split_lineup_4,game_subset$home_split_lineup_5,
                        game_subset$away_split_lineup_1, game_subset$away_split_lineup_2,game_subset$away_split_lineup_3,game_subset$away_split_lineup_4,game_subset$away_split_lineup_5)))
  
  for(j in 1:length(levels(game_subset$all_lineups))){
    matrix_list[[k]] = list()
    adj_matrix <- matrix(0L, nrow=length(game_player_list), ncol =length(game_player_list) )
    colnames(adj_matrix) = rownames(adj_matrix) = game_player_list
    this_stint <-levels(game_subset$all_lineups)[j]
    #print(paste0("this is stint: ",this_stint))
  #add 1 or zero to the adj matrix if colname is in the stint
  for(col in 1:length(colnames(adj_matrix))){
    for(row in 1:length(rownames(adj_matrix))){
      if(col == row) {
        next
      }
      #if the player in the col and row are present in the stint mark 1 in their adj area, otherwise 0
  adj_matrix[row,col] <- ifelse(sum(str_detect(as.character(this_stint), c(colnames(adj_matrix)[col], rownames(adj_matrix)[row]))) ==2,1,0)[1]
    }}
    matrix_list[[k]][[3]] <-adj_matrix
    matrix_list[[k]][[1]] <-game_subset$game_id[1]
    matrix_list[[k]][[2]] <-this_stint
    k=k+1
  }}
return(matrix_list)
}
