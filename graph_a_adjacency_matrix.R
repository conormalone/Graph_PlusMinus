library(tidyverse)
#function formatting pbp data from "Data-Prep.R" into adjacency matrices
#where every player in a game is featured, players on a stint have a value 1 to each other
graph_a_function <-function(df){
player_adj_list <-  c(top_player_list, "Replacement Player")
k = 1
matrix_list = list()
for(i in 1:length(levels(df$game_id))){
  #get just the game
  game_subset <- df %>%  dplyr::filter(game_id == levels(df$game_id)[i])
  game_subset<-droplevels(game_subset)
  #loop through each game
  for(j in 1:length(levels(game_subset$all_lineups))){
    stint_subset <- game_subset %>%  dplyr::filter(all_lineups == levels(game_subset$all_lineups)[j])
    stint_subset<-droplevels(stint_subset)
    matrix_list[[k]] = list()
    adj_matrix <- matrix(, nrow = 0, ncol =3)
    colnames(adj_matrix) = c("R","C","V")
    this_stint <-levels(game_subset$all_lineups)[j]
    this_stint_clean <-trimws(strsplit(as.character(this_stint), split = ",")[[1]])
    for(rep in 1:length(this_stint_clean[this_stint_clean %in% rep_level_players])){
      str_replace_all(this_stint, this_stint_clean[this_stint_clean %in% rep_level_players][rep],"Replacement Player")
    }
    this_stint_clean[this_stint_clean %in% rep_level_players] <- "Replacement Player"
    
  #iterate over stint
  for(col in 1:10){
    for(row in 1:10){
      if(col == row) {
        next
      }
      #if the player in the col and row are present in the stint mark 1 in their adj area, otherwise 0
      adj_matrix <- rbind(adj_matrix, c((which(player_adj_list == this_stint_clean[row])-1),
                           (which(player_adj_list == this_stint_clean[col])-1), 1))
    }}
    matrix_list[[k]] <-adj_matrix
    k=k+1
  }}
return(matrix_list)
}
