#putting "Data-Prep.R" into node feature format for each stint, where features are:
#player name one-hot (400 plus 1 rep level for additional players)
#on play one-hot(1) and own team in possessions (1)
#with 1 row per player (10 total)
#get all players not on rep level list using %ni% function inverting %in%
graph_x_function <- function(df){
  graph_x <- list()


iter = 1
for(i in 1:length(levels(df$game_id))){
  #get just the game
  game_subset <- df %>%  dplyr::filter(game_id == levels(df$game_id)[i])
  game_subset<-droplevels(game_subset)
  #get players from this game
  #replace replacement level players
  for(j in 1:length(levels(game_subset$all_lineups))){
    stint_subset <- game_subset %>%  dplyr::filter(all_lineups == levels(game_subset$all_lineups)[j])
    player_list <- unique(trimws(strsplit(as.character(stint_subset$all_lineups), split = ",")[[1]]))
    player_list[player_list %in% rep_level_players] <- "Replacement Player"
    node_feature_matrix <- matrix(0L,nrow = 10, ncol=1)
    colnames(node_feature_matrix) <- "offense"
    for(k in 1:10){
    #list is ordered offense defense so for OGPM each player has 1 for "offense" column
    if(k<=5)  node_feature_matrix[[k,ncol(node_feature_matrix)]] <-1
    }
    graph_x[[iter]] <-node_feature_matrix
    iter = iter+1
    }}
    return(graph_x)
  }