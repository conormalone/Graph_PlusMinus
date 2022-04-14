library(tidyverse)

val_list <- sample(levels(pbp_clean$game_id),length(levels(pbp_clean$game_id))*.25)
train<- pbp_clean %>% dplyr::filter(game_id %ni% val_list)
val<- pbp_clean %>% dplyr::filter(game_id %in% val_list)
train <-droplevels(train)
val <-droplevels(val)
training_data <- list(train_x = graph_x_function(train), train_a = graph_a_function(train), y = graph_y_function(train))
validation_data <- list(train_x = graph_x_function(val), train_a = graph_a_function(val), y = graph_y_function(val))
test_data <- list(train_x = graph_x_function(pbptest_clean), train_a = graph_a_function(pbptest_clean), y = graph_y_function(pbptest_clean))

library(reticulate)
source_python("GNN_Functions.py")


predictions

all_eval <- data.frame(matrix(ncol = 0, nrow = 0))

for(i in 1:nrow(predictions)){
  predict <- max(which(predictions[i,] > 0.5))/10
  actual <- test_data$y[i]
  difference <- actual - predict
  all_eval <- rbind(all_eval, c( predict,actual, difference))
}
colnames(all_eval) <- c("predict","actual", "difference")

write.csv(all_eval, "all_eval.csv")