library(pROC)
library(glmnet)
library(xgboost)

################## Work directory (change this)
setwd("/Users/kim.larsen/Documents/Code/NBA-RANKINGS-2024/")
options(dplyr.summarise.inform = FALSE)

################### Set up the training and validation data
x_vars <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10", "PC11", "home_team")

set.seed(2022)
base_df <- filter(model_df_agg, season>2015)
train <- sample_frac(base_df, .7)
valid <- filter(base_df, !(game_id %in% train$game_id)) 

Y_TRAIN <- train$label
Y_VALID <- valid$label
Y_ALL <- base_df$label
X_TRAIN <- model.matrix(as.formula(Y_TRAIN ~ .), dplyr::select(train, all_of(x_vars)))
X_VALID <- model.matrix(as.formula(Y_VALID ~ .), dplyr::select(valid, all_of(x_vars)))
X_ALL <- model.matrix(as.formula(Y_ALL ~ .), dplyr::select(base_df, all_of(x_vars)))


dtrain <- xgb.DMatrix(data = X_TRAIN, label= Y_TRAIN)
dvalid <- xgb.DMatrix(data = X_VALID, label= Y_VALID)
dall <- xgb.DMatrix(data = X_ALL, label= Y_ALL)

################### Fit the model
set.seed(2022)
watchlist <- list(train=dtrain, test=dvalid)
boost <- xgb.train(data = dtrain, # the data   
                   max.depth = 2,
                   watchlist=watchlist,
                   eta=.1,
                   nthread=2,
                   gamma=0.0,
                   subsample=1,
                   eval_metric="auc",
                   nround = 50, 
                   objective = "binary:logistic")  

ggplot(data=boost$evaluation_log, aes(x=iter, y=test_auc)) + 
  geom_line() + ylab("Validation AUC") + xlab("Iteration")

importance_matrix <- xgb.importance(model = boost)
xgb.plot.importance(importance_matrix = importance_matrix)

p_valid_xgb <- predict(boost, dvalid)
auc(roc(Y_VALID, p_valid_xgb))

################### Check AUC by season
validation_by_season <- data.frame(y=Y_VALID, 
                                   p=p_valid_xgb, 
                                   season=valid$season)
aucs <- data.frame()
for (i in min(validation_by_season$season):max(validation_by_season$season)){
  print(paste0("Season = ", i))
  s <- filter(validation_by_season, season==i)
  a <- auc(roc(s$y, s$p))
  aucs <- bind_rows(data.frame(season=i, auc=a[[1]]), aucs)
}

ggplot(data=aucs, aes(x=season, y=auc)) + geom_bar(stat="identity") + 
  ylab("Validation AUC") + xlab("Season") +
  scale_x_continuous(breaks=seq(from=min(base_df$season), to=max(base_df$season), by=1)) + 
  scale_y_continuous(breaks=seq(from=.0, to=.8, by=.05)) +
  geom_hline(yintercept = .7, linetype=2)


################### Save probabilities and add logos and conference information
conf <- dplyr::select(map, team_abbreviation, conference, division) 
logos <- distinct(schedule_this_season, home_abbreviation, .keep_all = TRUE) %>%
  dplyr::select(home_abbreviation, home_logo) %>%
  rename(team_abbreviation=home_abbreviation)

probabilities <- data.frame(p=predict(boost, dall), game_id=base_df$game_id) %>%
  inner_join(model_df, by="game_id") %>%
  filter(season==current_year) %>%
  mutate(prob_win=if_else(team_indicator==1, p, 1-p), 
         pred_win=if_else(prob_win>.5, 1, 0)) %>%
  dplyr::select(game_id, game_date, team_abbreviation, win, team_score, home_team, prob_win, pred_win) %>%
  inner_join(conf, by="team_abbreviation") %>%
  inner_join(logos, by="team_abbreviation")
