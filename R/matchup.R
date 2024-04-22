################## Helper function to get probabilities
matchup <- function(df, team1, team2){
  
  team1_stats <- filter(df, team_abbreviation==team1) 
  if (nrow(team1_stats) != 1){
    print(paste0("Team 1 rows: ", nrow(team1_stats)))
  }
  team2_stats <- filter(df, team_abbreviation==team2)  %>%
    mutate_at(vars(starts_with("PC")), ~(. * -1)) 
  if (nrow(team2_stats) != 1){
    print(paste0("Team 2 rows: ", nrow(team2_stats)))
  }
  
  inputs <- bind_rows(team1_stats, team2_stats) %>%
    summarise_at(vars(starts_with("PC")), sum) %>%
    mutate(home_team=1) %>%
    ungroup() %>%
     dplyr::select(starts_with("PC"), home_team)
  
  Y <- 0
  X <- model.matrix(as.formula(Y ~ .), data=inputs)

  if (nrow(X) != 1){
    print(paste0("X rows: ", nrow(X)))
  }
  df <- xgb.DMatrix(data = X, label= Y)
  p <- predict(boost_coeff, df)
  return(p)

}  
