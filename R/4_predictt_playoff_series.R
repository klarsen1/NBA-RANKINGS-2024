source("R/matchup.R")
library(R.utils)
sort(unique(box_df$team_abbreviation))


################### Model version used
boost_coeff <- boost

away_team_name <- "IND"
home_team_name <- "BOS"

season <- 2024

games <- filter(probabilities, team_abbreviation %in% c(away_team_name, home_team_name)) %>%
  filter(season==season & playoffs==1) %>%
  group_by(game_id) %>%
  filter(max(row_number())==2) %>%
  filter(team_abbreviation==home_team_name) %>%
  mutate(win_vector=if_else(win==1, 1, -1))
  
results <- rep(0,7)
xx <- as.numeric(games$win_vector)
print(xx)

if (length(xx)>0){
  for (i in 1:length(results)){
    if (i<=length(xx)){
      results[i] <- xx[i]
    }
  }
}

probs <- list()

date <- today()+2

  
################## Trade here
rostersxx <- 
   mutate(rosters, injured=case_when(is.na(return_date) ~ 0, 
                                      return_date<=date ~ 0, 
                                      TRUE ~ 1)) %>%
  mutate(injured=if_else(athlete_display_name %in% c("Jamal Murray"), 0, injured)) %>%
  filter(injured==0)
  
  
#### Players from both teams
players_home <- filter(rostersxx, team_abbreviation %in% home_team_name)$athlete_display_name 
players_away <- filter(rostersxx, team_abbreviation %in% away_team_name)$athlete_display_name 
  
#### Predictive variables
xvars <- 
    group_by(rolling_pcas, athlete_display_name) %>%
    arrange(athlete_display_name, game_date) %>%
    filter(row_number() == n()) %>%
    rename(original_team=team_abbreviation) %>%
    mutate(team_abbreviation=case_when(athlete_display_name %in% players_home ~ home_team_name, 
                                       athlete_display_name %in% players_away ~ away_team_name,
                                       TRUE ~ "REMOVE")) %>%
    filter(team_abbreviation != "REMOVE") %>%
    group_by(team_abbreviation) %>%
    summarise_at(vars(starts_with("PC")), list(~weighted.mean(., w=min))) %>%
    ungroup()
  

prob_home <- matchup(xvars, home_team_name, away_team_name) 
prob_away <- matchup(xvars, away_team_name, home_team_name) 
  
print(prob_home)
print(prob_away)
  
p <- c(prob_home, prob_home, 1-prob_away, 1-prob_away, prob_home, 1-prob_away, prob_home)
  
  for (k in 1:7){
    if (results[k]==1){
      p[k] <- 1
    } else if (results[k]==-1){
      p[k] <- 0
    }
  }
  
  n1 <- 0
  n2 <- 0
  for (k in 1:25000){
    nn1 <- 0
    nn2 <- 0
    for (g in 1:7){
      binomial <- as.numeric(rbinom(n=1, size=1, prob=p[g]))  
      if (binomial==1){nn1 <- nn1+1}
      else {nn2 <- nn2+1}
    }
    if (nn1>nn2){
      n1 <- n1+1
    } else{
      n2 <- n2+1
    }
  }
n1/(n1+n2)
  
 
