library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(hoopR)
library(readr)


################### Clear all
rm(list = ls())

################## Work directory (change this)
setwd("/Users/kim.larsen/Documents/Code/NBA-RANKINGS-2024/")
options(dplyr.summarise.inform = FALSE)

################### Settings
current_year <- 2024
start_year <- 2015
seasontxt <- paste0(current_year-1, "-", current_year-2000)
ma_window <- 20
attempts_threshold <- 4
pca_vars <- c("reboundss","stls","blks", "asts", "tos","ft_tendency","tp_tendency", "tsp", "ptss", "plus_minus_n", "height_cm")

################# API calls
schedule_this_season <- hoopR::load_nba_schedule()
schedule_train_period <- hoopR::load_nba_schedule(start_year:current_year)
stats <- hoopR::load_nba_player_box(start_year:current_year)

################### Locations and conferences
map <- read_csv("./misc/team_map.csv") %>%
  mutate(row=row_number())
conf <- dplyr::select(map, team_abbreviation, conference)

################### Player heights
heights <- data.frame(hoopR::nba_playerindex()[[1]]) %>%
  mutate(athlete_display_name=paste0(PLAYER_FIRST_NAME, " ", PLAYER_LAST_NAME)) %>%
  dplyr::select(athlete_display_name, HEIGHT) %>%
  separate(HEIGHT, into=c("ft", "inches"), sep="-") %>%
  mutate(height_cm=as.numeric(ft)*30.48 + as.numeric(inches)*2.54) %>%
  dplyr::select(athlete_display_name, height_cm) %>%
  arrange(-height_cm) %>%
  distinct(athlete_display_name, .keep_all = TRUE)
  
############## Injuries
url <- "https://www.cbssports.com/nba/injuries/daily"
page <- read_html(url)
injury_tables <- html_nodes(page, "table")
injury_df <- data.frame()
for (table in injury_tables) {
  df <- html_table(table)
  
  player_names <- html_text(html_nodes(table, ".CellPlayerName--long a"))
  df$Player <- player_names
  injury_df <- bind_rows(injury_df, df)
}
clean_injuries <- mutate(injury_df, 
                         clean_note=gsub("Expected to be out until at least ", "", `Injury Status`),
                         return_date=case_when(clean_note=="Out for the season" ~ Sys.Date() + 365, 
                                               clean_note=="Game Time Decision" ~ Sys.Date() + 1, 
                                               TRUE ~ as.Date(clean_note, format="%b %d")),
                         return_date=if_else(return_date<Sys.Date(), return_date+365, return_date)) %>%
  rename(athlete_display_name=Player) %>%
  dplyr::select(athlete_display_name, return_date, clean_note) %>%
  arrange(athlete_display_name, desc(return_date)) %>%
  distinct(athlete_display_name, .keep_all = TRUE)

############# Rosters
rosters <- data.frame()
for (i in 1:nrow(map)){
  m <- filter(map, row==i)
  rosteri <- data.frame(hoopR::nba_commonteamroster(
    league_id = "00",
    season = seasontxt, 
    team_id=m$team_id)["CommonTeamRoster"]) %>%
    mutate(team_abbreviation=m$team_abbreviation) %>%
    dplyr::select(team_abbreviation, CommonTeamRoster.PLAYER) %>%
    rename(athlete_display_name=CommonTeamRoster.PLAYER)  
  rosters <- bind_rows(rosteri, rosters)
  Sys.sleep(1)
}

rosters <- left_join(rosters, clean_injuries, by="athlete_display_name") %>%
  arrange(team_abbreviation) 
rm(injury_df)


################### Clean up the player level statistics and create features
box_df <- 
  filter(stats, as.numeric(season_type)>1 & !is.na(minutes)) %>%
  mutate(fg3_made=three_point_field_goals_made,
         fg3_attempts=three_point_field_goals_attempted,
         fg_attempts=field_goals_attempted,
         ft_attempts=free_throws_attempted,
         row=row_number(),
         home=if_else(home_away=="home", 1, 0),
         team_score=as.numeric(team_score),
         season=as.numeric(season),
         season_type=as.numeric(season_type),
         game_date=as.Date(game_date)-1,
         game_id=as.numeric(game_id),
         team_id=as.numeric(team_id),
         athlete_id=as.numeric(athlete_id),
         min=as.numeric(minutes), 
         win=if_else(team_winner==TRUE, 1, 0),
         playoffs=if_else(season_type==3, 1, 0),
         oreb=as.numeric(offensive_rebounds), 
         dreb=as.numeric(defensive_rebounds), 
         stl=as.numeric(steals), 
         ast=as.numeric(assists),
         blk=as.numeric(blocks), 
         to=as.numeric(turnovers),
         pts=as.numeric(points), 
         pf=as.numeric(fouls),
         fg3_made=as.numeric(fg3_made), 
         ft_attempts=as.numeric(ft_attempts), 
         fg_attempts=as.numeric(fg_attempts), 
         fg3_attempts=as.numeric(fg3_attempts),
         attempts=(fg_attempts+fg3_attempts+ft_attempts),
         possessions=fg_attempts + ast + to + .44 * ft_attempts,
         asts=if_else(possessions>attempts_threshold, as.numeric(ast)/possessions, 0),
         tos=if_else(possessions>attempts_threshold, as.numeric(to)/possessions, 0),
         rebounds=oreb+dreb,
         tsp=if_else(attempts>attempts_threshold,pts/(2*(fg_attempts+.44*ft_attempts)),0),
         ft_tendency=if_else(attempts>attempts_threshold, ft_attempts/attempts, 0),
         tp_tendency=if_else(attempts>attempts_threshold, fg3_attempts/attempts, 0),
         plus_minus_n=if_else(grepl("+", plus_minus), as.numeric(gsub("+", "", plus_minus)), -as.numeric(gsub("-", "", plus_minus)))) %>%
  left_join(heights, by="athlete_display_name") %>%
  group_by(season) %>%
  mutate(m_height=mean(height_cm, na.rm=TRUE)) %>% ### for inputation
  group_by(game_id) %>%
  mutate(total_blks=sum(blk),
         total_stls=sum(stl),
         total_pts=sum(pts),
         total_tp=sum(fg3_made),
         total_rebounds=sum(rebounds),
         reboundss=if_else(total_rebounds>0, rebounds/total_rebounds, 0), 
         blks=if_else(total_blks>0, blk/total_blks, 0), 
         stls=if_else(total_stls>0, stl/sum(stl), 0), 
         ptss=if_else(total_pts>0, pts/sum(pts), 0), 
         tpss=if_else(total_tp>0, fg3_made/total_tp, 0)) %>%
  filter(!(team_abbreviation %in% c("LEB", "USA", "WORLD", "GIA", "DUR", "WEST", "EAST"))) %>%
  filter(min>0) %>%
  filter(!between(game_date, as.Date("2020-07-09"), as.Date("2020-10-11"))) %>%
  filter(playoffs==0) %>%
  group_by(athlete_display_name)%>%
  mutate(height_cm=if_else(is.na(height_cm), m_height, height_cm)) %>% ### impute
  group_by(athlete_display_name, season) %>%
  mutate(minutes_played_season=sum(min)) %>%
  dplyr::select(athlete_id, athlete_display_name, athlete_headshot_href, 
                team_id, team_abbreviation, home, team_score, win,
                season, season_type, game_id, game_date, 
                min, minutes_played_season, playoffs,
                all_of(pca_vars)) %>%
  ungroup() 


################### Create player level normalized statistics (within season)
players_df_collapsed <- 
  group_by(box_df,athlete_display_name, season, minutes_played_season) %>%
  summarise_at(vars(all_of(pca_vars)), list(~weighted.mean(., w=min))) %>%
  group_by(season) %>%  
  mutate(across(all_of(pca_vars), ~ scale(.)[,1])) %>%
  group_by(athlete_display_name) %>%
  summarise_at(vars(all_of(pca_vars)), list(~weighted.mean(., w=minutes_played_season))) %>%
  mutate(across(all_of(pca_vars), ~ scale(.)[,1])) %>%
  dplyr::select(all_of(pca_vars))


################### Calculate the PCAs
pca <- prcomp(players_df_collapsed, center = TRUE, scale. = TRUE)
cumsum(pca$sd^2/sum(pca$sd^2)*100) 

################### Predict player in-game performance based on a rolling mean
rolling_means <- 
  group_by(box_df, athlete_display_name) %>%
  dplyr::select(season, athlete_display_name, game_id, game_date, all_of(pca_vars), 
                season_type, team_score, team_abbreviation, playoffs,
                height_cm, playoffs, home, min, athlete_headshot_href, win) %>%
  arrange(athlete_display_name, game_date) %>%
  mutate(mins=lag(frollsum(min, ma_window, align = "right", algo="exact")),
         across(all_of(pca_vars), ~ lag(frollsum(.x*min, ma_window, align = "right", algo="exact"))/mins), 
         min=lag(frollmean(min, ma_window, align = "right", algo="exact"))) %>%
  filter(row_number()>ma_window) %>%
  group_by(season) %>%  
  mutate(across(all_of(pca_vars), ~ scale(.)[,1])) %>%
  ungroup() %>%
  mutate(across(all_of(pca_vars), ~ scale(.)[,1]))
  
rolling_pcas <- bind_cols(data.frame(predict(pca, rolling_means)), rolling_means) %>% 
                dplyr::select(game_id, game_date, win, playoffs, 
                              team_abbreviation, team_score, min, athlete_display_name, athlete_headshot_href,
                              season, season_type, home, height_cm, starts_with("PC")) %>%
  arrange(athlete_display_name, game_date)
rm(rolling_means)

################### Create labels and assigns random signs for team net difference subtraction
model_df <- 
  group_by(rolling_pcas, season, season_type, game_id, game_date, team_abbreviation, team_score, home, playoffs, win) %>%
  summarise_at(vars(starts_with("PC")), list(~weighted.mean(., w=min))) %>%
  group_by(game_id) %>%
  mutate(full_record=if_else(n()==2, 1, 0),
         r = sample(100,1, replace=TRUE), 
         team_indicator=case_when(r<50 & row_number()==1 ~ 2, 
                                  r<50 & row_number()==2 ~ 1,
                                  TRUE ~ as.numeric(row_number()))) %>%
  filter(full_record==1) %>%
  dplyr::select(-full_record) %>%
  ungroup() %>%
  arrange(game_id, team_indicator) %>%
  mutate(m=if_else(team_indicator==1, 1, -1), 
         label=if_else(team_indicator==1 & win==1, 1, 0), 
         home_team=if_else(team_indicator==1 & home==1, 1, 0)) %>% 
  mutate_at(vars(starts_with("PC")), ~(.*m)) %>%
  arrange(game_date, game_id, team_abbreviation) %>%
  ungroup()

################### Collapse the data so we have one record per game with net differences and labels
model_df_agg <-
  group_by(model_df, game_id, season, season_type, playoffs, game_date) %>%
  summarise_at(vars(starts_with("PC"), label, home_team), sum) %>%
  ungroup() 

summary(model_df_agg)