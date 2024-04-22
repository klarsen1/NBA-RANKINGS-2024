library(ggplot2)
library(ggimage)
library(ggforce)
library(concaveman)
library(scales)

################## Work directory (change this)
setwd("/Users/kim.larsen/Documents/Code/NBA-RANKINGS-2024/")
options(dplyr.summarise.inform = FALSE)


################## Actual and predicted wins
wins <- group_by(probabilities, team_abbreviation, conference, home_logo) %>%
  filter(game_date>as.Date("2024-01-01")) %>%
  summarise(wins=sum(win), 
            pred_wins=sum(pred_win), 
            games=n()) %>%
  ungroup() %>%
  mutate(win_rate=wins/games, 
         pred_win_rate=pred_wins/games, 
         pred_rank=rank(pred_win_rate, ties="max")-1,
         rank=rank(win_rate,ties.method = "max")-1,
         record=paste0(round(wins), "-", games-round(wins)))

set.seed(2022)
km <- kmeans(dplyr::select(wins, win_rate, pred_win_rate), centers=5, nstart=50, iter.max=500)

################## Scatter plot based on raw rates
ggplot(wins, aes(x=pred_win_rate, y=win_rate)) +
  xlab("What the Model Expected (Based on Rosters)") + ylab(paste0(seasontxt, "  Season Win Rate (as of ",Sys.Date()-1, ")")) +
  geom_point(size = 2, color = 'black') +
  geom_image(aes(image=home_logo), size=.06) + 
  scale_y_continuous(breaks=seq(from=0, to=1, by=.1), limits=c(0,1), labels=percent_format(accuracy=1)) +
  scale_x_continuous(breaks=seq(from=0, to=1, by=.1), limits=c(0,1), labels=percent_format(accuracy=1)) +
  geom_mark_hull(aes(color = as.factor(km$cluster)), expand = unit(3.5,"mm"))+
  theme(legend.position = "none") + 
  geom_smooth(method = "lm", fullrange=TRUE) + 
  geom_text(
    x = .8, y = .8,
    label = "Top Teams",
    color = "black", 
    fontface="bold"
  )

################## Scatter plot based on raw rates
ggplot(wins, aes(x=pred_win_rate, y=win_rate)) +
  xlab("What the Model Expected (Based on Rosters)") + ylab("Win Rate (2024 games)") +
  geom_point(size = 2, color = 'black') +
  geom_image(aes(image=home_logo), size=.06) + 
  scale_y_continuous(breaks=seq(from=0, to=1, by=.1), limits=c(0,1), labels=percent_format(accuracy=1)) +
  scale_x_continuous(breaks=seq(from=0, to=1, by=.1), limits=c(0,1), labels=percent_format(accuracy=1)) +
  geom_mark_hull(aes(color = as.factor(km$cluster)), expand = unit(3.5,"mm"))+
  theme(legend.position = "none") + 
  geom_smooth(method = "lm", fullrange=TRUE) + 
  geom_text(
    x = .8, y = .8,
    label = "Top Teams",
    color = "black", 
    fontface="bold"
  )

ggsave(paste0("./pred/plot", "_",Sys.Date(),".jpg"))
  

