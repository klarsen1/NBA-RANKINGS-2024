library(ggplot2)
library(ggimage)
library(ggforce)
library(concaveman)

################## Work directory (change this)
setwd("/Users/kim.larsen/Documents/Code/NBA-RANKINGS-2024/")
options(dplyr.summarise.inform = FALSE)


################## Actual and predicted wins
wins <- group_by(probabilities, team_abbreviation, conference, home_logo) %>%
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
km <- kmeans(dplyr::select(wins, win_rate, pred_win_rate), centers=5, nstart=100, iter.max=100)

################## Scatter plot
ggplot(wins, aes(x=pred_rank, y=rank)) +
  xlab("What the Model Expected") + ylab("Season Rank Basedd on Wins and Losses") +
  geom_point(size = 2, color = 'black') +
  geom_abline(intercept = 0, slope = 1, size = 0.5, linetype=2) +
  geom_image(aes(image=home_logo), size=.06) + 
  scale_y_continuous(breaks=seq(from=0, to=30, by=3)) +
  scale_x_continuous(breaks=seq(from=0, to=30, by=3)) +
  geom_mark_hull(aes(color = as.factor(km$cluster)), expand = unit(3.5,"mm"))+
  theme(legend.position = "none")
  

ggsave(paste0("./pred/plot", "_",Sys.Date(),".jpg"))
  

