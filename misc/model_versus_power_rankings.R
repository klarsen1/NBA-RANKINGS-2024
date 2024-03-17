setwd("/Users/kim.larsen/Documents/Code/NBA-RANKINGS-2024/misc")

power_rankings <- read_csv("./Power Rankings - 1.3.24.csv")

compare <- group_by(probabilities, team_abbreviation, conference, home_logo) %>%
  summarise(wins=sum(win), 
            pred_wins=sum(pred_win), 
            prob_win=mean(prob_win),
            games=n()) %>%
  ungroup() %>%
  mutate(win_rate=wins/games, 
         pred_win_rate=pred_wins/games, 
         combined=pred_win_rate+win_rate,
         model_rank=rank(-combined,ties.method="max")) %>%
  left_join(power_rankings, by="team_abbreviation")

ggplot(compare, aes(y=model_rank, x=power_rank_01032024)) +
  ylab("Model") + xlab("Power Ranking") +
  geom_point(size = 2, color = 'black') +
  geom_image(aes(image=home_logo), size=.06) + 
  theme(legend.position = "none") + 
  geom_smooth(method = "lm", se=TRUE) + 
  scale_y_reverse(breaks=seq(from=1, to=30, by=5), limits=c(30,1)) + 
  scale_x_reverse(breaks=seq(from=1, to=30, by=5), limits=c(30,1))