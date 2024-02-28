library(ggplot2)
library(tidyr)
library(dplyr)
playerdat <- read.csv("FootballData.csv")
playerdat[is.na(playerdat)] <- 0


#Getting the means here
avgPerf <- playerdat %>% summarise(across(is.numeric, mean, na.rm = TRUE))

#Summarizing data that's been curated so far.
ggplot(gather(avgPerf, key = "metric", value = "average"), aes(x = metric, y = average, fill = metric)) +
  geom_bar(stat = "identity") + theme_minimal() + labs(title = "Avg. Performance of CFB Players",
  x = "Category", y = "Avg Val")  + scale_fill_brewer(palette = "Pastel1") 


#Initializing points for each action.
points_per_rush_yard = 0.1
points_per_rush_td = 6
points_per_rec_yard = 0.1
points_per_rec_td = 6

#Computing points for each player based on their performance statistics.
playerdat$FantasyPoints <- with(playerdat, ((Rush.Yds * points_per_rush_yard) + (Rush.TD * points_per_rush_td)
  + (Rec.Yds * points_per_rec_yard) + (Rec.TD * points_per_rec_td)))


#Plotting these Fantasy points
ggplot(playerdat, aes(x = reorder(Player, -FantasyPoints), y = FantasyPoints)) + geom_bar(stat = "identity") +
  labs(title = "Fantasy Points by Player", x = "Player", y = "Fantasy Points") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme_minimal()


#Fitting linear model for expected fantasy points
playMod <- lm(FantasyPoints ~ Rush.Yds + Rush.TD + Rec.Yds + Rec.TD, data = playerdat)
