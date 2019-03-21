boardgames_raw <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv")

head(boardgames_raw)

names(boardgames_raw)

library(tidyverse)

try <- boardgames_raw %>%
  filter(users_rated > 10000) %>%
  select(game_id, max_players:playing_time, year_published,
         category, mechanic, average_rating, users_rated) %>%
  select(name, max_players:min_playtime, playing_time:category, average_rating, users_rated)

  #count()
  ggplot(aes(x = users_rated)) +
  geom_histogram()

hist(boardgames_raw$users_rated)

model <- lm(average_rating ~ max_players + min_players + playing_time + year_published, data = try)
summary(model)
names(try)

ggResidpanel::resid_panel(model)
