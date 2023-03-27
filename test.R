library(baseballr) # install.packages('baseballr')
library(tidyverse)

# get current redsox roster

bref_daily_batter('2023-02-24', as.character(Sys.Date()))

tst = bref_daily_batter('2022-08-01', '2022-08-31')

baseballr::mlb_player_game_stats(person_id = "Devers")
?mlb_player_game_stats

playerid_lookup(last_name = "Devers")

baseballr::seas

?mlb_baseball_stats

tst = tst %>%
  mutate(hperab = as.numeric(H/AB))

class(tst$`h/ab`)

ggplot(tst %>% filter (PA > 100), aes(x = Name, y = hperab))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))
  
  
  