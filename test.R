library(baseballr) # install.packages('baseballr')
library(tidyverse)

# get current redsox roster

bref_daily_batter('2026-02-24', as.character(Sys.Date()))

tst = bref_daily_batter('2025-03-01', as.character(Sys.Date()))
colnames(tst)
unique(tst$Team)

# get stats for red sox


playerid_lookup(last_name = "Devers")

baseballr::seas

?mlb_baseball_stats

tst = tst %>%
  mutate(hperab = as.numeric(H/AB))

class(tst$`h/ab`)

ggplot(tst %>% filter (PA > 100), aes(x = Name, y = hperab))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))
  
  
  