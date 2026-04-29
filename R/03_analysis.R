library(tidyr)
library(ggplot2)
library(baseballr)
library(purrr)
library(lubridate)

most_recent_mlb_season()
BOS = baseballr::bref_team_results("BOS", most_recent_mlb_season())
head(BOS)

batters_26 = baseballr::bref_daily_batter(t1= paste0(most_recent_mlb_season(),"-01-01"), t2=paste0(most_recent_mlb_season(), "-", format(Sys.Date(), "%m-%d")))

# DDAY is day of Cora et al firing
DDAY = "2026-04-25"

BOS_batters = batters_26 %>% filter(Team == "Boston")

BOS_batters_names_ids = BOS_batters %>%
  select(bbref_id, Name)

BOS_batters_names_ids

ds <- baseballr::chadwick_player_lu()

ds_bos = ds %>%
  filter(key_bbref %in% BOS_batters_names_ids$bbref_id)

# Define date ranges to pull in ~2 week chunks
date_ranges <- tibble(
  start = seq(as.Date("2026-03-20"), as.Date(Sys.Date()), by = "7 days"),
  end   = pmin(start + 6, Sys.Date())
)

# Pull each chunk and bind together
sc_all <- map2_dfr(date_ranges$start, date_ranges$end, function(s, e) {
  message("Pulling ", s, " to ", e)
  statcast_search(start_date = as.character(s), end_date = as.character(e))
})

# Save
saveRDS(sc_all, "statcast_2026_all.rds")

# Load it back later
sc_all <- readRDS("statcast_2026_all.rds")

# Deduplicate just in case
sc_all <- sc_all %>% distinct()

summary(sc_all)

sc_bos = sc_all %>%
  filter(batter %in% ds_bos$key_mlbam)

colnames(sc_bos)

head(sc_bos)
##### #####
sc_bos_daily <- sc_bos %>%
  filter(!is.na(events) | description != "") %>%  # keep meaningful pitches
  group_by(player_name, game_date) %>%
  summarise(
    # Plate appearances and at bats
    PA  = sum(woba_denom == 1, na.rm = TRUE),
    AB  = sum(events %in% c("single","double","triple","home_run",
                            "strikeout","field_out","grounded_into_double_play",
                            "force_out","fielders_choice","field_error",
                            "strikeout_double_play","double_play",
                            "triple_play"), na.rm = TRUE),
    
    # Hits
    H   = sum(events %in% c("single","double","triple","home_run"), na.rm = TRUE),
    BB  = sum(events == "walk", na.rm = TRUE),
    HBP = sum(events == "hit_by_pitch", na.rm = TRUE),
    SF  = sum(events == "sac_fly", na.rm = TRUE),
    
    # Total bases for slugging
    TB  = sum(case_when(
      events == "single"    ~ 1,
      events == "double"    ~ 2,
      events == "triple"    ~ 3,
      events == "home_run"  ~ 4,
      TRUE ~ 0
    ), na.rm = TRUE),
    
    # Rate stats
    AVG = round(H / AB, 3),
    OBP = round((H + BB + HBP) / (AB + BB + HBP + SF), 3),
    SLG = round(TB / AB, 3),
    OPS = round(OBP + SLG, 3),
    
    # Statcast stuff
    avg_launch_speed = round(mean(launch_speed, na.rm = TRUE), 1),
    avg_launch_angle = round(mean(launch_angle, na.rm = TRUE), 1),
    avg_bat_speed    = round(mean(bat_speed, na.rm = TRUE), 1),
    avg_swing_length = round(mean(swing_length, na.rm = TRUE), 2),
    hard_hit_pct     = round(mean(launch_speed >= 95, na.rm = TRUE), 3),
    
    .groups = "drop"
  ) %>%
  filter(PA > 0)  # drop games with no plate appearances


#### ####
sc_bos_cumulative <- sc_bos_daily %>%
  arrange(player_name, game_date) %>%
  group_by(player_name) %>%
  mutate(
    cum_AB  = cumsum(AB),
    cum_H   = cumsum(H),
    cum_BB  = cumsum(BB),
    cum_HBP = cumsum(HBP),
    cum_SF  = cumsum(SF),
    cum_TB  = cumsum(TB),
    cum_AVG = round(cum_H / cum_AB, 3),
    cum_OBP = round((cum_H + cum_BB + cum_HBP) / (cum_AB + cum_BB + cum_HBP + cum_SF), 3),
    cum_SLG = round(cum_TB / cum_AB, 3),
    cum_OPS = round(cum_OBP + cum_SLG, 3)
  )
#####

sc_bos_daily %>%
  pivot_longer(
    cols = c(avg_bat_speed, avg_launch_angle, avg_launch_speed, avg_swing_length, hard_hit_pct),
    names_to  = "metric",
    values_to = "value"
  ) %>%
  ggplot(aes(x = game_date, y = value, color = player_name)) +
  geom_path() +
  facet_wrap(~ metric, scales = "free_y") +
  labs(title = "BOS Batter Statcast Metrics", x = NULL, color = "Player") +
  theme_minimal()

ggplot(sc_bos_daily, aes(x = game_date, y = avg_bat_speed, color = player_name, fill = player_name))+
  geom_path()
