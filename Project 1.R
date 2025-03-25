

# Load packages
library(tidyverse)
library(mice)
library(modelr)

# Load data
matchlog <- read_csv("Project 1/atlutd_datascientist_project1_matchlog.csv")
schedule <- read_csv("Project 1/atlutd_datascientist_project1_schedule.csv")

# Data cleaning -----------------------------------------------------------

matchlog <- matchlog %>% select(-"...1")
schedule <- schedule %>% select(-"...1")

# Join data sources
df <- matchlog %>% 
  left_join(schedule)

# Dealing with missing data
md.pattern(df, rotate.names = T)

df <- df %>%
  
  # Correct missing season_names
  group_by(match_id) %>%
  mutate(season_name = first(na.omit(season_name))) %>%
  ungroup() %>% 
  
  # Correct missing birthdays
  group_by(player_id) %>% 
  mutate(birth_date = first(na.omit(birth_date))) %>% 
  ungroup()

# Filtering data
df <- df %>%
  
  # Omit 2020 season
  filter(season_name != 2020) %>% 
  
  # Omit non-regular season games
  filter(competition_stage_id == 1) %>% 
  
  # Omit matches with 0 minutes played (bug)
  filter(player_match_minutes != 0)

# Omit non-regular season games
schedule <- schedule %>% filter(competition_stage_id == 1)


# Feature engineering -----------------------------------------------------

### FEATURE: Home/away
# - Accounts for home advantage
{
  df <- df %>% mutate(home_away =
                        case_when(
                          team_id == home_team_id ~ "Home",
                          team_id == away_team_id ~ "Away",
                          TRUE ~ NA
                        ))
}

### FEATURES: Player historical averages (npxG, xA)
# - Computed as rates (per 100 minutes)
# - Time-decayed minutes-weighted average
# - Account for a player's historical performance
{
  # Rate metrics
  df <- df %>%
    mutate(npxg_p100 = player_match_np_xg / (player_match_minutes / 100),
           xa_p100 = player_match_xa / (player_match_minutes / 100))
  
  
  # Compute time-decayed weighted averages
  df <- df %>%
    arrange(player_id, match_date) %>%
    group_by(player_id) %>%
    mutate(
      most_recent_match = max(match_date, na.rm = TRUE),  # Find the most recent match for each player
      days_before_most_recent_match = as.numeric(difftime(most_recent_match, match_date, units = "days"))  # Calculate the difference in days
    ) %>%
    mutate(
      
      # Decay factors without ifelse (for consistent decay over time)
      decay_factor_light = 0.999 ^ days_before_most_recent_match,
      decay_factor_heavy = 0.99 ^ days_before_most_recent_match,
      
      # Weighted match minutes
      weight_light = player_match_minutes * decay_factor_light,
      weight_heavy = player_match_minutes * decay_factor_heavy,
      
      # Cumulative minutes
      cumulative_mins = cumsum(player_match_minutes),
      
      # Time-decayed weighted averages with decay factor 0.999
      mins_wavg_light_td = cummean(weight_light * player_match_minutes) / cummean(weight_light),
      npxg_p100_wavg_light_td = cummean(weight_light * npxg_p100) / cummean(weight_light),
      xa_p100_wavg_light_td = cummean(weight_light * xa_p100) / cummean(weight_light),
      
      # Time-decayed weighted averages with decay factor 0.995
      mins_wavg_heavy_td = cummean(weight_heavy * player_match_minutes) / cummean(weight_heavy),
      npxg_p100_wavg_heavy_td = cummean(weight_heavy * npxg_p100) / cummean(weight_heavy),
      xa_p100_wavg_heavy_td = cummean(weight_heavy * xa_p100) / cummean(weight_heavy)
    ) %>%
    ungroup()
  
  
  # Calculating averages going into the game (not including the game)
  df <- df %>%
    group_by(player_id) %>%
    mutate(
      
      pre_cumulative_mins = lag(cumulative_mins),
      
      pre_mins_wavg_light_td = lag(mins_wavg_light_td),
      pre_npxg_p100_wavg_light_td = lag(npxg_p100_wavg_light_td),
      pre_xa_p100_wavg_light_td = lag(xa_p100_wavg_light_td),
      
      pre_mins_wavg_heavy_td = lag(mins_wavg_heavy_td),
      pre_npxg_p100_wavg_heavy_td = lag(npxg_p100_wavg_heavy_td),
      pre_xa_p100_wavg_heavy_td = lag(xa_p100_wavg_heavy_td),
      
      # Last game's playing time
      last_game_mins = lag(player_match_minutes),
    ) %>%
    ungroup()  
}


### FEATURE: Opponent historical defensive rating (goals allowed per game) 
# - Time-decayed average
# - Adjusted for league scoring pace
# - Accounts for opponent defensive strength
{
  long_schedule <- schedule %>%
    pivot_longer(cols = c(home_team_id, away_team_id), 
                 names_to = "home_away", 
                 values_to = "team_id") %>%
    mutate(gf = if_else(home_away == "home_team_id", home_score, away_score),
           ga = if_else(home_away == "home_team_id", away_score, home_score)) %>%
    select(match_id, team_id, match_date, gf, ga)
  
  # Compute time-decayed weighted averages
  long_schedule <- long_schedule %>% 
    arrange(team_id, match_date) %>% 
    group_by(team_id) %>% 
    mutate(
      most_recent_match = max(match_date, na.rm = TRUE),  # Find the most recent match for each team
      days_before_most_recent_match = as.numeric(difftime(most_recent_match, match_date, units = "days"))  # Calculate the difference in days
    ) %>%
    mutate(
      
      # Decay factors without ifelse (for consistent decay over time)
      decay_factor_light = 0.999 ^ days_before_most_recent_match,
      decay_factor_heavy = 0.99 ^ days_before_most_recent_match,
      
      # Cumulative games
      cumulative_games = row_number(),
      pre_cumulative_games = row_number() - 1,
      
      # Time-decayed weighted averages
      ga_wavg_light_td = cummean(decay_factor_light * ga) / cummean(decay_factor_light),
      ga_wavg_heavy_td = cummean(decay_factor_heavy * ga) / cummean(decay_factor_heavy),
      
      # Calculating averages going into the game (not including the game)
      pre_ga_wavg_light_td = lag(ga_wavg_light_td),
      pre_ga_wavg_heavy_td = lag(ga_wavg_heavy_td)
    ) %>% 
    ungroup() %>% 
    select(-most_recent_match, -days_before_most_recent_match)
  
  # Calculate time-decayed league average defensive rating
  lg_ga <- long_schedule %>% 
    arrange(match_date) %>% 
    group_by(match_date) %>% 
    reframe(n_games = n() / 2,
            ga = mean(ga, na.rm = T)) %>% 
    mutate(
      most_recent_match = max(match_date, na.rm = TRUE),  # Find the most recent match for each team
      days_before_most_recent_match = as.numeric(difftime(most_recent_match, match_date, units = "days"))  # Calculate the difference in days
    ) %>% 
    mutate(
      
      # Decay factors without ifelse (for consistent decay over time)
      decay_factor_light = 0.999 ^ days_before_most_recent_match,
      decay_factor_heavy = 0.99 ^ days_before_most_recent_match,
      
      # Weighted by number of games
      weight_light = n_games * decay_factor_light,
      weight_heavy = n_games * decay_factor_heavy,
      
      # Time-decayed weighted averages
      lg_ga_wavg_light_td = cummean(weight_light * ga) / cummean(weight_light),
      lg_ga_wavg_heavy_td = cummean(weight_heavy * ga) / cummean(weight_heavy),
      
      # Calculating averages going into the game (not including the game)
      pre_lg_ga_wavg_light_td = lag(lg_ga_wavg_light_td),
      pre_lg_ga_wavg_heavy_td = lag(lg_ga_wavg_heavy_td)
    ) %>% 
    
    fill(pre_lg_ga_wavg_light_td, pre_lg_ga_wavg_heavy_td, .direction = "down")
  
  # Calculate defensive rating relative to league
  long_schedule <- long_schedule %>% 
    left_join(lg_ga %>% 
                select(match_date, pre_lg_ga_wavg_light_td, pre_lg_ga_wavg_heavy_td)
              ) %>% 
    mutate(pre_rel_ga_wavg_light_td = pre_ga_wavg_light_td / pre_lg_ga_wavg_light_td,
           pre_rel_ga_wavg_heavy_td = pre_ga_wavg_heavy_td / pre_lg_ga_wavg_heavy_td) %>% 
    
    fill(pre_rel_ga_wavg_light_td, pre_rel_ga_wavg_heavy_td, .direction = "down")
  
  # Join into main data frame
  df <- df %>% 
    
    # Create opponent team ID
    mutate(opponent_team_id = 
             case_when(
               team_id == home_team_id ~ away_team_id,
               team_id == away_team_id ~ home_team_id,
               TRUE ~ NA
             )) %>% 
    
    # Join in opponent's defensive rating going into the game
    left_join(
      long_schedule %>% 
        select(match_id,
               opponent_team_id = team_id,
               opponent_pre_rel_ga_wavg_light_td = pre_rel_ga_wavg_light_td,
               opponent_pre_rel_ga_wavg_heavy_td = pre_rel_ga_wavg_heavy_td),
      by = join_by(match_id, opponent_team_id)
    )
}


### FEATURES: Player historical playing time
# - Need to fill in 0-minute games
# - Time-decayed minutes-weighted average
{
  player_seasons <- df %>% 
    select(player_id, team_id, season_id) %>% 
    distinct() %>% 
    left_join(
      df %>% 
        group_by(player_id, season_id) %>% 
        arrange(match_date) %>% 
        reframe(first_match_date = min(match_date) %>% as.Date(),
                last_match_date = max(match_date) %>% as.Date(),
                season_unique_teams = n_distinct(team_name))
    )
  
  # Join with match schedule and current playing time data, then fill with 0
  playing_time <- player_seasons %>% 
    
    # Team match schedule
    left_join(
      schedule %>%
        pivot_longer(cols = c(home_team_id, away_team_id), 
                     names_to = "home_away", 
                     values_to = "team_id") %>%
        select(team_id, season_id, match_id, match_date, match_status),
      relationship = "many-to-many"
    ) %>% 
    
    # Playing time data
    left_join(
      df %>% select(player_id, match_id, player_match_minutes)
    ) %>% 
    
    # Fill with 0 in games in which a player did not play
    mutate(player_match_minutes = 
             case_when(
               match_status == "available" ~ coalesce(player_match_minutes, 0),
               TRUE ~ player_match_minutes
             )
           )
  
  
  # Calculate time-decayed weighted average
  playing_time <- playing_time %>%
    arrange(player_id, match_date) %>%
    group_by(player_id) %>%
    mutate(
      most_recent_match = max(match_date, na.rm = TRUE),  # Find the most recent match for each player
      days_before_most_recent_match = as.numeric(difftime(most_recent_match, match_date, units = "days"))  # Calculate the difference in days
    ) %>%
    mutate(
      
      # Decay factors without ifelse (for consistent decay over time)
      decay_factor_light = 0.999 ^ days_before_most_recent_match,
      decay_factor_heavy = 0.99 ^ days_before_most_recent_match,
      
      # Cumulative minutes
      cumulative_mins = cumsum(player_match_minutes),
      
      # Time-decayed weighted averages with decay factor 0.999
      mins_wavg_light_td = cummean(decay_factor_light * player_match_minutes) / cummean(decay_factor_light),
      
      # Time-decayed weighted averages with decay factor 0.995
      mins_wavg_heavy_td = cummean(decay_factor_heavy * player_match_minutes) / cummean(decay_factor_heavy)
    ) %>%
    ungroup()
  
  # Calculating averages going into the game (not including the game)
  playing_time <- playing_time %>%
    group_by(player_id) %>%
    mutate(pre_cumulative_mins = lag(cumulative_mins),
           
           pre_mins_wavg_light_td = lag(mins_wavg_light_td),
           pre_mins_wavg_heavy_td = lag(mins_wavg_heavy_td),
           
           # Last game's minutes
           last_game_mins = lag(player_match_minutes)
    ) %>% 
    ungroup()
}


# npxG rate model ---------------------------------------------------------

npxg_model <- lm(
  npxg_p100 ~ 
    home_away : opponent_pre_rel_ga_wavg_light_td :
    pre_npxg_p100_wavg_light_td,
  data = df %>% filter(pre_cumulative_mins >= 1500)
)

summary(npxg_model)


# xA rate model -----------------------------------------------------------

xa_model <- lm(
  xa_p100 ~ 
    home_away : opponent_pre_rel_ga_wavg_light_td :
    pre_xa_p100_wavg_light_td,
  data = df %>% filter(pre_cumulative_mins >= 1500)
)

summary(xa_model)


  # Minutes model -----------------------------------------------------------

mins_model <- lm(
  player_match_minutes ~ 
    pre_mins_wavg_heavy_td + last_game_mins,
  data = playing_time %>% 
    filter(pre_cumulative_mins >= 1500) %>% 
    
    # Only include players who spent the whole season with the same team
    filter(month(first_match_date) <= 6,
           month(last_match_date) >= 9,
           season_unique_teams == 1)
)

summary(mins_model)


# Predictions -------------------------------------------------------------

# Most recent row for each player
df_most_recent <- df %>% 
  group_by(player_id, team_id, season_id) %>% 
  arrange(match_date) %>% 
  reframe(pre_cumulative_mins = last(cumulative_mins),
          pre_npxg_p100_wavg_light_td = last(npxg_p100_wavg_light_td),
          pre_xa_p100_wavg_light_td = last(xa_p100_wavg_light_td)) %>% 
  left_join(
    playing_time %>% 
      filter(match_date < "2025-03-22",
             match_status == "available") %>% 
      group_by(player_id, team_id, season_id) %>% 
      arrange(match_date) %>% 
      reframe(pre_mins_wavg_heavy_td = last(mins_wavg_heavy_td),
              last_game_mins = last(player_match_minutes))
  )

# This weekend's fixtures
df_preds <- long_schedule %>%
  filter(match_date >= "2025-03-22",
         match_date <= "2025-03-23") %>% 
  mutate(season_id = 315) %>% 
  select(season_id, match_id, team_id, match_date) %>% 
  
  # Join with most recent player data
  left_join(df_most_recent) %>% 
  
  # Calculate opponent_team_id
  left_join(
    schedule %>% 
      select(match_id, home_team_id, away_team_id)
  ) %>% 
  mutate(opponent_team_id = 
           case_when(
             team_id == home_team_id ~ away_team_id,
             team_id == away_team_id ~ home_team_id,
             TRUE ~ NA
           )) %>% 
  
  # Join in opponent's defensive rating going into the game
  left_join(
    long_schedule %>% 
      select(match_id,
             opponent_team_id = team_id,
             opponent_pre_rel_ga_wavg_light_td = pre_rel_ga_wavg_light_td,
             opponent_pre_cumulative_games = pre_cumulative_games),
    by = join_by(match_id, opponent_team_id)
  ) %>% 
  
  # Home/away
  mutate(home_away =
           case_when(
             team_id == home_team_id ~ "Home",
             team_id == away_team_id ~ "Away",
             TRUE ~ NA
           ))


### Regressing to the mean players and expansion teams with extremely low minutes 

mean_npxg_p100 <- df$npxg_p100 %>% mean() # Mean npxG rate
mean_xa_p100 <- df$xa_p100 %>% mean() # Mean xA rate

df_preds <- df_preds %>% 
  mutate(pre_npxg_p100_wavg_light_td =
           case_when(
             pre_cumulative_mins < 500 ~ 
               ((pre_cumulative_mins * pre_npxg_p100_wavg_light_td) +
               ((500 - pre_cumulative_mins) * mean_npxg_p100)) / 500,
             TRUE ~ pre_npxg_p100_wavg_light_td
           ),
         
         pre_xa_p100_wavg_light_td =
           case_when(
             pre_cumulative_mins < 500 ~ 
               ((pre_cumulative_mins * pre_xa_p100_wavg_light_td) +
                  ((500 - pre_cumulative_mins) * mean_xa_p100)) / 500,
             TRUE ~ pre_xa_p100_wavg_light_td
           ),
         
         # Regress San Diego FC (the only expansion team) to the mean
         opponent_pre_rel_ga_wavg_light_td =
           case_when(
             opponent_pre_cumulative_games < 20 ~
               ((opponent_pre_cumulative_games * opponent_pre_rel_ga_wavg_light_td) +
               ((20 - opponent_pre_cumulative_games) * 1)) / 20,
             TRUE ~ opponent_pre_rel_ga_wavg_light_td
           )
         )

# Apply predictions
df_preds <- df_preds %>% 
  add_predictions(npxg_model, var = "pred_npxg_p100") %>% 
  add_predictions(xa_model, var = "pred_xa_p100") %>% 
  add_predictions(mins_model, var = "pred_mins") %>% 
  mutate(pred_npxg_ga = (pred_npxg_p100 + pred_xa_p100) * (pred_mins / 100),
         pred_npxg_ga_p100 = pred_npxg_p100 + pred_xa_p100) %>% 
  arrange(desc(pred_npxg_ga))


# Add player/team names
df_preds <- df_preds %>% 
  
  # Player names
  left_join(
    df %>% 
      select(player_id, player_name) %>% 
      distinct()
  ) %>% 
  
  # Team names
  left_join(
    df %>% 
      select(team_id, team_name) %>% 
      distinct()
  ) %>% 
  
  # Opponent team names
  left_join(
    df %>% 
      select(opponent_team_id = team_id, opponent_team_name = team_name) %>% 
      distinct()
  ) %>% 
  mutate(fixture = 
           case_when(
             home_away == "Home" ~ paste0(opponent_team_name, " (h)"),
             home_away == "Away" ~ paste0(opponent_team_name, " (a)"),
             TRUE ~ NA
           ))
  
# Export predictions
write_csv(df_preds, "Project 1/predictions.csv")

