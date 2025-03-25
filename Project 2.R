

# Load packages
library(tidyverse)
library(janitor)
library(DescTools)
library(showtext)


# Load data
event <- readRDS("Project 2/atlutd_datascientist_project2_eventdata.rds")
player_pt <- read_csv("Project 2/atlutd_datascientist_project2_player_mins_appearances.csv")
schedule <- read_csv("Project 2/atlutd_datascientist_project2_schedule.csv")

# Clean names
event <- event %>% clean_names()


# Calculate player KPI season totals --------------------------------------

# Position
{
  position <- event %>% 
    filter(!is.na(position_name),
           position_name != "Substitute") %>% 
    mutate(posgroup = 
             case_when(
               position_name == "Goalkeeper" ~ "GK",
               position_name %>% str_detect("Center Back") ~ "CB",
               position_name %>% str_detect(c("t Back|Wing Back")) ~ "FB/WB",
               position_name %>% str_detect("Defensive Midfield") ~ "DM",
               position_name %>% str_detect("Center Midfield") ~ "CM",
               position_name %>% str_detect("Attacking Midfield") ~ "AM",
               position_name %in% c("Left Midfield", "Left Wing", "Right Midfield", "Right Wing") ~ "W",
               position_name %>% str_detect("Forward") ~ "FW",
               TRUE ~ NA_character_
             )) %>%
    group_by(player_id) %>% 
    reframe(main_posgroup = Mode(posgroup, na.rm = T),
            main_position_name = Mode(position_name, na.rm = T))
}

# Shot execution (modified)
{
  shot_execution <- event %>% 
    filter(type_name == "Shot",
           shot_type_name != "Penalty") %>%
    mutate(goal = if_else(shot_outcome_name == "Goal", 1, 0),
           shot_execution = 
             (0.75 * shot_shot_execution_xg + 0.25 * goal) - 
             shot_statsbomb_xg) %>% 
    group_by(player_id) %>% 
    reframe(shot_execution = sum(shot_execution, na.rm = T))
}

# Non-penalty xG
{
  npxg <- event %>% 
    filter(type_name == "Shot",
           shot_type_name != "Penalty") %>%
    group_by(player_id) %>% 
    reframe(npxg = sum(shot_statsbomb_xg, na.rm = T))
}

# xA
{
  xa <- event %>% 
    filter(pass_shot_assist == TRUE) %>% 
    select(player_id, pass_assisted_shot_id) %>% 
    left_join(
      event %>% 
        select(pass_assisted_shot_id = id,
               shot_statsbomb_xg)
    ) %>% 
    group_by(player_id) %>% 
    reframe(xa = sum(shot_statsbomb_xg, na.rm = T))
}

# Progression (Receiving, Dribbling/Carrying, Passing)
{
  # Dribbling/Carrying
  prog_dribcar <- event %>% 
    filter(type_name %in% c("Carry", "Dribble")) %>%
    group_by(player_id) %>%
    mutate(start_dist_to_goal = sqrt((120 - location_x)^2 + (40 - location_y)^2),
           end_dist_to_goal = sqrt((120 - carry_end_location_x)^2 + (40 - carry_end_location_y)^2),
           pct_closer_to_goal = (start_dist_to_goal - end_dist_to_goal) / start_dist_to_goal) %>% 
    filter(pct_closer_to_goal > 0) %>% 
    group_by(player_id) %>% 
    reframe(prog_dribcar = sum(pct_closer_to_goal, na.rm = T))
  
  # Identify passes moving closer to opposition goal
  prog_passes <- event %>% 
    filter(type_name %in% c("Pass"),
           !(pass_type_name %in% c("Corner", "Free Kick", "Goal Kick", "Throw-in")),
           is.na(pass_outcome_name)) %>%
    group_by(player_id) %>%
    mutate(start_dist_to_goal = sqrt((120 - location_x)^2 + (40 - location_y)^2),
           end_dist_to_goal = sqrt((120 - pass_end_location_x)^2 + (40 - pass_end_location_y)^2),
           pct_closer_to_goal = (start_dist_to_goal - end_dist_to_goal) / start_dist_to_goal) %>% 
    ungroup() %>% 
    filter(pct_closer_to_goal > 0) %>% 
    mutate(is_prog_pass = 1)
  
  # Passing
  prog_pass <- prog_passes %>% 
    group_by(player_id) %>% 
    reframe(prog_pass = sum(pct_closer_to_goal, na.rm = T))
  
  # Receiving
  prog_receipt <- event %>% 
    filter(type_name %in% c("Pass", "Ball Receipt*")) %>% 
    left_join(
      prog_passes %>% select(id, is_prog_pass, pct_closer_to_goal)
    ) %>% 
    group_by(match_id, team_id) %>% 
    arrange(elapsed_time) %>% 
    mutate(is_prog_receipt =
             case_when(
               type_name == "Ball Receipt*" &
                 lag(type_name) == "Pass" &
                 lag(is_prog_pass) == 1 &
                 lag(pass_recipient_id) == player_id ~ 1,
               TRUE ~ 0
             )) %>% 
    mutate(receipt_pct_closer_to_goal =
             case_when(
               is_prog_receipt == 1 ~ lag(pct_closer_to_goal),
               TRUE ~ NA
             )) %>% 
    ungroup() %>% 
    filter(is_prog_receipt == 1) %>% 
    group_by(player_id) %>% 
    reframe(prog_receipt = sum(receipt_pct_closer_to_goal, na.rm = T))
}

# Pass execution
{
  pass_execution <- event %>% 
    filter(type_name %in% c("Pass"),
           !(pass_type_name %in% c("Corner", "Free Kick", "Goal Kick", "Throw-in")),
           !is.na(pass_pass_success_probability)) %>% 
    mutate(pass_successful = 
             case_when(
               is.na(pass_outcome_name) ~ 1,
               TRUE ~ 0
             )) %>%
    mutate(pass_execution = pass_successful - pass_pass_success_probability) %>% 
    group_by(player_id) %>% 
    reframe(pass_execution = sum(pass_execution, na.rm = T))
}

# Turnovers
{
  non_pass_turnovers <- event %>% 
    filter(type_name %in% c("Dispossessed", "Miscontrol") |
             (type_name == "Dribble" & dribble_outcome_name == "Incomplete")) %>% 
    group_by(player_id) %>% 
    reframe(non_pass_turnovers = n())
  
  pass_turnovers <- event %>% 
    filter(type_name %in% c("Pass"),
           !(pass_type_name %in% c("Corner", "Free Kick", "Goal Kick", "Throw-in")),
           pass_outcome_name %in% c("Incomplete", "Out", "Pass Offside")) %>%
    group_by(player_id) %>% 
    reframe(pass_turnovers = n())
}

# Defensive activity
{
  defensive_actions <- event %>% 
    filter((type_name == "50/50" & x50_50_outcome_name %in% c("Won", "Success To Team")) |
             (type_name == "Ball Recovery" & is.na(ball_recovery_recovery_failure)) |
             (type_name == "Interception" & interception_outcome_name %in% c("Won", "Success In Play", "Success Out")) |
             type_name %in% c("Block", "Clearance", "Pressure") |
             duel_type_name == "Tackle") %>% 
    group_by(player_id) %>% 
    reframe(defensive_actions = n())
}

# Team offense
{
  team_offense <- event %>% 
    group_by(team_id, match_id) %>% 
    reframe(xg = sum(shot_statsbomb_xg, na.rm = T)) %>%
    group_by(team_id) %>% 
    reframe(team_season_xg_per_game = mean(xg, na.rm = T))
}

# Join all metrics
{
  player_pt <- player_pt %>% 
    left_join(team_offense) %>% 
    group_by(player_id) %>% 
    reframe(team_id = paste(unique(team_id), collapse = ", "),
            appearances = sum(player_season_appearances),
            minutes = sum(player_season_minutes),
            team_season_xg_per_game = weighted.mean(team_season_xg_per_game, player_season_appearances))
  
  player <- player_pt %>% 
    left_join(position) %>% 
    left_join(shot_execution) %>% 
    left_join(npxg) %>% 
    left_join(xa) %>% 
    left_join(prog_dribcar) %>% 
    left_join(prog_pass) %>% 
    left_join(prog_receipt) %>% 
    left_join(pass_execution) %>% 
    left_join(non_pass_turnovers) %>% 
    left_join(pass_turnovers) %>% 
    left_join(defensive_actions) %>% 
    mutate(across(where(is.numeric), ~ coalesce(.x, 0)))
}

# Calculate per 100
player <- player %>% 
  mutate(across(8:17, ~ . / (minutes / 100), .names = "{.col}_p100"))

# Calculate Z-scores
player <- player %>%
  filter(minutes >= 300) %>% 
  group_by(main_posgroup) %>% 
  mutate(across(ends_with("_p100") | ends_with("per_game"), 
                ~ ( . - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE),
                .names = "z_{.col}")) %>% 
  ungroup()

# Create composite score
player <- player %>% 
  mutate(composite_score =
           - shot_execution_p100
           + 5 * z_npxg_p100 # Some metrics weighted heavier based on my own experience
           + 2 * z_xa_p100
           + z_prog_receipt_p100
           + z_prog_dribcar_p100
           + 2 * z_prog_pass_p100
           + pass_execution_p100
           - z_non_pass_turnovers_p100
           - z_pass_turnovers_p100
           + 2 * z_defensive_actions_p100
           - z_team_season_xg_per_game,
         composite_score_with_pt = composite_score * sqrt(minutes)
         )

# Find 3 attacking players
player <- player %>% 
  filter(main_posgroup %in% c("FW", "W", "AM"),
         minutes >= 500) %>% 
  arrange(desc(composite_score_with_pt)) %>% 
  mutate(rank = row_number())


# Add Open Sans font
font_add_google("Open Sans", "open_sans")
showtext_auto()

for (i in 1:6) {
  
  player_id <- player[i, "player_id"]
  team_id <- player[i, "team_id"]
  minutes <- round(player[i, "minutes"], 0)
  main_position_name <- player[i, "main_position_name"]
  
  player %>%  
    filter(rank == i) %>%  
    select(shot_execution_p100, z_npxg_p100, z_xa_p100,  
           z_prog_receipt_p100, z_prog_dribcar_p100, z_prog_pass_p100,  
           pass_execution_p100, z_non_pass_turnovers_p100, z_pass_turnovers_p100,  
           z_defensive_actions_p100) %>%  
    mutate(z_non_pass_turnovers_p100 = -z_non_pass_turnovers_p100,  
           z_pass_turnovers_p100 = -z_pass_turnovers_p100) %>%  
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%  
    mutate(value = pmin(value, 3),
           value = value + 3) %>%  
    mutate(variable = factor(variable,  
                             levels = rev(c("shot_execution_p100", "z_npxg_p100", "z_xa_p100",  
                                            "z_prog_receipt_p100", "z_prog_dribcar_p100", "z_prog_pass_p100",  
                                            "pass_execution_p100", "z_non_pass_turnovers_p100", "z_pass_turnovers_p100",  
                                            "z_defensive_actions_p100")))) %>%  
    ggplot(aes(x = value, y = variable, fill = value)) +  
    geom_bar(stat = "identity") +  
    theme_void(base_family = "open_sans", base_size = 16) +  # Increase base font size
    labs(x = "Z-Score", y = NULL,  
         title = paste0("Player ", player_id),
         subtitle = paste0("Team: ", team_id, " | Minutes: ", minutes,
                           "\nPosition: ", main_position_name)) +  
    scale_y_discrete(labels = c(  
      "shot_execution_p100" = "Shot Execution",  
      "z_npxg_p100" = "Non-penalty xG",  
      "z_xa_p100" = "xA",  
      "z_prog_receipt_p100" = "Progression - Receiving",  
      "z_prog_dribcar_p100" = "Progression - Dribbling",  
      "z_prog_pass_p100" = "Progression - Passing",  
      "pass_execution_p100" = "Pass Execution",  
      "z_non_pass_turnovers_p100" = "Non-Pass Turnovers",  
      "z_pass_turnovers_p100" = "Pass Turnovers",  
      "z_defensive_actions_p100" = "Defensive Actions"  
    )) +  
    scale_x_continuous(limits = c(0, 6), 
                       breaks = seq(0, 6, by = 1), 
                       labels = seq(-3, 3, by = 1))  +
    scale_fill_gradient2(low = "#D61F1F", mid = "#FFD301", high = "#008000", midpoint = 2.75) +  
    theme(axis.text.y = element_text(angle = 0, color = "black", size = 14, hjust = 1),  # Right-align y-axis text
          axis.text.x = element_text(color = "black", size = 14),  
          axis.title.x = element_text(color = "black", size = 16),  
          axis.title.y = element_text(color = "black", size = 16),  
          legend.position = "none",  
          plot.title = element_text(hjust = 0.15, face = "bold", color = "black", size = 20),
          plot.subtitle = element_text(hjust = -0.25, color = "black", size = 14),
          plot.margin = margin(5, 5, 5, 5))
  
  # Save the plot
  ggsave(paste0("Project 2/player_", i, ".png"), 
         plot = last_plot(),  
         width = 1.8,       
         height = 2,       
         dpi = 300,            
         units = "in")         
}


