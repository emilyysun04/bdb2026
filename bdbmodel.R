setwd("/Users/emilyysun/Desktop/bdb")

library(dplyr)
library(purrr)
library(stringr)
library(mgcv)
library(ggplot2)
library(modelsummary)

source("get_moments03.R")     # build_play_df_R(), get_three_moments()
source("catch_control02.R")  # compute_control_at_frame()

parse_clock_seconds <- function(clock_mmss) {
  parts <- str_split_fixed(as.character(clock_mmss), ":", 2)
  as.numeric(parts[,1]) * 60 + as.numeric(parts[,2])
}

is_dropback <- function(meta_row) {
  meta_row$pass_result %in% c("C","I","S","IN","R")
}

# Control computation

get_control_at_frame <- function(play_df, frame_id_in, grid_x, grid_y) {
  
  if (length(frame_id_in) != 1 || is.na(frame_id_in)) return(NA_real_)
  
  df_m <- play_df %>% filter(frame_id == frame_id_in)
  
  cx <- play_df$ball_land_x[1]
  cy <- play_df$ball_land_y[1]
  if (is.na(cx) || is.na(cy)) return(NA_real_)
  
  control <- tryCatch(
    compute_control_at_frame(df_m, cx, cy, grid_x, grid_y),
    error = function(e) NULL
  )
  
  if (is.null(control) || is.null(control$scores)) return(NA_real_)
  
  scores <- control$scores
  
  if (!is.data.frame(scores) ||
      !"prob" %in% names(scores) ||
      nrow(scores) == 0 ||
      anyNA(scores$prob)) return(NA_real_)
  
  max(scores$prob, na.rm = TRUE)
}

# One row per play

make_one_play_row <- function(gid, pid,
                              df_input, df_output, supp,
                              grid_x, grid_y) {
  
  play_df <- tryCatch(
    build_play_df_R(gid, pid, df_input, df_output, supp),
    error = function(e) NULL
  )
  if (is.null(play_df) || nrow(play_df) == 0) return(NULL)
  
  meta <- play_df[1,]
  
  # dropbacks only
  if (!is_dropback(meta)) return(NULL)
  
  moments <- get_three_moments(play_df)
  
  snap_f  <- moments$frame[moments$moment == "Snap"]
  drop_f  <- moments$frame[moments$moment == "Top of Drop"]
  throw_f <- moments$frame[moments$moment == "Frame Before the Throw"]
  
  snap_def  <- get_control_at_frame(play_df, snap_f,  grid_x, grid_y)
  drop_def  <- get_control_at_frame(play_df, drop_f,  grid_x, grid_y)
  throw_def <- get_control_at_frame(play_df, throw_f, grid_x, grid_y)
  
  # EARLY control: prefer Top of Drop, fallback to Snap
  early_def <- ifelse(!is.na(drop_def), drop_def, snap_def)
  
  did_throw <- as.integer(meta$pass_result %in% c("C","I"))
  complete  <- ifelse(did_throw == 1,
                      as.integer(meta$pass_result == "C"),
                      NA_integer_)
  
  tibble(
    game_id = gid,
    play_id = pid,
    week    = meta$week,
    quarter = meta$quarter,
    down    = meta$down,
    yards_to_go = meta$yards_to_go,
    pass_length = meta$pass_length,
    team_coverage_man_zone = meta$team_coverage_man_zone,
    expected_points_added = meta$expected_points_added,
    defensive_team = meta$defensive_team,
    
    snap_def_max   = snap_def,
    drop_def_max   = drop_def,
    early_def_max  = early_def,
    throw_def_max  = throw_def,
    
    delta_snap_to_drop =
      ifelse(!is.na(drop_def) & !is.na(snap_def),
             drop_def - snap_def,
             NA_real_),
    
    delta_early_to_throw =
      ifelse(!is.na(throw_def) & !is.na(early_def),
             throw_def - early_def,
             NA_real_),
    
    did_throw = did_throw,
    complete  = complete
  )
}
weeks <- sprintf("%02d", 1:18)

df_input <- map_dfr(
  weeks,
  ~ read.csv(paste0("input_2023_w", .x, ".csv")) %>%
    mutate(week = as.integer(.x))
)

df_output <- map_dfr(
  weeks,
  ~ read.csv(paste0("output_2023_w", .x, ".csv")) %>%
    mutate(week = as.integer(.x))
)

supp <- read.csv("supplementary_data.csv")

grid_x <- seq(0, 120, length.out = 80)
grid_y <- seq(0, 53.3, length.out = 40)

plays <- df_input %>% distinct(game_id, play_id)

model_df <- map_dfr(
  seq_len(nrow(plays)),
  \(i) make_one_play_row(
    plays$game_id[i],
    plays$play_id[i],
    df_input,
    df_output,
    supp,
    grid_x,
    grid_y
  )
)


# Early defensive control:
# - Top of Drop when available
# - Snap as fallback

model_df_clean <- model_df %>%
  filter(
    !is.na(early_def_max),     # require early-play defensive control
    !is.na(pass_length),
    !is.na(yards_to_go)
  ) %>%
  mutate(
    team_coverage_man_zone = as.factor(team_coverage_man_zone)
  )

cat("Plays scanned:", nrow(plays), "\n")
cat("Plays kept:", nrow(model_df_clean), "\n")

model_df_clean %>% count(did_throw)

# MODEL 1: Completion probability (conditional on a throw)

train_throw <- model_df_clean %>%
  filter(did_throw == 1, !is.na(complete))

glm_complete_train <- glm(
  complete ~
    early_def_max +
    pass_length +
    yards_to_go +
    down +
    team_coverage_man_zone,
  data = train_throw,
  family = binomial()
)

summary(glm_complete_train)


# Score all dropbacks
# Counterfactual: P(complete | if throw)

scored_df <- model_df_clean %>%
  mutate(
    p_complete_if_throw =
      predict(glm_complete_train, newdata = ., type = "response")
  )

scored_df %>%
  group_by(did_throw) %>%
  summarise(
    n = n(),
    mean_p = mean(p_complete_if_throw, na.rm = TRUE),
    median_p = median(p_complete_if_throw, na.rm = TRUE),
    .groups = "drop"
  )


# Visualization

scored_df %>%
  mutate(outcome = ifelse(did_throw == 1,
                          "Throw (C/I)",
                          "No Throw (S/R/etc)")) %>%
  ggplot(aes(x = outcome, y = p_complete_if_throw, fill = outcome)) +
  geom_boxplot(width = 0.25, outlier.alpha = 0.15) +
  labs(
    title = "Predicted Completion Probability (Trained on Throws Only)",
    subtitle = "Applied to all dropbacks as a throwability score",
    x = NULL,
    y = "Predicted P(Complete | If Throw)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# MODEL 2: Throw decision

gam_throwability <- gam(
  did_throw ~ s(p_complete_if_throw, k = 5),
  data = scored_df,
  family = binomial(),
  method = "REML"
)

summary(gam_throwability)

plot(
  gam_throwability,
  select = 1,
  shade = TRUE,
  xlab = "Predicted P(Complete | If Throw)",
  ylab = "Partial Effect on Log-Odds of Throw",
  main = "Throw Decision vs Throwability"
)

library(modelsummary)

modelsummary(glm_complete_train, output = "latex", stars = TRUE)
modelsummary(gam_throwability, output = "latex", stars = TRUE)
