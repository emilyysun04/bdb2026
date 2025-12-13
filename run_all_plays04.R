setwd("/Users/emilysun/Desktop/bdb")
library(purrr)
library(dplyr)

source("field_control01.R")
source("catch_control02.R")
source("get_moments03.R")

grid_x <- seq(0, 120, length.out = 80)
grid_y <- seq(0, 53.3, length.out = 40)

weeks <- sprintf("%02d", 1:18)

df_input  <- map_dfr(weeks, ~ read.csv(paste0("input_2023_w", .x, ".csv"))  %>% mutate(week = as.integer(.x)))
df_output <- map_dfr(weeks, ~ read.csv(paste0("output_2023_w", .x, ".csv")) %>% mutate(week = as.integer(.x)))
supp <- read.csv("supplementary_data.csv")

play_weeks <- df_input %>% distinct(game_id, play_id, week)
plays <- play_weeks %>% distinct(game_id, play_id)

compute_metrics_for_all_plays <- function() {
  
  results <- vector("list", nrow(plays) * 3)
  k <- 1
  
  for (i in seq_len(nrow(plays))) {
    
    gid <- plays$game_id[i]
    pid <- plays$play_id[i]
    wk  <- play_weeks$week[play_weeks$game_id == gid & play_weeks$play_id == pid]
    if (length(wk) != 1) next
    
    play_df <- tryCatch(
      build_play_df_R(gid, pid, df_input, df_output, supp),
      error = function(e) NULL
    )
    if (is.null(play_df)) next
    
    cp <- tryCatch(get_ball_landing(play_df), error = function(e) NULL)
    if (is.null(cp) || any(is.na(cp))) next
    cx <- cp[1]; cy <- cp[2]
    
    moments <- get_three_moments(play_df)
    frames <- c(
      Snap    = moments$frame[moments$moment == "Snap"],
      TopDrop = moments$frame[moments$moment == "Top of Drop"],
      Throw   = moments$frame[moments$moment == "Frame Before the Throw"]
    )
    
    for (mom in names(frames)) {
      
      f <- frames[[mom]]
      if (is.na(f)) next
      
      df_f <- play_df[play_df$global_frame == f, ]
      if (!nrow(df_f)) next
      
      control <- tryCatch(
        compute_control_at_frame(df_f, cx, cy, grid_x, grid_y),
        error = function(e) NULL
      )
      if (is.null(control)) next
      
      sep <- compute_separation_at_frame(df_f)
      
      results[[k]] <- tibble(
        game_id = gid,
        play_id = pid,
        week    = wk,
        moment  = mom,
        controller_id   = control$owner$nfl_id,
        controller_name = control$owner$player_name,
        controller_prob = control$owner$prob,
        avg_separation  = if (!is.null(sep)) mean(sep$min_sep) else NA_real_
      )
      k <- k + 1
    }
  }
  
  bind_rows(results)
}

metrics_all <- compute_metrics_for_all_plays()

write.csv(metrics_all, "metrics_all_plays.csv", row.names = FALSE)
