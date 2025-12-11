setwd("/Users/emilysun/Desktop/bdb")
library(reshape2)

source("field_control01.R")
source("catch_control02.R")
source("get_moments03.R")

grid_x <- seq(0, 120, length.out = 80)
grid_y <- seq(0, 53.3, length.out = 40)

df_input  <- read.csv("input_2023_w01.csv")
df_output <- read.csv("output_2023_w01.csv")
supp      <- read.csv("supplementary_data.csv")

compute_metrics_for_all_plays <- function(df_input, df_output, supp, 
                                          grid_x, grid_y) {
  
  plays <- df_input %>%
    distinct(game_id, play_id)
  
  results <- list()
  
  for (idx in seq_len(nrow(plays))) {
    
    game_id <- plays$game_id[idx]
    play_id <- plays$play_id[idx]
    
    play_df <- tryCatch(
      build_play_df_R(game_id, play_id, df_input, df_output, supp),
      error = function(e) NULL
    )
    if (is.null(play_df)) next
    
    moments <- get_three_moments(play_df)
    
    snap_frame    <- moments$frame[moments$moment == "Snap"]
    topdrop_frame <- moments$frame[moments$moment == "Top of Drop"]
    throw_frame   <- moments$frame[moments$moment == "Frame Before the Throw"]
    
    cp <- get_ball_landing(play_df)
    cx <- cp[1]; cy <- cp[2]
    
    moment_frames <- list(
      Snap    = snap_frame,
      TopDrop = topdrop_frame,
      Throw   = throw_frame
    )
    
    for (mom in names(moment_frames)) {
      
      f <- moment_frames[[mom]]
      if (is.na(f)) next
      
      df_frame <- play_df %>% filter(global_frame == f)
      if (nrow(df_frame) == 0) next
      
      control <- tryCatch(
        compute_control_at_frame(df_frame, cx, cy, grid_x, grid_y),
        error = function(e) NULL
      )
      if (is.null(control)) next
      
      sep <- compute_separation_at_frame(df_frame)
      
      results[[length(results)+1]] <- tibble(
        game_id = game_id,
        play_id = play_id,
        moment  = mom,
        controller_id   = control$owner$nfl_id,
        controller_name = control$owner$player_name,
        controller_prob = control$owner$prob,
        avg_separation  = if (!is.null(sep)) mean(sep$min_sep) else NA_real_
      )
    }
  }
  
  bind_rows(results)
}

metrics_all <- compute_metrics_for_all_plays(
  df_input, df_output, supp,
  grid_x, grid_y
)
metrics_all

#group by players
metrics_all %>% 
  group_by(controller_name) %>% 
  summarize(
    avg_prob = mean(controller_prob, na.rm = TRUE),
    plays = n(),
    games = n_distinct(game_id),
    play_list = paste0(play_id, collapse = ", ")
  )


