source("catch_control02.R")

setwd("/Users/emilysun/Desktop/bdb")

df_input  <- read.csv("input_2023_w01.csv")
df_output <- read.csv("output_2023_w01.csv")
supp      <- read.csv("supplementary_data.csv")

# build_play_df_R to create unified play level tracking data frame
build_play_df_R <- function(game_id, play_id, df_input, df_output, supp) {
  
  inp  <- df_input  %>% filter(game_id == !!game_id, play_id == !!play_id)
  out  <- df_output %>% filter(game_id == !!game_id, play_id == !!play_id)
  meta <- supp      %>% filter(game_id == !!game_id, play_id == !!play_id)
  
  # static player information
  static_cols <- c(
    "game_id","play_id","nfl_id","player_name","player_height","player_weight",
    "player_birth_date","player_position","player_side","player_role",
    "play_direction","absolute_yardline_number"
  )
  
  static_players <- inp %>% select(all_of(static_cols)) %>% distinct()
  
  dyn_cols <- c("x","y","s","a","o","dir")
  for (col in dyn_cols) {
    if (!(col %in% names(out))) out[[col]] <- NA_real_
  }
  
  # merge static info into output
  out <- out %>%
    left_join(static_players, by = c("game_id","play_id","nfl_id"))
  
  # broadcast ball landing info to input and output
  ball_x <- inp$ball_land_x[1]
  ball_y <- inp$ball_land_y[1]
  n_out_frames <- inp$num_frames_output[1]
  
  inp$ball_land_x <- out$ball_land_x <- ball_x
  inp$ball_land_y <- out$ball_land_y <- ball_y
  inp$num_frames_output <- out$num_frames_output <- n_out_frames
  
  # label input frames + global index
  inp <- inp %>% mutate(phase = "input", global_frame = frame_id)
  
  max_inp_frame <- max(inp$frame_id, na.rm = TRUE)
  
  out <- out %>% mutate(
    phase = "output",
    global_frame = frame_id + max_inp_frame
  )
  
  # combine into a unified DF
  play_df <- bind_rows(inp, out)
  
  # attach supplementary metadata as columns
  meta_row <- meta[1, ]
  for (col in names(meta)) {
    play_df[[col]] <- meta_row[[col]]
  }
  
  # standardize coordinates so offense goes left to right
  play_dir <- unique(play_df$play_direction)
  
  if (play_dir == "left") {
    play_df <- play_df %>%
      mutate(
        x = 120 - x,
        y = 53.3 - y,
        ball_land_x = 120 - ball_land_x,
        ball_land_y = 53.3 - ball_land_y
      )
  }
  
  return(play_df)
}

# snap, max drop back, throw, ball arrival moment
get_three_moments <- function(play_df) {
  
  # snap = first global frame
  snap_frame <- min(play_df$global_frame, na.rm = TRUE)
  
  # Identify QB
  qb <- play_df %>% filter(player_role == "Passer")
  
  qb_input <- qb %>% filter(phase == "input") %>% arrange(frame_id)
  
  if (nrow(qb_input) == 0) {
    return(tibble(
      moment = c("Snap", "Top of Drop", "Frame Before the Throw"),
      frame  = c(snap_frame, NA_real_, NA_real_)
    ))
  }
  
  # Top of drop
  qb_start_x <- qb_input$x[1]
  
  qb_input <- qb_input %>%
    mutate(drop_depth = qb_start_x - x)
  
  valid_depths <- qb_input %>% filter(!is.na(drop_depth))
  
  if (nrow(valid_depths) == 0) {
    top_drop_frame <- NA_real_
  } else {
    top_drop_frame <- valid_depths %>%
      filter(drop_depth == max(drop_depth)) %>%
      slice(1) %>%
      pull(global_frame)
  }
  
  # frame right before the throw
  throw_frame <- max(play_df$global_frame[play_df$phase == "input"])
  
  tibble(
    moment = c("Snap", "Top of Drop", "Frame Before the Throw"),
    frame  = c(snap_frame, top_drop_frame, throw_frame)
  )
}

play_df <- build_play_df_R(2023090700, 3566, df_input, df_output, supp)
moments <- get_three_moments(play_df)
print(moments)
