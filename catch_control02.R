setwd("/Users/emilysun/Desktop/bdb")
source("field_control01.R")

df <- read.csv("input_2023_w01.csv")
supp <- read.csv("supplementary_data.csv")

# euclidean distance used for separation calculations
euclid_dist <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

# find the nearest grid index to a target point (cx, cy)
get_grid_index <- function(cx, cy, grid_x, grid_y) {
  ix <- which.min(abs(grid_x - cx))
  iy <- which.min(abs(grid_y - cy))
  list(ix = ix, iy = iy)
}

# Determine which defender controls the catch point
compute_catch_owner_control <- function(df_throw, cx, cy, grid_x, grid_y) {
  
  defenders <- df_throw %>% filter(player_side == "Defense")
  
  surfaces <- lapply(seq_len(nrow(defenders)), function(i) {
    list(
      nfl_id      = defenders$nfl_id[i],
      player_name = defenders$player_name[i],
      surface     = compute_player_surface(defenders[i, ])
    )
  })
  
  idx <- get_grid_index(cx, cy, grid_x, grid_y)
  ix <- idx$ix; iy <- idx$iy
  
  scores <- lapply(surfaces, function(obj) {
    data.frame(
      nfl_id      = obj$nfl_id,
      player_name = obj$player_name,
      prob        = obj$surface[ix, iy]
    )
  })
  
  df_scores <- bind_rows(scores)
  owner <- df_scores %>% arrange(desc(prob)) %>% slice(1)
  
  list(scores = df_scores, owner = owner)
}

# Extract ball landing location from input tracking
get_ball_landing <- function(df_play) {
  c(df_play$ball_land_x[1], df_play$ball_land_y[1])
}

# Last input frame = throw frame
get_throw_frame <- function(df_play) {
  throw_frame <- max(df_play$frame_id)
  df_play %>% filter(frame_id == throw_frame)
}

# Compute receiver–defender separation for all pre-throw frames
compute_separation_prethrow <- function(df_play) {
  
  frames <- sort(unique(df_play$frame_id))
  
  sep_store <- lapply(frames, function(f) {
    
    df_f <- df_play %>% filter(frame_id == f)
    
    receivers <- df_f %>% filter(player_side == "Offense",
                                 player_role != "Passer")
    defenders <- df_f %>% filter(player_side == "Defense")
    
    if (nrow(receivers) == 0 || nrow(defenders) == 0)
      return(NULL)
    
    # lapply for each receiver
    lapply(seq_len(nrow(receivers)), function(i) {
      
      dists <- euclid_dist(receivers$x[i], receivers$y[i],
                           defenders$x, defenders$y)
      
      data.frame(
        frame_id    = f,
        nfl_id      = receivers$nfl_id[i],
        player_name = receivers$player_name[i],
        min_sep     = min(dists)
      )
    }) |> bind_rows()
  })
  
  bind_rows(sep_store)
}

# compute defender control at a single frame
#   scores  = defender probabilities at catch point
#   owner   = highest-probability defender
#   surface = full grid control map
compute_control_at_frame <- function(df_frame, cx, cy, grid_x, grid_y) {
  
  defenders <- df_frame %>% filter(player_side == "Defense")
  if (nrow(defenders) == 0)
    stop("No defenders in this frame.")
  
  defender_surfaces <- lapply(seq_len(nrow(defenders)), function(i) {
    list(
      nfl_id      = defenders$nfl_id[i],
      player_name = defenders$player_name[i],
      surface     = compute_player_surface(defenders[i, ])
    )
  })
  
  idx <- get_grid_index(cx, cy, grid_x, grid_y)
  ix <- idx$ix; iy <- idx$iy
  
  scores <- lapply(defender_surfaces, function(obj) {
    data.frame(
      nfl_id      = obj$nfl_id,
      player_name = obj$player_name,
      prob        = obj$surface[ix, iy]
    )
  })
  df_scores <- bind_rows(scores)
  owner <- df_scores %>% arrange(desc(prob)) %>% slice(1)
  
  nx <- length(grid_x)
  ny <- length(grid_y)
  
  control_mat <- matrix(0, nrow = nx, ncol = ny)
  
  for (obj in defender_surfaces) {
    control_mat <- pmax(control_mat, obj$surface)
  }
  
  surface_df <- expand.grid(
    x = grid_x,
    y = grid_y
  ) %>%
    mutate(control = as.vector(control_mat))
  
  list(
    scores  = df_scores,
    owner   = owner,
    surface = surface_df
  )
}

# Compute separation at a single frame for all receivers
compute_separation_at_frame <- function(df_frame) {
  
  receivers <- df_frame %>% filter(player_side == "Offense",
                                   player_role != "Passer")
  defenders <- df_frame %>% filter(player_side == "Defense")
  
  if (nrow(receivers) == 0 || nrow(defenders) == 0)
    return(NULL)
  
  out <- lapply(seq_len(nrow(receivers)), function(i) {
    d_min <- euclid_dist(receivers$x[i], receivers$y[i],
                         defenders$x, defenders$y)
    
    data.frame(
      nfl_id      = receivers$nfl_id[i],
      player_name = receivers$player_name[i],
      min_sep     = min(d_min)
    )
  })
  
  bind_rows(out)
}

