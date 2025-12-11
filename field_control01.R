getwd()
setwd("/Users/emilysun/Desktop/bdb")

df_1 <- read.csv("input_2023_w01.csv")
df_2 <- read.csv("supplementary_data.csv")

library(ggplot2)
library(reshape2)
library(dplyr)

# Pitch dimensions (adjust for NFL if needed)
field_length <- 120  # yards
field_width <- 53    # meters

# Parameters
reaction_time <- 0.1  # seconds
max_speed <- max(df_1$s)       # m/s

# Create grid for control surface
grid_x <- seq(0, field_length, length.out = field_length - 0)
grid_y <- seq(0, field_width, length.out = field_width - 0)

compute_player_surface <- function(player) {
  px <- player$x
  py <- player$y
  speed <- player$s
  acc <- player$a
  
  dx <- outer(grid_x, rep(1, length(grid_y))) - px
  dy <- outer(rep(1, length(grid_x)), grid_y) - py
  distance <- sqrt(dx^2 + dy^2)
  
  effective_speed <- min(speed + acc * reaction_time, max_speed)
  time_to_reach <- distance / (effective_speed + 1e-5)
  
  prob <- exp(-time_to_reach)
  return(prob)
}

compute_space_control <- function(df) {
  results <- list()
  
  grouped <- df %>%
    group_by(game_id, play_id, frame_id)
  
  for (group in split(grouped, interaction(grouped$game_id, grouped$play_id, grouped$frame_id))) {
    players <- as.data.frame(group)
    
    for (i in seq_len(nrow(players))) {
      prob_surface <- compute_player_surface(players[i, ])
      results <- append(results, list(list(
        game_id = players$game_id[i],
        play_id = players$play_id[i],
        frame_id = players$frame_id[i],
        nfl_id = players$nfl_id[i],
        surface = prob_surface
      )))
    }
  }
  
  return(results)  # Each element contains player info + 2D matrix of control
}

control_surfaces <- compute_space_control(df_1 %>%
                                            filter(game_id == 2023090700,
                                                   play_id == 3461, 
                                                   frame_id == 26))
player_positions <- df_1 %>% filter(game_id == 2023090700,
                                    play_id == 3461, 
                                    frame_id == 26)

# Example: Access player A's surface
player_A_surface <- control_surfaces[[1]]$surface
dim(player_A_surface)  # 100 x 68 grid

# Assume you have `player_surface` from compute_player_surface()
# Example: player_surface <- control_surfaces[[1]]$surface

plot_player_control <- function(surface, player_id) {
  # Convert matrix to data frame for ggplot
  df <- melt(surface)
  colnames(df) <- c("x_idx", "y_idx", "prob")
  
  # Map grid indices to actual field coordinates
  df$x <- seq(0, 105, length.out = nrow(surface))[df$x_idx]
  df$y <- seq(0, 68, length.out = ncol(surface))[df$y_idx]
  
  ggplot(df, aes(x = y, y = x, fill = prob)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "blue") +
    coord_fixed() +
    labs(title = paste("Spatial Control for Player", player_id),
         x = "Field Width (yards)", y = "Field Length (yards)", fill = "Control") +
    theme_minimal()
}

# Example usage:
player_surface <- control_surfaces[[1]]$surface
plot_player_control(player_surface, control_surfaces[[1]]$nfl_id)

normalize_surfaces <- function(control_surfaces) {
  total_surface <- Reduce("+", lapply(control_surfaces, function(x) x$surface))
  lapply(control_surfaces, function(player) {
    player$surface <- player$surface / (total_surface + 1e-5)
    return(player)
  })
}

plot_overlay_with_positions <- function(control_surfaces, player_positions) {
  normalized <- normalize_surfaces(control_surfaces)
  
  # Combine surfaces into one data frame
  df_list <- list()
  for (player in normalized) {
    surf <- melt(player$surface)
    colnames(surf) <- c("x_idx", "y_idx", "prob")
    surf$x <- seq(0, 105, length.out = nrow(player$surface))[surf$x_idx]
    surf$y <- seq(0, 68, length.out = ncol(player$surface))[surf$y_idx]
    surf$nfl_id <- player$nfl_id
    df_list <- append(df_list, list(surf))
  }
  
  df <- bind_rows(df_list)
  
  # Filter out low-probability areas (e.g., prob < 0.05)
  df_filtered <- df %>% filter(prob > 0.05)
  
  ggplot() +
    geom_tile(data = df_filtered, aes(x = y, y = x, fill = as.factor(nfl_id), alpha = prob)) +
    geom_text(data = player_positions, aes(x = y, y = x, label = player_name), size = 2) +
    scale_alpha(range = c(0, 0.6)) +
    coord_fixed(xlim = range(df_filtered$y), ylim = range(df_filtered$x)) +  # Crop to filtered region
    labs(title = "Spatial Control with Player Positions",
         x = "Field Width (yards)", y = "Field Length (yards)") +
    theme_minimal() +
    theme(legend.position = "none")
  
}

plot_overlay_with_positions(control_surfaces, player_positions)
