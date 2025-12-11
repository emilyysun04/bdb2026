library(ggplot2)
library(gganimate)
library(dplyr)

#BASIC ANIMATION RN WILL IMPROVE OR MOVE TO PYTHON LATER

source("base_field_plot.R")

animate_play_bdb <- function(play_df4, fps = 5) {
  
  play_df <- play_df %>% arrange(global_frame, nfl_id)
  
  xmin <- 0
  xmax <- 120
  ymin <- 0
  ymax <- 53.3
  
  p <- baseNFLField() +  
    
    # Players
    geom_point(data = play_df,
               aes(x = x, y = y, color = player_side, group = nfl_id),
               size = 4, alpha = 0.9) +
    
    geom_text(data = play_df,
              aes(x = x, y = y, label = gsub(" ", "\n", player_name)),
              color = "white", size = 2.4, vjust = -1) +
    
    coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
    
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.text  = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 16)
    ) +
    
    labs(
      title = "{unique(play_df$home_team_abbr)} @ {unique(play_df$visitor_team_abbr)}, Week {play_df$week} Play {unique(play_df$play_id)} Frame: {frame_time}",
      subtitle = "(8:31) J.Goff pass short right to J.Reynolds to KC 14 for 33 yards (W.Gay).",
      color = "Player Side"
    ) +
    
    transition_time(global_frame) +
    ease_aes("linear")
  
  animate(p, fps = fps, nframes = length(unique(play_df$global_frame)))
}


play_df <- build_play_df_R(2023090700, 3566, df_input, df_output, supp)

anim <- animate_play_bdb(play_df)

play_df1 <- build_play_df_R(2023090700,3566, df_input, df_output, supp)
play_df2 <- build_play_df_R(2023091004,2312, df_input, df_output, supp)
play_df3 <- build_play_df_R(game_id = 2023091010,play_id = 4426, df_input, df_output, supp)
play_df4 <- build_play_df_R(game_id = 2023091005,play_id = 2518, df_input, df_output, supp)

anim1 <- animate_play_bdb(play_df1)
anim2 <- animate_play_bdb(play_df2)
anim3 <- animate_play_bdb(play_df3)
anim4 <- animate_play_bdb(play_df4)

anim4


