# This is just the barebones script to run the simulation.

library(tidyverse)


roll_dice <- function(spot = 0) { 
  spot + sample(1:6, 1) 
}

check_for_chute_or_ladder <- function(spot = 0) {
  case_when(
    
    # Ladder
    spot ==  1 ~  38,
    spot ==  4 ~  14,
    spot ==  9 ~  31,
    spot == 28 ~  84,
    spot == 51 ~  67,
    spot == 71 ~  91,
    spot == 80 ~ 100,
    
    # Chute
    spot == 16 ~   7,
    spot == 48 ~  26,
    spot == 49 ~  11,
    spot == 56 ~  53,
    spot == 62 ~  19,
    spot == 64 ~  60,
    spot == 87 ~  24,
    spot == 93 ~  73,
    spot == 98 ~  78,
    
    # No change
    TRUE ~ spot)
}

take_turn <- function(spot = 0) {
  spot %>%
    roll_dice() %>% 
    check_for_chute_or_ladder()
}

simulate_game <- function(game_num = 0) {
  n <- 1000
  turns <- tibble(turn_num = 1:n,
                  roll     = sample(1:6, n, replace = TRUE),
                  start    = NA,
                  land     = NA,
                  end      = NA,
                  chute_or_ladder = NA)
  
  i <- 1
  keep_playing <- TRUE
  while(keep_playing) {
    
    # Start at zero
    if (i == 1) {
      turns$start[[i]] <- 0
      
    # Otherwise, start where the last turn ended.
    } else {
      turns$start[[i]] <- turns$end[[i - 1]]
    }

    turns$land[[i]] <- turns$start[[i]] + turns$roll[[i]]
    turns$end[[i]] <- check_for_chute_or_ladder(turns$land[[i]])
    
    if (turns$land[[i]] > turns$end[[i]]) {
      turns$chute_or_ladder[[i]] <- "ladder"
    } else if (turns$land[[i]] < turns$end[[i]]) {
      turns$chute_or_ladder[[i]] <- "chute"
    } else {
      turns$chute_or_ladder[[i]] <- NA
    }
    
    if (turns$end[[i]] >= 100) {
      keep_playing <- FALSE
    } else {
      i <- i + 1
    }
  }
  
  turns %>%
    filter(turn_num <= i) %>%
    return()
  
}

games <- tibble(game_num = 1:10) %>%
  mutate(game = map(game_num, simulate_game)) %>%
  unnest() %>%
  print()

games_summary <- games %>%
  group_by(game_num) %>%
  summarize(turns = max(turn_num),
            n_chutes = sum(chute_or_ladder == "chute", na.rm = TRUE),
            n_ladder = sum(chute_or_ladder == "ladder", na.rm = TRUE)) %>%
  print()

summary(games_summary)

games %>%
  group_by(game_num) %>%
  filter(max(turn_num) == 7)

ggplot(games_summary, aes(turns)) + 
  geom_histogram(binwidth = 2, fill = "gray4", color = "gray50") + 
  geom_density() + 
  theme_classic()

