#*****************************************************
## Code for the p-value presentation for retreat 2020*
#*****************************************************


library(ggplot2)
library(gganimate)
library(magrittr)
library(tibble)
library(dplyr)


n <- 20
n_trials <- 1000
flips_freq <- 
  tibble(Number_of_heads=as.factor(1:n), frequency=rep(0,n), trials=as.integer(rep(0,n)))

flips <- rbinom(n_trials, n, 0.5)

for (i in 1:n_trials){
  if (i%%1000 == 0) print(i)

  flps <-
    tibble(Number_of_heads = factor(as.character(flips[1:i]), levels=as.character(1:n))) %>% 
    group_by(Number_of_heads, .drop = FALSE) %>% 
    summarise(frequency= n()) %>%
    mutate(trials=as.integer(rep(i,n)))

  flips_freq <-
    rbind(flips_freq, flps)

}


g <- flips_freq %>%
  ggplot(aes(Number_of_heads, frequency)) +
  geom_bar(stat = 'identity') + 
  transition_states(trials) +
  labs(title = "Trials: {closest_state}")

#animate(g + enter_fade() + exit_shrink(), fps = 50, nframes = 2000)
