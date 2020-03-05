#*****************************************************
## Code for the p-value presentation for retreat 2020*
#*****************************************************


library(ggplot2)
library(gganimate)
library(magrittr)
library(tibble)
library(dplyr)


n <- 40
n_trials <- 100000
flips_freq <- 
  tibble(Number_of_heads=as.factor(1:n), frequency=rep(0,n), trials=as.character(rep(0,n)))

flips <- rbinom(n_trials, n, 0.5)

for (i in 1:n_trials){
  if (i%%1000 == 0) print(i)

  flps <-
    tibble(Number_of_heads = factor(flips[1:i], levels=as.character(1:n))) %>% 
    group_by(Number_of_heads, .drop = FALSE) %>% 
    summarise(frequency= n()) %>%
    mutate(trials=as.character(rep(i,n)))

  flips_freq <-
    rbind(flips_freq, flps)

}


flips_freq %>%
  ggplot(aes(Number_of_heads, frequency)) +
  geom_segment( aes(x=Number_of_heads , xend=Number_of_heads, y=0, yend=frequency), color="grey") +
  geom_point( color="orange", size=4) +
  transition_states(
    trials,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')
