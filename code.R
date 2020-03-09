#*********************************************************************
## Make an animated simulation with a barplot illustrating           *
## the number of successes in a sequence of n independent coin flips.*
#*********************************************************************

# Load required libraries
library(ggplot2)
library(gganimate)
library(magrittr)
library(tibble)
library(dplyr)

# Set the number of trials and the number of iterations of the simulation
n_trials <- 20
n_iter <- 1000

# Initialize the tibble that will store the results
flips_freq <-
  tibble(Number_of_heads=as.factor(1:n_trials), frequency=rep(0,n_trials), iter=as.integer(rep(0,n_trials)))

# Generate the simulated data from a binomial distribution
set.seed(13)
flips <- rbinom(n_iter, n_trials, 0.5)

# Calculate the cumulative frequency of successes for every iteration of the simulation
for (i in 1:n_iter){
  if (i%%1000 == 0) print(i)

  if (i%%10 == 0){
    flps <-
      tibble(Number_of_heads = factor(as.character(flips[1:i]), levels=as.character(1:n_trials))) %>%
      group_by(Number_of_heads, .drop = FALSE) %>%
      summarise(frequency= n()) %>%
      mutate(iter=as.integer(rep(i,n_trials)))

    flips_freq <-
      rbind(flips_freq, flps)
  }

}

# Make the animation of the simulation
g <- flips_freq %>%
  ggplot(aes(Number_of_heads, frequency)) +
  geom_bar(stat = 'identity') +
  transition_states(iter) +
  labs(title = "Iteration: {closest_state}") + 
  theme(axis.text=element_text(size=15), axis.title=element_text(size=25))

animate(g + enter_fade() + exit_shrink(), fps = 20, nframes = 200)
