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


# Make plots marking the area of the p-value

g1 <- flips_freq %>%
  filter(iter==1000) %>%
  mutate(Significance=c("False", "True")[(.$Number_of_heads %>% as.character %>% as.numeric >= 14) %>% as.numeric %>% `+`(1)]) %>%
  ggplot(aes(Number_of_heads, frequency, fill = Significance)) +
  geom_bar(stat = 'identity') +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=25), legend.position = "none") + 
  scale_fill_manual(values=c("grey30", "red"))

g2 <- flips_freq %>%
  filter(iter==1000) %>%
  mutate(Significance=c("False", "True")[(.$Number_of_heads %>% as.character %>% as.numeric >= 15) %>% as.numeric %>% `+`(1)]) %>%
  ggplot(aes(Number_of_heads, frequency, fill = Significance)) +
  geom_bar(stat = 'identity') +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=25), legend.position = "none") + 
  scale_fill_manual(values=c("grey30", "red"))

pdf("p_value_area.pdf")
gridExtra::grid.arrange(g1, g2, nrow=1)
dev.off()
