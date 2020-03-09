# code from https://www.rpubs.com/pgrosse/545948
library(ggplot2)
library(dplyr)
library(magrittr)
library(gganimate)

set.seed(13)
flipsim <-
  data.frame(
             Heads=sample(c(0, 1), 5000, replace = TRUE),
             Trial=1:5000
             ) %>%
  mutate(Cum_Heads=cumsum(Heads), Pct_Heads = Cum_Heads/Trial)
  

fair_plot <- 
  flipsim %>% 
  ggplot(aes(y = Pct_Heads, x = Trial)) + 
  ggtitle("Percentage of Heads \n Fair Coin") + 
  geom_line() + 
  geom_segment(aes(xend = 5000, yend = Pct_Heads), linetype = 2,color = "red") + 
  geom_point(size = 2) + 
  transition_reveal(Trial) + 
  ylim(0,1) + 
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
	axis.text=element_text(size=15), axis.title=element_text(size=25))

animate(fair_plot)
