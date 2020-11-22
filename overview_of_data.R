library(dplyr)
library(grf)
library(ggplot2)

setwd("~/Optimal policy")
source('marginal_learning.R')
source('datasets.R')


######## SANITATION ############

set.seed(0) # 2 good!! stability???
sanitation_data  <- prepare_sanitation_data()
df <- build_models(sanitation_data)

df %>%
  cumulative_te(cpredictions_1) %>%
  cumulative_te(random) %>%
  cumulative_te(predictions) %>%
  cumulative_te(cpredictions) %>%
  cumulative_te(cpredictions_regr) %>%
  ggplot() + geom_line(aes(y=cumulative_Y1_random, x=cumulative_Y2_random, color='random')) +
  geom_line(aes(y=cumulative_Y1_predictions, x=cumulative_Y2_predictions, color='IV')) +
  geom_line(mapping=aes(y=cumulative_Y1_cpredictions_1, x=cumulative_Y2_cpredictions_1, color='model O only')) +
  geom_line(mapping=aes(y=cumulative_Y1_cpredictions, x=cumulative_Y2_cpredictions, color='knapsack')) +
  geom_line(mapping=aes(y=cumulative_Y1_cpredictions_regr, x=cumulative_Y2_cpredictions_regr, color='diff')) +
  labs(x='Number of vouchers realized per h/h', y='Increase in access to sanitation points (share of h/h)')



############## MALARIA ##################

set.seed(1)
malaria_data <- prepare_malaria_data()
df <- build_models(malaria_data, group=c('treatment'))


df %>%
  cumulative_te(cpredictions_1) %>%
  cumulative_te(random) %>%
  cumulative_te(predictions) %>%
  cumulative_te(cpredictions) %>%
  cumulative_te(cpredictions_regr) %>%
  ggplot() + geom_line(aes(y=cumulative_Y1_random, x=cumulative_Y2_random, color='random')) +
  geom_line(aes(y=cumulative_Y1_predictions, x=cumulative_Y2_predictions, color='IV')) +
  geom_line(mapping=aes(y=cumulative_Y1_cpredictions_1, x=cumulative_Y2_cpredictions_1, color='model O only')) +
  geom_line(mapping=aes(y=cumulative_Y1_cpredictions, x=cumulative_Y2_cpredictions, color='knapsack')) +
  geom_line(mapping=aes(y=cumulative_Y1_cpredictions_regr, x=cumulative_Y2_cpredictions_regr, color='diff')) +
  labs(x='Number of vouchers realized per h/h', y='Number of malaria treatments bought for children under 14') #+


# smth bad

############# IMUNIZATION #############

set.seed(0)
immunization_data <- prepare_immunization_data()
df <- build_models(immunization_data)


df %>%
  cumulative_te(cpredictions_1) %>%
  cumulative_te(random) %>%
  cumulative_te(predictions) %>%
  cumulative_te(cpredictions) %>%
  cumulative_te(cpredictions_regr) %>%
  ggplot() + geom_line(aes(y=cumulative_Y1_random, x=cumulative_Y2_random, color='random')) +
  geom_line(aes(y=cumulative_Y1_predictions, x=cumulative_Y2_predictions, color='IV')) +
  geom_line(mapping=aes(y=cumulative_Y1_cpredictions_1, x=cumulative_Y2_cpredictions_1, color='model O only')) +
  geom_line(mapping=aes(y=cumulative_Y1_cpredictions, x=cumulative_Y2_cpredictions, color='knapsack')) +
  geom_line(mapping=aes(y=cumulative_Y1_cpredictions_regr, x=cumulative_Y2_cpredictions_regr, color='diff')) +
  labs(x='Number of vouchers realized per h/h', y='Number of malaria treatments bought for children under 14') #+


# smth bad

######## IMMUNIZATION IMITATION DOESN'T HELP ############
