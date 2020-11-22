library(dplyr)
library(grf)
library(ggplot2)

setwd("~/Optimal policy")
source('marginal_learning.R')
source('datasets.R')

# TODO -- ADD PARAMETER TO CONTROL SAMPLE SIZE AND DIFF MODEL


decompose_data <- function(data, import_th=0.05) { # LESS AGNOSTIC SIMULATIONS + MORE PARAMS
  attach(data)
  forest <- instrumental_forest(pretreatment_vars, Y1, Y2, treatment, cluster=cluster, num.trees = 100)#, tune.parameters = 'all')
  X_for_sim <- pretreatment_vars[,variable_importance(forest) > import_th]
  cforest_1 <- causal_forest(X_for_sim, Y1, treatment, cluster=cluster, num.trees = 100)#, tune.parameters = 'all') #complete???
  cforest_2 <- causal_forest(X_for_sim, Y2, treatment, cluster=cluster, num.trees = 100)#, tune.parameters = 'all')
  
  
  cpredictions_1 <- predict(cforest_1, X_for_sim)$predictions # pass data
  cpredictions_2 <- predict(cforest_2, X_for_sim)$predictions
  
  rforest_1 <- regression_forest(X_for_sim, Y1 - treatment * cpredictions_1, cluster=cluster, num.trees = 100)#, tune.parameters = 'all') #complete???
  rforest_2 <- regression_forest(X_for_sim, Y2 - treatment * cpredictions_2, cluster=cluster, num.trees = 100)#, tune.parameters = 'all')
  
  rpredictions_1 <- predict(rforest_1, X_for_sim)$predictions # pass data
  rpredictions_2 <- predict(rforest_2, X_for_sim)$predictions
  
  data$rpredictions_1 <- rpredictions_1
  data$rpredictions_2 <- rpredictions_2
  data$cpredictions_1 <- cpredictions_1
  data$cpredictions_2 <- cpredictions_2
  data
}

parametrize_imitation <- function(data, Y1_treat_var=1, Y2_treat_var=1,
                                  base_treat_1=1, base_treat_2=1, 
                                  RSS1=1, RSS2=1, ESS1=1, ESS2=1) {
  attach(data)
  Y1_resid <- Y1 - rpredictions_1 - treatment * cpredictions_1
  Y2_resid <- Y2 - rpredictions_2 - treatment * cpredictions_2
  demeaned_rpred1 <- rpredictions_1 - mean(rpredictions_1)
  demeaned_rpred2 <- rpredictions_2 - mean(rpredictions_2)
  
  Y1_new <- 
    mean(rpredictions_1) + RSS1 * demeaned_rpred1 + ESS1 * Y1_resid +
    treatment * (base_treat_1 * mean(cpredictions_1) + Y1_treat_var * (cpredictions_1 - mean(cpredictions_1)))
  Y2_new <- 
    mean(rpredictions_2) + RSS2 * demeaned_rpred2 + ESS2 * Y2_resid +
    treatment * (base_treat_2 * mean(cpredictions_2) + Y2_treat_var * (cpredictions_2 - mean(cpredictions_2)))
  
  data$Y1 <- Y1_new
  data$Y2 <- Y2_new
  data
}

imitate_data <- function(data, Y1_treat_var=1, Y2_treat_var=1,
                         base_treat_1=1, base_treat_2=1, 
                         RSS1=1, RSS2=1, ESS1=1, ESS2=1, import_th=0.05) {
  data <- decompose_data(data, import_th)
  parametrize_imitation(data, Y1_treat_var, Y2_treat_var,
                        base_treat_1, base_treat_2, 
                        RSS1, RSS2, ESS1, ESS2)
}


######## SANITATION IMITATION ############



cache <- cache_filesystem("cache")
#decompose_data <- memoise(decompose_data, cache = cache)
sanitation_data  <- decompose_data(prepare_sanitation_data())

sanitation_imitation <- function(Y1_treat_var=1, Y2_treat_var=1,
                                  base_treat_1=1, base_treat_2=1, 
                                  RSS1=1, RSS2=1, ESS1=1, ESS2=1) {
  set.seed(0)
  
  imitated_data <- parametrize_imitation(sanitation_data, Y1_treat_var, Y2_treat_var,
                                base_treat_1, base_treat_2, 
                                RSS1, RSS2, ESS1, ESS2)
  df1 <- build_models(imitated_data)
  df1
}

cached_sanitation_imitation <- memoise(sanitation_imitation, cache = cache)


get_sanitation_figures <- function(Y1_treat_var=1, Y2_treat_var=1,
                                   base_treat_1=1, base_treat_2=1, 
                                   RSS1=1, RSS2=1, ESS1=1, ESS2=1, FC=0.7) {
  df1 <- cached_sanitation_imitation(Y1_treat_var, Y2_treat_var,
                                     base_treat_1, base_treat_2, 
                                     RSS1, RSS2, ESS1, ESS2)
  
  Y1_baseline <- with(df1, mean(Y1[W==1]))
  
  cum_data <- df1 %>%
  cumulative_te(cpredictions_1) %>%
  cumulative_te(random) %>%
  cumulative_te(predictions) %>%
  cumulative_te(cpredictions)
  # cumulative_te(cpredictions_regr) %>%
  plot1 <- cum_data %>% ggplot() + geom_line(aes(y=cumulative_Y1_random, x=cumulative_Y2_random, color='random')) +
  geom_line(aes(y=cumulative_Y1_predictions, x=cumulative_Y2_predictions, color='IV')) +
  geom_line(mapping=aes(y=cumulative_Y1_cpredictions_1, x=cumulative_Y2_cpredictions_1, color='model O only')) +
  geom_line(mapping=aes(y=cumulative_Y1_cpredictions, x=cumulative_Y2_cpredictions, color='knapsack')) +
  # geom_line(mapping=aes(y=cumulative_Y1_cpredictions_regr, x=cumulative_Y2_cpredictions_regr, color='diff')) +
  labs(x='Number of vouchers realized per h/h', y='Increase in access to sanitation points (share of h/h)')
  
  plot2 <- cum_data %>% ggplot() +
  geom_line(aes(y=1/predictions, x=cumulative_Y1_predictions, color='IV')) +
    geom_line(aes(y=(FC + cumulative_Y2_predictions) / (Y1_baseline + cumulative_Y1_predictions), x=cumulative_Y1_predictions, color='Average costs')) +
    geom_line(aes(y=(FC + cumulative_Y2_random) / (Y1_baseline + cumulative_Y1_random), x=cumulative_Y1_random, color='Average costs random')) +
  # geom_line(mapping=aes(y=1/cpredictions_1, x=cumulative_Y1_cpredictions_1, color='model O only')) +
  # geom_line(mapping=aes(y=1/cpredictions, x=cumulative_Y1_cpredictions, color='knapsack')) +
  # geom_line(mapping=aes(y=cumulative_Y1_cpredictions_regr, x=cumulative_Y2_cpredictions_regr, color='diff')) +
  labs(x='Increase in access to sanitation points (share of h/h)', y='Fixed costs, marginal costs (inverse model score)')
  
  list(plot1, plot2)
}





# Вывод!

####### TODO: ADD KALLUS!! ##########
