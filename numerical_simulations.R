library(dplyr)
library(ggplot2)
library(grf)


# TODO: 
# LINKEDIN
# POST THE CODE
# IMPLEMENT 2018
# fomulate theoretical result
# reread the text
# make a template for poster and slides etc
# sexier examples and the acuality discussion

# Simulated data
N_tr <- 200 #500
N <- N_tr + 100000


W <- sample(0:1, N, replace=TRUE)

const_1 <- 0
const_2 <- 0
obs_var_1 <- 0 * rnorm(N, 0, 1)
obs_var_2 <- 0 * rnorm(N, 0, 1)
unobs_var_1 <- 2
unobs_var_2 <- 2
unobs_effect_1 <- 0 * rnorm(N, 0, 1)
unobs_effect_2 <- 0 * rnorm(N, 0, 1)
unobs_effect_correl <- rnorm(N, 0, 1) * 5
base_effect_1 <- 2
base_effect_2 <- 0
obs_effect_1 <- 1 * rnorm(N, 0, 1)
obs_effect_2 <- 5 * rnorm(N, 0, 1)
effect_correl <- 2 * rnorm(N, 0, 1)
local_correlation <- matrix(c(rnorm(N, 0, 1) * 1, rep(0, N)), ncol=2)
# local_correlation <- local_correlation * 

# cos, sin, -sin, cos

Y10 <- 0 #const_1 + obs_var_1 + unobs_var_1 * rnorm(N, 0, 1)
Y20 <- 0 #const_2 + obs_var_2 + unobs_var_2 * rnorm(N, 0, 1)
Y21 <- Y20 + obs_effect_2 + effect_correl + base_effect_1
margin <- 3 + obs_effect_1
Y11 <- Y10 + margin * (Y21 - Y20)

# потом разобраться в примерах говна


  
# LOOKS LIKE MIXED EFFECTS!

mean(Y11) - mean(Y10)
mean(Y21) - mean(Y20)
cov(Y11 - Y10, Y21 - Y20)
sum(Y11 - Y10 < 0)
sum(Y21 - Y20 < 0)

Y1 <- (Y10 * (1 - W) + Y11 * W)
Y2 <- (Y20 * (1 - W) + Y21 * W)

X <- data.frame(obs_var_1=obs_var_1, obs_var_2=obs_var_2, 
                obs_effect_1 = obs_effect_1,
                obs_effect_2 = obs_effect_2,
                effect_correl = effect_correl
                # rotation #well we can locally decorrelate with Bootstrap #BUT WE HARM MONOTHONICITY!
                # monothonicity is okay assumption for 
                # put accent on business interest of total picture + why + what kind of problems?
                # well, no, problem of monothonicity is solvable -- then why fit together?
                # The split can accidentially in fact taking into account the magnitude
                # fit together for correlation? obesevable? local?
)



# the bias in ... у меня же была идея про то как кто-то чего-то не учитывает. IO идея. А впомнил. Это было про маркетинг и учет того, что легко померить (CTR) и неучет того, что сложно (CR)


forest <- instrumental_forest(X[1:N_tr,], Y1[1:N_tr], Y2[1:N_tr], W[1:N_tr], num.trees=100)

cforest_1 <- causal_forest(X[1:N_tr,], Y1[1:N_tr], W[1:N_tr], num.trees=100)
cforest_2 <- causal_forest(X[1:N_tr,], Y2[1:N_tr], W[1:N_tr], num.trees=100)

predictions <- predict(forest, X[(N_tr + 1):N,])
cpredictions_1 <- predict(cforest_1, X[(N_tr + 1):N,])
cpredictions_2 <- predict(cforest_2, X[(N_tr + 1):N,])

cpredictions <- cpredictions_1/cpredictions_2
colnames(cpredictions) <- 'cpredictions'
colnames(cpredictions_1) <- 'cpredictions_1'
colnames(cpredictions_2) <- 'cpredictions_2'


dt <- data.frame(cpredictions_2=cpredictions_2, cpredictions_1=cpredictions_1, predictions=predictions, cpredictions=cpredictions, Y1=Y1[(N_tr + 1):N], Y2=Y2[(N_tr + 1):N], W=W[(N_tr + 1):N]) %>%
  arrange(runif(N - N_tr,0,1)) %>%
  mutate(Effect=cumsum((Y1*W - Y1*(1-W))*2/(N - N_tr)), Cost=cumsum((Y2*W - Y2*(1-W))*2/(N - N_tr))) %>%
  arrange(desc(predictions)) %>%
  mutate(cumulative_Y1_1=cumsum((Y1*W - Y1*(1-W))*2/(N - N_tr)), cumulative_Y2_1=cumsum((Y2*W - Y2*(1-W))*2/(N - N_tr))) %>%
  arrange(desc(cpredictions_1)) %>%
  mutate(cumulative_Y1_3=cumsum((Y1*W - Y1*(1-W))*2/(N - N_tr)), cumulative_Y2_3=cumsum((Y2*W - Y2*(1-W))*2/(N - N_tr))) %>%
  arrange(desc(cpredictions)) %>%
  #mutate(cumulative_Y1_2=cumsum((Y1*W - Y1*(1-W))*2/(N - N_tr)), cumulative_Y2_2=cumsum((Y2*W - Y2*(1-W))*2/(N - N_tr)))
  mutate(cumulative_Y1_2=cumsum(((Y1*W - Y1*(1-W)) * (cpredictions_2 > 0) - (Y1*W - Y1*(1-W)) * (cpredictions_2 <= 0))*2/(N - N_tr)) + sum((Y1*W - Y1*(1-W)) * (cpredictions_2 <= 0)*2/(N - N_tr)), cumulative_Y2_2=cumsum(((Y2*W - Y2*(1-W)) * (cpredictions_2 > 0) - (Y2*W - Y2*(1-W)) * (cpredictions_2 <= 0))*2/(N - N_tr)) + sum((Y2*W - Y2*(1-W)) * (cpredictions_2 <= 0)*2/(N - N_tr)))
#
  
  


axes <- dt %>% ggplot() 
axes + geom_line(aes(y=Effect, x=Cost, color='random')) +
  geom_line(aes(y=cumulative_Y1_1, x=cumulative_Y2_1, color='IV')) +
  geom_line(mapping=aes(y=cumulative_Y1_3, x=cumulative_Y2_3, color='model O only')) +
  geom_line(mapping=aes(y=cumulative_Y1_2, x=cumulative_Y2_2, color='knapsack')) #+
  #ggtitle('No monotonicity assumption')

# LOCAL HAVE FASTER CONVERGENCE!!!

# Simulated data 2
N_tr <- 200 #500
N <- N_tr + 100000


W <- sample(0:1, N, replace=TRUE)

const_1 <- 0
const_2 <- 0
obs_var_1 <- 0 * rnorm(N, 0, 1)
obs_var_2 <- 0 * rnorm(N, 0, 1)
unobs_var_1 <- 2
unobs_var_2 <- 2
unobs_effect_1 <- 0 * rnorm(N, 0, 1)
unobs_effect_2 <- 0 * rnorm(N, 0, 1)
unobs_effect_correl <- rnorm(N, 0, 1) * 5
base_effect_1 <- 2
base_effect_2 <- 0
obs_effect_1 <- 1 * rnorm(N, 0, 1)
obs_effect_2 <- 10 * rnorm(N, 0, 1)
local_correlation <- matrix(c(rnorm(N, 0, 1) * 1, rep(0, N)), ncol=2)
# local_correlation <- local_correlation * 

# cos, sin, -sin, cos

Y10 <- 0 #const_1 + obs_var_1 + unobs_var_1 * rnorm(N, 0, 1)
Y20 <- 0 #const_2 + obs_var_2 + unobs_var_2 * rnorm(N, 0, 1)
Y21 <- Y20 + abs(obs_effect_2 + base_effect_1)
margin <- 3 + obs_effect_1
Y11 <- Y10 + margin * (Y21 - Y20)

# потом разобраться в примерах говна



# LOOKS LIKE MIXED EFFECTS!

mean(Y11) - mean(Y10)
mean(Y21) - mean(Y20)
cov(Y11 - Y10, Y21 - Y20)
sum(Y11 - Y10 < 0)
sum(Y21 - Y20 < 0)

Y1 <- (Y10 * (1 - W) + Y11 * W)
Y2 <- (Y20 * (1 - W) + Y21 * W)

X <- data.frame(obs_var_1=obs_var_1, obs_var_2=obs_var_2, 
                obs_effect_1 = obs_effect_1,
                obs_effect_2 = obs_effect_2
                # rotation #well we can locally decorrelate with Bootstrap #BUT WE HARM MONOTHONICITY!
                # monothonicity is okay assumption for 
                # put accent on business interest of total picture + why + what kind of problems?
                # well, no, problem of monothonicity is solvable -- then why fit together?
                # The split can accidentially in fact taking into account the magnitude
                # fit together for correlation? obesevable? local?
)



# the bias in ... у меня же была идея про то как кто-то чего-то не учитывает. IO идея. А впомнил. Это было про маркетинг и учет того, что легко померить (CTR) и неучет того, что сложно (CR)


forest <- instrumental_forest(X[1:N_tr,], Y1[1:N_tr], Y2[1:N_tr], W[1:N_tr], num.trees=100)

cforest_1 <- causal_forest(X[1:N_tr,], Y1[1:N_tr], W[1:N_tr], num.trees=100)
cforest_2 <- causal_forest(X[1:N_tr,], Y2[1:N_tr], W[1:N_tr], num.trees=100)

predictions <- predict(forest, X[(N_tr + 1):N,])
cpredictions_1 <- predict(cforest_1, X[(N_tr + 1):N,])
cpredictions_2 <- predict(cforest_2, X[(N_tr + 1):N,])

cpredictions <- cpredictions_1/cpredictions_2
colnames(cpredictions) <- 'cpredictions'
colnames(cpredictions_1) <- 'cpredictions_1'
colnames(cpredictions_2) <- 'cpredictions_2'


#IMPORTANCE 1
forest # -- ОН ВСЕ ЕЩЕ ДАЕТ IMPORTANCE НЕРЕЛЕВАНТНОЙ ХЕРНЕ!


#IMPORTANCE 2
cforest_1

dt <- data.frame(cpredictions_2=cpredictions_2, cpredictions_1=cpredictions_1, predictions=predictions, cpredictions=cpredictions, Y1=Y1[(N_tr + 1):N], Y2=Y2[(N_tr + 1):N], W=W[(N_tr + 1):N]) %>%
  arrange(runif(N - N_tr,0,1)) %>%
  mutate(Effect=cumsum((Y1*W - Y1*(1-W))*2/(N - N_tr)), Cost=cumsum((Y2*W - Y2*(1-W))*2/(N - N_tr))) %>%
  arrange(desc(predictions)) %>%
  mutate(cumulative_Y1_1=cumsum((Y1*W - Y1*(1-W))*2/(N - N_tr)), cumulative_Y2_1=cumsum((Y2*W - Y2*(1-W))*2/(N - N_tr))) %>%
  arrange(desc(cpredictions_1)) %>%
  mutate(cumulative_Y1_3=cumsum((Y1*W - Y1*(1-W))*2/(N - N_tr)), cumulative_Y2_3=cumsum((Y2*W - Y2*(1-W))*2/(N - N_tr))) %>%
  arrange(desc(cpredictions)) %>%
  #mutate(cumulative_Y1_2=cumsum((Y1*W - Y1*(1-W))*2/(N - N_tr)), cumulative_Y2_2=cumsum((Y2*W - Y2*(1-W))*2/(N - N_tr)))
  mutate(cumulative_Y1_2=cumsum(((Y1*W - Y1*(1-W)) * (cpredictions_2 > 0) - (Y1*W - Y1*(1-W)) * (cpredictions_2 <= 0))*2/(N - N_tr)) + sum((Y1*W - Y1*(1-W)) * (cpredictions_2 <= 0)*2/(N - N_tr)), cumulative_Y2_2=cumsum(((Y2*W - Y2*(1-W)) * (cpredictions_2 > 0) - (Y2*W - Y2*(1-W)) * (cpredictions_2 <= 0))*2/(N - N_tr)) + sum((Y2*W - Y2*(1-W)) * (cpredictions_2 <= 0)*2/(N - N_tr)))
#




axes <- dt %>% ggplot() 
axes + geom_line(aes(y=Effect, x=Cost, color='random')) +
  geom_line(aes(y=cumulative_Y1_1, x=cumulative_Y2_1, color='IV')) +
  geom_line(mapping=aes(y=cumulative_Y1_3, x=cumulative_Y2_3, color='model O only')) +
  geom_line(mapping=aes(y=cumulative_Y1_2, x=cumulative_Y2_2, color='knapsack')) #+
  #ggtitle('Monotonicity assumption')



# тут все ок. Почему тогда такие странные гистограммы?
predict(forest, data.frame(obs_var_1=rep(1, 400), obs_var_2=rep(1, 400), 
                                          obs_effect_1 = rep(0, 400),
                                          obs_effect_2 = (1:400 - 200) / 50))


predict(cforest_1, data.frame(obs_var_1=rep(0, 200), obs_var_2=rep(0, 200), 
                           obs_effect_1 = rep(1, 200),
                           obs_effect_2 = (1:200 - 100) / 50)) /
  predict(cforest_2,data.frame(obs_var_1=rep(0, 200), obs_var_2=rep(0, 200), 
                           obs_effect_1 = rep(1, 200),
                           obs_effect_2 = (1:200 - 100) / 50))



# _________________

results <- list()
#results_b <- list()
results_inst <- list()
results_common <- list()
levels <- 1 + 0:15 / 4
for (i in 1:30) {
  
  N_tr <- 2000 #500
  N <- N_tr + 10000
  
  
  W <- sample(0:1, N, replace=TRUE)
  
  const_1 <- 0
  const_2 <- 0
  obs_var_1 <- 0 * rnorm(N, 0, 1)
  obs_var_2 <- 0 * rnorm(N, 0, 1)
  unobs_var_1 <- 2
  unobs_var_2 <- 2
  unobs_effect_1 <- 0 * rnorm(N, 0, 1)
  unobs_effect_2 <- 0 * rnorm(N, 0, 1)
  unobs_effect_correl <- rnorm(N, 0, 1) * 5
  base_effect_1 <- 0
  base_effect_2 <- 2
  obs_effect_1 <- 0.5 * matrix(rnorm(N*100, 0, 1), ncol=100)
  obs_effect_3 <- 1 * rnorm(N, 0, 1)
  
  obs_effect_2 <- matrix(rnorm(N*25, 0, 1), ncol=25)
  local_correlation <- matrix(c(rnorm(N, 0, 1) * 1, rep(0, N)), ncol=2)
  # local_correlation <- local_correlation * 
  
  # cos, sin, -sin, cos
  
  Y10 <- obs_var_1 #const_1 + obs_var_1 + unobs_var_1 * rnorm(N, 0, 1)
  Y20 <- obs_var_2 #const_2 + obs_var_2 + unobs_var_2 * rnorm(N, 0, 1)
  Y21 <- Y20 + abs(rowSums(obs_effect_2) + base_effect_2) # ADD MONOTONICITY
  margin <- 3 + obs_effect_1
  Y11 <- Y10 + margin * (Y21 - Y20) #  + abs(obs_effect_3 + base_effect_1)
  
  # потом разобраться в примерах говна
  
  
  
  # LOOKS LIKE MIXED EFFECTS!
  
  mean(Y11) - mean(Y10)
  mean(Y21) - mean(Y20)
  cov(Y11 - Y10, Y21 - Y20)
  sum(Y11 - Y10 < 0)
  sum(Y21 - Y20 < 0)
  
  Y1 <- (Y10 * (1 - W) + Y11 * W)
  Y2 <- (Y20 * (1 - W) + Y21 * W)
  
  X <- data.frame(obs_var_1=obs_var_1, obs_var_2=obs_var_2, 
                  obs_effect_1 = obs_effect_1,
                  obs_effect_2 = obs_effect_2,
                  obs_effect_2 = obs_effect_3
                  # rotation #well we can locally decorrelate with Bootstrap #BUT WE HARM MONOTHONICITY!
                  # monothonicity is okay assumption for 
                  # put accent on business interest of total picture + why + what kind of problems?
                  # well, no, problem of monothonicity is solvable -- then why fit together?
                  # The split can accidentially in fact taking into account the magnitude
                  # fit together for correlation? obesevable? local?
  )

forest <- instrumental_forest(X[1:N_tr,], Y1[1:N_tr], Y2[1:N_tr], W[1:N_tr], num.trees=100)
predictions <-  predict(forest, X[(N_tr + 1):N,])$predictions
#b_forest <- causal_forest(X[1:N_tr,], Y1[1:N_tr] - 4.5*Y2[1:N_tr], W[1:N_tr], num.trees=100)
#predictions_b <-  predict(b_forest, X[(N_tr + 1):N,])$predictions > 0
dt <- data.frame(
  predictions=predictions, Y1=Y1[(N_tr + 1):N], Y2=Y2[(N_tr + 1):N], W=W[(N_tr + 1):N]
  #predictions_b=predictions_b
)

for (level in levels) {
  instrumental_forest <- instrumental_forest(X[1:N_tr,], Y1[1:N_tr] - level*Y2[1:N_tr], level*Y1[1:N_tr] + Y2[1:N_tr], W[1:N_tr], num.trees=100)
  binary_forest <- causal_forest(X[1:N_tr,], Y1[1:N_tr] - level*Y2[1:N_tr], W[1:N_tr], num.trees=100)
  pr <- predict(binary_forest, X[(N_tr + 1):N,])$predictions > 0
  ipr <- predict(instrumental_forest, X[(N_tr + 1):N,])$predictions > 0
  results[[as.character(level)]] <- c(results[[as.character(level)]], sum(dt[pr & dt$W == 1, 'Y1'] - level*dt[pr & dt$W == 1, 'Y2']))
  #results_b[[as.character(level)]] <- c(results[[as.character(level)]], sum(dt[dt$predictions_b & dt$W == 1, 'Y1'] - level*dt[dt$predictions_b & dt$W == 1, 'Y2']))
  results_inst[[as.character(level)]] <- c(results_inst[[as.character(level)]], sum(dt[ipr & dt$W == 1, 'Y1'] - level*dt[ipr & dt$W == 1, 'Y2']))
  results_common[[as.character(level)]] <- c(results_common[[as.character(level)]], sum(dt[dt$predictions > level & dt$W == 1, 'Y1'] - level*dt[dt$predictions > level & dt$W == 1, 'Y2']))
}
}
# mean -- неверно. Надо sum

df <- data.frame(levels=levels,
  results=sapply(results, mean),
  #results_b=sapply(results_b, mean),
  results_inst=sapply(results_inst, mean), results_common=sapply(results_common, mean)
)

df %>% ggplot(mapping=aes(x=levels, y=results)) +
  geom_line() +
  # geom_line(aes(y=results_b), color='green') +
  geom_line(aes(y=results_inst), color='blue') +
  geom_line(aes(y=results_common), color='red')

# четче пример нужен!
# ПРЕИМУЩЕСТВА ПРОЯВЛЯЮТСЯ ПРИ НЕМНОТОННОСТИ

