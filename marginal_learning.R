library(proto)
library(dplyr)
library(splitstackshape)
library(grf)

cumulative_te <- function(data, order_var) { # weights! + naming + more vars. vs linear algebra?
  data %>% arrange(desc({{ order_var }})) %>%
    mutate(
      'cumulative_Y1_{{ order_var }}' := cumsum(Y1*W/sum(W) - Y1*(1-W)/sum(1-W)),
      'cumulative_Y2_{{ order_var }}' := cumsum(Y2*W/sum(W) - Y2*(1-W)/sum(1-W)))
}


StatCumulative <- ggproto("StatCumulative", Stat,
                          required_aes=c('order_var'),
                          compute_group = function(data, scales) {
                            data <- cumulative_te(data, order_var)
                            data$x <- data$cumulative_Y1
                            data$y <- data$cumulative_Y2
                            data.frame(data)
                          }
)


stat_cumulative <- function(mapping = NULL, data = NULL, geom = "line",
                            position = "identity", na.rm = FALSE, show.legend = NA, 
                            inherit.aes = TRUE, ...) {
  layer(
    stat = StatCumulative, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


# MAKE A SEPARATE PREIDICT/FIT PIPELINE
build_models <- function(data, group=c('treatment'), size=0.5, best_factor=0.5, variable_importance_th = 0) {
  # ADD KALLUS, ADD DUFLO/CHERNOZUKOV
  attach(data)
  train_ind <- as.integer(stratified(stratify_data, group=group, size=size)$cid)
  
  forest <- instrumental_forest(pretreatment_vars[cluster %in% train_ind,], Y1[cluster %in% train_ind], Y2[cluster %in% train_ind], treatment[cluster %in% train_ind], cluster=cluster[cluster %in% train_ind])#, tune.parameters = 'all')
  if (variable_importance_th > 0) {
    pretreatment_vars <- pretreatment_vars[,variable_importance(forest) > variable_importance_th]
    forest <- instrumental_forest(pretreatment_vars[cluster %in% train_ind,], Y1[cluster %in% train_ind], Y2[cluster %in% train_ind], treatment[cluster %in% train_ind], cluster=cluster[cluster %in% train_ind])#, tune.parameters = 'all')
    print(variable_importance(forest))
  }
  
  cforest_1 <- causal_forest(pretreatment_vars[cluster %in% train_ind,], Y1[cluster %in% train_ind], treatment[cluster %in% train_ind], cluster=cluster[cluster %in% train_ind])#, tune.parameters = 'all') #complete???
  cforest_2 <- causal_forest(pretreatment_vars[cluster %in% train_ind,], Y2[cluster %in% train_ind], treatment[cluster %in% train_ind], cluster=cluster[cluster %in% train_ind])#, tune.parameters = 'all')
  cforest_regr <- causal_forest(pretreatment_vars[cluster %in% train_ind,], Y1[cluster %in% train_ind] - best_factor*Y2[cluster %in% train_ind], treatment[cluster %in% train_ind], cluster=cluster[cluster %in% train_ind])#, tune.parameters = 'all')
  
  
  
  predictions <- predict(forest, pretreatment_vars[!cluster %in% train_ind,])$predictions
  cpredictions_1 <- predict(cforest_1, pretreatment_vars[!cluster %in% train_ind,])$predictions # pass data
  cpredictions_2 <- predict(cforest_2, pretreatment_vars[!cluster %in% train_ind,])$predictions
  cpredictions_regr <- predict(cforest_regr, pretreatment_vars[!cluster %in% train_ind,])$predictions
  
  cpredictions <- cpredictions_1/cpredictions_2
  data.frame(predictions=predictions, cpredictions_2=cpredictions_2, cpredictions_regr=cpredictions_regr, cpredictions_1=cpredictions_1, cpredictions=cpredictions, Y1=Y1[!cluster %in% train_ind], Y2=Y2[!cluster %in% train_ind], W=treatment[!cluster %in% train_ind], random=runif(length(Y1[!cluster %in% train_ind]),0,1))
}
