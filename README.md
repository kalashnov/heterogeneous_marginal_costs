# Heterogeneous Marginal Costs

## Abstract

The dashboard demonstrates an optimization of subsidies based on observed heterogeneity in a cost of treatment and an effect of treatment between different subjects. The optimization procedure finds a predictive model of marginal cost using an instrumental version of the generalized random forest (Athey et al., 2019) and then chooses the most cost-effective ones based on the predictions of the model. In particular, I take a Latrines promotion program with subsidies in Bangladesh (Guiteras et al., 2015). The costs (number of realized vouchers) an the outcomes (effect on latrines adoption) are heterogeneous, highly correlated, but not perfectly. I use village characteristics from the census data, baseline survey responses to choose the most cost-effective households

Athey, S., Tibshirani, J., & Wager, S. (2019). Generalized random forests. Annals of Statistics, 47(2), 1148-1178.

Guiteras, R., Levinsohn, J., & Mobarak, A. M. (2015). Encouraging sanitation investment in the developing world: a cluster-randomized trial. Science, 348(6237), 903-906.

## Run instructions

Run the dashboard with 

```
Rscript dashboard.R
```

A live version of the dashboard can be found at http://kalashnov-ge.org/dash/
