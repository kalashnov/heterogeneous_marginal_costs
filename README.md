# Heterogeneous Marginal Costs

## Abstract

The dashboard demonstrates an optimization of subsidies based on observed heterogeneity in a cost of treatment and an effect of treatment between different subjects. The optimization procedure finds a predictive model of marginal cost using an instrumental version of the generalized random forest \citep{Athey2019GeneralizedForests} and then chooses the most cost-effective ones based on the predictions of the model. In particular, I take a Latrines promotion program with subsidies in Bangladesh. The costs (number of realized vouchers) an the outcomes (effect on latrines adoption) are heterogeneous, highly correlated, but not perfectly. I use village characteristics from the census data, baseline survey responses to choose the most cost-effective households.

## Run instructions

Run the dashboard with 

```
Rscript dashboard.R
```

A live version of the dashboard can be found at http://kalashnov-ge.org/dash/
