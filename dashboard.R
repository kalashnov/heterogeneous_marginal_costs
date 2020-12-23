library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dplyr)
library(plotly)
library(memoise)
set.seed(0)

setwd("~/heterogeneous_marginal_costs")
source('imitation.R')

caluclate_sliders <- function(data) {
  attach(data)
  Y1_resid <- Y1 - rpredictions_1 - treatment * cpredictions_1
  sd_Y1_resid <- sd(Y1_resid)
  levels_Y1_resid <- 1/6
  marks_Y1_resid <- as.character(round((0:6) * levels_Y1_resid * sd_Y1_resid, 3))
  marks_Y1_resid[7] <- paste(marks_Y1_resid[7], "(The level in the data)")
  
  Y2_resid <- Y2 - rpredictions_2 - treatment * cpredictions_2
  sd_Y2_resid <- sd(Y2_resid)
  levels_Y2_resid <- 1/3
  marks_Y2_resid <- as.character(round((0:6) * levels_Y2_resid * sd_Y2_resid, 3))
  marks_Y2_resid[4] <- paste(marks_Y2_resid[4], "(The level in the data)")
  
  sd_cpredictions_1 <- sd(cpredictions_1)
  levels_cpred_1 <- 1/2
  marks_cpred_1 <- as.character(round((0:6) * levels_cpred_1 * sd_cpredictions_1, 3))
  marks_cpred_1[3] <- paste(marks_cpred_1[3], "(The level in the data)")
  
  sd_cpredictions_2 <- sd(cpredictions_2)
  levels_cpred_2 <- 1/2
  marks_cpred_2 <- as.character(round((0:6) * levels_cpred_2 * sd_cpredictions_2, 3))
  marks_cpred_2[3] <- paste(marks_cpred_2[3], "(The level in the data)")
  
  levels_FC <- 1/10
  marks_FC <- as.character((7:12) * levels_FC)
  
  list(levels_Y1_resid=levels_Y1_resid, marks_Y1_resid=marks_Y1_resid,
       levels_Y2_resid=levels_Y2_resid, marks_Y2_resid=marks_Y2_resid,
       levels_cpred_1=levels_cpred_1, marks_cpred_1=marks_cpred_1,
       levels_cpred_2=levels_cpred_2, marks_cpred_2=marks_cpred_2,
       levels_FC=levels_FC, marks_FC=marks_FC)
}

sliders <- caluclate_sliders(sanitation_data)

df <- expand.grid(0:6 * sliders$levels_cpred_1, 0:6 * sliders$levels_cpred_2, 0:6 * sliders$levels_Y1_resid, 0:6 * sliders$levels_Y2_resid)
for (row in 1:nrow(df)) {
  get_sanitation_figures(df[row, 1], df[row, 2], df[row, 3], df[row, 4])
  print(c(df[row, 1], df[row, 2], df[row, 3], df[row, 4]))
}

# parameters: sliders for options + labels
# switch off and on the models
# add controls to the FE application
# add controls to the 
# other applications
# description
# metrics on signal/noize


# Dash supports [Markdown](http://commonmark.org/help).
# Markdown is a simple way to write and format text.
# It includes a syntax for things like **bold text** and *italics*,
# [links](http://commonmark.org/help), inline `code` snippets, lists,
# quotes, and more.
# ")

# ADD derivatives to the plot

app <- Dash$new()


colors <- list(
  background = '#000000',
  text = '#FFFFFF'
)

pageTitle <- htmlH1(
  'Heterogeneous marginal costs and policies based on them',
  style = list(
    textAlign = 'center'#,
    #color = colors$text
  )
)

pageSubTitle <- htmlDiv(
  'Georgy Kalashnov',
  style = list(
    textAlign = 'center'#,
    #color = colors$text
  )
)

pageParagraphs <- htmlDiv(list(
  htmlP("Along with clearing on the size of the average effect the recent initiatives aim to use these results to make the interventions cost effective, attempting to find a subset of eligible recipients, who need the treatment most and do not spend the budget of the initiative too much (e.g. in development economics literature, or in public finance. The approach the researchers take to this problem is mostly ad hoc and is driven by domain knowledge or take a path of trial and error, exploring the data."),
  htmlP("Along with clearing on the size of the average effect the recent initiatives aim to use these results to make the interventions cost effective, attempting to find a subset of eligible recipients, who need the treatment most and do not spend the budget of the initiative too much (e.g. in development economics literature, or in public finance. The approach the researchers take to this problem is mostly ad hoc and is driven by domain knowledge or take a path of trial and error, exploring the data.")
),
  style = list(
    textAlign = 'left'#,
    #color = colors$text
  )
)

app$layout(
  htmlDiv(list(
    pageTitle,
    pageSubTitle,
#    pageParagraphs,
    dccGraph(id = 'sanitation-graph'),
    dccGraph(id = 'FE-graph'),
    dccMarkdown("Note, that you can swith off one of the models by clicking on it's name in the legend"),
    dccMarkdown("Standard deviation of explained increase in access to sanitation (power of the signal)"),
    dccSlider(
      id = 'Y1_treat_var--slider',
      min = 0,
      max = 6,
      marks = sliders$marks_cpred_1,
      value = 2
    ),
    dccMarkdown("Standard deviation of explained increase in redeemed vouchers (power of the signal)"),
    dccSlider(
      id = 'Y2_treat_var--slider',
      min = 0,
      max = 6,
      marks = sliders$marks_cpred_2,
      value = 2
    ),
    dccMarkdown("Standard deviation of the residual of access to sanitation (noize)"),
    dccSlider(
      id = 'ESS1--slider',
      min = 0,
      max = 6,
      marks = sliders$marks_Y1_resid,
      value = 6
    ),
    dccMarkdown("Standard deviation of the residual of redeemed vouchers (noize)"),
    dccSlider(
      id = 'ESS2--slider',
      min = 0,
      max = 6,
      marks = sliders$marks_Y2_resid,
      value = 3
    ),
    dccMarkdown("Fixed costs per baseline effect"),
    dccSlider(
      id = 'FC--slider',
      min = 7,
      max = 12,
      marks = sliders$marks_FC,
      value = 7
    )
  ))
)

app$callback(
  output = list(output(id='sanitation-graph', property='figure'),
                output(id='FE-graph', property='figure')),
  params = list(input(id='Y1_treat_var--slider', property='value'),
                input(id='Y2_treat_var--slider', property='value'),
                input(id='ESS1--slider', property='value'),
                input(id='ESS2--slider', property='value'),
                input(id='FC--slider', property='value')),
  function(Y1_treat_var, Y2_treat_var, ESS1, ESS2, FC) {
    lapply(get_sanitation_figures(Y1_treat_var*sliders$levels_cpred_1, Y2_treat_var*sliders$levels_cpred_2,
                             ESS1=ESS1*sliders$levels_Y1_resid, ESS2=ESS2*sliders$levels_Y2_resid,
                             FC = FC * sliders$levels_FC),
           ggplotly
      )
  }
)

app$run_server(host='91.201.40.160', port='80')

