options(
  knitr.kable.NA = ''
)
knitr::opts_chunk$set(
  echo=FALSE,
  message=FALSE,
  warning=FALSE
)
source('res/nice_format_helper_fcn.R')
require(ggplot2)
require(tibble)
require(dplyr)
require(tidyr)
require(knitr)

####################################
# helper functions
####################################

diff_dotplot <- function(df_1, df_2) {
  # Plots a dotplot with two groups.
  # Args:
  #   df_1: point data
  #   df_2: average data
  ggplot(df_1,
         aes(Kraft, 1, color = Gruppe)) +
    geom_point(size=3) +
    geom_segment(data = df_2, aes(xend = Kraft, y=0.99, yend=1.01), size=1.3) +
    annotate("segment",
             x = df_2$Kraft[1],
             xend = df_2$Kraft[2],
             y = 1.01,
             yend = 1.01,
             arrow = arrow(ends = "both", angle = 45, length = unit(.2,"cm"))) +
    annotate("text",
             x = mean(df_2$Kraft), y = 1.01, label = 'D', vjust=-.5) +
    scale_x_continuous('Kraftwerte[N]', breaks = seq(1800, 3200, 200), limits=c(1800, 3200)) +
    scale_y_continuous('', breaks = NULL, limits=c(0.975, 1.025))
}


dnorm_plot <- function(mu = 0, sigma = 1, length.out = 100) {
  # Plots a normal distribution
  # Args:
  #   mu: mu parameter
  #   sigma: sigma parameter
  #   length.out: number of graph support point
  p1 <- tibble(
    x = seq(mu - sigma*3, mu + sigma*3, length.out = length.out),
    p = dnorm(x, mean = mu, sd = sqrt(sigma))) |> 
    ggplot(aes(x,p)) +
      geom_ribbon(aes(ymin = 0, ymax = p), fill = 'red', alpha=.3) +
      geom_line(size=2, col='red') + 
      labs(x = 'Werte', y = 'Dichte') 
  p1
}

rplot <- function(df) {
  ggplot(df, aes(y_hat, resid)) +
    geom_point() +
    geom_hline(yintercept = 0, col = 'red', linetype = 'dashed') +
    scale_x_continuous(expression(hat(y)[i])) +
    scale_y_continuous(expression(e[i]))
}
