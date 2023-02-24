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