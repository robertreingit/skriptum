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
require(emmeans)
require(patchwork)

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
      geom_line(linewidth=2, col='red') + 
      labs(x = 'Werte', y = 'Dichte') 
  p1
}

rplot <- function(df, text_size = 11, point_size = 1.5) {
  ggplot(df, aes(y_hat, resid)) +
    geom_point(size = point_size) +
    geom_hline(yintercept = 0, col = 'red', linetype = 'dashed') +
    scale_x_continuous(expression(hat(y)[i])) +
    scale_y_continuous(expression(e[i])) +
    theme(text = element_text(size = text_size))
}


# ICC helperfunctions
icc <- function(ratings)
{
  ratings <- as.matrix(ratings)
  ns <- nrow(ratings)
  nr <- ncol(ratings)
  SStotal <- var(as.numeric(ratings)) * (ns * nr - 1)
  MSr <- var(apply(ratings, 1, mean)) * nr
  MSw <- sum(apply(ratings, 1, var)/ns)
  MSc <- var(apply(ratings, 2, mean)) * ns
  MSe <- (SStotal - MSr * (ns - 1) - MSc * (nr - 1))/((ns - 1) * (nr - 1))
  
  icc_s <- numeric(3) 
  icc_s[1] <- (MSr - MSw)/(MSr + (nr - 1) * MSw) # oneway
  icc_s[2] <- (MSr - MSe)/(MSr + (nr - 1) * MSe) # twoway consistency
  icc_s[3] <- (MSr - MSe)/(MSr + (nr - 1) * MSe + (nr/ns) * (MSc - MSe)) # twoway agreement
  tibble::tibble(
    type = c("ICC(1)","ICC(2,C)", "ICC(2,A)"),
    icc = icc_s
  )
}

# MISC

r_1 <- function(x) round(x, 1)
r_2 <- function(x) round(x, 2)
r_3 <- function(x) round(x, 3)
paste_com <- function(x) paste(x, collapse=',')
ncol_text <- theme(text = element_text(size=20))