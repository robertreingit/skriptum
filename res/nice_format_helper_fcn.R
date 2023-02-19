# Printing functions to make tables nicer in R
## Helper functions

nice_p_s <- function(p_s, digits) {
  dplyr::if_else(p_s < 0.001,
                     '<0.001',
                     as.character(round(p_s,digits)))
}

## Functions

nice_anova <- function(mod) {
  header <- stringr::str_split(attr(mod, 'heading')[2],"\n")
  tbl <- cbind(header[[1]],mod['Res.Df'], mod$Df, mod['Sum of Sq'], mod$F, mod['Pr(>F)'])
  names(tbl) <- c("Model","ResDF", "DF", "SS","F","p-val")
  tbl
}



lm_tbl <- function(mod, digits=3) {
  tbl <- summary(mod)[['coefficients']]
  p_s <- tbl[,4]
  df <- data.frame(
    est= tbl[,1],
    se=tbl[,2],
    t = tbl[,3],
    p=dplyr::if_else(p_s < 0.001,
                     '<0.001',
                     as.character(round(p_s,digits)))
  )
  row.names(df) <- stringr::str_replace_all(row.names(tbl),'_','\\\\_')
  df
}

lm_tbl_knitr <- function(mod, digits=3, caption = "Modellfit", long = T, add_sigma = F) {
  tbl <- lm_tbl(mod) 
  if (add_sigma) {
    tbl <- rbind(tbl, c(round(sigma(mod), digits), NA, NA, NA))
    row.names(tbl)[dim(tbl)[1]] <- "$\\hat{\\sigma}$"
  }
  
  if (long) { 
    tbl_out <- tbl
    col_names = c('$\\hat{\\beta}$','$s_e$','t','p')
  }
  else { 
    tbl_out <- tbl[,1:2]
    col_names = c('$\\hat{\\beta}$','$s_e$')
  }
  
  tbl_out %>% knitr::kable(col.names=col_names,
           booktabs=T, escape=F, digits=digits,
           caption=caption, linesep='')
}

anova_tbl_knitr <- function(sse) {
  nice_anova(sse) %>%
    knitr::kable(booktabs=T,
                 digits=2,
                 caption="Vergleich der Modellfits")
}

lm_anova_tbl_knitr <- function(sse, caption = 'Modellfit', digits = 2) {
  repl_na <- function(v) stringr::str_replace(v, 'NA', '')
  tbl <- tibble::tibble(
    cols = row.names(sse[1]),
    Df = sse[[1]],
    SSQ = sse[[2]],
    MSQ = sse[[3]],
    F = sse[[4]],
    p = nice_p_s(sse[[5]], digits)
  ) 
  tbl %>% knitr::kable(
    booktabs = T,
    col.names = c('',names(tbl)[2:6]),
    digits = digits,
	caption = caption
  )
}

nice_aov_tbl <- function(mod, caption = 'Modellfit', digits = 3) {
  df <- broom::tidy(mod_aov_1) 
  last_col <- dim(df)[2]
  p_s <- round(df$p.value, digits)
  id_alpha <- p_s < 0.001
  p_s[id_alpha] <- 'p < 0.001'
  df$p.value <- p_s
  knitr::kable(df, booktabs = T,
               caption = caption,
               col.names = c('','Df', 'SSQ', 'MSQ', 'F', 'p'),
               digits = digits)
}

# Matrix functions
as_matrix <- function(x){
   if(!tibble::is_tibble(x) ) stop("x must be a tibble")
  y <- as.matrix.data.frame(x[,-1])
  rownames(y) <- x[[1]]
  y
}


beauty_t_test <- function(mod, digits=2) {
  level <- attr(mod$conf.int, 'conf.level')
  conf_int <- round(mod$conf.int, digits)
  p_val <- if(mod$p.value < 0.001) {
    "<0.001"
  } else {
    as.character(round(mod$p.value, digits))
  }
  cat(mod$method,
      "\n\ndata: ", mod$data.name,
      "\n\nt = $", round(mod$statistic,2),"$",
      " df = $", mod$parameter, "$, p-value = $", p_val, "$",
      "\n\n",level*100," percent confidence intervall",
      "\n\n$", conf_int[1], "$ $", conf_int[2],"$",
      sep = '')
}

nice_ttest <- function(mod, digits=2) {
  test_method <- mod$method
  level <- attr(mod$conf.int, 'conf.level')
  conf_int <- round(mod$conf.int, digits)
  
  p_val <- if(mod$p.value < 0.001) {
    "<0.001"
  } else {
    as.character(round(mod$p.value, digits))
  }
  
  df <- mod$parameter
  if (stringr::str_detect(test_method, 'Welch'))
    df <- round(df,2)
  
  if (stringr::str_detect(test_method, 'Two')) {
    s_e <- round(mod$stderr, digits)
    d <- paste0("\nd = ", as.character(round(diff(mod$estimate[c(2,1)]),digits)), ", s_e = ", s_e)
  } else
    d <- ""
  
  cat('\n',test_method,
      "\n\ndata: ", mod$data.name,
      "\nt = ", round(mod$statistic,2),
      ", df = ", df, ", p-value = ", p_val,
      d,
      "\n",level*100," percent confidence interval",
      "\n[", conf_int[1], ", ", conf_int[2],"]",
      sep = '')
}


## emmeans make nice for simple pairs table
nice_emmeans_simple <- function(df, caption) {
  as_tibble(df) |> 
  dplyr::select(-c(df)) |> 
  dplyr::mutate("p.value" = dplyr::if_else(p.value < 0.001,
                                         "<0.001",
                                         as.character(round(p.value,3)))) |> 
  knitr::kable(
    booktabs=T,
    digits = 3,
    col.names = c('contrast', 'estimate', '$s_e$', '$\\text{CI}_l$', '$\\text{CI}_u$', 't', 'p-value'),
    escape=F,
    linesep='',
    caption=caption
    )
}

nice_emmeans_per_factor <- function(df, caption) {
  as_tibble(df) |> 
  dplyr::select(-c(df)) |> 
  dplyr::mutate("p.value" = dplyr::if_else(p.value < 0.001,
                                         "<0.001",
                                         as.character(round(p.value,3)))) |> 
  knitr::kable(
    booktabs=T,
    digits = 3,
    col.names = c('contrast', 'factor', 'estimate', '$s_e$', '$\\text{CI}_l$', '$\\text{CI}_u$', 't', 'p-value'),
    escape=F,
    linesep='',
    caption=caption
    )
}
