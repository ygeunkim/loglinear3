# wrangling
library(tidyverse)
#-----------------
# tidy anova
library(broom)

find_xname <- function(fit) {
  #----------------------------------#
  # fit glm object                   #
  # return independence formula form #
  #----------------------------------#
  x_term <-
    fit[[1]] %>% 
    terms() %>% 
    attr("term.labels")
  paste(x_term, collapse = " + ")
}

#------------------------------------------------------------------------
# tidy.anova() does not support column names of anova.glm
# so add some of them
# https://github.com/tidymodels/broom/blob/master/R/stats-anova-tidiers.R
#------------------------------------------------------------------------
tidy.anova <- function(x, ...) {
  # there are many possible column names that need to be transformed
  renamers <- c(
    "AIC" = "AIC",              # merMod
    "BIC" = "BIC",              # merMod
    "deviance" = "deviance",    # merMod
    "logLik" = "logLik",        # merMod
    "Df" = "df",
    "Chi.Df" = "df",
    "Sum Sq" = "sumsq",
    "Mean Sq" = "meansq",
    "F value" = "statistic",
    "Pr(>F)" = "p.value",
    "Res.Df" = "res.df",
    "RSS" = "rss",
    "Sum of Sq" = "sumsq",
    "F" = "statistic",
    "Chisq" = "statistic",
    "P(>|Chi|)" = "p.value",
    "Pr(>|Chi|)" = "p.value",
    "Pr(>Chi)" = "p.value",
    "Pr..Chisq." = "p.value",
    "Pr..Chi." = "p.value",
    "p.value" = "p.value",
    "Chi.sq" = "statistic",
    "LR.Chisq" = "statistic",
    "LR Chisq" = "statistic",
    "edf" = "edf",
    "Ref.df" = "ref.df",
    "loglik" = "logLik",
    "Deviance" = "deviance", # glm object
    "Resid. Df" = "resid.df", # glm object
    "Resid. Dev" = "resid.dev" # glm object
  )
  
  names(renamers) <- make.names(names(renamers))
  
  ret <- fix_data_frame(x)
  unknown_cols <- setdiff(colnames(ret), c("term", names(renamers)))
  if (length(unknown_cols) > 0) {
    warning(
      "The following column names in ANOVA output were not ",
      "recognized or transformed: ",
      paste(unknown_cols, collapse = ", ")
    )
  }
  
  colnames(ret) <- dplyr::recode(colnames(ret), !!!renamers)
  
  if("term" %in% names(ret)){
    # if rows had names, strip whitespace in them
    ret <- mutate(ret, term = stringr::str_trim(term))
  }
  as_tibble(ret)
}
