#----------------------------------------#
# Calculating chi-square goodness-of-fit #
#----------------------------------------#

source("https://raw.githubusercontent.com/ygeunkim/loglinear3/master/r/_common.R")

good_loglin <- function(x, test = "LRT", ...) {
  #-----------------------------#
  # x is glm object             #
  # collaborate with purrr::map #
  #-----------------------------#
  mod_name <- find_xname(x)
  tidy(anova(x[[1]], test = test, ...)) %>% 
    slice(n()) %>% 
    add_column(model = mod_name, .before = 1) %>% 
    select(-term, -deviance, -p.value)
}
