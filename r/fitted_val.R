#------------------------------------------#
# Fitted values for each independece model #
#------------------------------------------#

source("https://raw.githubusercontent.com/ygeunkim/loglinear3/master/r/_common.R")

fit_loglin <- function(x, ...) {
  #-----------------------------#
  # x is glm object             #
  # collaborate with purrr::map #
  #-----------------------------#
  mod_name <- find_xname(x)
  x[[1]]$model %>% 
    bind_cols(predict(x[[1]], newdata = ., type = "response", ...) %>% tbl_df()) %>% 
    rename_at(.vars = vars(value), .funs = list(~mod_name))
}
