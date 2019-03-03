#------------------------------------------#
# Fitted values for each independece model #
#------------------------------------------#

source("r/_common.R")

fit_loglin <- function(x, ...) {
  mod_name <- find_xname(x)
  x[[1]]$model %>% 
    bind_cols(predict(x[[1]], newdata = ., type = "response", ...) %>% tbl_df()) %>% 
    rename_at(.vars = vars(value), .funs = list(~return(mod_name)))
}
