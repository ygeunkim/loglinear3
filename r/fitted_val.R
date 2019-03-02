fit_loglin <- function(x, ...) {
  # mod_name <-
  #   as.character(x[[1]]$call)[2] %>% 
  #   str_extract(pattern = "(?<=~).*") %>% 
  #   str_trim()
  x_term <-
    x[[1]] %>% 
    terms() %>% 
    attr("term.labels")
  mod_name <- paste(x_term, collapse = " + ")
  x[[1]]$model %>% 
    bind_cols(predict(x[[1]], newdata = ., type = "response", ...) %>% tbl_df()) %>% 
    rename_at(.vars = vars(value), .funs = list(~return(mod_name)))
}
