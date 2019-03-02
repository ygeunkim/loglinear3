good_loglin <- function(x, test = "LRT", ...) {
  #-----------------------------#
  # x is glm object             #
  # collaborate with purrr::map #
  #-----------------------------#
  # mod_name <-
  #   as.character(x[[1]]$call)[2] %>% 
  #   str_extract(pattern = "(?<=~).*") %>% 
  #   str_trim()
  x_term <-
    x[[1]] %>% 
    terms() %>% 
    attr("term.labels")
  mod_name <- paste(x_term, collapse = " + ")
  broom::tidy(anova(x[[1]], test = test, ...)) %>% 
    slice(n()) %>% 
    add_column(model = mod_name, .before = 1) %>% 
    select(-term, -Deviance, -p.value)
}
