model_loglin <- function(.data, yname, glm_fam = gaussian(), ...) {
  xname <-
    .data %>% 
    select(-yname) %>% 
    names()
  two_int <-
    apply(combn(3, 2), 2, function(x) {
      xname[x] %>% 
        paste0(collapse = ":")
    })
  mutual <- paste(yname, ".", sep = "~") %>% as.formula()
  two <- paste(yname, ".^2", sep = "~")
  jt1 <- paste(two, two_int[1], two_int[2], sep = "-") %>% as.formula()
  jt2 <- paste(two, two_int[1], two_int[3], sep = "-") %>% as.formula()
  jt3 <- paste(two, two_int[2], two_int[3], sep = "-") %>% as.formula()
  cond1 <- paste(two, two_int[1], sep = "-") %>% as.formula()
  cond2 <- paste(two, two_int[2], sep = "-") %>% as.formula()
  cond3 <- paste(two, two_int[3], sep = "-") %>% as.formula()
  two <- two %>% as.formula()
  three <- paste(yname, ".^3", sep = "~") %>% as.formula()
  .data %>% 
    do(
      indep = glm(formula = mutual, data = ., family = glm_fam),
      joint1 = glm(formula = jt1, data = ., family = glm_fam),
      joint2 = glm(formula = jt2, data = ., family = glm_fam),
      joint3 = glm(formula = jt3, data = ., family = glm_fam),
      conditional1 = glm(formula = cond1, data = ., family = glm_fam),
      conditional2 = glm(formula = cond2, data = ., family = glm_fam),
      conditional3 = glm(formula = cond3, data = ., family = glm_fam),
      homogen = glm(formula = two, data = ., family = glm_fam),
      threefac = glm(formula = three, data = ., family = glm_fam)
    )
}


