#------------------------------------------#
# Modeling every type of independece model #
# for three-way tables                     #
#------------------------------------------#
# indep: mutual independence, (X, Y, Z)
# joint1, joint2, joint3: joint independence, (XY, Z)
# conditional1, conditional2, conditional3: conditional independence, (XZ, YZ)
# homogen: homogeneous association / no three-factor interaction, (XY, YZ, XZ)
# three: three-factor interaction / saturated model, (XYZ)
#-------------------------------------------

model_loglin <- function(.data, yname, glm_fam = poisson(), ...) {
  #------------------------------------------------------------------------------#
  # .data to be fit glm                                                          #
  # yname character object which is response variable                            #
  # glm_fam function corresponding to family option of glm(), error distribution #
  # Since loglinear model, glm_fam = poisson() is set to be default              #
  #------------------------------------------------------------------------------#
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


