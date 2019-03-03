
# Loglinear Models for Three-way tables

## Overview

`R` codes to select the best models fitting three-way tables.

  - `dplyr`
  - `broom`
  - `plyr`

packages would be used. Install these before use this set.

### `_common.R`

1.  define `find_xname()`: used in `fitted_val.R`, `goodness_fit.R`
2.  modify `broom::tidy.anova()` function

### `model_threeway.R`

function `model_loglin()`

  - fit `glm` for given data
  - for *every pair of independence model*

### `goodness_fit.R`

function `good_loglin()`

  - for fitted `model_loglin`,
  - compute **goodness-of-fit**
  - implement with `purrr::map()` and `dplyr::bind_rows()`

### `fitted_val.R`

function `fit_loglin()`

  - for fitted `model_loglin`,
  - compute **fitted values** for *every pair of independence model*
  - implement with `purrr::map()` and `plyr::join_all()`

-----

This file will show how these functions can be applied.

1.  call `tidyverse` and `broom` libraries, and the other functions with
    `r/_common.R`
2.  fit with `model_loglin()`
3.  goodness-of-fit with `good_loglin()`
4.  choosing the best model based on the goodness-of-fit statistic
5.  compute fitted values with `fitted_val`

## Begin

``` r
source("r/_common.R") # tidyverse and broom
#> ── Attaching packages ────────────────────────────────────────────────── tidyverse 1.2.1 ──
#> ✔ ggplot2 3.1.0       ✔ purrr   0.3.0  
#> ✔ tibble  2.0.1       ✔ dplyr   0.8.0.1
#> ✔ tidyr   0.8.2       ✔ stringr 1.4.0  
#> ✔ readr   1.3.1       ✔ forcats 0.4.0
#> ── Conflicts ───────────────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
```

Here we should modify `tidy` function of tidymodels (2018).
`tidy.anova()` is not defined for `anova.glm`, so we should add some
lines against `warning messages`.

``` r
tidy.anova
#> function (x, ...) 
#> {
#>     renamers <- c(AIC = "AIC", BIC = "BIC", deviance = "deviance", 
#>         logLik = "logLik", Df = "df", Chi.Df = "df", `Sum Sq` = "sumsq", 
#>         `Mean Sq` = "meansq", `F value` = "statistic", `Pr(>F)` = "p.value", 
#>         Res.Df = "res.df", RSS = "rss", `Sum of Sq` = "sumsq", 
#>         F = "statistic", Chisq = "statistic", `P(>|Chi|)` = "p.value", 
#>         `Pr(>Chi)` = "p.value", Pr..Chisq. = "p.value", Pr..Chi. = "p.value", 
#>         p.value = "p.value", Chi.sq = "statistic", LR.Chisq = "statistic", 
#>         `LR Chisq` = "statistic", edf = "edf", Ref.df = "ref.df", 
#>         Deviance = "deviance", `Resid. Df` = "resid.df", `Resid. Dev` = "resid.dev")
#>     names(renamers) <- make.names(names(renamers))
#>     ret <- fix_data_frame(x)
#>     unknown_cols <- setdiff(colnames(ret), c("term", names(renamers)))
#>     if (length(unknown_cols) > 0) {
#>         warning("The following column names in ANOVA output were not ", 
#>             "recognized or transformed: ", paste(unknown_cols, 
#>                 collapse = ", "))
#>     }
#>     colnames(ret) <- dplyr::recode(colnames(ret), !!!renamers)
#>     if ("term" %in% names(ret)) {
#>         ret <- mutate(ret, term = stringr::str_trim(term))
#>     }
#>     as_tibble(ret)
#> }
```

In addition,

``` r
find_xname
#> function (fit) 
#> {
#>     x_term <- fit[[1]] %>% terms() %>% attr("term.labels")
#>     paste(x_term, collapse = " + ")
#> }
```

This function would be used later to construct the various independence
models.

## Data: Alcohol, cigarette, and marijuana use

Data from Agresti (2012) would be used.

> refers to a survey by the Wright State University School of Medicine
> and the United Health Services in Dayton, Ohio. The survey asked 2276
> students in their final year of high school in a nonurban area near
> Dayton, Ohio, whether they had ever used alcohol, cigarettes, or
> maijuana.

``` r
(substance <-
  read_delim("data/Substance_use.dat", delim = " ") %>% 
  mutate(alcohol = str_trim(alcohol)) %>% 
  mutate_if(is.character, factor) %>% 
  mutate_if(is.factor, fct_rev))
#> # A tibble: 8 x 4
#>   alcohol cigarettes marijuana count
#>   <fct>   <fct>      <fct>     <dbl>
#> 1 yes     yes        yes         911
#> 2 yes     yes        no          538
#> 3 yes     no         yes          44
#> 4 yes     no         no          456
#> 5 no      yes        yes           3
#> 6 no      yes        no           43
#> 7 no      no         yes           2
#> 8 no      no         no          279
```

Long data format as above is easy to fit loglinear model.

## Fitting GLMs

### in hand

We can write every formula in hand. However, it is annoying.

``` r
subs_hierarchy <-
  substance %>% 
  do(
    indep = glm(count ~ alcohol + cigarettes + marijuana, data = ., family = poisson()),
    ac_m = glm(count ~ alcohol + cigarettes + marijuana + alcohol:cigarettes, 
               data = ., family = poisson()),
    amcm = glm(count ~ alcohol + cigarettes + marijuana + alcohol:marijuana + cigarettes:marijuana, 
               data = ., family = poisson()),
    acamcm = glm(count ~ alcohol + cigarettes + marijuana + alcohol:cigarettes + alcohol:marijuana + cigarettes:marijuana, 
                 data = ., family = poisson()),
    acm = glm(count ~ alcohol * cigarettes * marijuana, 
              data = ., family = poisson())
  )
```

### Defining function

``` r
source("r/model_threeway.R")
```

Function can be defined for more general usage. This return `tibble`
with list element. Each list has `glm` object with `[[1]]`

``` r
model_loglin
#> function (.data, yname, glm_fam = poisson(), ...) 
#> {
#>     xname <- .data %>% select(-yname) %>% names()
#>     two_int <- apply(combn(3, 2), 2, function(x) {
#>         xname[x] %>% paste0(collapse = ":")
#>     })
#>     mutual <- paste(yname, ".", sep = "~") %>% as.formula()
#>     two <- paste(yname, ".^2", sep = "~")
#>     jt1 <- paste(two, two_int[1], two_int[2], sep = "-") %>% 
#>         as.formula()
#>     jt2 <- paste(two, two_int[1], two_int[3], sep = "-") %>% 
#>         as.formula()
#>     jt3 <- paste(two, two_int[2], two_int[3], sep = "-") %>% 
#>         as.formula()
#>     cond1 <- paste(two, two_int[1], sep = "-") %>% as.formula()
#>     cond2 <- paste(two, two_int[2], sep = "-") %>% as.formula()
#>     cond3 <- paste(two, two_int[3], sep = "-") %>% as.formula()
#>     two <- two %>% as.formula()
#>     three <- paste(yname, ".^3", sep = "~") %>% as.formula()
#>     .data %>% do(indep = glm(formula = mutual, data = ., family = glm_fam), 
#>         joint1 = glm(formula = jt1, data = ., family = glm_fam), 
#>         joint2 = glm(formula = jt2, data = ., family = glm_fam), 
#>         joint3 = glm(formula = jt3, data = ., family = glm_fam), 
#>         conditional1 = glm(formula = cond1, data = ., family = glm_fam), 
#>         conditional2 = glm(formula = cond2, data = ., family = glm_fam), 
#>         conditional3 = glm(formula = cond3, data = ., family = glm_fam), 
#>         homogen = glm(formula = two, data = ., family = glm_fam), 
#>         threefac = glm(formula = three, data = ., family = glm_fam))
#> }
```

``` r
(subs_hierarchy <- model_loglin(substance, yname = "count", glm_fam = poisson()))
#> # A tibble: 1 x 9
#>   indep joint1 joint2 joint3 conditional1 conditional2 conditional3 homogen
#>   <lis> <list> <list> <list> <list>       <list>       <list>       <list> 
#> 1 <S3:… <S3: … <S3: … <S3: … <S3: glm>    <S3: glm>    <S3: glm>    <S3: g…
#> # … with 1 more variable: threefac <list>
```

## Chi-square Goodness-of-fit tests

### Statistic

\[G^2 = 2\sum n_{ijk}\ln\frac{n_{ijk}}{\hat\mu_{ijk}}\]

\[X^2 = \sum\frac{(n_{ijk} - \hat\mu_{ijk})^2}{\hat\mu_{ijk}}\]

with

\[\text{residual df} = \text{the number of cell count} - \text{the number of non-redundant parameters}\]

``` r
source("r/goodness_fit.R")
```

``` r
good_loglin
#> function (x, test = "LRT", ...) 
#> {
#>     mod_name <- find_xname(x)
#>     tidy(anova(x[[1]], test = test, ...)) %>% slice(n()) %>% 
#>         add_column(model = mod_name, .before = 1) %>% select(-term, 
#>         -deviance, -p.value)
#> }
```

`test = "LRT"` option of `anova.glm()` produces \(G^2\). As noticed,
this function *is designed to be* applied with `purrr::map()`.

``` r
(subs_good <-
  subs_hierarchy %>% 
  map(good_loglin, test = "LRT") %>% 
  bind_rows()) %>% 
  pander::pander()
```

|                                                              model                                                              | df | resid.df |  resid.dev  |
| :-----------------------------------------------------------------------------------------------------------------------------: | :-: | :------: | :---------: |
|                                                alcohol + cigarettes + marijuana                                                 | 1  |    4     |    1286     |
|                                     alcohol + cigarettes + marijuana + cigarettes:marijuana                                     | 1  |    3     |    534.2    |
|                                      alcohol + cigarettes + marijuana + alcohol:marijuana                                       | 1  |    3     |    939.6    |
|                                      alcohol + cigarettes + marijuana + alcohol:cigarettes                                      | 1  |    3     |    843.8    |
|                           alcohol + cigarettes + marijuana + alcohol:marijuana + cigarettes:marijuana                           | 1  |    2     |    187.8    |
|                          alcohol + cigarettes + marijuana + alcohol:cigarettes + cigarettes:marijuana                           | 1  |    2     |    92.02    |
|                            alcohol + cigarettes + marijuana + alcohol:cigarettes + alcohol:marijuana                            | 1  |    2     |    497.4    |
|                alcohol + cigarettes + marijuana + alcohol:cigarettes + alcohol:marijuana + cigarettes:marijuana                 | 1  |    1     |    0.374    |
| alcohol + cigarettes + marijuana + alcohol:cigarettes + alcohol:marijuana + cigarettes:marijuana + alcohol:cigarettes:marijuana | 1  |    0     | \-4.152e-14 |

### Choosing the best model

From \(G^2\), we compare reduced model to complex
model

\[G^2(M_0 \mid M_1) = G^2(M_0) - G^2(M_1) \approx \chi^2\Big(df = df(M_0) - df(M_1)\Big)\]

``` r
subs_good %>% 
  rename(alternative = model) %>% 
  group_by(resid.df) %>% 
  slice(which.min(resid.dev)) %>% 
  arrange(resid.df) %>% 
  mutate(
    goodness = c(resid.dev[1], -diff(resid.dev)),
    df_good = c(resid.df[1], -diff(resid.df))
  ) %>% 
  mutate(p_value = pchisq(goodness, df = df_good, lower.tail = FALSE)) %>% 
  pander::pander()
```

|                                                           alternative                                                           | df | resid.df |  resid.dev  |  goodness   |
| :-----------------------------------------------------------------------------------------------------------------------------: | :-: | :------: | :---------: | :---------: |
| alcohol + cigarettes + marijuana + alcohol:cigarettes + alcohol:marijuana + cigarettes:marijuana + alcohol:cigarettes:marijuana | 1  |    0     | \-4.152e-14 | \-4.152e-14 |
|                alcohol + cigarettes + marijuana + alcohol:cigarettes + alcohol:marijuana + cigarettes:marijuana                 | 1  |    1     |    0.374    |    0.374    |
|                          alcohol + cigarettes + marijuana + alcohol:cigarettes + cigarettes:marijuana                           | 1  |    2     |    92.02    |    92.02    |
|                                     alcohol + cigarettes + marijuana + cigarettes:marijuana                                     | 1  |    3     |    534.2    |    534.2    |
|                                                alcohol + cigarettes + marijuana                                                 | 1  |    4     |    1286     |    1286     |

Table continues below

| df\_good |  p\_value  |
| :------: | :--------: |
|    0     |     1      |
|    1     |   0.5408   |
|    2     | 1.043e-20  |
|    3     | 1.837e-115 |
|    4     | 3.574e-277 |

1.  `(AC, AM, CM)` vs saturated model `(ACM)`: cannot reject \(M_0\)
    with p-value `0.5408`, so we choose `(AC, AM, CM)`
2.  `(A, CM)` vs `(AC, AM, CM)`: reject \(M_0\)

Thus, **we use the model `(AC, AM, CM)`**

## Fitted values

``` r
source("r/fitted_val.R")
```

``` r
fit_loglin
#> function (x, ...) 
#> {
#>     mod_name <- find_xname(x)
#>     x[[1]]$model %>% bind_cols(predict(x[[1]], newdata = ., type = "response", 
#>         ...) %>% tbl_df()) %>% rename_at(.vars = vars(value), 
#>         .funs = list(~return(mod_name)))
#> }
```

``` r
subs_hierarchy %>% 
  map(fit_loglin) %>% 
  plyr::join_all(by = c("count", "alcohol", "cigarettes", "marijuana")) %>% 
  pander::pander()
```

| count | alcohol | cigarettes | marijuana | alcohol + cigarettes + marijuana |
| :---: | :-----: | :--------: | :-------: | :------------------------------: |
|  911  |   yes   |    yes     |    yes    |               540                |
|  538  |   yes   |    yes     |    no     |              740.2               |
|  44   |   yes   |     no     |    yes    |              282.1               |
|  456  |   yes   |     no     |    no     |              386.7               |
|   3   |   no    |    yes     |    yes    |               90.6               |
|  43   |   no    |    yes     |    no     |              124.2               |
|   2   |   no    |     no     |    yes    |              47.33               |
|  279  |   no    |     no     |    no     |              64.88               |

Table continues
below

| alcohol + cigarettes + marijuana + cigarettes:marijuana | alcohol + cigarettes + marijuana + alcohol:marijuana |
| :-----------------------------------------------------: | :--------------------------------------------------: |
|                          782.7                          |                        627.3                         |
|                          497.5                          |                        652.9                         |
|                          39.39                          |                        327.7                         |
|                          629.4                          |                        341.1                         |
|                          131.3                          |                        3.284                         |
|                          83.47                          |                        211.5                         |
|                          6.609                          |                        1.716                         |
|                          105.6                          |                        110.5                         |

Table continues
below

| alcohol + cigarettes + marijuana + alcohol:cigarettes | alcohol + cigarettes + marijuana + alcohol:marijuana + cigarettes:marijuana |
| :---------------------------------------------------: | :-------------------------------------------------------------------------: |
|                         611.2                         |                                    909.2                                    |
|                         837.8                         |                                    438.8                                    |
|                         210.9                         |                                    45.76                                    |
|                         289.1                         |                                    555.2                                    |
|                         19.4                          |                                    4.76                                     |
|                         26.6                          |                                    142.2                                    |
|                         118.5                         |                                   0.2396                                    |
|                         162.5                         |                                    179.8                                    |

Table continues
below

| alcohol + cigarettes + marijuana + alcohol:cigarettes + cigarettes:marijuana | alcohol + cigarettes + marijuana + alcohol:cigarettes + alcohol:marijuana |
| :--------------------------------------------------------------------------: | :-----------------------------------------------------------------------: |
|                                    885.9                                     |                                    710                                    |
|                                    563.1                                     |                                    739                                    |
|                                    29.45                                     |                                    245                                    |
|                                    470.6                                     |                                    255                                    |
|                                    28.12                                     |                                  0.7034                                   |
|                                    17.88                                     |                                   45.3                                    |
|                                    16.55                                     |                                   4.297                                   |
|                                    264.4                                     |                                   276.7                                   |

Table continues
below

| alcohol + cigarettes + marijuana + alcohol:cigarettes + alcohol:marijuana + cigarettes:marijuana | alcohol + cigarettes + marijuana + alcohol:cigarettes + alcohol:marijuana + cigarettes:marijuana + alcohol:cigarettes:marijuana |
| :----------------------------------------------------------------------------------------------: | :-----------------------------------------------------------------------------------------------------------------------------: |
|                                              910.4                                               |                                                               911                                                               |
|                                              538.6                                               |                                                               538                                                               |
|                                              44.62                                               |                                                               44                                                                |
|                                              455.4                                               |                                                               456                                                               |
|                                              3.617                                               |                                                                3                                                                |
|                                              42.38                                               |                                                               43                                                                |
|                                              1.383                                               |                                                                2                                                                |
|                                              279.6                                               |                                                               279                                                               |

<div id="refs" class="references">

<div id="ref-Agresti:2012aa">

Agresti, Alan. 2012. *Categorical Data Analysis*. 3rd ed. Wiley.

</div>

<div id="ref-broom">

tidymodels. 2018. “Stats-Anova-tidiers.R.”
<https://github.com/tidymodels/broom/blob/master/R/stats-anova-tidiers.R>.

</div>

</div>
