
# Loglinear Models for Three-way tables

> *Note: This is not an `r` package but just a set of `r` functions
> aiming at one purpose.*

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

Frow now on, this document will show how these functions can be applied.

1.  call `tidyverse` and `broom` libraries, and the other functions with
    `r/_common.R`
2.  fit with `model_loglin()`
3.  goodness-of-fit with `good_loglin()`
4.  choosing the best model based on the goodness-of-fit statistic
5.  compute fitted values with `fitted_val`

## Loading Functions

``` r
# install.packages("httr")
library(httr)
git_api <- GET("https://api.github.com/repos/ygeunkim/loglinear3/git/trees/master?recursive=1")
stop_for_status(git_api)
repo_list <- unlist(lapply(content(git_api)$tree, "[", "path"), use.names = FALSE)
# R files in r folder -----------------------------
repo_list <- stringr::str_subset(repo_list, pattern = "^r/")
repo_list <- stringr::str_c("https://raw.githubusercontent.com/ygeunkim/loglinear3/master/", repo_list)
# source every file
sapply(repo_list, source)
#>         https://raw.githubusercontent.com/ygeunkim/loglinear3/master/r/_common.R
#> value   ?                                                                       
#> visible FALSE                                                                   
#>         https://raw.githubusercontent.com/ygeunkim/loglinear3/master/r/fitted_val.R
#> value   ?                                                                          
#> visible FALSE                                                                      
#>         https://raw.githubusercontent.com/ygeunkim/loglinear3/master/r/goodness_fit.R
#> value   ?                                                                            
#> visible FALSE                                                                        
#>         https://raw.githubusercontent.com/ygeunkim/loglinear3/master/r/model_threeway.R
#> value   ?                                                                              
#> visible FALSE
```

## Beginning

<!-- ```{r} -->

<!-- source("r/_common.R") -->

<!-- ``` -->

`tidyverse` and `broom` are loaded. Here we should modify `tidy`
function of tidymodels (2018). `tidy.anova()` is not defined for
`anova.glm`, so we should add more elements in `renamers` in the
function. Using the original `tidy` gives `warning message`.

    renamers <- c(
        ...
        "Ref.df" = "ref.df",
        "Deviance" = "deviance", # glm object
        "Resid. Df" = "resid.df", # glm object
        "Resid. Dev" = "resid.dev" # glm object
      )

In addition, `find_xname()` function would be used later to construct
the various independence models.

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

Long data format as above is easy to fit loglinear model. Against wide
format, i.e. contingency table, try `tidyr::gather()`.

### Change to the long data format

For example, look at the below table.

``` r
(aids <- read_table("data/AIDS.dat"))
#> # A tibble: 4 x 4
#>   race  azt     yes    no
#>   <chr> <chr> <dbl> <dbl>
#> 1 white yes      14    93
#> 2 white no       32    81
#> 3 black yes      11    52
#> 4 black no       12    43
```

``` r
aids %>% 
  gather(yes, no, key = symptom, value = count)
#> # A tibble: 8 x 4
#>   race  azt   symptom count
#>   <chr> <chr> <chr>   <dbl>
#> 1 white yes   yes        14
#> 2 white no    yes        32
#> 3 black yes   yes        11
#> 4 black no    yes        12
#> 5 white yes   no         93
#> 6 white no    no         81
#> 7 black yes   no         52
#> 8 black no    no         43
```

Finally, we get the long data.

## Fitting GLMs

### in hand

We can write every formula in hand. However, it is annoying.

``` r
(subs_hierarchy <-
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
  ))
#> # A tibble: 1 x 5
#>   indep  ac_m   amcm   acamcm acm   
#>   <list> <list> <list> <list> <list>
#> 1 <glm>  <glm>  <glm>  <glm>  <glm>
```

### Defining function

Function can be defined for more general usage. See `r/model_threeway.R`

<!-- ```{r} -->

<!-- source("r/model_threeway.R") -->

<!-- ``` -->

`model_loglin()` fits loglinear model for every pair of independence
model.

  - `.data`: data
  - `yname`: character, response variable
  - `glm_fam`: random component of glm, option for `family` of `glm()`.
    Since we are fitting loglinear model, `poisson()` is set to be
    default.

It returns `tibble` with list element.

``` r
(subs_hierarchy <- model_loglin(substance, yname = "count", glm_fam = poisson()))
#> # A tibble: 1 x 9
#>   indep joint1 joint2 joint3 conditional1 conditional2 conditional3 homogen
#>   <lis> <list> <list> <list> <list>       <list>       <list>       <list> 
#> 1 <glm> <glm>  <glm>  <glm>  <glm>        <glm>        <glm>        <glm>  
#> # … with 1 more variable: threefac <list>
```

Each list has `glm` object with `[[1]]`.

``` r
subs_hierarchy$indep[[1]]
#> 
#> Call:  glm(formula = mutual, family = glm_fam, data = .)
#> 
#> Coefficients:
#>  (Intercept)     alcoholno  cigarettesno   marijuanano  
#>        6.292        -1.785        -0.649         0.315  
#> 
#> Degrees of Freedom: 7 Total (i.e. Null);  4 Residual
#> Null Deviance:       2850 
#> Residual Deviance: 1290  AIC: 1340
```

## Chi-square Goodness-of-fit tests

### Statistic

  
![G^2 = 2\\sum
n\_{ijk}\\ln\\frac{n\_{ijk}}{\\hat\\mu\_{ijk}}](https://latex.codecogs.com/png.latex?G%5E2%20%3D%202%5Csum%20n_%7Bijk%7D%5Cln%5Cfrac%7Bn_%7Bijk%7D%7D%7B%5Chat%5Cmu_%7Bijk%7D%7D
"G^2 = 2\\sum n_{ijk}\\ln\\frac{n_{ijk}}{\\hat\\mu_{ijk}}")  

  
![X^2 = \\sum\\frac{(n\_{ijk} -
\\hat\\mu\_{ijk})^2}{\\hat\\mu\_{ijk}}](https://latex.codecogs.com/png.latex?X%5E2%20%3D%20%5Csum%5Cfrac%7B%28n_%7Bijk%7D%20-%20%5Chat%5Cmu_%7Bijk%7D%29%5E2%7D%7B%5Chat%5Cmu_%7Bijk%7D%7D
"X^2 = \\sum\\frac{(n_{ijk} - \\hat\\mu_{ijk})^2}{\\hat\\mu_{ijk}}")  

with **residual df = the number of cell count - the number of
non-redundant parameters**

<!-- ```{r} -->

<!-- source("r/goodness_fit.R") -->

<!-- ``` -->

`good_loglin()` calculates the above statistic for given option `test`.
`test = "LRT"` option of `anova.glm()` produces
![G^2](https://latex.codecogs.com/png.latex?G%5E2 "G^2"). `test = Chisq`
gives ![X^2](https://latex.codecogs.com/png.latex?X%5E2 "X^2"). As
noticed, this function *is designed to be* applied with `purrr::map()`
and `dplyr::bind_rows()`.

``` r
(subs_good <-
  subs_hierarchy %>% 
  map(good_loglin, test = "LRT") %>% 
  bind_rows()) %>% 
  pander::pander()
```

|                                                              model                                                              | df | resid.df | resid.dev |
| :-----------------------------------------------------------------------------------------------------------------------------: | :-: | :------: | :-------: |
|                                                alcohol + cigarettes + marijuana                                                 | 1  |    4     |   1286    |
|                                     alcohol + cigarettes + marijuana + cigarettes:marijuana                                     | 1  |    3     |   534.2   |
|                                      alcohol + cigarettes + marijuana + alcohol:marijuana                                       | 1  |    3     |   939.6   |
|                                      alcohol + cigarettes + marijuana + alcohol:cigarettes                                      | 1  |    3     |   843.8   |
|                           alcohol + cigarettes + marijuana + alcohol:marijuana + cigarettes:marijuana                           | 1  |    2     |   187.8   |
|                          alcohol + cigarettes + marijuana + alcohol:cigarettes + cigarettes:marijuana                           | 1  |    2     |   92.02   |
|                            alcohol + cigarettes + marijuana + alcohol:cigarettes + alcohol:marijuana                            | 1  |    2     |   497.4   |
|                alcohol + cigarettes + marijuana + alcohol:cigarettes + alcohol:marijuana + cigarettes:marijuana                 | 1  |    1     |   0.374   |
| alcohol + cigarettes + marijuana + alcohol:cigarettes + alcohol:marijuana + cigarettes:marijuana + alcohol:cigarettes:marijuana | 1  |    0     |     0     |

### Choosing the best model

From ![G^2](https://latex.codecogs.com/png.latex?G%5E2 "G^2"), we
compare reduced model to complex model

  
![G^2(M\_0 \\mid M\_1) = G^2(M\_0) - G^2(M\_1) \\approx \\chi^2\\Big(df
= df(M\_0) -
df(M\_1)\\Big)](https://latex.codecogs.com/png.latex?G%5E2%28M_0%20%5Cmid%20M_1%29%20%3D%20G%5E2%28M_0%29%20-%20G%5E2%28M_1%29%20%5Capprox%20%5Cchi%5E2%5CBig%28df%20%3D%20df%28M_0%29%20-%20df%28M_1%29%5CBig%29
"G^2(M_0 \\mid M_1) = G^2(M_0) - G^2(M_1) \\approx \\chi^2\\Big(df = df(M_0) - df(M_1)\\Big)")  

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

|                                                           alternative                                                           | df | resid.df | resid.dev | goodness |
| :-----------------------------------------------------------------------------------------------------------------------------: | :-: | :------: | :-------: | :------: |
| alcohol + cigarettes + marijuana + alcohol:cigarettes + alcohol:marijuana + cigarettes:marijuana + alcohol:cigarettes:marijuana | 1  |    0     |     0     |    0     |
|                alcohol + cigarettes + marijuana + alcohol:cigarettes + alcohol:marijuana + cigarettes:marijuana                 | 1  |    1     |   0.374   |  0.374   |
|                          alcohol + cigarettes + marijuana + alcohol:cigarettes + cigarettes:marijuana                           | 1  |    2     |   92.02   |  92.02   |
|                                     alcohol + cigarettes + marijuana + cigarettes:marijuana                                     | 1  |    3     |   534.2   |  534.2   |
|                                                alcohol + cigarettes + marijuana                                                 | 1  |    4     |   1286    |   1286   |

Table continues below

| df\_good | p\_value |
| :------: | :------: |
|    0     |    1     |
|    1     |  0.541   |
|    2     |    0     |
|    3     |    0     |
|    4     |    0     |

1.  `(AC, AM, CM)` vs saturated model `(ACM)`: cannot reject
    ![M\_0](https://latex.codecogs.com/png.latex?M_0 "M_0") with p-value
    `0.5408`, so we choose `(AC, AM, CM)`
2.  `(A, CM)` vs `(AC, AM, CM)`: reject
    ![M\_0](https://latex.codecogs.com/png.latex?M_0 "M_0")

Thus, **we use the model `(AC, AM, CM)`**

## Fitted values

<!-- ```{r} -->

<!-- source("r/fitted_val.R") -->

<!-- ``` -->

`fit_loglin()` computes fitted values for each independent model. Use
this function with `purrr::map()` and `plyr::join_all()`. In
`plyr::join_all()`, every variable name including response is written.

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

Table continues below

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

Table continues below

| alcohol + cigarettes + marijuana + alcohol:cigarettes | alcohol + cigarettes + marijuana + alcohol:marijuana + cigarettes:marijuana |
| :---------------------------------------------------: | :-------------------------------------------------------------------------: |
|                         611.2                         |                                    909.2                                    |
|                         837.8                         |                                    438.8                                    |
|                         210.9                         |                                    45.76                                    |
|                         289.1                         |                                    555.2                                    |
|                         19.4                          |                                    4.76                                     |
|                         26.6                          |                                    142.2                                    |
|                         118.5                         |                                    0.24                                     |
|                         162.5                         |                                    179.8                                    |

Table continues below

| alcohol + cigarettes + marijuana + alcohol:cigarettes + cigarettes:marijuana | alcohol + cigarettes + marijuana + alcohol:cigarettes + alcohol:marijuana |
| :--------------------------------------------------------------------------: | :-----------------------------------------------------------------------: |
|                                    885.9                                     |                                    710                                    |
|                                    563.1                                     |                                    739                                    |
|                                    29.45                                     |                                    245                                    |
|                                    470.6                                     |                                    255                                    |
|                                    28.12                                     |                                   0.703                                   |
|                                    17.88                                     |                                   45.3                                    |
|                                    16.55                                     |                                   4.297                                   |
|                                    264.4                                     |                                   276.7                                   |

Table continues below

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

## References

<div id="refs" class="references hanging-indent">

<div id="ref-Agresti:2012aa">

Agresti, Alan. 2012. *Categorical Data Analysis*. 3rd ed. Wiley.

</div>

<div id="ref-broom">

tidymodels. 2018. “Stats-Anova-tidiers.R.”
<https://github.com/tidymodels/broom/blob/master/R/stats-anova-tidiers.R>.

</div>

</div>
