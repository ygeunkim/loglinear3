# Loglinear Models for Three-way tables

`R` codes to select the best models fitting three-way tables.

- `dplyr`
- `broom`
- `plyr`

packages would be used.

## `_common.R`

1. define `find_xname()`: used in `fitted_val.R`, `goodness_fit.R`
2. modify `broom::tidy.anova()` function

## `model_threeway.R`

function `model_loglin()`

- fit `glm` for given data
- for _every pair of independence model_

## `goodness_fit.R`

function `good_loglin()`

- for fitted `model_loglin`,
- compute **goodness-of-fit**
- implement with `purrr::map()` and `dplyr::bind_rows()`

## `fitted_val.R`

function `fit_loglin()`

- for fitted `model_loglin`,
- compute **fitted values** for _every pair of independence model_
- implement with `purrr::map()` and `plyr::join_all()`

***

`example.Rmd` shows how these functions can be applied.

1. call `tidyverse` and `broom` libraries, and the other functions with `r/_common.R`
2. fit with `model_loglin()`
3. goodness-of-fit with `good_loglin()`
4. choosing the best model based on the goodness-of-fit statistic
5. compute fitted values with `fitted_val`
