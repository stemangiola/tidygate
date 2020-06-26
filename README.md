tidygate: high-level data analysis and manipulation in tidyverse style.
================

**It tidies up your playground\!**

# <img src="inst/logo-02.png" height="139px" width="120px" />

<!---

[![Build Status](https://travis-ci.org/stemangiola/tidygate.svg?branch=master)](https://travis-ci.org/stemangiola/tidygate) [![Coverage Status](https://coveralls.io/repos/github/stemangiola/tidygate/badge.svg?branch=master)](https://coveralls.io/github/stemangiola/tidygate?branch=master)

-->

<!-- badges: start -->

[![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

## Functions/utilities available

| Function            | Description                                                             |
| ------------------- | ----------------------------------------------------------------------- |
| `reduce_dimensions` | Perform dimensionality reduction (PCA, MDS, tSNE)                       |
| `rotate_dimensions` | Rotate two dimensions of a degree                                       |
| `cluster_elements`  | Labels elements with cluster identity                                   |
| `remove_redundancy` | Filter out elements with highly correlated features                     |
| `fill_missing`      | Fill values of missing element/feature pairs                            |
| `impute_missing`    | Impute values of missing element/feature pairs                          |
| `permute_nest`      | From one column build a two permuted columns with nested information    |
| `combine_nest`      | From one column build a two combination columns with nested information |
| `keep_variable`     | Keep top variable features                                              |
| `lower_triangular`  | keep rows corresponding to a lower triangular matrix                    |

| Utilities   | Description                                                      |
| ----------- | ---------------------------------------------------------------- |
| `as_matrix` | Robustly convert a tibble to matrix                              |
| `subset`    | Select columns with information relative to a column of interest |

## Minimal input data frame

| element         | feature         | value     |
| --------------- | --------------- | --------- |
| `chr` or `fctr` | `chr` or `fctr` | `numeric` |

## Output data frame

| element         | feature         | value     | new information |
| --------------- | --------------- | --------- | --------------- |
| `chr` or `fctr` | `chr` or `fctr` | `numeric` | …               |

## Installation

``` r
devtools::install_github("stemangiola/tidygate")
```

## Introduction

tidygate is a collection of wrapper functions for high level data
analysis and manipulation following the tidy paradigm.

## Tidy data

``` r
mtcars_tidy = 
    mtcars %>% 
    as_tibble(rownames="car_model") %>% 
    mutate_at(vars(-car_model,- hp, -vs), scale) %>%
    gather(feature, value, -car_model, -hp, -vs)

mtcars_tidy
```

    ## # A tibble: 288 x 5
    ##    car_model            hp    vs feature  value
    ##    <chr>             <dbl> <dbl> <chr>    <dbl>
    ##  1 Mazda RX4           110     0 mpg      0.151
    ##  2 Mazda RX4 Wag       110     0 mpg      0.151
    ##  3 Datsun 710           93     1 mpg      0.450
    ##  4 Hornet 4 Drive      110     1 mpg      0.217
    ##  5 Hornet Sportabout   175     0 mpg     -0.231
    ##  6 Valiant             105     1 mpg     -0.330
    ##  7 Duster 360          245     0 mpg     -0.961
    ##  8 Merc 240D            62     1 mpg      0.715
    ##  9 Merc 230             95     1 mpg      0.450
    ## 10 Merc 280            123     1 mpg     -0.148
    ## # … with 278 more rows

## `ADD` versus `GET` versus `ONLY` modes

Every function takes a tidyfeatureomics structured data as input, and
(i) with action=“add” outputs the new information joint to the original
input data frame (default), (ii) with action=“get” the new information
with the element or feature relative informatin depending on what the
analysis is about, or (iii) with action=“only” just the new information.
For example, from this data set

``` r
  mtcars_tidy
```

    ## # A tibble: 288 x 5
    ##    car_model            hp    vs feature  value
    ##    <chr>             <dbl> <dbl> <chr>    <dbl>
    ##  1 Mazda RX4           110     0 mpg      0.151
    ##  2 Mazda RX4 Wag       110     0 mpg      0.151
    ##  3 Datsun 710           93     1 mpg      0.450
    ##  4 Hornet 4 Drive      110     1 mpg      0.217
    ##  5 Hornet Sportabout   175     0 mpg     -0.231
    ##  6 Valiant             105     1 mpg     -0.330
    ##  7 Duster 360          245     0 mpg     -0.961
    ##  8 Merc 240D            62     1 mpg      0.715
    ##  9 Merc 230             95     1 mpg      0.450
    ## 10 Merc 280            123     1 mpg     -0.148
    ## # … with 278 more rows

**action=“add”** (Default) We can add the MDS dimensions to the original
data set

``` r
  mtcars_tidy %>%
    reduce_dimensions(
        car_model, feature, value, 
        method="MDS" ,
        .dims = 3,
        action="add"
    )
```

**action=“get”** We can add the MDS dimensions to the original data set
selecting just the element-wise column

``` r
  mtcars_tidy %>%
    reduce_dimensions(
        car_model, feature, value, 
        method="MDS" ,
        .dims = 3,
        action="get"
    )
```

**action=“only”** We can get just the MDS dimensions relative to each
element

``` r
  mtcars_tidy %>%
    reduce_dimensions(
        car_model, feature, value, 
        method="MDS" ,
        .dims = 3,
        action="only"
    )
```
