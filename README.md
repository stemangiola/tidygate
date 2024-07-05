tidygate: add gate information to your tibble
================

<!---
[![Build Status](https://travis-ci.org/stemangiola/tidygate.svg?branch=master)](https://travis-ci.org/stemangiola/tidygate) [![Coverage Status](https://coveralls.io/repos/github/stemangiola/tidygate/badge.svg?branch=master)](https://coveralls.io/github/stemangiola/tidygate?branch=master)
-->
<!-- badges: start -->

[![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
<!-- badges: end -->

## Introduction

tidygate allows you to interactively gate points on a scatter plot.
Interactively drawn gates are recorded and can be applied
programmatically to reproduce results exactly. Programmatic gating is
based on the package [gatepoints](https://github.com/wjawaid/gatepoints)
by Wajid Jawaid.

For more tidy data analysis:

- [tidyomics](https://github.com/tidyomics) - A software ecosystem for
  tidy analysis of omic data.
- [tidyHeatmap](https://github.com/stemangiola/tidyHeatmap) - Produce
  heatmaps with tidy principles.

## Installation

``` r
# From Github
devtools::install_github("stemangiola/tidygate")

# From CRAN
install.package("tidygate")
```

## Example usage

tidygate provides a single user-facing function: `gate`. The following
examples make use of this function, four packages from the tidyverse and
the inbuilt `mtcars` dataset.

``` r
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(tidygate)

mtcars |>
  head()
```

    ##                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
    ## Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
    ## Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
    ## Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
    ## Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
    ## Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
    ## Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1

By default, `gate` creates an interactive scatter plot based on
user-defined X and Y coordinates. Colour, shape, size and alpha can be
defined as constant values, or can be controlled by values in a
specified column.

Once the plot has been created, multiple gates can be drawn with the
mouse. When you have finished, click continue. `gate` will then return a
vector of strings, recording the gates each X and Y coordinate pair is
within.

``` r
mtcars_gated <- 
  mtcars |>
  mutate(gated = gate(x = mpg, y = wt, colour = disp))
```

![](man/figures/demo_gate.gif)

To select points which appear within any gates, filter for non-empty
strings. To select points which appear within a specific gate, string
pattern matching can be used.

``` r
# Select points within any gate
mtcars_gated |> 
  filter(gated != "")
```

    ##                    mpg cyl  disp  hp drat    wt  qsec vs am gear carb gated
    ## Hornet 4 Drive    21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1     2
    ## Hornet Sportabout 18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2   1,2
    ## Valiant           18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1   1,2
    ## Duster 360        14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4     1
    ## Merc 240D         24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2     2
    ## Merc 230          22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2     2
    ## Merc 280          19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4   1,2
    ## Merc 280C         17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4   1,2
    ## Merc 450SE        16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3     1
    ## Merc 450SL        17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3   1,2
    ## Merc 450SLC       15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3     1
    ## Dodge Challenger  15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2     1
    ## AMC Javelin       15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2     1
    ## Camaro Z28        13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4     1
    ## Pontiac Firebird  19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2   1,2
    ## Ford Pantera L    15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4     1
    ## Maserati Bora     15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8     1

``` r
# Select points within gate 2
mtcars_gated |>
  filter(str_detect(gated, "2"))
```

    ##                    mpg cyl  disp  hp drat    wt  qsec vs am gear carb gated
    ## Hornet 4 Drive    21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1     2
    ## Hornet Sportabout 18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2   1,2
    ## Valiant           18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1   1,2
    ## Merc 240D         24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2     2
    ## Merc 230          22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2     2
    ## Merc 280          19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4   1,2
    ## Merc 280C         17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4   1,2
    ## Merc 450SL        17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3   1,2
    ## Pontiac Firebird  19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2   1,2

Details of the interactively drawn gates are saved to
`tidygate_env$gates`. This variable is overwritten each time interactive
gates are drawn, so save it right away if you would like to access it
later.

``` r
# Inspect previously drawn gates
tidygate_env$gates |>
  head()
```

    ## # A tibble: 6 × 3
    ##       x     y .gate
    ##   <dbl> <dbl> <dbl>
    ## 1  20.4  3.60     1
    ## 2  20.3  3.86     1
    ## 3  18.7  4.26     1
    ## 4  16.0  4.34     1
    ## 5  12.1  4.34     1
    ## 6  11.7  4.26     1

``` r
# Save if needed
tidygate_env$gates |>
  write_rds("important_gates.rds")
```

If previously drawn gates are supplied to the `programmatic_gates`
argument, points will be gated programmatically. This feature allows the
reproduction of previously drawn interactive gates.

``` r
important_gates <-
  read_rds("important_gates.rds")

mtcars |>
  mutate(gated = gate(x = mpg, y = wt, programmatic_gates = important_gates)) |>
  filter(gated != "")
```

    ##                    mpg cyl  disp  hp drat    wt  qsec vs am gear carb gated
    ## Hornet 4 Drive    21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1     2
    ## Hornet Sportabout 18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2   1,2
    ## Valiant           18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1   1,2
    ## Duster 360        14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4     1
    ## Merc 240D         24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2     2
    ## Merc 230          22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2     2
    ## Merc 280          19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4   1,2
    ## Merc 280C         17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4   1,2
    ## Merc 450SE        16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3     1
    ## Merc 450SL        17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3   1,2
    ## Merc 450SLC       15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3     1
    ## Dodge Challenger  15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2     1
    ## AMC Javelin       15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2     1
    ## Camaro Z28        13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4     1
    ## Pontiac Firebird  19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2   1,2
    ## Ford Pantera L    15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4     1
    ## Maserati Bora     15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8     1
