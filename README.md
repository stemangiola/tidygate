tidygate: add gate information to your tibble
================

<!---
[![Build Status](https://travis-ci.org/stemangiola/tidygate.svg?branch=master)](https://travis-ci.org/stemangiola/tidygate) [![Coverage Status](https://coveralls.io/repos/github/stemangiola/tidygate/badge.svg?branch=master)](https://coveralls.io/github/stemangiola/tidygate?branch=master)
-->
<!-- badges: start -->

[![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
<!-- badges: end -->

Please have a look also to

-   [nanny](https://github.com/stemangiola/nanny) for tidy high-level
    data analysis and manipulation

-   [tidyHeatmap](https://github.com/stemangiola/tidyHeatmap) for
    producing heatmaps following tidy principles

-   [tidybulk](https://github.com/stemangiola/tidybulk) for tidy and
    modular transcriptomics analyses

## Installation

``` r
# From Github
devtools::install_github("stemangiola/tidygate")

# From CRAN
install.package("tidygate")
```

## What is tidygate

It interactively or programmately labels points within custom gates on
two dimensions, according to tidyverse principles. The information is
added to your tibble. It is based on the package `gatepoints` from Wajid
Jawaid.

The main benefits are

-   in interactive mode you can draw your gates on extensive ggplot-like
    scatter plots
-   you can draw multiple gates
-   you can save your gates and apply the programmatically.

## Input

A tibble of this kind

| dimension1      | dimension2 | annotations |
|-----------------|------------|-------------|
| `chr` or `fctr` | `numeric`  | …           |

## Step-by-step instructons for Rstudio

##### 1) Execute the following code in the console panel

``` r
tidygate_gate <-
  tidygate_data %>%
  mutate( gate = gate_chr( Dim1, Dim2 ) )
```

##### 2) look at the Viewer and draw a gate clicking at least three times on the plot

![](inst/tidygate.gif)

##### 3) Click the finish button on the top-right corner, or press `escape` on your keyboard

## The output tibble

``` r
tidygate_gate
```

    ## # A tibble: 2,240 x 9
    ##    group   hierarchy `ct 1`    `ct 2`    relation cancer_ID   Dim1    Dim2 gate 
    ##    <chr>       <dbl> <chr>     <chr>        <dbl> <chr>      <dbl>   <dbl> <chr>
    ##  1 adrenal         1 endothel… epitheli…    -1    ACC       -0.874 -0.239  0    
    ##  2 adrenal         1 endothel… fibrobla…    -1    ACC       -0.740  0.114  1    
    ##  3 adrenal         1 endothel… immune_c…    -1    ACC       -0.988  0.118  0    
    ##  4 adrenal         1 epitheli… endothel…     1    ACC        0.851  0.261  0    
    ##  5 adrenal         1 epitheli… fibrobla…     1    ACC        0.839  0.320  0    
    ##  6 adrenal         1 epitheli… immune_c…     1    ACC        0.746  0.337  0    
    ##  7 adrenal         1 fibrobla… endothel…     1    ACC        0.722 -0.0696 0    
    ##  8 adrenal         1 fibrobla… epitheli…    -1    ACC       -0.849 -0.317  0    
    ##  9 adrenal         1 fibrobla… immune_c…     0.52 ACC       -0.776 -0.383  0    
    ## 10 adrenal         1 immune_c… endothel…     1    ACC        0.980 -0.116  0    
    ## # … with 2,230 more rows

Gates are saved in a temporary file for later use

    ## [[1]]
    ##            x          y
    ## 1 -0.9380459  0.2784375
    ## 2 -0.9555544 -0.1695209
    ## 3 -0.3310857  0.2116150
    ## 
    ## [[2]]
    ##             x          y
    ## 1  0.01324749  0.2165648
    ## 2 -0.31065917 -0.1026984
    ## 3 -0.11514794 -0.2982161
    ## 4  0.48013998  0.1225183

## Programmatic gating

We can use previously drawn gates to programmately add the gate column

``` r
tidygate_data %>%
  mutate( gate = gate_chr(
    Dim1, Dim2,
     # Pre-defined gates
    gate_list = my_gates
  ))
```

    ## # A tibble: 2,240 x 9
    ##    group   hierarchy `ct 1`    `ct 2`    relation cancer_ID   Dim1    Dim2 gate 
    ##    <chr>       <dbl> <chr>     <chr>        <dbl> <chr>      <dbl>   <dbl> <chr>
    ##  1 adrenal         1 endothel… epitheli…    -1    ACC       -0.874 -0.239  0    
    ##  2 adrenal         1 endothel… fibrobla…    -1    ACC       -0.740  0.114  1    
    ##  3 adrenal         1 endothel… immune_c…    -1    ACC       -0.988  0.118  0    
    ##  4 adrenal         1 epitheli… endothel…     1    ACC        0.851  0.261  0    
    ##  5 adrenal         1 epitheli… fibrobla…     1    ACC        0.839  0.320  0    
    ##  6 adrenal         1 epitheli… immune_c…     1    ACC        0.746  0.337  0    
    ##  7 adrenal         1 fibrobla… endothel…     1    ACC        0.722 -0.0696 0    
    ##  8 adrenal         1 fibrobla… epitheli…    -1    ACC       -0.849 -0.317  0    
    ##  9 adrenal         1 fibrobla… immune_c…     0.52 ACC       -0.776 -0.383  0    
    ## 10 adrenal         1 immune_c… endothel…     1    ACC        0.980 -0.116  0    
    ## # … with 2,230 more rows
