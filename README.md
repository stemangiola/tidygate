tidygate: add gate information to your tibble
================

<!---

[![Build Status](https://travis-ci.org/stemangiola/tidygate.svg?branch=master)](https://travis-ci.org/stemangiola/tidygate) [![Coverage Status](https://coveralls.io/repos/github/stemangiola/tidygate/badge.svg?branch=master)](https://coveralls.io/github/stemangiola/tidygate?branch=master)

-->

<!-- badges: start -->

[![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

It interactively or programmately labels points within custom gates on
two dimensions, according to tidyverse principles. The information is
added to your tibble. It is based on the package `gatepoints` from Wajid
Jawaid.

The main benefits are

  - in interactive mode you can draw your gates on extensive ggplot-like
    scatter plots
  - you can draw multiple gates
  - you can save your gates and apply the programmatically.

**Input (tibble)**

| element         | dimension1      | dimension2 | annotations |
| --------------- | --------------- | ---------- | ----------- |
| `chr` or `fctr` | `chr` or `fctr` | `numeric`  | …           |

**Command** `input %>% gate(element, dimension1, dimension2)`

![](inst/tidygate.gif)

**Output (tibble)**

| element         | dimension1      | dimension2 | annotations | Gate |
| --------------- | --------------- | ---------- | ----------- | ---- |
| `chr` or `fctr` | `chr` or `fctr` | `numeric`  | …           | …    |

## Installation

``` r
devtools::install_github("stemangiola/tidygate")
```
