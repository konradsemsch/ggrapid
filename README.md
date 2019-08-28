
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggrapid: Create neat & complete ggplot visualizations with as little code as possible

ggrapid enables creation of the most common ggplot-based visualizations
fast and with just a few lines of code. ggrapid comes handy when you’d
like to do an initial and quick EDA over various columns of your dataset
programatically, without the need of writing a lot of custom ggplot
code.

## Installation

``` r
# Install development version from GitHub
devtools::install_github("konradsemsch/ggrapid")
```

## Main functions

  - plot\_density
  - plot\_boxplot
  - plot\_deciles
  - plot\_correlation
  - plot\_bars
  - plot\_line

## Simple examples

``` r
library(tidyverse)
library(ggrapid)

diamonds %>%
  plot_density(x = carat)
```

![](man/figures/unnamed-chunk-3-1.png)<!-- -->

``` r
diamonds %>%
  plot_density(x = carat,
               fill = cut,
               position = "stack")
```

![](man/figures/unnamed-chunk-4-1.png)<!-- -->

``` r
diamonds %>%
  plot_density(x = carat,
               fill = cut,
               position = "fill")
```

![](man/figures/unnamed-chunk-5-1.png)<!-- -->

``` r
diamonds %>%
  plot_density(x = carat,
               fill = cut,
               facet = cut,
               title = "Write your title here",
               subtitle = "Write your subtitle here",
               caption = "Write your caption here",
               lab_x = "Carat",
               alpha = .5,
               vline = 1)
```

![](man/figures/unnamed-chunk-6-1.png)<!-- -->

## Complete usage
