---
title: "Growth variation through time"
subtitle: "Adapted by Mauro Lepore from the original tutorial by Richard Condit (https://goo.gl/c9RjMY)"
date: "2017-04-14"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{"growth variation through time"}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



# Goal

The goal is two fold:

- to show how ctfs functions can help you solve problems that may be relevant to you;

- to show how to explore research problems using modern programatic tools and style in the tidyverse (xxxinsert link)

The original version of this tutorial, by Richard Condit, is at https://goo.gl/c9RjMY.

# Conventions to refer to code

I refer to code following the conventions of [R for data science](http://r4ds.had.co.nz/introduction.html), by Hadley Wickham & Garrett Grolemund:

> Functions are in a code font and followed by parentheses, like `sum()`, or `mean()`.

> Other R objects (like data or function arguments) are in a code font, without parentheses, like `flights` or `x`.

> If we want to make it clear what package an object comes from, we’ll use the package name followed by two colons, like` dplyr::mutate()`, or `nycflights13::flights`. This is also valid R code.

# Hypotheses

The question addressed here is how growth has changed with time, in particular how species differ in growth changes. There are two alternative models described:

* Growth rate changing linearly with time
* Growth rate varying from census to census, but not following a consistent change

# Packages

The functions we use here come with the [ctfs](https://forestgeo.github.io/ctfs/) and [lme4](https://github.com/lme4/lme4/) packages, and the data comes in the [bci](https://forestgeo.github.io/bci/) package. This, and all exploratory data analyses, benefit from using tools that come in the tidyverse package.


```r
# Links provide installation instructions
library(ctfs)       # https://github.com/forestgeo/ctfs
library(lme4)       # https://github.com/lme4/lme4/
library(bci)        # https://forestgeo.github.io/bci/
library(tidyverse)  # xxx insert link
```

To access functions commonly used for data analysis, load the tidyverse package:



# 1. Calculate growth rates of individual trees between census intervals

First, let's table the growth rate of every tree in every census interval. To calculate growth rates for individual trees and table them in the format lmer understands, we use `ctfs::individual_grow.table()`. `individual_grow.table()` collects results from several other functions.


```r
# See default arguments, definition of output variables and other details with
# ?individual_grow.table

# Example choosing censuses 1-7
census17_chr <- paste0("bci12full", 1:7)
census17_list <- lapply(census17_chr, get)
grate <- individual_grow.table(
  census17_list, 
  mindbh = 400, maxdbh = 10000  # e.g. with trees of relatively large diameter
)
# Overview
str(grate)
#> 'data.frame':	8594 obs. of  14 variables:
#>  $ treeID    : int  39 59 102 103 104 117 126 129 131 134 ...
#>  $ tag       :Class 'AsIs'  chr [1:8594] "000022" "000044" "000087" "000088" ...
#>  $ gx        : num  989 992 986 984 989 ...
#>  $ gy        : num  421 342 226 229 233 ...
#>  $ species   :Class 'AsIs'  chr [1:8594] "anacex" "loncla" "anacex" "tri2tu" ...
#>  $ dbh1      : num  1761 655 426 553 642 ...
#>  $ dbh2      : num  1855 670 533 587 661 ...
#>  $ LnSize    : num  1.1295 0.1405 -0.2897 -0.0287 0.1205 ...
#>  $ incgr     : num  22.84 3.66 26.23 8.33 4.66 ...
#>  $ LnGrowth  : num  3.13 1.3 3.27 2.12 1.54 ...
#>  $ CRGrowth  : num  4.09 1.79 4.35 2.6 2 ...
#>  $ time      : num  -8.31 -8.33 -8.34 -8.34 -8.34 ...
#>  $ census    : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ censusfact: Factor w/ 6 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
# Table summary
growth_smry <- grate %>% 
  dplyr::group_by(census) %>% 
  dplyr::summarize(
    yr_mean = mean(time),
    increment_mean = mean(incgr),
    n = n()
  )
# Visual summary
growth_smry %>% 
  ggplot(aes(x = yr_mean, y = increment_mean)) +
  geom_line() +
  geom_point()
```

![](C:\Users\dora\AppData\Local\Temp\RtmpKyRzBi\preview-3ac82b726288.dir\growth-variation_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

# 2. Summarize mean growth, forest-wide

Second, for each census period, summarize the data to give the mean growth rate of all individuals.


```r
by_census_interval <- dplyr::group_by(grate, census) 
growth_summary <- dplyr::summarize(by_census_interval,
  yr_mean        = mean(time),       # mean years since 1992
  increment_mean = mean(incgr),      # mean of untransformed growth increment
  increment_med  = median(incgr),    # as above but median
  root_mean      = mean(CRGrowth),   # mean of cube root of growth rate
  root_med       = median(CRGrowth)  # as above but median
)
growth_summary
#> # A tibble: 6 × 6
#>   census   yr_mean increment_mean increment_med root_mean root_med
#>    <int>     <dbl>          <dbl>         <dbl>     <dbl>    <dbl>
#> 1      1 -8.341837      10.280166      8.380528  2.456828 2.602987
#> 2      2 -3.963116       7.510867      5.092428  1.991760 2.080253
#> 3      3  1.207770       4.644751      3.349970  1.604515 1.722932
#> 4      4  6.046461       5.183409      3.422960  1.731147 1.739726
#> 5      5 11.042803       4.777542      3.326767  1.650958 1.717547
#> 6      6 16.079248       5.080532      3.644401  1.744470 1.789500
# (To refresh definition of variables in grate run ?individual_grow.table)
```

To visualize the growth summary, let's graph mean growth vs. time. Although you may simply graph the mean growth increment, here I use the mean of the cube-root of growth.


```r
exponent <- 0.45
growth_summary <- mutate(growth_summary, 
  root_exp = root_mean^(1/exponent)  # cube it to restore the original scale
  )
ggplot(data = growth_summary, aes(x = yr_mean, y = root_exp)) +
  geom_line() +
  geom_point() +
  labs(x = "years since 1992", y = "median growth increment")
```

![](C:\Users\dora\AppData\Local\Temp\RtmpKyRzBi\preview-3ac82b726288.dir\growth-variation_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

# 3. Mixed model to assess species variation in linear changes through time

Third, let's model a linear regression of growth rate vs. time, with species as a random effect. This is equivalent to a hierarchical model, where a Gaussian distribution describes the variation in species responses. As the response variable, we use the cube root of growth. This allows us to come closer to a normal error term while not requiring artificial exclusion of negative growth rates (as a log-tranformation would).

To build the model we can use `lme4::lmer()` with appropriate formula and data (see `?lmer`). 

An appropriate formula is 


```r
myformula <- CRGrowth ~ 1 + time + (1 + time | species)
```


After `~`, the component "`1 + time`" means to fit a model of `CRGrowth` against `time`, with an intercept (the `1`). Next, `(1 + time | species)` means that the intercept and slope should both be allowed to vary across `species`. Following the same logic, `(1 + time | tag)` would account for repeated measures of individuals by using the tree `tag` (identifying individual trees) as a random effect.

The data is more appropriately split in two than used as a whole, so that we can build two models to describe our data. This seems important because the plot above shows that growth rates through time had a different trend before and since the third census (before, they declined; since, they sabilized). So let's run two separate models, for censuses ≤ 3 and censuses ≥ 3.


```r
# Split data to capture trends before and since 3rd census
dat3_under <- dplyr::filter(grate, census <= 3)
dat3_over  <- dplyr::filter(grate, census >= 3)

# Apply a model function to each component of the split data set
mymodels <- lapply(
  list(mod3_under = dat3_under, mod3_over = dat3_over),
  function(x) lmer(myformula, data = x)
)
mymodels
#> $mod3_under
#> Linear mixed model fit by REML ['lmerMod']
#> Formula: CRGrowth ~ 1 + time + (1 + time | species)
#>    Data: x
#> REML criterion at convergence: 12650.79
#> Random effects:
#>  Groups   Name        Std.Dev. Corr 
#>  species  (Intercept) 0.59096       
#>           time        0.04298  -0.30
#>  Residual             1.25747       
#> Number of obs: 3775, groups:  species, 108
#> Fixed Effects:
#> (Intercept)         time  
#>     1.74035     -0.09083  
#> 
#> $mod3_over
#> Linear mixed model fit by REML ['lmerMod']
#> Formula: CRGrowth ~ 1 + time + (1 + time | species)
#>    Data: x
#> REML criterion at convergence: 18898.28
#> Random effects:
#>  Groups   Name        Std.Dev. Corr 
#>  species  (Intercept) 0.640728      
#>           time        0.005354 -0.52
#>  Residual             1.036247      
#> Number of obs: 6411, groups:  species, 112
#> Fixed Effects:
#> (Intercept)         time  
#>    1.733435     0.004236
```

The key result are the coefficients of each model. The fixed effect gives the forest-wide response, which is the average growth of the species. It is thus not the same as the mean forest-wide growth, because the fixed effect weights each species equally.

To do the same in a more convenient way, we can use the tidyverse. This shows how to manipulate the output of multiple models so that it is easier to visualize and to work with later:


```r
# Same output as mymodel, but easier to visualize and to work with
# (see the pipe operator "%>%" at http://r4ds.had.co.nz/pipes.html)
tidymodels <- mymodels %>%  # take mymodels, then transform
  lapply(broom::tidy) %>%   # from model summmaries to data frames (df), then
  tibble::enframe() %>%     # from named lists of df to single nested df, then
  tidyr::unnest()           # from nested df to normal df.
tidymodels
#> # A tibble: 12 × 6
#>          name                         term     estimate   std.error
#>         <chr>                        <chr>        <dbl>       <dbl>
#> 1  mod3_under                  (Intercept)  1.740353823 0.073316149
#> 2  mod3_under                         time -0.090829008 0.008406036
#> 3  mod3_under       sd_(Intercept).species  0.590962143          NA
#> 4  mod3_under              sd_time.species  0.042976877          NA
#> 5  mod3_under cor_(Intercept).time.species -0.300123339          NA
#> 6  mod3_under      sd_Observation.Residual  1.257467074          NA
#> 7   mod3_over                  (Intercept)  1.733434746 0.072898890
#> 8   mod3_over                         time  0.004236226 0.002526474
#> 9   mod3_over       sd_(Intercept).species  0.640728243          NA
#> 10  mod3_over              sd_time.species  0.005354221          NA
#> 11  mod3_over cor_(Intercept).time.species -0.516921709          NA
#> 12  mod3_over      sd_Observation.Residual  1.036246880          NA
#> # ... with 2 more variables: statistic <dbl>, group <chr>
```

The following graph uses the function fixef to get the parameters, then the function curve to add them to the graph. The points are forest-wide means, as graphed above.





```r
tidymodels %>% 
  select(name, term, estimate) %>% 
  filter(term %in% c("(Intercept)", "time")) %>% 
  mutate(term = gsub("^..ntercept.$", "intercept", term)) %>% 
  spread(term, estimate)
#> # A tibble: 2 × 3
#>         name intercept         time
#> *      <chr>     <dbl>        <dbl>
#> 1  mod3_over  1.733435  0.004236226
#> 2 mod3_under  1.740354 -0.090829008
  

mycoefs <- lapply(mymodels, fixef)

ggplot(data = growth_summary, aes(x = yr_mean, y = root_med)) +
  geom_point() +
  geom_abline(
    slope = mycoefs$mod3_under[2],
    intercept = mycoefs$mod3_under[1]
    ) +
  geom_abline(
    slope = mycoefs$mod3_over[2],
    intercept = mycoefs$mod3_over[1]
    ) +
  labs(x = "years since 1992", y = "median growth")
```

![](C:\Users\dora\AppData\Local\Temp\RtmpKyRzBi\preview-3ac82b726288.dir\growth-variation_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

xxxcont 

- move  faster, focus on models more and on plots less. 
- review model chapters from r4ds and see what I can use here.
- find a better way to do this with modelr
- note that the scale of axis y is different  from that in vignette



# References

[Repository](https://dx.doi.org/10.5479/data.bci.20130603) of the Barro Colorado Forest Census Plot Data (version 2012).

[Description](https://repository.si.edu/bitstream/handle/10088/20925/RoutputFull.pdf?sequence=1&isAllowed=y) of the Barro Colorado Forest Census Plot Data.

# Todo

MOST IMPORTANT

- Use data from several censuses in bci
- Document missing arguments in `individual_grow.table()`
- Document variable names of output in `individual_grow.table()`

LESS IMPORTANT

- track functions called by `individual_grow.table()`
