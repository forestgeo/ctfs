## ---- echo = FALSE, message=FALSE, warning=FALSE-------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE
)

## ---- message=FALSE------------------------------------------------------
# installation instructions at https://github.com/forestgeo/ctfs
library(ctfs)
# installation instructions at https://github.com/lme4/lme4/
library(lme4)
# installation instructions at https://forestgeo.github.io/bci/
library(bci)

## ---- message=FALSE------------------------------------------------------
# install.packages("tidyverse")
library(tidyverse)

## ------------------------------------------------------------------------
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
head(grate)
dplyr::count(grate, census)  # 1, 2 ...: 1st, 2nd ... interval between censuses

## ------------------------------------------------------------------------
by_census <- dplyr::group_by(grate, census) 
growth_summary <- dplyr::summarize(by_census,
  yr_mean        = mean(time),       # mean years since 1992
  increment_mean = mean(incgr),      # mean of untransformed growth increment
  increment_med  = median(incgr),    # as above but median
  root_mean      = mean(CRGrowth),   # mean of cube root of growth rate
  root_med       = median(CRGrowth)  # as above but median
)
growth_summary
# (To refresh definition of variables in grate run ?individual_grow.table)

## ------------------------------------------------------------------------
exponent <- 0.45
growth_summary <- mutate(growth_summary, 
  root_exp = root_mean^(1/exponent)  # cube it to restore the original scale
  )

ggplot(data = growth_summary, aes(x = yr_mean, y = root_exp)) +
  geom_line() +
  geom_point() +
  labs(x = "years since 1992", y = "median growth increment")

## ------------------------------------------------------------------------
myformula <- CRGrowth ~ 1 + time + (1 + time | species)

## ------------------------------------------------------------------------
# Split data to capture trends before and since 3rd census
dat3_under <- dplyr::filter(grate, census <= 3)
dat3_over  <- dplyr::filter(grate, census >= 3)

# Apply a model function to each component of the split data set
mymodels <- lapply(
  list(mod3_under = dat3_under, mod3_over = dat3_over),
  function(x) lmer(myformula, data = x)
)
mymodels

## ------------------------------------------------------------------------
# Same output as mymodel, but easier to visualize and to work with
# (see the pipe operator "%>%" at http://r4ds.had.co.nz/pipes.html)
tidymodels <- mymodels %>%  # take mymodels, then transform
  lapply(broom::tidy) %>%   # from model summmaries to data frames (df), then
  tibble::enframe() %>%     # from named lists of df to single nested df, then
  tidyr::unnest()           # from nested df to normal df.
tidymodels

## ------------------------------------------------------------------------
tidymodels %>% 
  select(name, term, estimate) %>% 
  filter(term %in% c("(Intercept)", "time")) %>% 
  mutate(term = gsub("^..ntercept.$", "intercept", term)) %>% 
  spread(term, estimate)
  

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

