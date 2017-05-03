## ---- echo = FALSE, message=FALSE, warning=FALSE-------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE
)

# Not simply library(tidyverse) to avoid a note in check, but should work.
library(dplyr)
library(purrr)
library(tibble)

## ---- message=FALSE------------------------------------------------------
# Links provide installation instructions
library(ctfs)       # https://github.com/forestgeo/ctfs
library(lme4)       # https://github.com/lme4/lme4/
library(bci)        # https://forestgeo.github.io/bci/
library(tidyverse)  # xxx insert link
library(modelr)     # xxx insert link

## ---- message=FALSE------------------------------------------------------

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
(grate <- as_tibble(grate))

## ------------------------------------------------------------------------
# Table summary
growth_smry <- grate %>% 
  dplyr::group_by(census) %>% 
  dplyr::summarize(
    yr_mean = mean(time),  # mean years since 1992
    increment_mean = mean(incgr),  # mean of untransformed growth increment
    n = n()  # count observations
  )
growth_smry

# Visual summary
growth_smry %>% 
  ggplot(aes(x = yr_mean, y = increment_mean)) +
  geom_line() +
  geom_point()

## ------------------------------------------------------------------------
by_census_interval <- dplyr::group_by(grate, census) 
growth_summary <- dplyr::summarize(by_census_interval,
  yr_mean        = mean(time),       # mean years since 1992
  increment_mean = mean(incgr),      # mean of untransformed growth increment
  increment_med  = median(incgr),    # as above but median
  root_mean      = mean(CRGrowth),   # mean of cube root of growth rate
  root_med       = median(CRGrowth)  # as above but median
)
growth_summary
# (To refresh definition of variables in grate run ?individual_grow.table)

## ---- eval=FALSE, include=FALSE------------------------------------------
#  exponent <- 0.45
#  growth_summary <- mutate(growth_summary,
#    root_exp = root_mean^(1/exponent)  # cube it to restore the original scale
#    )
#  ggplot(data = growth_summary, aes(x = yr_mean, y = root_exp)) +
#    geom_line() +
#    geom_point() +
#    labs(x = "years since 1992", y = "median growth increment")

## ------------------------------------------------------------------------
myformula <- incgr ~ 1 + time + (1 + time | species)

## ------------------------------------------------------------------------
mixed_model <- function(df) {
  lmer(incgr ~ 1 + time + (1 + time | species), data = df)
}
modlist <- grate %>% 
  mutate(uo3 = ifelse(census <= 3, "under3", "over3")) %>% 
  group_by(uo3) %>% 
  nest() %>%
  mutate(models = purrr::map(data, mixed_model))

# extract coefficients
bind_rows(
  fixef(modlist$models[[1]]) %>% enframe() %>% mutate(uo3 = "under3"),
  fixef(modlist$models[[1]]) %>% enframe() %>% mutate(uo3 = "over3")
)

# add predictions and residuals to data
map2(modlist$data, modlist$models, add_predictions) %>% 
  map2(modlist$models, add_residuals) %>% 
  set_names(c("under3", "over3")) %>% 
  enframe() %>% 
  unnest() %>% 
  rename(uo3 = name) %>% 
  # plot predictions and residuals
  ggplot(aes(time, pred)) +
  geom_point() +
  geom_point(aes(y = resid), colour = "red", alpha = 0.2)

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

## ---- eval=FALSE, echo=TRUE----------------------------------------------
#  # Same output as mymodel, but easier to visualize and to work with
#  # (see the pipe operator "%>%" at http://r4ds.had.co.nz/pipes.html)
#  tidymodels <- mymodels %>%  # take mymodels, then transform
#    lapply(broom::tidy) %>%   # from model summmaries to data frames (df), then
#    tibble::enframe() %>%     # from named lists of df to single nested df, then
#    tidyr::unnest()           # from nested df to normal df.
#  tidymodels

## ---- eval=FALSE, echo=TRUE----------------------------------------------
#  tidymodels %>%
#    select(name, term, estimate) %>%
#    filter(term %in% c("(Intercept)", "time")) %>%
#    mutate(term = gsub("^..ntercept.$", "intercept", term)) %>%
#    spread(term, estimate)
#  
#  
#  mycoefs <- lapply(mymodels, fixef)
#  
#  ggplot(data = growth_summary, aes(x = yr_mean, y = root_med)) +
#    geom_point() +
#    geom_abline(
#      slope = mycoefs$mod3_under[2],
#      intercept = mycoefs$mod3_under[1]
#      ) +
#    geom_abline(
#      slope = mycoefs$mod3_over[2],
#      intercept = mycoefs$mod3_over[1]
#      ) +
#    labs(x = "years since 1992", y = "median growth")

