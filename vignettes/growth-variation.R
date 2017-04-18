## ---- echo = FALSE, message=FALSE, warning=FALSE-------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE
)

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

## ---- eval=FALSE, include=FALSE------------------------------------------
#  by_census_interval <- dplyr::group_by(grate, census)
#  growth_summary <- dplyr::summarize(by_census_interval,
#    yr_mean        = mean(time),       # mean years since 1992
#    increment_mean = mean(incgr),      # mean of untransformed growth increment
#    increment_med  = median(incgr),    # as above but median
#    root_mean      = mean(CRGrowth),   # mean of cube root of growth rate
#    root_med       = median(CRGrowth)  # as above but median
#  )
#  growth_summary
#  # (To refresh definition of variables in grate run ?individual_grow.table)

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

