## ----setup, echo = FALSE, message=FALSE, warning=FALSE-------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE
)

## ----packages------------------------------------------------------------
# To install packages see ?install.packages and ?devtools::install_github
library(bci)  # to access data of forest dynamics from BCI
library(forestr)  # to analyse forest dynamics
library(ggplot2)  # to visualize data
library(dplyr)  # to transform data
library(tibble)  # to handle large data easier

## ------------------------------------------------------------------------
seven <- bci12full7  # give the data a clearer and more memorable name

# Tibbles print better than dataframes
seven <- as_tibble(seven)
seven
# Another view; like str() but shows as much data as possible
glimpse(seven)

## ------------------------------------------------------------------------
library(lubridate)

# "Duration" is a useful intermediate; learn more with ?lubridate::duration
# %/%: integere diviison removes useless and annoying fraction of seconds.
seven <- mutate(seven, duration = dseconds((date * 24 * 60 * 60) %/% 1))
seven <- mutate(seven, datetime = as_datetime(duration, origin = "1960-01-01"))
seven %>% 
  select(date, datetime) %>%
  print(n = 20)  # print some more rows than tibble's default

## ------------------------------------------------------------------------
seven$date <- lubridate::as_date(seven$date, origin = "1960-01-01")
seven %>% 
  select(date) %>% 
  glimpse()

## ---- fig.cap="Values distribution of the categorical variable `status`"----
ggplot(seven) +
  geom_bar(aes(x = DFstatus))

## ------------------------------------------------------------------------
count(seven, DFstatus)

## ------------------------------------------------------------------------
barwidth <- 10  # set once, then use multiple times

ggplot(seven) +
  geom_histogram(aes(x = date), binwidth = barwidth)

## ------------------------------------------------------------------------
seven %>% 
  mutate(date = cut_width(date, width = barwidth)) %>% 
  count(date) %>% 
  tail()

## ---- echo=FALSE---------------------------------------------------------
nas_cnt <- seven %>% 
  mutate(date = cut_width(date, width = barwidth)) %>% 
  count(date) %>% 
  dplyr::filter(is.na(date)) %>% 
  pull(n)

## ------------------------------------------------------------------------
ggplot(seven) +
  geom_histogram(aes(x = date), binwidth = 1)

## ---- fig.cap="Lines overlaid are easier to understand than bars."-------
ggplot(seven) +
  geom_freqpoly(aes(x = date, color = DFstatus), binwidth = barwidth)

## ------------------------------------------------------------------------
ggplot(seven) +
  geom_histogram(aes(x = dbh), binwidth = 50)

## ------------------------------------------------------------------------
library(skimr)  # xxx remove or declare in DESCRIPTION in Suggest:
skim(seven) %>% filter(stat == "hist") %>% as.matrix()

## ---- eval=FALSE---------------------------------------------------------
#  library(skimr)
#  smry <- skim(diamonds)
#  smry %>%
#    dplyr::filter(stat == "hist") %>%
#    as.matrix()  # Only needed in Windows for histograms (https://goo.gl/S8MaZW)

## ------------------------------------------------------------------------
# xxx turn this off because the data type changes
# Handle and print large data nicely *xxx
# library(tibble)
# bci12full7 <- tibble::as_tibble(bci12full7)
# *xxx Note: If you get this error message:
# "Can't use matrix or array for column indexing"
# watch for problematic interaction with legacy code (https://goo.gl/g7rg2P)
# and try solve it by with as.data.frame().

