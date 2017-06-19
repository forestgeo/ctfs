## ----setup, echo = FALSE, message=FALSE, warning=FALSE-------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE
)

## ----packages------------------------------------------------------------
# To install packages see ?install.packages and ?devtools::install_github

# Data
library(bci)  # to access data from BCI

# Functions
library(forestr)  # to analyse forest dynamics
library(tidyverse)  # to visualize and transform data, and much more

## ----data----------------------------------------------------------------
# Data from Barro Colorado Island, released in 2012, FullViewTable census 7
# tibbles print faster and nicer than dataframes, but read "Interacting with 
# legacy code" at https://goo.gl/CJ5jLs.
bci12full7 <- as_tibble(bci12full7)  
bci12full7

## ---- eval=FALSE---------------------------------------------------------
#  as.data.frame(bci12full7)  # takes long and doesn't print nicely
#  View(bci12full7)  # prints to a viewer panel
#  str(bci12full7)  # informative and prints nice
#  glimpse(bci12full7)  # like str() but shows as much data as possible

## ---- fig.cap="Values distribution of the categorical variable `status`"----
ggplot(bci12full7) +
  geom_bar(aes(x = DFstatus))

## ------------------------------------------------------------------------
count(bci12full7, DFstatus)

## ------------------------------------------------------------------------
barwidth <- 10  # set once, then use multiple times

ggplot(bci12full7) +
  geom_histogram(aes(x = date), binwidth = barwidth)

## ------------------------------------------------------------------------
bci12full7 %>% 
  mutate(date = cut_width(date, width = barwidth)) %>% 
  count(date) %>% 
  tail()

## ---- echo=FALSE---------------------------------------------------------
nas_cnt <- bci12full7 %>% 
  mutate(date = cut_width(date, width = barwidth)) %>% 
  count(date) %>% 
  dplyr::filter(is.na(date)) %>% 
  pull(n)

## ------------------------------------------------------------------------
ggplot(bci12full7) +
  geom_histogram(aes(x = date), binwidth = 1)

## ---- fig.cap="Lines overlaid are easier to understand than bars."-------
ggplot(bci12full7) +
  geom_freqpoly(aes(x = date, color = DFstatus), binwidth = barwidth)

## ------------------------------------------------------------------------
ggplot(bci12full7) +
  geom_histogram(aes(x = dbh), binwidth = 50)

## ------------------------------------------------------------------------
library(lubridate)

# "Duration" is a useful intermediate; learn more with ?lubridate::duration
# %/%: integere diviison removes useless and annoying fraction of seconds.
bci12full7 <- mutate(bci12full7, duration = dseconds((date * 24 * 60 * 60) %/% 1))
bci12full7 <- mutate(bci12full7, datetime = as_datetime(duration, origin = "1960-01-01"))
bci12full7 %>% 
  select(date, datetime) %>%
  print(n = 20)  # print some more rows than tibble's default

## ------------------------------------------------------------------------
bci12full7$date <- lubridate::as_date(bci12full7$date, origin = "1960-01-01")
bci12full7 %>% 
  select(date) %>% 
  glimpse()

## ------------------------------------------------------------------------
library(skimr)  # xxx remove or declare in DESCRIPTION in Suggest:
skim(bci12full7) %>% filter(stat == "hist") %>% as.matrix()

## ---- eval=FALSE---------------------------------------------------------
#  library(skimr)
#  smry <- skim(diamonds)
#  smry %>%
#    dplyr::filter(stat == "hist") %>%
#    as.matrix()  # Only needed in Windows for histograms (https://goo.gl/S8MaZW)

