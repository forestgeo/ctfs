## ----setup, echo = FALSE, message=FALSE, warning=FALSE-------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE
)

## ----packages------------------------------------------------------------
# To install packages see ?install.packages and ?devtools::install_github
library(bci)      # to access data of forest dynamics from BCI
library(forestr)  # to analyse forest dynamics
library(ggplot2)  # to visualize data
library(dplyr)    # to transform data
library(tibble)   # to print large data nicely

bci12full7 <- tibble::as_tibble(bci12full7)  # xxx remove once bci::* is tibble

## ------------------------------------------------------------------------
glimpse(bci12full7)

## ---- fig.cap="Values distribution of the categorical variable `status`"----
ggplot(data = bci12full7) +
  geom_bar(mapping = aes(x = DFstatus))

## ------------------------------------------------------------------------
bci12full7 %>% 
  count(DFstatus)

## ------------------------------------------------------------------------
ggplot(data = bci12full7) +
  geom_histogram(mapping = aes(x = date), binwidth = 20)

## ------------------------------------------------------------------------
bci12full7 %>% 
  mutate(date = cut_width(date, width = 20)) %>% 
  count(date)

## ---- fig.cap="Lines overlaid are easier to understand than bars."-------
ggplot(data = bci12full7) +
  geom_freqpoly(mapping = aes(x = date, color = DFstatus), binwidth = 20)

## ------------------------------------------------------------------------
library(skimr)
skim(bci12full7) %>% filter(stat == "hist") %>% as.matrix()

## ---- eval=FALSE---------------------------------------------------------
#  library(skimr)
#  smry <- skim(diamonds)
#  smry %>%
#    dplyr::filter(stat == "hist") %>%
#    as.matrix()  # Only needed in Windows for histograms (https://goo.gl/S8MaZW)

