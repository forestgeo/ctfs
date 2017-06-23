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

## ----viz-data------------------------------------------------------------
head(bci::bci12full7)

## ------------------------------------------------------------------------
bci12full7 <- tibble::as_tibble(bci12full7)  
bci12full7

## ----viz-categorical-bar, fig.cap="Values distribution of the categorical variable `status`"----
ggplot2::ggplot(data = bci12full7) +
  ggplot2::geom_bar(mapping = aes(x = status))

## ----introduce-count-----------------------------------------------------
count_status <- dplyr::count(bci12full7, status)
count_DFstatus <- dplyr::count(bci12full7, DFstatus)
dplyr::full_join(count_status, count_DFstatus)

## ----viz-continuous-hist, fig.show="hold"--------------------------------
small_dbh <- dplyr::filter(bci12full7, dbh < 500)
gg_small_dbh <- ggplot(data = small_dbh, mapping = aes(x = dbh))
gg_small_dbh + geom_histogram(binwidth = 10)

## ------------------------------------------------------------------------
gg_small_dbh + geom_histogram(aes(dbh), binwidth = 30)

## ------------------------------------------------------------------------
gg_small_dbh + geom_histogram(aes(dbh), binwidth = 60)

## ----set-barwidth--------------------------------------------------------
useful_barwidth <- 30

## ----hist-vs-freqpoly-compare, fig.cap="For multiple histograms, lines are easier to understand than bars."----
# Make n groups with ~ numbers of observations with `ggplot2::cut_number()`
small_cut_num <- mutate(small_dbh, equal_n = cut_number(dbh, n = 5))

# Now, compare the next two plots:
ggplot(small_cut_num) + geom_histogram(aes(dbh, color = equal_n))
ggplot(small_cut_num) + geom_freqpoly(aes(dbh, color = equal_n))

## ------------------------------------------------------------------------
small_cut_width <- dplyr::mutate(
  small_dbh,
  dbh_cut = ggplot2::cut_width(dbh, width = useful_barwidth)
)
small_cut_width %>% dplyr::count(dbh_cut, sort = TRUE)

## ------------------------------------------------------------------------
largest_dbh <- dplyr::filter(bci12full7, dbh > 2000)

## ----clustering, fig.show="hold"-----------------------------------------
gg_largest_dbh <- ggplot(largest_dbh, aes(dbh))
gg_largest_dbh + geom_histogram(binwidth = useful_barwidth)
gg_largest_dbh + geom_histogram(binwidth = useful_barwidth * 2)

## ------------------------------------------------------------------------
bci_hist <- ggplot(bci12full7, aes(x = dbh)) +
  geom_histogram(binwidth = useful_barwidth)
bci_hist

## ------------------------------------------------------------------------
bci_hist + coord_cartesian(ylim = c(0, 50))

## ------------------------------------------------------------------------
unusual_trees <- bci12full7 %>% 
  filter(dbh > 2000) %>% 
  select(treeID, ExactDate, status, gx, gy, dbh) %>%
  arrange(dbh)
unusual_trees

## ----locate-unusual-trees------------------------------------------------
# For faster analysis and to avoid overplotting, choose only 100 trees at random
sample_100 <- bci12full7 %>% sample_n(100)

unusual_tree_loc <- ggplot(sample_100, aes(gx, gy)) + 
    # plot a few points for reference with only 1/10 opacity (less distracting)
    geom_point(alpha = 1/10) +
    # highlight location of unusual trees in the plot
    geom_point(data = unusual_trees, colour = "red")

## ------------------------------------------------------------------------
edge_trees <- unusual_trees %>% filter(gy < 100)
unusual_tree_loc + 
  geom_point(
    data = edge_trees, 
    size = 3, shape = 1  # highlight edge trees with bigger and hollow points
  )

## ---- eval = FALSE-------------------------------------------------------
#  diamonds2 <- diamonds %>%
#    filter(between(y, 3, 20))

## ------------------------------------------------------------------------
diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

## ---- dev = "png"--------------------------------------------------------
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point()

## ---- eval = FALSE-------------------------------------------------------
#  ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
#    geom_point(na.rm = TRUE)

## ------------------------------------------------------------------------
nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
    geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)

## ------------------------------------------------------------------------
library(lubridate)

# "Duration" is a useful intermediate; learn more with ?lubridate::duration
# %/%: integere diviison removes useless and annoying fraction of seconds.
bci12full7 <- mutate(bci12full7, duration = dseconds((date * 24 * 60 * 60) %/% 1))
bci12full7 <- mutate(bci12full7, datetime = as_datetime(duration, origin = "1960-01-01"))
bci12full7 %>% 
  select(date, datetime)

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

