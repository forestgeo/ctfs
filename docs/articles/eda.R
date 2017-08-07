## ----setup, echo = FALSE, message=FALSE, warning=FALSE-------------------
# hadley's settings
set.seed(1014)
options(digits = 3)

knitr::opts_chunk$set(
  echo = TRUE,  # {mine}
  comment = "#>",
  collapse = TRUE,
  # cache = TRUE,
  out.width = "70%",
  fig.align = "center",
  fig.width = 6,
  fig.asp = 0.618,  # 1 / phi
  fig.show = "hold"
)

options(dplyr.print_min = 6, dplyr.print_max = 6)

## ----packages------------------------------------------------------------
library(bci)  # data from BCI (alternative: https://goo.gl/VKbcMz)
library(tidyverse)  # visualize and transform data (and more)

## ----viz-data------------------------------------------------------------
head(bci::bci12full7)

## ------------------------------------------------------------------------
bci12full7 <- tibble::as_tibble(bci12full7)  
bci12full7

## ----viz-categorical-bar, fig.cap="Values distribution of the categorical variable `status`."----
ggplot2::ggplot(data = bci12full7) +
  ggplot2::geom_bar(mapping = aes(x = status))

## ----introduce-count-----------------------------------------------------
# Compare similar code without and with the pipe 
# ?magrittr::`%>%` (Ctrl | Command + shift + M)
status_n <- dplyr::count(bci12full7, status)
DFstatus_n <- bci12full7 %>% dplyr::count(DFstatus)
dplyr::full_join(status_n, DFstatus_n)

## ----viz-continuous-hist, fig.show="hold"--------------------------------
small_dbh <- dplyr::filter(bci12full7, dbh < 500)

# Save data and mappings in "p" to reuse them later.
p <- ggplot(data = small_dbh, mapping = aes(x = dbh))
p + geom_histogram(binwidth = 10)

## ------------------------------------------------------------------------
p + geom_histogram(aes(dbh), binwidth = 30)

## ------------------------------------------------------------------------
p + geom_histogram(aes(dbh), binwidth = 60)

## ----set-barwidth--------------------------------------------------------
# Save to reuse
useful_barwidth <- 30

## ---- out.width="30%", fig.width = 6 * 0.3 / 0.7, fig.align="default"----
# Use the DRY principle (Don't Repeat Yourself)
ggplot_histogram_by_binwidth <- function(binwidth) {
  ggplot(data = small_dbh, mapping  = aes(x = dbh)) +
    geom_histogram(binwidth = binwidth)
}

binwidths_to_try <- c(10, 30, 60)
purrr::map(.x = binwidths_to_try, .f = ggplot_histogram_by_binwidth)

## ----hist-vs-freqpoly-compare, fig.cap="For multiple histograms, lines are easier to understand than bars.", out.width="45%", fig.width= 6 * 0.45 / 0.7, fig.align="default"----
# Make n groups with ~ numbers of observations with `ggplot2::cut_number()`
small_cut_number <- mutate(small_dbh, equal_n = cut_number(dbh, n = 5))

p <- ggplot(data = small_cut_number, aes(x = dbh, color = equal_n)) +
  # use available space on plot area (defult plots legend outside)
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top")
  )

# Left
p + geom_histogram()

# Right
p + geom_freqpoly()

## ------------------------------------------------------------------------
small_cut_width <- small_dbh %>% 
  dplyr::mutate(dbh_cut = ggplot2::cut_width(dbh, width = useful_barwidth))

small_cut_width %>% 
  dplyr::count(dbh_cut, sort = TRUE)

## ------------------------------------------------------------------------
largest_dbh <- bci12full7 %>% 
  filter(dbh > 2000)

## ----is-clustering, fig.align="default", out.width="45%", fig.width= 6 * 0.45 / 0.7----
p <- ggplot(largest_dbh, aes(dbh))

# Left: apparent clustering
p + geom_histogram(binwidth = useful_barwidth)

# Right: clusters are less apparent with bars twice as wide
p + geom_histogram(binwidth = useful_barwidth * 2)

## ------------------------------------------------------------------------
p <- ggplot(bci12full7, aes(x = dbh)) +
  geom_histogram(binwidth = useful_barwidth)
p

## ------------------------------------------------------------------------
p + coord_cartesian(ylim = c(0, 50))

## ------------------------------------------------------------------------
unusual_trees <- bci12full7 %>% 
  filter(dbh > 2000) %>% 
  dplyr::select(treeID, ExactDate, status, gx, gy, dbh) %>%
  dplyr::arrange(dbh)

unusual_trees

## ----locate-unusual-trees, fig.align="default", out.width="45%", fig.width= 6 * 0.45 / 0.7----
# Left
sample_100 <- bci12full7 %>% 
  dplyr::sample_n(100)  # for faster analysis and to avoid overplotting

ggplot(sample_100, aes(gx, gy)) + 
    # plot a few points for reference with only 1/10 opacity (less distracting)
    geom_point(alpha = 1/10) +
    # highlight location of unusual trees in the plot
    geom_point(data = unusual_trees, colour = "red")

# Right
edge_trees <- unusual_trees %>% 
  filter(gy < 100)

ggplot2::last_plot() + 
  geom_point(data = edge_trees, size = 3, shape = 1)  # highlight edge trees

## ------------------------------------------------------------------------
are_usual <- !bci12full7$treeID %in% unusual_trees$treeID
usual_trees <- filter(bci12full7, are_usual)

# Confirm data set of usual trees has less rows than full data set.
nrow(bci12full7)
nrow(usual_trees)

## ------------------------------------------------------------------------
are_unusual <- !are_usual
with_unusual_made_NA <- bci12full7 %>% 
  mutate(dbh = ifelse(are_unusual, NA_real_, dbh))

## ------------------------------------------------------------------------
# Confirm no rows have been removed,
nrow(bci12full7)
nrow(with_unusual_made_NA)

# but dbh of unusual trees is NA
unusual_only <- with_unusual_made_NA %>% 
  filter(are_unusual) %>% 
  select(dbh, treeID)
unusual_only

## ---- fig.align="default", out.width="45%", fig.width= 6 * 0.45 / 0.7----
# Left: don't remove NAs and get a warning on the console
ggplot(unusual_only, aes(dbh)) + 
  geom_histogram(binwidth = useful_barwidth) +
  labs(title = "Expect empty plot but get a warning")

# Right: NAs explicitely removed in geom_histogram(), so no warning
ggplot(unusual_only, aes(dbh)) + 
  geom_histogram(binwidth = useful_barwidth, na.rm = TRUE) +
  labs(title = "Expect empty plot but no warning")

## ------------------------------------------------------------------------
missing_dbh_trees <- bci12full7 %>% 
  mutate(missing_dbh = is.na(dbh))

ggplot(missing_dbh_trees) + 
  geom_bar(aes(DFstatus, fill = missing_dbh))

## ------------------------------------------------------------------------
has_single_unique_value <- function(x) {dplyr::n_distinct(x) == 1}

missing_dbh_trees %>% 
  filter(missing_dbh == TRUE, DFstatus == "alive") %>% 
  select_if(has_single_unique_value) %>% 
  unique()

## ------------------------------------------------------------------------
# The data set has too many species; choosing just a few to make this point
a_few_species <- bci12full7 %>% 
  select(sp) %>% 
  unique() %>% 
  pull() %>% 
  .[1:5]
data_few_sp <- bci12full7 %>% 
  filter(sp %in% a_few_species)

ggplot(data_few_sp, aes(dbh)) +
  geom_freqpoly(aes(color = sp))

## ------------------------------------------------------------------------
library(forcats)  # tools for dealing with categorical variables

ggplot(data_few_sp) + 
  # order `sp` from high to low frequency
  geom_bar(mapping = aes(x = forcats::fct_infreq(sp)))

## ------------------------------------------------------------------------
ggplot(data_few_sp, aes(x= dbh, y = ..density..)) +
  geom_freqpoly(aes(color = sp))

## ---- echo = FALSE, out.width = "100%"-----------------------------------
knitr::include_graphics(
  "https://raw.githubusercontent.com/hadley/r4ds/master/images/EDA-boxplot.png"
)

## ------------------------------------------------------------------------
ggplot(data_few_sp, aes(sp, dbh)) +
  geom_boxplot()

## ------------------------------------------------------------------------
data_few_sp <- data_few_sp %>% 
  mutate(sp = reorder(sp, dbh, FUN = median, na.rm = TRUE))

ggplot(data_few_sp, aes(sp, dbh)) + 
  geom_boxplot()

## ---- out.width="45%", fig.width= 6 * 0.45 / 0.7, fig.align="default"----
# install.packages("ggstance")
library(ggstance)

# Left
ggplot2::last_plot() +
  coord_flip()

# Right
ggplot(data_few_sp) +
  ggstance::geom_boxploth(aes(x = dbh, y = sp))  # swap x, y; compare to above

## ------------------------------------------------------------------------
# install.packages("lvplot")
library(lvplot)

ggplot(data_few_sp, aes(sp, dbh)) + 
  lvplot::geom_lv()

## ---- out.width="45%", fig.width= 6 * 0.45 / 0.7, fig.align="default"----
# Left
ggplot(data_few_sp, aes(sp, dbh)) + 
  geom_violin()

# Right
ggplot(data_few_sp, aes(dbh)) + 
  geom_histogram() +
  facet_wrap(~sp)

## ---- out.width="30%", fig.align="default", fig.width= 6 * 0.3 / 0.7-----
# install.packages("ggbeeswarm")
library(ggbeeswarm)

small_dataset <- data_few_sp %>% 
  group_by(sp) %>% 
  sample_n(50)

p <- ggplot(small_dataset, aes(sp, dbh))

# Left
p + geom_point()

# Center
p + geom_jitter()

# Right
p + ggbeeswarm::geom_quasirandom()

## ------------------------------------------------------------------------
# Simple count
ggplot(data_few_sp) +
  geom_count(aes(x = sp, y = DFstatus))

## ------------------------------------------------------------------------
# Proportion; columns sum to 1.
ggplot(data_few_sp, aes(x = sp, y = DFstatus)) +
  geom_count(aes(size = ..prop.., group = sp)) +
  scale_size_area(max_size = 10)

## ------------------------------------------------------------------------
few_spp_n <- data_few_sp %>% 
  count(sp, DFstatus)

few_spp_n

## ------------------------------------------------------------------------
ggplot(few_spp_n, aes(sp, DFstatus)) +
  geom_tile(aes(fill = n))

## ---- echo=FALSE---------------------------------------------------------
set.seed(1234)

## ------------------------------------------------------------------------
large_dataset <- bci12full7 %>% 
  filter(dbh < 400) %>% 
  sample_n(100000)

p <- ggplot(large_dataset, aes(dbh, agb))
p + geom_point()

## ------------------------------------------------------------------------
p + geom_point(alpha = 1 / 50)

## ---- fig.asp = 1, fig.align="default", out.width = "45%", fig.width= 6 * 0.45 / 0.7, message = FALSE----
# Left
p + geom_bin2d()

# Right
p + geom_hex()

## ------------------------------------------------------------------------
p <- ggplot(large_dataset, aes(dbh, agb))
p + geom_boxplot(aes(group = cut_width(dbh, useful_barwidth)))

## ------------------------------------------------------------------------
p + geom_boxplot(aes(group = cut_number(dbh, 20)))

## ----odd-agb-------------------------------------------------------------
large_strange <- large_dataset %>% filter(dbh > 80 & dbh < 140)

ggplot(data = large_dataset, aes(x = dbh, y = agb)) + 
  geom_point() +
  geom_point(data = large_strange, color = "red")

## ----model-agb, out.width="45%", fig.width= 6 * 0.45 / 0.7, fig.align="default"----
library(modelr)

mod <- lm(log(agb) ~ log(dbh), data = large_dataset)
resid_added <- large_dataset %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

# Left
resid_strange <- resid_added %>% filter(dbh > 80 & dbh < 140)
p <- ggplot(data = resid_added, aes(x = dbh, y = resid)) + 
  geom_point() +
  geom_point(data = resid_strange, color = "red")
p

# Right
resid_strange_too <- resid_added %>% filter(dbh < 80, resid > 5)
p + geom_point(data = resid_strange_too, size = 3, colour = "red", shape = 1)

