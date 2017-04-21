# inst --------------------------------------------------------------------

tk <- "58c3725a83a8c53f8a8883bfd747442867279173"
devtools::install_github("forestgeo/bci@dev", auth_token = tk)



# pkg ---------------------------------------------------------------------

library(testthat)
library(bci)
library(ctfs)
library(tibble)
library(dplyr)



# dat ---------------------------------------------------------------------

cns1 <- bci::bci12full1
cns2 <- bci::bci12full2



# tst ---------------------------------------------------------------------

coords.col02 =
  fullplot.imageJ(
    path = mapfolder,
    include.subdir = T,
    gridsize = c(20, 20),
    outfile = NULL,
    corners = c('p1', 'p2', 'p3', 'p4'),
    prefix = 'q',
    colrange = c(0, 49),
    rowrange = c(0, 24),
    subquadsuffix = c('_1', '_2', '_3', '_4')
)







fullplot.imageJ(path = "", outfile = "plotLxLy.txt", delim = ",",
  include.subdir = T, corners = c("p1", "p2", "p3", "p4"), colrange = c(0,
  49), rowrange = c(0, 24), prefix = "Map_", suffix = ".txt",
  subquadsuffix = c("_1", "_2", "_3", "_4"), gridsize = c(10, 10),
  debug = NULL)


mapfolder = '/maps/rabi/'
coords =
  fullplot.imageJ(
    path = mapfolder,
    include.subdir = T,
    gridsize = c(10, 10),
    outfile = 'location.txt',
    corners = c('p2', 'p1', 'p4', 'p3'),
    prefix = 'Map_',
    colrange = c(0, 49),
    rowrange = c(0, 24),
    subquadsuffix = c('_1', '_2', '_3', '_4')
  )
head(coords)
dim(coords)
range(coords$lx)
range(coords$ly)











