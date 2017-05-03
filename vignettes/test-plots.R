## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  fig.align = "center",
  fig.asp = 0.618,
  fig.width = 6,
  out.width = "75%",
  cache = TRUE,
  echo = FALSE 
)

library(ctfs)
library(knitr)

## ---- include=FALSE, error=TRUE------------------------------------------
full <- extract.growthdata(
  census1 = bci::bci12full1,
  census2 = bci::bci12full2,
  growcol = 'incgr',
  growthfunc = growth.biomass.indiv,
  logit = 'x',
  rounddown = FALSE,
  mindbh = 100,
  dbhunit = 'mm',
  err.limit = 4000,
  maxgrow = 7500
)
trimmed <- extract.growthdata(
  census1 = bci::bci12full1,
  census2 = bci::bci12full2,
  growcol = 'incgr',
  growthfunc = growth.biomass.indiv,
  logit = 'x',
  rounddown = FALSE,
  mindbh = 100,
  dbhunit = 'mm',
  err.limit = 4,
  maxgrow = 75
)
png("actual_graph_outliers_spp.png")
  graph.outliers.spp(
    full,
    trimmed,
    spname = "gustsu",
    fit = NULL,
    size = "agb",
    export = NULL,
    xtitle = "log(agb)",
    ytitle = "growth"
  )
dev.off()

## ----fig.cap="Reference"-------------------------------------------------
include_graphics("ref_graph_outliers_spp.png")

## ----fig.cap="Actual"----------------------------------------------------
include_graphics("actual_graph_outliers_spp.png")

## ----echo=TRUE-----------------------------------------------------------
extract.growthdata

## ---- include=FALSE------------------------------------------------------
png("actual_complete_plotmap.png")
complete.plotmap(
  cns = bci::bci12full6,
  spnames = NULL,
  mindbh = 10,
  export = "no",
  nospp = 3,
  plotdim = c(1000, 500),
  clrlist = c("blue",
    "green", "red", "yellow", "gray"),
  ptsize = c(0.45, 0.3),
  xrange = c(0,
    100),
  yrange = c(0, 100),
  wd = 1100,
  ht = 850,
  side = 6,
  labsize = 1.75,
  axisdiv = 10,
  filepath = NULL,
  outfile = NULL
)
dev.off()

## ----fig.cap="Reference"-------------------------------------------------
include_graphics("ref_complete_plotmap.png")

## ----fig.cap="Actual"----------------------------------------------------
include_graphics("actual_complete_plotmap.png")

## ---- echo=TRUE----------------------------------------------------------
complete.plotmap

## ---- include=FALSE------------------------------------------------------
png("actual_spparea_sq.png")
sppa <- spparea.sq(
  bci::bci12full6,
  size = c(10, 20, 50),
  mindbh = 10,
  plotdim = c(1000, 500),
  replicates = 5,
  unidennames = c('unid')
)
dev.off()

## ----fig.cap="Reference"-------------------------------------------------
include_graphics("ref_spparea_sq.png")

## ----fig.cap="Actual"----------------------------------------------------
include_graphics("actual_spparea_sq.png")

## ---- echo=TRUE----------------------------------------------------------
spparea.sq

## ---- include=FALSE------------------------------------------------------
data_split <- split_data(censdata = bci::bci12full6, splitcol = 'sp')
png("actual_map.png")
map_poular <- map(splitdatafile = data_split, species = 'poular')
dev.off()

## ----fig.cap="Reference"-------------------------------------------------
include_graphics("ref_map.png")

## ----fig.cap="Actual"----------------------------------------------------
include_graphics("actual_map.png")

## ----echo=TRUE-----------------------------------------------------------
map

