# Test abund.manycensus to see if it works before and after replacing 
# subset by [.


# pkgs --------------------------------------------------------------------

library(testthat)
load_all()

# tst data ----------------------------------------------------------------
png("complete.plotmap.png")
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
  # filepath = "./",
  # outfile = "complete.plotmap_src"
)
dev.off()











graph.outliers.spp(
  full,
  trimmed,
  spname = "gustsu"
  fit = NULL
  size = "agb"
  export = NULL
  xtitle = "log(agb)"
  ytitle = "growth"
)







