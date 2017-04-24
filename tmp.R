# Test pdf.allplot() to see if replacing subset by [ in map() fixed
# pdf.allplot

library(ctfs)
load_all()
library(bci)



# Small sub set of data for example
splitdata <- split.data(bci::bci12full1, splitcol = 'sp')["hybapr"]
spplist <- bci::bci12spptable[bci12spptable$sp == "hybapr", ]
elev <- bci::bci_elevation



pdf.allplot(
  splitdata = splitdata,
  spplist = spplist,
  elev = elev,
  path = "./"
)



png.allplot(
  splitdata = splitdata,
  spplist = spplist,
  elev = elev,
  path = "./"
)
