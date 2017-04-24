library(ctfs)
load_all()
library(bci)
head(bci::bci12full1)


species <- "swars1"
splitdatafile <- split.data(bci::bci12full1, splitcol = 'sp')


pdf()
map(splitdatafile = splitdatafile, species = species)
dev.off()
