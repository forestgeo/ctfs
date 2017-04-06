tk <- "58c3725a83a8c53f8a8883bfd747442867279173"
devtools::install_github("forestgeo/ctfs@amend_namespace", auth_token = tk)
library(ctfs)



# Check throws error ----

# Error: package or namespace load failed for 'ctfs'
#   Execution halted
# * checking whether the package can be unloaded cleanly ... WARNING
# Error : object 'density' not found whilst loading namespace 'ctfs'
# Error: package or namespace load failed for 'ctfs'
#   Execution halted
# Error: object 'density' not found whilst loading namespace 'ctfs'
#   Execution halted



# My interpretation ----

# Some functions may be interpreted as S3 methods. Solution may be to rename the
# problematic functions to replace dot "." by underscore "_". Or lern more about
# S3 methods and see if I can force those functions to not be methods.



# In NAMESPACE ------------------------------------------------------------

# Functions named fun_start.fun_end that have a similar fun in other packages 
# are being interpreted as methods. This shows in NAMESPACE as follows:

S3method(density,ind)
S3method(exp,"2par")
S3method(hist,compare)
S3method(image,dataframe)
S3method(log,model)
S3method(merge,data)
S3method(plot,wavelet)
S3method(solve,topo)
S3method(split,data)



# In check ----------------------------------------------------------------

* checking S3 generic/method consistency ... WARNING
merge:
  function(x, y, ...)
merge.data:
  function(listdata, showOutput)

solve:
  function(a, b, ...)
solve.topo:
  function(coldata, column1, column2, diffcolumn, basept, baseelev,
           debug)

split:
  function(x, f, drop, ...)
split.data:
  function(censdata, splitcol, keepsplitcol, allsplit, showOutput)

hist:
  function(x, ...)
hist.compare:
  function(x, y, div, breaks)

image:
  function(x, ...)
image.dataframe:
  function(data, xcol, ycol, zcol, breaks, div, colors, xname, yname,
           xrange, yrange, ptsize, graphfile, export, newgraph, h, w,
           plotsize)

plot:
  function(x, ...)
plot.wavelet:
  function(x)

density:
  function(x, ...)
density.ind:
  function(df, plot, wsgdata, denscol)

exp:
  function(x)
exp.2par:
  function(x, param, asymp)

log:
  function(x, base)
log.model:
  function(x, param)


