# forestr 0.0.0.9000

TODO

Remove comma: replace "\n#' @param word," by "\n#' @param word".



DOING

Improve documentation in forestr by incorporating documentation in CTFS-CRAN where appropriate. Where possible, I use documentation in forestr exclusively. That from CTFS-CRAN is only used to add missing documentation (most often params) and as a source of information to rewrite forestr's documentation.

FUNCTIONS WHICH NAME IS THE SAME IN BOTH PACKAGES

SOURCE OF PARAMS

- `abundance`, I mostly used forestr, but I re-wrote for clarity. Common params:
    - censdata
    - type
    - alivecode
    - mindbh
    - dbhunit
    - split1
    - split2


- `findborderquads`
    - index Quadrate number, between 0 and 1249 in the standard plot.
    - dist Distance in m within which the neighboring quadrates are located. Distance is measured from any side of the index quadrate.
    - gridsize Side of the square quadrate, 20 x 20m by default.
    - plotdim Dimensions of the plot: east-west 1000m and north-south 500m


INHERITING PARAMPS

Inherit from abundance:
- `abundance.spp`, Some args don't match.
- `biomass.change`
- `ba`

OTHER FUNCTIONS

- `assemble.demography`, Some argument names don't match.
- `elev.to.list`, may be deprecated. Converts a data.frame into a list, but this can be done with list(data.frame(x))
- `find.climits`



 [9] "growth.dbh.Rd"           
[10] "growth.eachspp.Rd"
[11] "growth.indiv.Rd"         
[12] "growth.Rd"               
[13] "gxgy.to.hectindex.Rd"    
[14] "gxgy.to.index.Rd"        
[15] "gxgy.to.rowcol.Rd"       
[16] "index.to.gxgy.Rd"        
[17] "index.to.rowcol.Rd"      
[18] "map.Rd"                  
[19] "maptopo.Rd"              
[20] "mortality.calculation.Rd"
[21] "mortality.dbh.Rd"        
[22] "mortality.eachspp.Rd"    
[23] "mortality.Rd"            
[24] "readelevdata.Rd"         
[25] "recruitment.eachspp.Rd"  
[26] "recruitment.Rd"          
[27] "rowcol.to.index.Rd"      
[28] "tojulian.Rd"             
[29] "trim.growth.Rd"          






## Enhanced

- To help users find the functions they need the online [functions' reference](https://forestgeo.github.io/forestr/reference/index.html) now gives more information about each function and better organized. Using the folder and file name where each function lived in the CTFSRPackage, I arranged the functions alphabetically by the name of folder, file and function. And I extracted text from the description of each function to give some indication of what each function does. I did all this pragmatically; it would improve with human curation.

- Clone from ctfs 0.0.0.9005 ([advice](https://goo.gl/GhNLyz))

# ctfs 0.0.0.9005

### Fixed

- Fixed name of functions that where automatically interpreted as S3 methods

To permanently fix this problem, I replaced dots "." by "_" in the names of those problematic functions. The change should be easy to digest by users. The new names of those functions shows up with RStudio's autocompletion, and the change has been detailed in the documentation of each function.

The functions are these:

```R
1     density.ind  # replaced by density_ind
2        exp.2par  # replaced by exp_2par
3    hist.compare  # replaced by hist_compare
4 image.dataframe  # replaced by image_dataframe
5       log.model  # replaced by log_model
6      merge.data  # replaced by merge_data
7    plot.wavelet  # replaced by plot_wavelet
8      solve.topo  # replaced by solve_topo
9      split.data  # replaced by split_data
```

- Fixed NOTE on partially matched arguments.

```R
complete.plotmap: warning ...: partial argument match of 'exp' to 'export'

dgammaMinusdexp: warning in pgamma(z, shape = a, rate = r + lambda,
  lower.tail = FALSE, log = TRUE): partial argument match of 'log' to
  'log.p'
  
map: warning in define.graphwindow(exp = export, h = ht, w = wd, file =
  filename): partial argument match of 'exp' to 'export'
  
map: warning in maptopo(elev = elevdata, plotdim = plotdim, xaxis =
  xaxis, yaxis = yaxis, interval = topoint, ht = ht, wd = wd, plotside
  = plotside, labelsize = labsize, axspos = axspos, bgcolor = bgcolor,
  clr = topoclr): partial argument match of 'elev' to 'elevmat'
run.growthbin.manyspp: warning in run.growthfit.bin(growthdata =
  spdata, size = size, binoption = binoption, startpar = startpar,
  sdmodel = sdmodel, startsdpar = startsdpar, badsdfunc = badsdfunc,
  norep = noreps, noburn = noburn, noshow = noshow, ...): partial
  argument match of 'norep' to 'noreps'

selectrandomquad2: warning in index.to.gxgy(r, grid = 1, plotdim =
  plotdim - size): partial argument match of 'grid' to 'gridsize'
```

- Fixed one problem in `wavelet.univariate()`, which erroneously refered to `wavelet.univariate()` as `wavelet.var()`. Also fixed an erroneous reference to `wavelet.var()` in examples of `wavelet.univariate()`.

- Fixed bug in `model.littleR.Gibbs()`. In a code chunk, lowercase names of the data set passed to the argument sptable because the variable `idlevel` was referred to with inconsistent case. After that chunk the original names were recovered to avoid potential problems downstream.

- Fixed bugs in `pdf.allplot()` and `png.allplot()` by replacing `subset()` by `[` in `map()`. `subset()`  [should not be used inside functions](http://adv-r.had.co.nz/Computing-on-the-language.html).

- Fixed `wavelet.bivariate()`, replace `as.real` (defunc) by `as.double`. Detect missing argument type and wrote a warning in the help file.



### Enhanced

- Updated persons' roles

The version of CTFS archived on CRAN contained the following information (file AUTHORS):

```r
Authors to date:
Rick Condit	<condit@ctfs.si.edu>
Pamela Hall <phall@alum.mit.edu>
Suzanne Lao  <laoz@si.edu>
Kyle Harms	<kharms@lsu.edu>
Akira Itoh	<itoh57@hotmail.com>
```

Conservatively, every person listed above is described as authors in the current development (if this needs to change let me konw):

```R
Authors@R: c(
    person("CTFS-ForestGEO", , , "ForestGEO@si.edu", role = c("cph", "fnd")),
    person("Rick", "Condit", , "condit@ctfs.si.edu", role = c("aut")),
    person("Pamela", "Hall", , "phall@alum.mit.edu", role = c("aut")),
    person("Suzanne", "Lao", , "laoz@si.edu", role = c("aut")),
    person("Kyle", "Harms", , "kharms@lsu.edu", role = c("aut")),
    person("Mauro", "Lepore", , "leporem@si.edu", role = c("aut", "ctr", "cre"))
    )
```

Persons responsabilities are listed in `?person`. Some useful ones are these:

"aut": (Author) Use for full authors who have made substantial contributions to the package and should show up in the package citation.

"ctb": (Contributor) Use for authors who have made smaller contributions (such as code patches etc.) but should not show up in the package citation.

"cph": (Copyright holder) Use for all copyright holders.

"cre": (Creator) Use for the package _maintainer_.

"ctr": (Contractor) Use for authors who have been contracted to write (parts of) the package and hence do not own intellectual property.

- Refer to functions from external packages explicitly with pkg::fun()

    - `mvtnorm::dmvnorm` and `mvtnorm::rmvnorm`, used in
        - `graph.mvnorm()`
        - `lmerBayes.hyperllike.sigma()`
        - `lmerBayes.hyperllike.mean()`
        - `llike.model.lmer()`
        
    - `MCMCpack::rinvgamma`, used in:
        - `Gibbs.regsigma()`
        - `Gibbs.normalvar()`
        
    - `splancs::spoints`, used in:
        -`NeighborDensities()`
        -`NDcount()`
        -`RipUvK()`
    
    - `splancs::inpip`, used in:
        `NeighborDensities()`
        
    - `splancs::dsquare`, used in:
        - `NeighborDensities()`
    
    - `date::mdy.date` and `date::date.mmddyy`, used in:
        - `tojulian()`
        - `fromjulian()`
        - `mortality.eachspp()`

> ...this is what I recommend: list the package in DESCRIPTION so that it’s installed, then always refer to it explicitly with pkg::fun(). Unless there is a strong reason not to, it’s better to be explicit.

--[R packages, by Hadley Wickham](http://r-pkgs.had.co.nz/namespace.html)

- Enhanced source code of functions listed below to defensively avoid non standar evaluation (not appropriate for programming). Replaced `subset()` by `[` in:

    - `model.littleR.Gibbs()`,
    - `density.ind()`,
    - `abund.manycensus()`
    - `individual_grow.table()`
    - `individual_mort.table()`
    - `extract.growthdata()`
    - `graph.outliers.spp()`
    - `lmerBayes()`
    - `complete.plotmap()`
    - `modelBayes()`: but errs. It also erred before replacing `subset()` by `[`
    - `NeighborDensities()`
    - `allquadratslopes()`
    - `spparea.sq()`
    - `split.data()`
    - `countspp()`

- Enhanced documentation of functions listed below by introducing `?wsgdata_dummy()`, a function to create dummy wood density tables.
    
    - `biomass.CTFSdb()`
    - `density.ind()`

- In `individual_grow.table()`

    - enhanced documentation: @return, @param rnd, @param cnsdata
    - enhanced source code: remove default of data argument cnsdata

- Suggest package bci, doesn't import it because bci is in a private repo, so the user needs to provide a private token.

- Enhanced documentation in `wavelet.bivariate()`: argument type is missing from function definition, so I added a warning.



### Deprecated

- `attach_if_needed()`: "In programming, functions should not change the search path unless that is their purpose" (_Good practice_ in `?attach`).

- `CTFSplot()`: no longer necessary because bci data is now available via the _bci_ package.



### Notes

- Added tests to verify that functions amended output equal values before and after the fix.

- Added a new vignette to test plots output remain the same after fixes to plotting functions. Follows advice in _Testing R Code_, by Richard Cotton.

- Added a new vignette to report package quality

- Added a new utility functions to remove rows full of NAs from data frames and matrices. Exported to access it from other functions easyly, but removed from index with `@keywords internal`.

- Created a private repo on forestgeo to store the CTFS version archived on CRAN, which seems to be a useful source for missing documentation and data, and for deciding what are the most important functions.

- Tested all functions used in tutorials and out of a total of 28 functions, 13 failed to run. In addition to errors, I detected a number of other problems, described next.

- In website references, gropued functions by file. This will later improve to include original source folder and the first line of the description of each function.

### To fix

- Document undocumented arguments

The R CMD check throws 1 warning for over 1,000 undocumented arguments in documentation objects.

```R
Undocumented arguments in documentation object 'AGB.dbtable'
  'df' 'dbname' 'plot' 'code' 'censusno'
Undocumented arguments in documentation object 'AGB.ind'
  'df' 'dbhunit' 'plot' 'wsgdata' 'forest' 'ht.param' 'htmodel'
...(about 1,150 more)
```
> Checking for missing documentation entries. All exported objects must be documented. See ?tools::undoc for more details.

http://r-pkgs.had.co.nz/check.html

This may take long to fix but important. Some arguments seems easy to define because they are defined somewhere else or because they are obvious. But many others are not not obvious and I may need help from those who have used the functions before.

One source of useful information may be the version of CTFS that is archived on CRAN, which has relatively good documentation. However, that version contains only 88 functions, versus about 400 that has the complete CTFS R Package.

- Fix errors in functions below; they likely err because they along the way of calling `linear.model`

    - `growth.flexbin()`
    - `run.growthfit.bin()`
    - `run.growthbin.manyspp()`

> "Error in x %*% b : requires numeric/complex matrix/vector arguments". (Seems to be called from linear.model.ctr.) This limits running these other funtions:

I tried `as.matrix(b)` but did not solve the problem, neither other less obvious things that I tried. Because `linear.model()` seems to work, I suspect that the problem is in some function in between the ones listed above and `linear.model()`. Also, these functions fail likely because they involve `linear.model()`:

    - needs the output of `gwoth.flexibin()`
        - `graph.growthmodel.spp()`
        
    - needs the output of `run.growthbin.manyspp()`
        - `compare.growthbinmodel()`
        - `overlay.growthbinmodel()`

- fix `wavelet.allsp()`; it errs with message:

> "Error in dimnames(variance) <- list(names(splitdata), paste("scale", 1:ncol(variance))) : length of 'dimnames' [1] not equal to array extent"

- Avoid `with()` (non-standard evaluation) in:

    - `wavelet.allsp()`, 
    - `NeighborDensities()`,
    - `NDcount()`

- fix `modelBayes()`; it calls debug before R throws error. At first, it erred calling `subset()`, so I replaced `subset()` by `[`, but continues to call debug from inside the function.



### To enhance

- For all suggested packages, use `requireNamespace("package", quietly = TRUE) to test if package is installed, then use `package::fun()` to refer to functions.

- Address one note with multiple components

Before working on it, consider: 

> If you're not submitting to CRAN, carefully read each NOTE, but don’t go out of your way to fix things that you don’t think are problems

--http://r-pkgs.had.co.nz/check.html

```R
checking R code for possible problems ... NOTE

Found an obsolete/platform-specific call in the following functions:
  'compare.growthbinmodel' 'define.graphwindow' 'graph.abundmodel'
  'graph.growthmodel' 'graph.modeldiag' 'graph.outliers' 'imageGraph'
  'image_dataframe' 'map2species' 'maptopo' 'overlay.growthbinmodel'
  'regsum'
  
Found the platform-specific devices:
  'X11' 'quartz' 'win.graph' 'win.metafile' 'x11'
dev.new() is the preferred way to open a new device, in the unlikely
event one is needed.

AGB.dbtable: no visible global function definition for 'odbcConnect'
AGB.dbtable: no visible global function definition for 'odbcClose'
...(about 30 more)

Undefined global functions or variables:
  as.points bci.full1 bci.full2 bci.full3 bci.full6 bci.split6
  bci.spptable bci3.spp composeParam.GaussianMap contour.quaddata
  coords2 ctfs.elev date.mdy dbh decimal.form gen.logistic index
  insideRectange inter khat korup.spp line.intersection mvrnorm
  odbcClose odbcConnect plotspp riwish samplemapfile sp spp20 sqlQuery
  trim.growth.mismeasure wsg.ctfs2

Found the following calls to attach():
File 'ctfs/R/utilities.R':
  attach(file)
```

- Document data accurately. Now, BCI data is available from the _bci_ package, so old references to data should change to new references. For example:

    - the new name of bci.full1 is bci12full1, 
    - of bci.stem4 is bci12stem4, 
    - of bci.spptable is bci12spptable
    - and _bci_ also contains bci_elevation and bci_habitatat

- Add example data. These functions lack a file or data to test the function or run examples

    - `fullplot.imageJ()` 
    -    rearrangeSurveyData()`
    -    `solve.topo()`

- Avoid side effect in `run.growthbin.manyspp()`; it saves object to working directory without warning on the console or description in documentation.

- `graph.abundmodel()` transforms and prints data and plots data, should do 1 thing. Best to plot and return the first argument invisibly.

> Side-effects functions should “invisibly” return the first argument, so that while they’re not printed they can still be used in a pipeline.

--http://r4ds.had.co.nz/functions.html

- Consider improving performance of `model.littleR.Gibbs()` and `fitSeveralAbundModel()`; a test took about 20 minutes to run. If important, I may search the bottlenecks.



- Enhance source code of functions listed below to defensively avoid non standar evaluation (not appropriate for programming) by replacing `subset()` with `[`. They were not fixed yet because, for the reasons listed below, I can't run the functions, and therefore I can't ensure that my fix doesn't brake some other part of the code.

    - function errs:
        - `run.growthbin.manyspp()`
    - function lacks required input:
        - `imageJ.to.lxly()`
    - data is missing to run the function
        - `solve.topo()`
        - `rearrangeSurveyData()`



### To deprecate

These functions should be deprecated because they use `attach()`
- `graph.abundmodel()`: Argument `datafile` passed to `graph.abundmodel()` may be deprecated becauses it uses `attach()` (see _Good practice_ in `?attach`). The function documentation now includes a warning.



### To keep in mind, but disregard for now

- R CMD check throws one additional warning that I disregard for now:

```R
'qpdf' is needed for checks on size reduction of PDFs
```

> I think this error is related to a glitch in R CMD check

https://github.com/krlmlr/r-appveyor/issues/24

> ...a package that has an HTML vignette (but no PDF vignette) failed R CMD check --as-cran ... I think the warning 
originates ... due to a premature check for the existence of qpdf

https://stat.ethz.ch/pipermail/r-devel/2015-October/071917.html






# ctfs 0.0.0.9004

- Document process to build package in `data-raw/src2doc_make_step1.R` and `...step2.R`

- Solve most important warnings during package checks. Most notably, amend NAMESPACE after documenting with roxygen. Because function names use the format function.name, roxygen exports some functions as S3 methods. This is a problem that arises during package checks. Now, this is solved by ammending NAMESPACE, but later versions should replace "." by "_" in function names.

- Combine roxygen documentation and source code into a sigle file

- Import package date, stated as a dependency on original CTFS documentation

- In a vignette, report complexity of ctfs and compare with other complex and simple packages.

- Create and document data object month names that is presumably crucial.

# ctfs 0.0.0.9003

* Tidy up roxygen documentation. Now empty lines span 2 lines or less.

* Accept Rmarkdown in roxygen documentation. This allows lists to display nicely.

* Solve warnings that result from empty descriptions.

# ctfs 0.0.0.9002

* Solve warnings listed in ctfs 0.0.0.9001. Some warnings remain, but to solve those, we need to improve the documentation (not simply reformat available documentation), for example:

```
Warning: found non-ASCII strings

Undocumented arguments in documentation object 'AGB.dbtable'
  'df' 'dbname' 'plot' 'code' 'censusno'

checkRd: (5) bad.binsdpar.Rd:0-12: Must have a \description
```

# ctfs 0.0.0.9001

* Document function names. (Source code was removed to bypass checks that fail and block the package from building). Help files are now available with `?<FUNCTION_NAME>`, `help(<FUNCTION_NAME>)` or online at https://forestgeo.github.io/ctfs/.

* I still need to solve multiple warnings

```
Warning: @param [spatial_RipUvK.R#28]: mismatched braces or quotes
Warning: @author [spatial_block.analysis.R#13]: mismatched braces or quotes
Warning: @examples [spatial_block.analysis.R#69]: mismatched braces or quotes
Warning: @examples [spatial_quadfunc.R#353]: mismatched braces or quotes
Warning: @examples [topography_slope.R#65]: mismatched braces or quotes
Warning: @examples [topography_slope.R#169]: mismatched braces or quotes
Warning: @examples [topography_slope.R#218]: mismatched braces or quotes
Warning: @examples [topography_solvetopo.R#59]: mismatched braces or quotes
Warning: @examples [utilities_distributions.R#72]: mismatched braces or quotes
Warning: @examples [utilities_distributions.R#395]: mismatched braces or quotes
Warning: @examples [utilities_distributions.R#1364]: mismatched braces or quotes
Warning: @description [utilities_geometry.R#218]: mismatched braces or quotes
Warning: @description [utilities_geometry.R#334]: mismatched braces or quotes
Warning: @examples [utilities_lmerBayes.R#193]: mismatched braces or quotes
Warning: @examples [utilities_statistics.R#258]: mismatched braces or quotes
Warning: @examples [utilities_statistics.R#368]: mismatched braces or quotes
Warning: @examples [utilities_statistics.R#538]: mismatched braces or quotes
Warning: @examples [utilities_statistics.R#578]: mismatched braces or quotes
Warning: @examples [utilities_statistics.R#611]: mismatched braces or quotes
Warning: @description [utilities_utilities.R#667]: mismatched braces or quotes
Warning: @examples [utilities_utilitiesCTFS.R#24]: mismatched braces or quotes
Warning: @examples [utilities_utilitiesCTFS.R#96]: mismatched braces or quotes
Warning: utilities_utilities.R:30: Section title spans multiple lines:
```
