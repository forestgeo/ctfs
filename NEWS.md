# ctfs 0.0.0.9005


## R CMD check results

```r
0 errors | 2 warnings | 3 notes

R CMD check succeeded
```

WARNING 1

```R
checking DESCRIPTION meta-information ... WARNING
Non-standard license specification:
  What license is it under?
Standardizable: FALSE
```

Easy to fix, we just need to choose a license



WARNING 2

```R
checking Rd \usage sections ... WARNING
Undocumented arguments in documentation object 'AGB.dbtable'
  'df' 'dbname' 'plot' 'code' 'censusno'

... 1153 lines ...
```

Can take long to fix, there are about 1150 undocumented arguments. Some may be easy to define because they may be defined somewhere else or because they may be obvious. But I expect that some other arguments I won't be able to define without some help from those who have used the functions before. In any case, the version of CTFS that is archived on CRAN has relatively good documentation that could be used as a model. However, that version has only 88 functions versus about 400 that has the complete CTFS R Package.



NOTE 1

```R
checking top-level files ... NOTE
Non-standard file/directory found at top level:
  'data-raw'
```

This is not a problem. It is easy to justify because it follows modern best practices.



NOTE 2

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

... 75 lines ...
```


```R
Found the following calls to attach():
File 'ctfs/R/utilities.R':
  attach(file)
See section 'Good practice' in '?attach'.
```

Easy to fix. There is only one function, `attach_if_needed()`, that contains `attach()` in its source. It is a convenience function that could be removed. It is called only in two other functions:

- `graph.abundmodel()`
- `CTFSplot()`

To access the data, a good alternative to `attach_if_needed` is to load data with `bci::<DATA>`.



## Another check 

```R
Status: 3 WARNINGs, 3 NOTEs
```

Another type of check throws this additional warning:

```R
'qpdf' is needed for checks on size reduction of PDFs
```


































- Try replacing all these weird devices by `dev.new()` as suggested in the message. But first do a little google search.

```R
Found the platform-specific devices:
  'X11' 'quartz' 'win.graph' 'win.metafile' 'x11'
dev.new() is the preferred way to open a new device, in the unlikely
event one is needed.
```

- It seems that those devices are used in all the functions below, which warning may dissapear once they are replaced by dev.new. (so check if the functions below use the weird devices above).

```R
Found an obsolete/platform-specific call in the following functions:
  'compare.growthbinmodel' 'define.graphwindow' 'graph.abundmodel'
  'graph.growthmodel' 'graph.modeldiag' 'graph.outliers' 'imageGraph'
  'image_dataframe' 'map2species' 'maptopo' 'overlay.growthbinmodel'
  'regsum' 'wavelet.bivariate'
```














NOTES

checking Rd line widths ... NOTE

- Rd file 'RipUvK.Rd':
- Rd file 'spparea.sq.Rd':
- Rd file 'abund.manycensus.Rd':
- ... 44 lines ...

lines wider than 100 characters: These lines will be truncated in the PDF manual.



MORE TO IMPROVE: learn from the CTFS-CRAN version

- Incorporate the documentation of the 88 functions from CTFS-CRAN.

- Are those 88 functions the most important ones? The ones that do not depend on more stuff than that?





DONE

- Pushed to private repo on forestgeo the CTFS version archived on CRAN. Consider using the archived version of CTFS as a source for missing documentation and data.

- I Made CTFS-ForestGEO the author of the package and removed Richard Condit



FUNCTIONS INTERPRETED AS S3 METHODS

Some functions were treated as S3 methods, and automatically changed NAMESPACE in a problematic way. Those functions caused these problems:

- In the section "Usage" of the documentation of each problematic function:

    - a lable like this printed: \#\# S3 method for classs '<NAME>';
    - the `<FUNCTION.NAME>` appeared as `<FUNCTION>`, withouth `<NAME>`.

- Automatic package checks throwed scary messages and the package failed to build.

To permanently fix this problem, I replaced dots "." by "_" in the names of those problematic functions. The change should be easy to digest by users. The nuew names of those functions shows up with RStudio's autocompletion.

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

- @importFrom magrittr %>% ([example](https://github.com/tidyverse/dplyr/edit/master/R/utils.r))

- New vignette to test plots output remain the same after fixes to plotting functions. Follows advice in _Testing R Code_, by Richard Cotton.

- New utility functions, not-exported, to remove rows full of NAs from data frames and matrices.

- Fixed one problem in `wavelet.univariate()`, which erroneously refered to `wavelet.univariate()` as `wavelet.var()`. Also fixed an erroneous reference to `wavelet.var()` in examples of `wavelet.univariate()`.

- Fixed bug in `model.littleR.Gibbs()`. In a code chunk, lowercase names of the data set passed to the argument sptable because the variable `idlevel` was referred to with inconsistent case. After that chunk the original names were recovered to avoid potential problems downstream.

- Fixed bugs in functions listed below, by replacing `subset()` by `[` in `map()` (notice that `[` is a function that subsets using standard evaluation rules but `subset()` uses non-standard evaluation and therefore `subset()` should [not be used inside functions](http://adv-r.had.co.nz/Computing-on-the-language.html).

    - `pdf.allplot()`
    - `png.allplot()`

> "Error in subset.default(sppdata, gx >= 0 & gy >= 0 & gx < plotdim[1] &  : object 'gx' not found"

Fixes with commit bc417c38; example that functions work: ebe8a601.

- Refer to functions from external packages explicitly with pkg::fun()

> ...this is what I recommend: list the package in DESCRIPTION so that it’s installed, then always refer to it explicitly with pkg::fun(). Unless there is a strong reason not to, it’s better to be explicit

-- R packages, by Hadley Wickham (http://r-pkgs.had.co.nz/namespace.html)

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

- Enhanced source code of functions listed below to defensively avoid non standar evaluation (not appropriate for programming). Replaced subset by "[" in:

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
    - Pendent:
        - Impossible to test because functions err:
            - `run.growthbin.manyspp()`
        - Impossible to test because functions lacks required input:
            - `imageJ.to.lxly()`
        - Impossible to test because data is missing to run the function
            - `solve.topo()`
            - `rearrangeSurveyData()`

- Enhanced documentation of functions listed below by introducing `?wsgdata_dummy()`, a function to create dummy wood density tables.
    
    - `biomass.CTFSdb()`
    - `density.ind()`

- In `individual_grow.table()`

    - enhanced documentation: @return, @param rnd, @param cnsdata
    - enhanced source code: remove default of data argument cnsdata

- Suggest package bci, doesn't import it because bci is in a private repo, so the user needs to provide a private token.

> Unless there is a good reason otherwise, you should always list packages in Imports not Depends. That’s because a good package is self-contained, and minimises changes to the global environment (including the search path).

-- http://r-pkgs.had.co.nz/namespace.html

- Use date::<DATE_FUNCTION> where appropriate.

- Wrote a vignette to report package quality

- Fixed `wavelet.bivariate()`, replace `as.real` (defunc) by `as.double`. Detect missing argument type and wrote a warning in the help file.
















**Known issues**

I tested all functions used in tutorials and out of a total of 28 functions, 13 failed to run. In addition to errors, I detected a number of other problems, described next.

ERRORS

Here is a list of problems and functions that share those problems.

- Err likely because the all call `linear.model`

    - `growth.flexbin()`
    - `run.growthfit.bin()`
    - `run.growthbin.manyspp()`

> "Error in x %*% b : requires numeric/complex matrix/vector arguments". (Seems to be called from linear.model.ctr.) This limits running these other funtions:

These did not solve the problem: `as.matrix(b)`, nor other less obvious things that I tried. Because `linear.model()` seems to work, I suspect that the problem is in some function in between the ones listed above and `linear.model()`.

Related to the problem above are the problems below. These functions fail apparently because some functions that use `linear.model()` fail before:

    - needs the output of `gwoth.flexibin()`
        - `graph.growthmodel.spp()`
        
    - needs the output of `run.growthbin.manyspp()`
        - `compare.growthbinmodel()`
        - `overlay.growthbinmodel()`


- `wavelet.allsp()` errs with message:

> "Error in dimnames(variance) <- list(names(splitdata), paste("scale", 1:ncol(variance))) : length of 'dimnames' [1] not equal to array extent"

Also, `wavelet.allsp()` uses `with()`, which may cause problems because it uses non-standard evaluation.

- `modelBayes()` Calls debug before R throws error. At first, it erred calling `subset()`, so I replaced `subset()` by `[` but continues to call debug from inside the function.



ENHANCEMENTS

Some functions still use `subset()`, generally because those functions fail, so any additional bug my fix may introduce would go unnoticed. The plan is to fix the functions first, then replace `subset()` by `[`.

INACURATE DOCUMENTATION

- Documentation refers to BCI data in a way that is no longer accurate. For example, in the _bci_ package 

    - the new name of bci.full1 is bci12full1, 
    - of bci.stem4 is bci12stem4, 
    - of bci.spptable is bci12spptable
    - and _bci_ also contains bci_elevation and bci_habitatat

- In `wavelet.bivariate()`, argument type is missing from function definition. A warning was added to the help file.



LACKS EXAMPLE FILE OR DATA

- Lacks (dummy) file or data to test the function or run examples

    - `fullplot.imageJ()` 
    -    rearrangeSurveyData()`
    -    `solve.topo()`

SIDE EFFECTS

- `run.growthbin.manyspp()` saves object to working directory without warning on the console or description in documentation.

- graph.abundmodel transforms and prints data and plots data, should do 1 thing. Best to plot and return the first argument invisibly.

SLOW

- Functions listed below are slow. A test took about 20 minutes to run. If important, I may search the bottlenecks.

    - model.littleR.Gibbs
    - fitSeveralAbundModel

TO DEPRECATE

- CTFSplot is no longer necessary because bci data is now available via the _bci_ package.

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
