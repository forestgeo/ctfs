# ctfs 0.0.0.9005

- Fixed one problem in `wavelet.univariate()`, which erroneously refered to `wavelet.univariate()` as `wavelet.var()`. Also fixed an erroneous reference to `wavelet.var()` in examples of `wavelet.univariate()`.

- Fixed bug in `model.littleR.Gibbs()`. In a code chunk, lowercase names of the data set passed to the argument sptable because the variable `idlevel` was referred to with inconsistent case. After that chunk the original names were recovered to avoid potential problems downstream.

- Fixed bugs in functions listed below, by replacing `subset()` by `[` in `map()` (notice that `[` is a function that subsets using standard evaluation rules but `subset()` uses non-standard evaluation and therefore `subset()` should [not be used inside functions](http://adv-r.had.co.nz/Computing-on-the-language.html).

    - `pdf.allplot()`
    - `png.allplot()`

> "Error in subset.default(sppdata, gx >= 0 & gy >= 0 & gx < plotdim[1] &  : object 'gx' not found"

Fixes with commit bc417c38; example that functions work: ebe8a601.



- Fixed bugs in functions listed below, they needed functions in packages

    - _mvtnorm_ and _MCMCpack_

        - `lmerBayes()`,
        - `llike.model.lmer()` (needed function `dmvnorm()`)
    
    - _date_

        - `mortality.eachspp()`

- Enhanced source code of functions listed below to defensively avoid non standar evaluation (not appropriate for programming). Replaced subset by "[" in:

    - `model.littleR.Gibbs()`,
    - `density.ind()`,
    - `abund.manycensus()`
    - `individual_grow.table()`
    - ... (14 more to go).

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

**Known issues**

I tested all functions used in tutorials and out of a total of 28 functions, 13 failed to run. In addition to errors, I detected a number of other problems, described next.

BUILD PACKAGE

To build the package NAMESPACE must be amended first. The reson is that NAMESPACE treats some functions as methods. Those problematic functions contain dots "." in their names (e.g. split.data and density.ind). To avoid this problem, the format <FUNCTION.NAME> should eventually change to <FUNCTION_NAME>.

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

ENHANCEMENTS

Next I suggest some enhances in the order I think it makes sense to address them:

UNSAFE

- Subset is OK for interactive use but unreliable in functions becaue it uses non-standard evaluation and lacks a hatch. Wherever posible, it should be replaced by "[". ~15 functions use subset. (To find functions that use `subset()` use Edit/Find in Files.)

> ...While subset() saves typing, it’s actually difficult to use non-interactively.

-- from **Calling from another function** at http://adv-r.had.co.nz/Computing-on-the-language.html

INACURATE DOCUMENTATION

- Documentation refers to BCI data in a way that is no longer accurate. For example, in the _bci_ package 

    - the new name of bci.full1 is bci12full1, 
    - of bci.stem4 is bci12stem4, 
    - of bci.spptable is bci12spptable
    - and _bci_ also contains bci_elevation and bci_habitatat


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
