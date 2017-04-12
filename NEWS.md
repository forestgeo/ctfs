# ctfs 0.0.0.9005

> How about working with the growth, mortality and recruitment functions (Demography functions)? They are already in the R Package, but need work. Use the BCI data to test and build the various documents.

--Stuart

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
