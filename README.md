
<!-- README.md is generated from README.Rmd. Please edit that file -->
ctfs
====

**ctfs** is almost identical to the CTFS R Package (<http://ctfs.si.edu/Public/CTFSRPackage/>; see section **Differences between ctfs and the CTFS R package**).

The CTFS R Package includes a wide variety of R functions aimed at analysis of data from CTFS forest census plots. Most of the functions use the CTFS R Analytical tables, which are tables of plot data organizing tree and stem measurements in a precise format. The Package also includes many supporting functions that carry out basic, oft-used calculations, such as date conversion, geometry, model-fitting, probability distributions, and rearrangement of R data objects.

### Installation

    # install.packages("devtools")
    devtools::install_github("forestgeo/ctfs")

### Maintenance

**ctfs** will not be maintained. Therefore, **ctfs** is released and at the same time deprecated.

### Differences between ctfs and the CTFS R package

-   Function names: replaced "." by "\_" in the names of some functions to overcome conflicts with R's S3 system (<http://adv-r.had.co.nz/OO-essentials.html#s3>).

-   Documentation: edited some documentation to make it clearer, or added documentation that was missing (e.g. some arguments where documented in some but not all functions that used those arguments).

-   **ctfs** is an R package **sensu stricto**, but not the CTFS R Package.

    -   To install **ctfs** see the section **Installation** and to learn how to use it see help files as you would normally do for any R package, i.e. with `help(FUNCTION)` or `?FUNCTION`. Alternatively, go to <https://forestgeo.github.io/ctfs/>.

    -   To install the CTFS R Package and to learn how to use it, go to <http://ctfs.si.edu/Public/CTFSRPackage/>.

### Acknowledgments

The CTFS R Package was developed with the support of NSF grant \#1046113 to Stuart J. Davies through the NSF-IRCN program on the Dimensions of Biodiversity.
