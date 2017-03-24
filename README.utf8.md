---
title: "ctfs"
author: "[Richard Condit](conditr@gmail.com)"
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



ctfs provides the original CTFS R Package (Richard Condit) with the structure of an R package _sensu stricto_ (with some hacks). After you install it, functions will be available with `<FUNCTION_NAME>`; and documentation with `?<FUNCTION_NAME>`, `help(<FUNCTION_NAME>)` or online at https://forestgeo.github.io/ctfs/ or http://ctfs.si.edu/Public/CTFSRPackage/.

## Installation

If you are authorized (via [Stuart Davies](daviess@si.edu)), you can install ctfs from a private GitHub repo with:

```R
# install.packages("devtools")
GUEST_PAT <- "d60277dad4177c278455e885366a7170e22ae092"
devtools::install_github("forestgeo/ctfs", auth_token = GUEST_PAT)
library(ctfs)
```

### Reference

```
@article{anderson2015ctfs,
  title={CTFS-ForestGEO: a worldwide network monitoring forests in an era of global change},
  author={Anderson-Teixeira, Kristina J and Davies, Stuart J and Bennett, Amy C and Gonzalez-Akre, Erika B and Muller-Landau, Helene C and Joseph Wright, S and Abu Salim, Kamariah and Almeyda Zambrano, Ang{\'e}lica M and Alonso, Alfonso and Baltzer, Jennifer L and others},
  journal={Global Change Biology},
  volume={21},
  number={2},
  pages={528--549},
  year={2015},
  publisher={Wiley Online Library}
}
```

Package maintainer: [Mauro Lepore](maurolepore@gmail.com)