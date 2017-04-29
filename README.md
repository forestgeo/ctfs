ctfs
================
CTFS-ForestGEO

<!-- README.md is generated from README.Rmd. Please edit that file -->
This package provides tools for the analysis of forest dynamics. It evolves from the source code of the CTFS R Package by Richard Condit, available at <http://ctfs.si.edu/Public/CTFSRPackage/>.

Installation
------------

If you are authorized (via [Stuart Davies](daviess@si.edu)), you can install ctfs from ForestGEO's private GitHub repo.

    # From ?devtools::install_github:
    # To install from a private repo, use auth_token with a token
    # from https://github.com/settings/tokens. You only need the
    # repo scope. Best practice is to save your PAT in env var called
    # GITHUB_PAT.

    # install.packages("devtools")
    devtools::install_github("forestgeo/ctfs", auth_token = "<YOUR_TOKEN>")
    library(ctfs)

### Reference

    @article{anderson2015ctfs,
      title={CTFS-ForestGEO: a worldwide network monitoring forests in an era of global change},
      author={Anderson-Teixeira, Kristina J and Davies, Stuart J and Bennett, Amy C and Gonzalez-Akre, Erika B and Muller-Landau, Helene C and Joseph Wright, S and Abu Salim, Kamariah and Almeyda Zambrano, Angelica M and Alonso, Alfonso and Baltzer, Jennifer L and others},
      journal={Global Change Biology},
      volume={21},
      number={2},
      pages={528--549},
      year={2015},
      publisher={Wiley Online Library}
    }

Package maintainer: [Mauro Lepore](maurolepore@gmail.com)
