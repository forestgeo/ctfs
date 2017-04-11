# STEP 2: AFTER src2doc_make_step1.R do this:

# - document and export functions via NAMESPACE, e.g. by clicking Build & Reload;
# - Ammend namespace for functions to export as functions, not as S3 methods;
source("./data-raw/src2doc_namespace_amend.R")
# - check package (if necessary, remove errors and restart this process);
# - build package (click build source package);
# - build site (click Addins build pkgdown);
# - push;
# - try install branch;
# - merge with master and push;
# - try install.

