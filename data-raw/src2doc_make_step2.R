# STEP 2: AFTER src2doc_make_step1.R do this:

# - document and export functions via NAMESPACE, e.g. by clicking Build & Reload;

# - Ammend namespace for functions to export as functions, not as S3 methods;
source("./data-raw/src2doc_namespace_amend.R")

# - check package and remove errors and minimize warnings and notes;

# - build package.
