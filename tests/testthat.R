opar <- options(keep.source=TRUE, keep.source.pkgs=TRUE)
library(testthat)
test_check("parsetools")
options(opar)
