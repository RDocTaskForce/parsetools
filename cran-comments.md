## Test environments
* local windows install, R 3.7.0 (Devel)
* local windows install, R 3.6.0
* ubuntu 14.04 (on travis-ci), R 3.6.0
* win-builder (devel and release)
* r-hub builder

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Notes

This should fix the bug related to the assertError change in R-3.7.0 
which resulted in parsetools being removed from CRAN.
There are also packages that are dependent on parsetools that I would 
request be restored now that parsetools is fixed.
