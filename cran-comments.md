## Test environments
* local Windows 10 install, R 4.0.2 
* Ubuntu 16.04.6 LTS (on travis-ci), R 4.0.2  
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.  

There was 1 NOTE:
> checking for future file timestamps ... NOTE
  unable to verify current time 
  
  This is due to a known bug with R CMD BUILD on local Windows installation.  
  This error does not show up when run on Travis-CI and CRAN win-builder.  
  This error can be solved by setting environment variable `_R_CHECK_SYSTEM_CLOCK_` to zero.  

## Downstream dependencies
There were no errors in downstream dependencies 