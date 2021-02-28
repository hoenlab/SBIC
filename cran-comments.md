## Resubmission  
This is a resubmission. In this version I have:  
* Re-ran all checks and incremented the version to 1.0.0  
* Reduced the title length to 62 characters (Under the 65 character limit)  
* Added `\donttest{}` in place of `dontrun{}` in places where examples take more than 5 seconds  
* Made sure to change `par` options in vignettes back to user defaults  

## Test environments
* local Windows 10 install, R 4.0.4 
* Ubuntu 18.04 LTS (on Github Actions) (devel, release, oldrel, R 3.5, R 3.4, R 3.3)  
* MacOS Latest (on Github Actions) (devel, release)  
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.  

There were 2 NOTES:
> checking for future file timestamps ... NOTE
  unable to verify current time 
  
  This is due to a known bug with R CMD BUILD on local Windows installation.  
  This error does not show up when run on Travis-CI and CRAN win-builder.  
  This error can be solved by setting environment variable `_R_CHECK_SYSTEM_CLOCK_` to zero.  

> Possibly mis-spelled words in DESCRIPTION:
  Zhou (22:99)
  
  This is a co-author's real name. 

## Downstream dependencies
There were no errors in downstream dependencies 