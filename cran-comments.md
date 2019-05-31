## Test environments
* local OS X (x86_64-apple-darwin15.6.0 (64-bit)), R 3.6.0
* Fedora Linux, R-devel, clang, gfortran (R-hub)
* win-builder

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE (from Fedora Linux and win-builder):

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Katherine Goode <kgoode@iastate.edu>’

New submission

Possibly mis-spelled words in DESCRIPTION:
  ggplot (9:11)
  plotly (9:23)

I am not concerned about this since the words that it believes are mis-spelled are referring to R packages.

## Downstream dependencies
There are currently no downstream dependencies for this package