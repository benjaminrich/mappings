# Version 0.1

This is the first submission of `mappings` to CRAN.

Addressed the following comments from CRAN reviewer:

* From: Julia Haider <julia.haider@wu.ac.at> on 21-Jun-2021:

  - Please add \value to .Rd files regarding exported methods and explain the
    functions results in the documentation. Please write about the structure of
    the output (class) and also what the output means. (If a function does not
    return a value, please document that too, e.g. \value{No return value, called
    for side effects} or similar)
    
    Missing Rd-tags:
    * domain.Rd: \value
    * text2mapping.Rd: \value

    _I fixed this, and while I was at it I updated my roxygen comments._

## Test environments

* Local:
  - Windows 10, R 4.1.0 (x86_64-w64-mingw32/x64 (64-bit))
  - Ubuntu Linux 20.04.2 LTS, R 4.1.0 (x86_64-pc-linux-gnu)
* win-builder:
  - R Under development (unstable) (2021-06-16 r80504):
    - 1 NOTE:
      * New submission
* R-hub builder (https://builder.r-hub.io)
  - Windows Server 2008 R2 SP1, R-devel, 32/64 bit
    - Produced an error unrelated to my package:
      * Error: Bioconductor does not yet build and check packages for R version 4.2
  - Ubuntu Linux 20.04.1 LTS, R-release, GCC
    - 1 NOTE:
      * New submission
  - Fedora Linux, R-devel, clang, gfortran
    - 1 NOTE:
      * New submission

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are currently no downstream dependencies for this package.

