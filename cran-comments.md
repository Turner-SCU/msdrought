## Submission of revised version: v 0.1.0 - May 21, 2024

* This is a revised version for a new package that contains the requested fixes after the first submission attempt. Changes included the following:
    - Revised DESCRIPTION to avoid starting with "This package"
    - Added single quotes around software names where they appeared
    - Removed code lines that had been commented out
    - Added short examples in Rd files
              - msdFilter.Rd
              - msdGraph.Rd
              - msdMain.Rd
              - msdStats.Rd
    - Replaced print() with message() or stop() to allow user control of messages
--------------------------------------------------------------------------------
**From remote Windows check - devtools::check_win_devel()

* using log directory 'd:/RCompile/CRANguest/R-devel/msdrought.Rcheck'
* using R Under development (unstable) (2024-05-20 r86569 ucrt)
* using platform: x86_64-w64-mingw32
* R was compiled by
    gcc.exe (GCC) 13.2.0
    GNU Fortran (GCC) 13.2.0
* running under: Windows Server 2022 x64 (build 20348)
* using session charset: UTF-8
* checking for file 'msdrought/DESCRIPTION' ... OK
* this is package 'msdrought' version '0.1.0'
* package encoding: UTF-8
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Ed Maurer <emaurer@scu.edu>'

New submission

Possibly misspelled words in DESCRIPTION:
  MSD (10:56, 11:27)
  Maurer (14:44)
  al (14:54)
  et (14:51)
* ...
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... [10s] OK
* checking PDF version of manual ... [13s] OK
* checking HTML version of manual ... OK
* DONE
Status: 1 NOTE

--------------------------------------------------------------------------------
** From remote MacOS check - devtools::check_mac_release()

* using log directory ‘/Volumes/PkgBuild/work/1716257047-96630b797c7d168a/packages/big-sur-arm64/results/4.4/msdrought.Rcheck’
* using R version 4.4.0 (2024-04-24)
* using platform: aarch64-apple-darwin20
* R was compiled by
    Apple clang version 14.0.0 (clang-1400.0.29.202)
    GNU Fortran (GCC) 12.2.0
* running under: macOS Ventura 13.3.1
* using session charset: UTF-8
* checking for file ‘msdrought/DESCRIPTION’ ... OK
* this is package ‘msdrought’ version ‘0.1.0’
* package encoding: UTF-8
* ...
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... [14s/22s] OK
* checking PDF version of manual ... [2s/2s] OK
* DONE
Status: OK
* using check arguments '--no-clean-on-error '

* elapsed time (check, wall clock): 0:36
