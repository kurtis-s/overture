## Test environments
* local OS X install, x86_64-apple-darwin15.6.0 (64-bit); R 3.5.2
* ubuntu 14.04.5 LTS on travis CI; R version 3.5.2 (2017-01-27), R Under development (unstable) (2019-01-14 r75992) 
* win-builder; R version 3.5.2 (2018-12-20), R Under development (unstable) (2019-01-14 r75992)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Kurtis Shuler <kurtis.s.1122+CRAN@gmail.com>’

New submission

## R CMD check results with --run-donttest
Ran on local install.  There were no ERRORs or WARNINGs.

There were 2 NOTEs:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Kurtis Shuler <kurtis.s.1122+CRAN@gmail.com>’

New submission

* checking examples ... NOTE
Examples with CPU or elapsed time > 5s
      user system elapsed
Amwg 7.240  0.039   7.296
Peek 0.454  0.042 103.755

## Comments from previous submission
"please replace \dontrun{} by \donttest{} or unwap the examples if they can be executed in less than 5 sec per Rd-file."
* Action taken: Changed \dontrun{} to \donttest{} in the example for the Amwg method.  Modified the example for the Peek method so that it would be compatible with \donttest{}.

"If there are references describing the methods in your package, please add these in the Description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
with no space after 'doi:', 'arXiv:' and angle brackets for auto-linking."
* Action taken: Added the reference for the Amwg method to the DESCRIPTION file.
