## An updated version
This submission is an updated version to our previous package
stablespec 0.1.3 with the following detail.

* Added a new function dataReshape() to reshape longitudinal data.
* Added a new argument "mixture"" into function stableSpec() which is a 
feature to handle data with both continuous and discrete variables.
* Added new features in output graph of stableSpec();
annotation and node names (correspond to variable names).
* Rewrote the Description to make it more representative and
the English better.
* Fixed a bug when setting argument consMatrix in function stableSpec()
with NULL.
* data sets are update; adding crossdata6V and longiData4V3T and
removing adhd and artificialLongiData (no longer relevant)

## Test environments

* local Windows 10 install, R 3.3.2
* win-builder (devel)
* local OS X Sierra install, R 3.3.2
* local Ubuntu 14.04.5 LTS install, R. 3.2.1

## R CMD check results

There were no ERRORs or WARNINGs.

There was one NOTE:

Maintainer: 'Ridho Rahmadi <r.rahmadi@cs.ru.nl>'

License components with restrictions and base license permitting such:
  MIT + file LICENSE
File 'LICENSE':
  YEAR: 2016
  COPYRIGHT HOLDER: Ridho Rahmadi

Possibly mis-spelled words in DESCRIPTION:
  NSGA (16:5)
  Optimality (13:61)
  multi (15:29)
  subsample (12:59)

Regarding the possible mis-spelled words, 
NSGA is a correct abbreviation of Non-dominated Sorting 
Genetic Algorithm, and subsample is also a
correct word according to the Oxford dictionary.

## Downstream dependencies

There are currently no downstream dependencies for this package.
