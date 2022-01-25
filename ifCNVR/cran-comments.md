## Test environments
* local R installation, R 4.0.3
* ubuntu 16.04 (on travis-ci), R 4.0.3
* win-builder (devel)

## R CMD check results

Duration: 29.7s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

R CMD check succeeded


## Comments by Julia Haider 

Please always explain all acronyms in the description text. 

Please always write package names, software names and API (application 
programming interface) names, etc. in single quotes in title and 
description. e.g: 'Python', 'ifCNV'

If there are references describing the methods in your package, please 
add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for 
auto-linking.
(If you want to add a title as well please put it in quotes: "Title")

You write information messages to the console that cannot be easily 
suppressed. It is more R like to generate objects that can be used to 
extract the information a user is interested in, and then print() that 
object. Instead of print()/cat() rather use message()/warning() or 
if(verbose)cat(..) (or maybe stop()) if you really have to write text to 
the console. (except for print, summary, interactive functions)

You are setting options(warn=-1) in your function. This is not allowed.
To avoid unnecessary warning output you could use e.g. suppressWarnings().

Please fix and resubmit.

* This is a new release.
