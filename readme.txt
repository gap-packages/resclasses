
                         The ResClasses Package
                         ======================


                                Abstract

ResClasses is a GAP 4 package which implements set-theoretic unions of
residue classes of the ring of integers, its semilocalisations Z_(pi)
at some finite set of primes pi and the polynomial rings GF(q)[x] for
some prime power q as GAP domains. It provides basic functionality for
computing with these sets: intersection, union, difference etc., any
of these also when one of the operands is a finite set of elements.
It also implements the above-mentioned rings Z_(pi) as GAP domains.


                              Requirements

The ResClasses Package needs at least version 4.4 of GAP, is completely
written in the GAP language and does neither contain nor require
external binaries. A recent version of the package GAPDoc is needed.


                              Installation

Like any other GAP package, ResClasses must be installed in the pkg/
subdirectory of the GAP distribution. This is accomplished by extracting
the distribution file in this directory. By default, ResClasses is
autoloaded. This means that it is loaded automatically when you start GAP.

If you got the package from CVS, you have to build the manual by issueing
ResClassesBuildManual( ); then. Otherwise this is not necessary since the
distribution file already contains all files produced by this function.

For further advice on questions of technical nature please see the
chapter `Auxiliary functions' in the manual.

                                   ---

If you have problems with this package, wish to make comments or
suggestions, or if you find bugs, please send e-mail to

Stefan Kohl, kohl@mathematik.uni-stuttgart.de

