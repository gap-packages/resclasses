
                       The ResClasses Package
                       ======================


                              Abstract

This package for GAP 4 (at least version 4.4) implements set-theoretic
unions of residue classes of the ring of integers, its semilocalisations
Z_(pi) at some finite set of primes pi and the polynomial rings GF(q)[x]
for some prime power q as GAP domains and provides basic functionality
for computing with these sets (intersection, union, difference etc.,
any of these also when one of the operands is a finite set of elements).
It also implements the above-mentioned rings Z_(pi) as GAP domains.


                            Installation

Like any other GAP package, ResClasses must be installed in the pkg/
subdirectory of the GAP distribution. This is accomplished by extracting the
distribution file in this directory. By default, `ResClasses' is autoloaded,
otherwise you can load the package via LoadPackage( "resclasses" );

Then you can build the manual by issueing ResClassesBuildManual( );
(this works only under UNIX, but should not be necessary unless you
got the package from CVS, since the distribution file already
contains all files produced by this function).

The ResClasses Package needs at least version 4.4 of GAP, is completely
written in the GAP language and does neither contain nor require
external binaries.

A recent version of the package `GAPDoc' is needed.

For further advice on questions of technical nature please see the
chapter `Auxiliary functions' in the manual.

                                 ---

If you have problems with this package, wish to make comments
or suggestions, or if you find bugs, please send e-mail to

Stefan Kohl, kohl@mathematik.uni-stuttgart.de

