README file for `ResClasses' package.

This package for GAP 4 (at least version 4.3) implements
unions of residue classes of the integers, the polynomial rings
GF(q)[x] for some prime power q and of the semilocalisations Z_pi
of the integers at some finite set of primes pi as GAP domains
and provides basic functionality for computing with these sets
(intersection, union, difference, adding and subtracting finite
sets of elements, etc.). It also implements the above-mentioned
rings Z_pi as GAP domains.

The package is completely written in the GAP language and contains /
requires no external binaries.

A recent version of the package `GAPDoc' is needed.

The package `ResClasses' must be installed in the pkg/ - subdirectory
of the GAP distribution.

After extracting the distribution file in the proper place,
you can load the package via LoadPackage( "resclasses" );

Then you can build the manual by issueing BuildResClassesManual( );
(this works only under UNIX, but should not be necessary
unless you got the package from CVS, since the distribution file
already contains all files produced by this function).

For further advice on questions of technical nature please see
the chapter `Auxiliary functions' in the manual.

If you have problems with this package, wish to make comments
or suggestions, or if you find bugs, please send e-mail to

Stefan Kohl, kohl@mathematik.uni-stuttgart.de

