#############################################################################
##
#W  z_pi.gd               GAP4 Package `ResClasses'               Stefan Kohl
##
##  This file contains the declaration part for the semilocalizations Z_(pi)
##  of the ring of integers.
##
#############################################################################

#############################################################################
##
#O  Z_piCons( <pi> ) . . . . . . . semilocalization Z_(pi) for prime set <pi>
#F  Z_pi( <pi> )
#P  IsZ_pi( <R> )
##
DeclareConstructor( "Z_piCons", [ IsRing, IsList ] );
DeclareGlobalFunction( "Z_pi" );
DeclareCategory( "IsZ_pi", IsEuclideanRing and IsCyclotomicCollection );

#############################################################################
##
#F  NoninvertiblePrimes( <R> ) . . the set of non-inv. primes in the ring <R>
##
DeclareAttribute( "NoninvertiblePrimes", IsRing );

#############################################################################
##
#E  z_pi.gd . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here