#############################################################################
##
#W  fixedrep.gd             GAP4 Package `ResClasses'             Stefan Kohl
##
#H  @(#)$Id$
##
##  This file contains declarations needed for computing with unions of
##  residue classes with distinguished ("fixed") representatives.
##
Revision.fixedrep_gd :=
  "@(#)$Id$";

#############################################################################
##
#C  IsUnionOfResidueClassesWithFixedRepresentatives
##
##  The category of all unions of residue classes which are endowed with
##  distinguished ('fixed') representatives.
##
DeclareCategory( "IsUnionOfResidueClassesWithFixedRepresentatives",
                 IsDomain and IsListOrCollection );

#############################################################################
##
#C  IsUnionOfResidueClassesOfZWithFixedRepresentatives
#C  IsUnionOfResidueClassesOfZ_piWithFixedRepresentatives
#C  IsUnionOfResidueClassesOfGFqxWithFixedRepresentatives
#C  IsUnionOfResidueClassesOfZorZ_piWithFixedRepresentatives
##
##  The category of the unions of residue classes with fixed representatives
##  of Z, Z_(pi) or GF(q)[x], respectively. The last-mentioned category
##  is the union of the first-mentioned two categories.
##
DeclareCategory( "IsUnionOfResidueClassesOfZWithFixedRepresentatives",
                 IsDomain and IsListOrCollection );
DeclareCategory( "IsUnionOfResidueClassesOfZ_piWithFixedRepresentatives",
                 IsDomain and IsListOrCollection );
DeclareCategory( "IsUnionOfResidueClassesOfGFqxWithFixedRepresentatives",
                 IsDomain and IsListOrCollection );
DeclareCategory( "IsUnionOfResidueClassesOfZorZ_piWithFixedRepresentatives",
                 IsDomain and IsListOrCollection );

#############################################################################
##
#R  IsUnionOfResidueClassesWithFixedRepsStandardRep
##
##  Representation of unions of residue classes of the integers, a
##  semilocalization Z_(pi) of the integers or a univariate polynomial
##  ring GF(q)[x] over a finite field, with fixed representatives.
## 
##  The component <classes> is a list of residue classes, given as pairs
##  ( <modulus>, <representative> ). The representatives are *not* reduced
##  modulo the respective moduli, and the moduli may be different --
##  in contrast to `IsResidueClassUnionResidueListRep' no common modulus is
##  stored, and *unions which are equal as sets are distinguished* if their
##  respective <classes> - components are distinct.
##
DeclareRepresentation( "IsUnionOfResidueClassesWithFixedRepsStandardRep",
                       IsComponentObjectRep and IsAttributeStoringRep, 
                       [ "classes" ] );

#############################################################################
##
#O  UnionOfResidueClassesWithFixedRepresentativesCons(<filter>,<R>,<classes>)
#F  UnionOfResidueClassesWithFixedRepresentatives( <R>, <classes> )
#F  UnionOfResidueClassesWithFixedRepresentatives( <classes> )
#F  UnionOfResidueClassesWithFixedReps( <R>, <classes> )
#F  UnionOfResidueClassesWithFixedReps( <classes> )
##
##  Constructor for unions of residue classes with fixed representatives.
##
##  Constructs the union of the residue classes
##
##   <classes>[i][2] ( mod <classes>[i][1] )
##
##  of the ring <R>.
##
DeclareConstructor( "UnionOfResidueClassesWithFixedRepresentativesCons",
                    [ IsUnionOfResidueClassesWithFixedRepresentatives,
                      IsRing, IsList ] );
DeclareGlobalFunction( "UnionOfResidueClassesWithFixedRepresentatives" );
DeclareSynonym( "ResidueClassUnionWithFixedRepresentatives",
                UnionOfResidueClassesWithFixedRepresentatives );
DeclareSynonym( "UnionOfResidueClassesWithFixedReps",
                UnionOfResidueClassesWithFixedRepresentatives );

#############################################################################
##
#F  ResidueClassWithFixedRepresentative( <R>, <m>, <r> )  same with fixed rep
#F  ResidueClassWithFixedRepresentative( <m>, <r> )
#F  ResidueClassWithFixedRep( <R>, <m>, <r> )
#F  ResidueClassWithFixedRep( <m>, <r> )
##
##  The residue class <r> ( mod <m> ) of the ring <R>, with the fixed
##  representative <r>. If the argument <R> is not given, it defaults to
##  `Integers'.
##
DeclareGlobalFunction( "ResidueClassWithFixedRepresentative" );
DeclareSynonym( "ResidueClassWithFixedRep",
                ResidueClassWithFixedRepresentative );

#############################################################################
##
#O  Modulus( <U> ) . .  modulus of a union of residue classes with fixed reps
##
DeclareOperation( "Modulus",
                  [ IsUnionOfResidueClassesWithFixedRepresentatives ] );

#############################################################################
##
#O  Classes( <U> )
##
##  The list of pairs ( <representative>, <modulus> ) as given as argument
##  <classes> when constructing <U> via
##  `UnionOfResidueClassesWithFixedRepresentatives'.
##
DeclareOperation( "Classes",
                  [ IsUnionOfResidueClassesWithFixedRepresentatives ] );

#############################################################################
##
#F  AllResidueClassesWithFixedRepresentativesModulo( [ <R>, ] <m> )
#F  AllResidueClassesWithFixedRepsModulo( [ <R>, ] <m> )
##
##  The same as `AllResidueClassesModulo', but with fixed representatives.
##  The fixed representatives are the same as those returned by the function
##  `AllResidues' when called with the arguments <R> and <m>.
##
DeclareGlobalFunction( "AllResidueClassesWithFixedRepresentativesModulo" );
DeclareSynonym( "AllResidueClassesWithFixedRepsModulo",
                AllResidueClassesWithFixedRepresentativesModulo );

#############################################################################
##
#O  AsListOfClasses( <U> )
##
##  A list of the classes which form the union <U> of residue classes with
##  fixed representatives.
##
DeclareOperation( "AsListOfClasses",
                  [ IsUnionOfResidueClassesWithFixedRepresentatives ] );

#############################################################################
##
#O  Multiplicity( <n>, <U> )  . . . . . . . . . . multiplicity of  <n> in <U>
#O  Multiplicity( <cl>, <U> ) . . . . . . . . . . multiplicity of <cl> in <U>
##
##  The multiplicity of the ring element <n> / the residue class <cl> in the
##  residue class union <U> with fixed representatives.
##
DeclareOperation( "Multiplicity",
                  [ IsObject,
                    IsUnionOfResidueClassesWithFixedRepresentatives ] );

#############################################################################
##
#P  IsOverlappingFree( <U> )
##
##  We call a residue class union <U> with fixed representatives
##  *overlapping free* if and only if it consists of pairwisely disjoint 
##  residue classes.
##
DeclareProperty( "IsOverlappingFree",
                 IsUnionOfResidueClassesWithFixedRepresentatives );

#############################################################################
##
#O  AsOrdinaryUnionOfResidueClasses( <U> )
##
##  The set-theoretic union of the residue classes in the union <U> of
##  residue classes with fixed representatives. The returned object is an
##  ordinary residue class union without fixed representatives which
##  behaves like a subset of the underlying ring.
##
DeclareOperation( "AsOrdinaryUnionOfResidueClasses",
                  [ IsUnionOfResidueClassesWithFixedRepresentatives ] );

#############################################################################
##
#O  RepresentativeStabilizingRefinement( <U>, <k> ) . . . . refinement of <U>
##
##  We define the *representative stabilizing refinement* of a residue class
##  [r/m] of Z with fixed representative into k parts by [r/km] U [(r+m)/km]
##  U ... U [(r+(k-1)m)/km].
##
##  We extend this definition in a natural way to unions of residue classes.
##
##  If the argument <k> is zero, it is tried to simplify <U> by iteratively
##  uniting residue classes by the reverse of the process described above.  
##
DeclareOperation( "RepresentativeStabilizingRefinement",
                  [ IsUnionOfResidueClassesWithFixedRepresentatives,
                    IsRingElement ] );

#############################################################################
##
#A  Delta( <U> ) . . . . . . . . . . . . . . . . . . . . .  invariant `Delta'
##
##  For a residue class [r/m] with fixed representative we set
##  Delta([r/m]) := r/m - 1/2 and extend this definition additively to unions
##  of such residue classes.
##
##  If no representatives are fixed, this definition is still unique (mod 1).
##
DeclareAttribute( "Delta", IsUnionOfResidueClassesWithFixedRepresentatives );
DeclareAttribute( "Delta", IsResidueClassUnion );

#############################################################################
##
#A  Rho( <U> ) . . . . . . . . . . . . . . . . . . . . . . .  invariant `Rho'
##
##  For a residue class [r/m] with fixed representative and
##  fixed orientation, i.e. fixed sign of m, we set
##  Rho([r/m]) := exp(sgn(m)*Delta([r/m])/2) and extend this definition in
##  the obvious way to unions of such residue classes.
##
##  If no representatives and no orientation is fixed, this definition
##  can still be made unique by restricting the exponent to the interval
##  [0,1/2[.
##
DeclareAttribute( "Rho", IsUnionOfResidueClassesWithFixedRepresentatives );
DeclareAttribute( "Rho", IsResidueClassUnion );

#############################################################################
##
#E  fixedrep.gd . . . . . . . . . . . . . . . . . . . . . . . . . . ends here