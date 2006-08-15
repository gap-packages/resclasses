#############################################################################
##
#W  resclass.gd             GAP4 Package `ResClasses'             Stefan Kohl
##
#H  @(#)$Id$
##
##  This file contains declarations needed for dealing with unions of
##  residue classes and finite sets.
##
Revision.resclass_gd :=
  "@(#)$Id$";

#############################################################################
##
#C  IsUnionOfResidueClasses . . . . unions of residue classes and finite sets
##
##  The category of all unions of residue classes and finite sets.
##
DeclareCategory( "IsUnionOfResidueClasses",
                 IsDomain and IsListOrCollection );

#############################################################################
##
#C  IsUnionOfResidueClassesWithFixedRepresentatives
##
##  The category of all unions of residue classes which are endowed with
##  fixed representatives.
##
DeclareCategory( "IsUnionOfResidueClassesWithFixedRepresentatives",
                 IsDomain and IsListOrCollection );

#############################################################################
##
#R  IsResidueClassUnionSparseRep . . .  `sparse' rep. of residue class unions
##
##  Representation of unions of residue classes of the integers,
##  a semilocalization Z_(pi) of the integers or a univariate polynomial ring
##  GF(q)[x] over a finite field.
## 
##  The component <m> stores the common modulus, <r> is the list of class
##  representatives and <included> resp. <excluded> are lists of single
##  elements added to resp. subtracted from the union of classes.
##
DeclareRepresentation( "IsResidueClassUnionSparseRep", 
                       IsComponentObjectRep and IsAttributeStoringRep, 
                       [ "m", "r", "included", "excluded" ] );

#############################################################################
##
#R  IsFixedRepResidueClassUnionRep
##
##  Representation of unions of residue classes of the integers, a
##  semilocalization Z_(pi) of the integers or a univariate polynomial
##  ring GF(q)[x] over a finite field, with fixed representatives.
## 
##  The component <classes> is a list of residue classes, given as pairs
##  ( <modulus>, <representative> ). The representatives are *not* reduced
##  modulo the respective moduli, and the moduli may be different --
##  in contrast to `IsResidueClassUnionSparseRep' no common modulus is
##  stored, and *unions which are equal as sets are distinguished* if their
##  respective <classes> - components are distinct.
##
DeclareRepresentation( "IsFixedRepResidueClassUnionRep",
                       IsComponentObjectRep and IsAttributeStoringRep, 
                       [ "classes" ] );

#############################################################################
##
#C  IsUnionOfResidueClassesOfZ . .  unions of residue classes and finite sets
#C  IsUnionOfResidueClassesOfZWithFixedRepresentatives
##
##  The category of unions of residue classes of the integers and finite sets
##  of integers, resp. of unions of residue classes of the integers with
##  fixed representatives.
##
DeclareCategory( "IsUnionOfResidueClassesOfZ",
                 IsDomain and IsListOrCollection );
DeclareCategory( "IsUnionOfResidueClassesOfZWithFixedRepresentatives",
                 IsDomain and IsListOrCollection );

#############################################################################
##
#C  IsUnionOfResidueClassesOfZ_pi . unions of residue classes and finite sets
#C  IsUnionOfResidueClassesOfZ_piWithFixedRepresentatives
##
##  The category of unions of residue classes of some ring Z_(pi) and finite
##  subsets of of this ring, resp. of unions of residue classes of these
##  rings with fixed representatives.
##
DeclareCategory( "IsUnionOfResidueClassesOfZ_pi",
                 IsDomain and IsListOrCollection );
DeclareCategory( "IsUnionOfResidueClassesOfZ_piWithFixedRepresentatives",
                 IsDomain and IsListOrCollection );

#############################################################################
##
#C  IsUnionOfResidueClassesOfZorZ_pi . unions of res. classes and finite sets
#C  IsUnionOfResidueClassesOfZorZ_piWithFixedRepresentatives
##
##  The union of the categories `IsUnionOfResidueClassesOfZ' and
##  `IsUnionOfResidueClassesOfZ_pi' resp. the categories
##  `IsUnionOfResidueClassesOfZWithFixedRepresentatives' and
##  `IsUnionOfResidueClassesOfZ_piWithFixedRepresentatives`.
##
DeclareCategory( "IsUnionOfResidueClassesOfZorZ_pi",
                 IsDomain and IsListOrCollection );
DeclareCategory( "IsUnionOfResidueClassesOfZorZ_piWithFixedRepresentatives",
                 IsDomain and IsListOrCollection );

#############################################################################
##
#C  IsUnionOfResidueClassesOfGFqx . . .  unions of res. classes and fin. sets
#C  IsUnionOfResidueClassesOfGFqxWithFixedRepresentatives
##
##  The category of unions of residue classes of some ring GF(q)[x] and
##  finite subsets of of this ring, resp. of unions of residue classes of
##  these rings with fixed representatives.
##
DeclareCategory( "IsUnionOfResidueClassesOfGFqx",
                 IsDomain and IsListOrCollection );
DeclareCategory( "IsUnionOfResidueClassesOfGFqxWithFixedRepresentatives",
                 IsDomain and IsListOrCollection );

#############################################################################
##
#F  ResidueClassUnionsFamily( <R> [ , <fixedreps> ] )
##
##  The family of all residue class unions of <R>. The optional argument
##  <fixedreps> is a boolean which determines whether the function returns
##  the family of "usual" residue class unions of <R> (<fixedreps> = `false'
##  or not given) or the family of unions of residue classes with fixed
##  representatives (<fixedreps> = `true').
##
DeclareGlobalFunction( "ResidueClassUnionsFamily" );

#############################################################################
##
#F  Z_piResidueClassUnionsFamily( <R> [ , <fixedreps> ] )
##
##  Family of unions of residue classes of the ring R = Z_(pi) and finite
##  subsets of this ring. For the meaning of the optional argument
##  <fixedreps> see `ResidueClassUnionsFamily'.
##
DeclareGlobalFunction( "Z_piResidueClassUnionsFamily" );

#############################################################################
##
#F  GFqxResidueClassUnionsFamily( <R> [ , <fixedreps> ] )
##
##  Family of unions of residue classes of the ring R = GF(q)[x] and finite
##  subsets of this ring. For the meaning of the optional argument
##  <fixedreps> see `ResidueClassUnionsFamily'.
##
DeclareGlobalFunction( "GFqxResidueClassUnionsFamily" );

#############################################################################
##
#F  UnderlyingRing( <fam> ) . . . . . . . underlying ring of the family <fam>
##
##  The underlying ring of the family <fam>.
##
DeclareAttribute( "UnderlyingRing", IsFamily );

#############################################################################
##
#F  UnderlyingIndeterminate( <fam> ) . . indet. of underlying polynomial ring
##
##  The indeterminate of the underlying polynomial ring of the family <fam>.
##
DeclareAttribute( "UnderlyingIndeterminate", IsFamily );

#############################################################################
##
#O  ResidueClassUnionCons( <filter>, <R>, <m>, <r>, <included>, <excluded> )
##
##  Constructor for unions of residue classes +/- finite sets of elements.
##
##  Constructs the union of the residue classes <r>[i] ( mod <m> ) of the
##  ring <R>, where <included> resp. <excluded> denote lists of single
##  elements which should be included in resp. excluded from the union.
##
DeclareConstructor( "ResidueClassUnionCons",
                    [ IsUnionOfResidueClasses, IsRing, IsRingElement,
                      IsList, IsList, IsList ] );

#############################################################################
##
#O  ResidueClassUnionWithFixedRepresentativesCons( <filter>, <R>, <classes> )
##
##  Constructor for unions of residue classes with fixed representatives.
##
##  Constructs the union of the residue classes
##
##   <classes>[i][2] ( mod <classes>[i][1] )
##
##  of the ring <R>.
##
DeclareConstructor( "ResidueClassUnionWithFixedRepresentativesCons",
                    [ IsUnionOfResidueClassesWithFixedRepresentatives,
                      IsRing, IsList ] );

#############################################################################
##
#F  ResidueClassUnion( <R>, <m>, <r> ) . . . . . . . union of residue classes
#F  ResidueClassUnion( <R>, <m>, <r>, <included>, <excluded> )
##
##  Calls `ResidueClassUnionCons'.
##  For a description of the arguments, see there.
##
DeclareGlobalFunction( "ResidueClassUnion" );

#############################################################################
##
#F  ResidueClassUnionWithFixedRepresentatives( <R>, <classes> )
#F  ResidueClassUnionWithFixedRepresentatives( <classes> )
#F  ResidueClassUnionWithFixedReps( <R>, <classes> )
#F  ResidueClassUnionWithFixedReps( <classes> )
##
##  Calls `ResidueClassUnionWithFixedRepresentativesCons'.
##  For a description of the arguments, see there.
##  If the argument <R> is not given, it defaults to `Integers'.
##
DeclareGlobalFunction( "ResidueClassUnionWithFixedRepresentatives" );
DeclareSynonym( "ResidueClassUnionWithFixedReps",
                ResidueClassUnionWithFixedRepresentatives );

#############################################################################
##
#F  ResidueClass( <R>, <m>, <r> ) . . . . . . . . . . .  residue class of <R>
#F  ResidueClass( <m>, <r> )  . . . . . . . . . residue class of the integers
#F  ResidueClass( <r>, <m> )  . . . . . . . . . . . . . . . . . . .  ( dito )
##
##  The residue class <r> ( mod <m> ) of the ring <R>, resp. the residue
##  class <r> ( mod <m> ) of the integers. In the two-argument case, <r> and
##  <m> must be non-negative, and <r> must lie in the range [0..<m>-1].
##
DeclareGlobalFunction( "ResidueClass" );

#############################################################################
##
#P  IsResidueClass( <obj> ) . . . . . . . . . . <obj> is single residue class
##
DeclareProperty( "IsResidueClass", IsObject );

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
#O  Modulus( <U> ) . . . . . . . . . . . . . modulus of a residue class union
##
DeclareOperation( "Modulus", [ IsUnionOfResidueClasses ] );
DeclareOperation( "Modulus",
                  [ IsUnionOfResidueClassesWithFixedRepresentatives ] );
DeclareSynonym( "Mod", Modulus );

#############################################################################
##
#O  Residues( <U> ) . . . . . . . . . . . . . residues of residue class union
#O  Residue( <cl> ) . . . . . . . . . . . . . . . .  residue of residue class
##
DeclareOperation( "Residues", [ IsUnionOfResidueClasses ] );
DeclareOperation( "Residue", [ IsResidueClass ] );

#############################################################################
##
#O  IncludedElements( <U> ) . included single elements of residue class union
##
DeclareOperation( "IncludedElements", [ IsUnionOfResidueClasses ] );

#############################################################################
##
#O  ExcludedElements( <U> ) . excluded single elements of residue class union
##
DeclareOperation( "ExcludedElements", [ IsUnionOfResidueClasses ] );

#############################################################################
##
#O  Classes( <U> )
##
##  The list of pairs ( <representative>, <modulus> ) as given as argument
##  <classes> when constructing <U> via
##  `ResidueClassUnionWithFixedRepresentatives'.
##
DeclareOperation( "Classes",
                  [ IsUnionOfResidueClassesWithFixedRepresentatives ] );

#############################################################################
##
#F  AllResidues( <R>, <m> ) . . . . the residues (mod <m>) in canonical order
##
##  Returns a sorted list of all residues modulo <m> in the ring <R>.
##
DeclareGlobalFunction( "AllResidues" );

#############################################################################
##
#F  AllResidueClassesModulo( [ <R>, ] <m> ) . . the residue classes (mod <m>)
##
##  Returns a sorted list of all residue classes modulo <m> in the ring <R>.
##  If the argument <R> is not given the function will choose the default
##  ring of <m>.
##
DeclareGlobalFunction( "AllResidueClassesModulo" );

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
#O  AsUnionOfFewClasses( <U> ) . . . . .  write <U> as a union of few classes
##
##  The result includes only whole residue classes and does not specify
##  included / excluded single elements.
## 
DeclareOperation( "AsUnionOfFewClasses", [ IsUnionOfResidueClasses ] );

#############################################################################
##
#O  SplittedClass( <cl>, <t> ) . . partition of <cl> into <t> residue classes
##
##  The argument <cl> must be a single residue class. The return value is a
##  partition of <cl> into <t> residue classes with the same modulus.
##
DeclareOperation( "SplittedClass", [ IsUnionOfResidueClasses, IsPosInt ] );

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
#A  Density( <S> ) . .  natural density of the set <S> in the underlying ring
##
DeclareAttribute( "Density", IsUnionOfResidueClasses );

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
DeclareAttribute( "Delta", IsUnionOfResidueClasses );

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
DeclareAttribute( "Rho", IsUnionOfResidueClasses );

#############################################################################
##
#R  IsResidueClassUnionsIteratorRep( <U> ) . . . . .  iterator representation
##
DeclareRepresentation( "IsResidueClassUnionsIteratorRep",
                       IsComponentObjectRep,
                       [ "structure", "counter", "element", "classpos" ] );

#############################################################################
##
#E  resclass.gd . . . . . . . . . . . . . . . . . . . . . . . . . . ends here