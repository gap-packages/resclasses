#############################################################################
##
#W  resclass.gd             GAP4 Package `ResClasses'             Stefan Kohl
##
##  This file contains declarations needed for computing with unions of
##  residue classes +/- finite sets ("residue class unions").
##
#############################################################################

#############################################################################
##
#C  IsResidueClassUnion . . . . . . . . . . . . . . . .  residue class unions
#C  IsUnionOfResidueClasses
##
##  The category of all unions of residue classes +/- finite sets, hence of
##  all "residue class unions".
##
DeclareCategory( "IsResidueClassUnion", IsDomain and IsListOrCollection );
DeclareSynonym( "IsUnionOfResidueClasses", IsResidueClassUnion );

#############################################################################
##
#C  IsResidueClassUnionOfZ . . . . . . . . . . . .  residue class unions of Z
#C  IsUnionOfResidueClassesOfZ                                          ditto
#C  IsResidueClassUnionOfZxZ                    "                      of Z^2
#C  IsUnionOfResidueClassesOfZxZ                                        ditto
#C  IsResidueClassUnionOfZ_pi                   "                   of Z_(pi)
#C  IsUnionOfResidueClassesOfZ_pi                                       ditto
#C  IsResidueClassUnionOfGFqx                   "                 of GF(q)[x]
#C  IsUnionOfResidueClassesOfGFqx                                       ditto
#C  IsResidueClassUnionOfZorZ_pi                "              of Z or Z_(pi)
#C  IsUnionOfResidueClassesOfZorZ_pi                                    ditto
##
##  The category of the residue class unions of Z, of Z^2, of a (semi-)
##  localization Z_(pi) of Z or of a polynomial ring GF(q)[x] over a finite
##  field, respectively. The last-mentioned category is the union of the
##  first-mentioned and the third-mentioned one.
##
DeclareCategory( "IsResidueClassUnionOfZ",
                 IsDomain and IsListOrCollection );
DeclareSynonym( "IsUnionOfResidueClassesOfZ", IsResidueClassUnionOfZ );
DeclareCategory( "IsResidueClassUnionOfZxZ",
                 IsDomain and IsListOrCollection );
DeclareSynonym( "IsUnionOfResidueClassesOfZxZ", IsResidueClassUnionOfZxZ );
DeclareCategory( "IsResidueClassUnionOfZ_pi",
                 IsDomain and IsListOrCollection );
DeclareSynonym( "IsUnionOfResidueClassesOfZ_pi", IsResidueClassUnionOfZ_pi );
DeclareCategory( "IsResidueClassUnionOfGFqx",
                 IsDomain and IsListOrCollection );
DeclareSynonym( "IsUnionOfResidueClassesOfGFqx", IsResidueClassUnionOfGFqx );
DeclareCategory( "IsResidueClassUnionOfZorZ_pi",
                 IsDomain and IsListOrCollection );
DeclareSynonym( "IsUnionOfResidueClassesOfZorZ_pi",
                IsResidueClassUnionOfZorZ_pi );

#############################################################################
##
#R  IsResidueClassUnionResidueListRep . .  representation by list of residues
##
##  The representation of unions of residue classes of the ring Z of the
##  integers, of Z^2, of semilocalizations Z_(pi) of the integers and of
##  univariate polynomial rings GF(q)[x] over finite fields, by list of
##  residues.
## 
##  Components:
##
##  - <m>:        the modulus
##  - <r>:        the list of class representatives
##  - <included>: set of elements added to the union of residue classes
##  - <excluded>: set of elements subtracted from  "      "      "
##
##  The representation is unique, i.e. two residue class unions are equal
##  if and only if their stored representations are equal.
##
##  This is achieved in the following way:
##
##  - If the underlying ring R is Z, Z_(pi) or GF(q)[x], the modulus <m> is
##    chosen to be multiplicatively minimal, and to be its own standard
##    associate in R. If R is Z^2, the modulus <m> is a lattice, which is
##    stored as an invertible 2x2 matrix whose rows are the spanning vectors.
##    That matrix is chosen to be in Hermite normal form and to have the
##    smallest possible determinant.
##  - The set <included> contains only elements which are not congruent to
##    one of the residues in <r> modulo <m>.
##  - The set <excluded> contains only elements which are congruent to one
##    of the residues in <r> modulo <m>.
##
DeclareRepresentation( "IsResidueClassUnionResidueListRep", 
                       IsComponentObjectRep and IsAttributeStoringRep, 
                       [ "m", "r", "included", "excluded" ] );

#############################################################################
##
#R  IsResidueClassUnionClassListRep . . . . representation by list of classes
##
##  The representation of unions of residue classes of the ring Z of the
##  integers, of Z^2, of semilocalizations Z_(pi) of the integers and of
##  univariate polynomial rings GF(q)[x] over finite fields, by normalized
##  list of disjoint residue classes.
## 
##  Components:
##
##  - <cls>:      the list of residue classes, as pairs [ residue, modulus ]
##  - <m>:        the modulus
##  - <included>: set of elements added to the union of residue classes
##  - <excluded>: set of elements subtracted from  "      "      "
##
##  The representation is unique, i.e. two residue class unions are equal
##  if and only if their stored representations are equal.
##
##  This is achieved in the following way:
##
##  - The list <cls> of residue classes is sorted, firstly by ascending
##    moduli and secondly by ascending residues.
##  - The list of residue classes is always as short as possible, and the
##    list of their moduli is always lexicographically minimal, i.e. for
##    example 0(3) U 1(6) U 5(6) is not represented as [[0,3],[1,6],[5,6]],
##    but rather as [[1,2],[0,6]].
##  - Residue classes with the same modulus are ordered by ascending residue.
##  - The set <included> contains only elements which are neither in one of
##    the residue classes in the list <cls> nor in <excluded>.
##  - The set <excluded> contains only elements which are neither in one of
##    the residue classes in the list <cls> nor in <included>.
##
##  At present, this representation is only available for residue class
##  unions of Z. 
##
DeclareRepresentation( "IsResidueClassUnionClassListRep", 
                       IsComponentObjectRep and IsAttributeStoringRep, 
                       [ "cls", "m", "included", "excluded" ] );

#############################################################################
##
#O  ClassListRep( <U> )
#O  SparseRep( <U> )
#O  SparseRepresentation( <U> )
#O  ResidueListRep( <U> )
#O  StandardRep( <U> )
#O  StandardRepresentation( <U> )
##
##  Conversion between the two representations of residue class unions:
##  `IsResidueClassUnionResidueListRep' and `IsResidueClassUnionClassListRep'
##
##  Remark: the declarations are for `IsObject' since `SparseRep' and
##          `StandardRep' are used in RCWA for rcwa mappings as well.
##
DeclareOperation( "ClassListRep", [ IsObject] );
DeclareSynonym( "SparseRep", ClassListRep );
DeclareSynonym( "SparseRepresentation", ClassListRep );
DeclareOperation( "ResidueListRep", [ IsObject ] );
DeclareSynonym( "StandardRep", ResidueListRep );
DeclareSynonym( "StandardRepresentation", ResidueListRep );

#############################################################################
##
#R  IsResidueClassUnionsIteratorRep . . . . . . . . . iterator representation
##
DeclareRepresentation( "IsResidueClassUnionsIteratorRep",
                       IsComponentObjectRep,
                       [ "U", "counter", "element", "classpos" ] );

#############################################################################
##
#O  ResidueClassUnionCons( <filter>, <R>, <m>, <r>, <included>, <excluded> )
#O  ResidueClassUnionCons( <filter>, <R>, <cls> <included>, <excluded> )
#F  ResidueClassUnion( [ <R>, ] <m>, <r>, [ <included>, <excluded> ] )
#F  ResidueClassUnion( [ <R>, ] <cls>, [ <included>, <excluded> ] )
#F  ResidueClassUnionNC( [ <R>, ] <m>, <r>, [ <included>, <excluded> ] )
#F  ResidueClassUnionNC ( [ <R>, ] <cls>, [ <included>, <excluded> ] )
##
##  The constructor for *residue class unions*,
##  i.e. set-theoretic unions of residue classes +/- finite sets of elements.
##
##  Returns
##    - the union of the residue classes <r>[i] ( mod <m> ), respectively
##    - the union of the residue classes <cls>[i][1] ( mod <cls>[i][2] )
##  of the ring <R>, plus a finite set <included> and minus a finite set
##  <excluded> of elements of <R>.
##
DeclareConstructor( "ResidueClassUnionCons",
                    [ IsResidueClassUnion, IsRing, IsRingElement,
                      IsList, IsList, IsList ] );
DeclareConstructor( "ResidueClassUnionCons",
                    [ IsResidueClassUnion, IsRing,
                      IsList, IsList, IsList ] );
DeclareConstructor( "ResidueClassUnionCons",
                    [ IsResidueClassUnion, IsRowModule, IsMatrix,
                      IsList, IsList, IsList ] );
DeclareConstructor( "ResidueClassUnionCons",
                    [ IsResidueClassUnion, IsRowModule,
                      IsList, IsList, IsList ] );
DeclareGlobalFunction( "ResidueClassUnion" );
DeclareGlobalFunction( "ResidueClassUnionNC" );

#############################################################################
##
#F  ResidueClass( <R>, <m>, <r> ) . . . . . . . . . . .  residue class of <R>
#F  ResidueClass( <m>, <r> )  . . . . . . . . . residue class of the integers
#F  ResidueClass( <r>, <m> )  . . . . . . . . . . . . . . . . . . . ( ditto )
#F  ResidueClassNC( <R>, <m>, <r> ) . . . . . . . . . .  residue class of <R>
#F  ResidueClassNC( <m>, <r> )  . . . . . . . . residue class of the integers
#F  ResidueClassNC( <r>, <m> )  . . . . . . . . . . . . . . . . . . ( ditto )
#P  IsResidueClass( <obj> ) . . . . . . . . . . <obj> is single residue class
##
##  Returns the residue class <r> ( mod <m> ) of the ring <R>, respectively
##  the residue class <r> ( mod <m> ) of the default ring of <m> and <r>.
##
##  In the two-argument case, if <r> and <m> are integers, they must be
##  nonnegative, and <r> must lie in the range [0..<m>-1]. This is used to
##  decide which argument is <m> and which is <r>. For other rings, similar
##  criteria are used.
##
##  Residue classes have the property `IsResidueClass'.
##
DeclareGlobalFunction( "ResidueClass" );
DeclareGlobalFunction( "ResidueClassNC" );
DeclareProperty( "IsResidueClass", IsObject );

#############################################################################
##
#V  RESCLASSES_VIEWINGFORMAT . . . current viewing format ("short" or "long")
#F  ResidueClassUnionViewingFormat( format ) . short <--> long viewing format
##
DeclareGlobalVariable( "RESCLASSES_VIEWINGFORMAT" );
DeclareGlobalFunction( "ResidueClassUnionViewingFormat" );

#############################################################################
##
#O  Modulus( <U> ) . . . . . . . . . . . . . modulus of a residue class union
##
DeclareOperation( "Modulus", [ IsResidueClassUnion ] );
DeclareSynonym( "Mod", Modulus );

#############################################################################
##
#O  Residues( <U> ) . . . . . . . . . . . . . residues of residue class union
#O  Residue( <cl> ) . . . . . . . . . . . . . . . .  residue of residue class
#O  Classes( <U> )  . . . . . . . residue classes forming residue class union
##
DeclareOperation( "Residues", [ IsResidueClassUnion ] );
DeclareOperation( "Residue", [ IsResidueClass ] );
DeclareOperation( "Classes", [ IsResidueClassUnion ] );

#############################################################################
##
#O  IncludedElements( <U> ) . included single elements of residue class union
#O  ExcludedElements( <U> ) . excluded single elements of residue class union
#O  IncludedLines( <U> ) . . . . included lines of residue class union of Z^2
#O  ExcludedLines( <U> ) . . . . excluded lines of residue class union of Z^2
##
DeclareOperation( "IncludedElements", [ IsResidueClassUnion ] );
DeclareOperation( "ExcludedElements", [ IsResidueClassUnion ] );
DeclareOperation( "IncludedLines", [ IsResidueClassUnionOfZxZ ] );
DeclareOperation( "ExcludedLines", [ IsResidueClassUnionOfZxZ ] );

#############################################################################
##
#F  ResidueClassUnionsFamily( <R> [ , <fixedreps> ] )
##
##  Returns the family of all residue class unions of <R>.
##
##  The optional argument <fixedreps> is a boolean which determines whether
##  the function returns the family of "usual" residue class unions of <R>
##  (<fixedreps> = `false' or not given) or the family of unions of residue
##  classes with fixed representatives (<fixedreps> = `true').
##
DeclareGlobalFunction( "ResidueClassUnionsFamily" );

#############################################################################
##
#F  ZResidueClassUnionsFamily( <fixedreps> )
#F  Z_piResidueClassUnionsFamily( <R> [ , <fixedreps> ] )
#F  GFqxResidueClassUnionsFamily( <R> [ , <fixedreps> ] )
##
##  Returns the family of unions of residue classes of Z,
##  of a ring <R> = Z_(pi) or of a ring <R> = GF(q)[x], respectively.
##
##  For the meaning of the optional argument <fixedreps>,
##  see `ResidueClassUnionsFamily'.
##
DeclareGlobalFunction( "ZResidueClassUnionsFamily" );
DeclareGlobalFunction( "Z_piResidueClassUnionsFamily" );
DeclareGlobalFunction( "GFqxResidueClassUnionsFamily" );

#############################################################################
##
#A  UnderlyingRing( <fam> ) . . . . . . . underlying ring of the family <fam>
#A  UnderlyingIndeterminate( <fam> ) . . indet. of underlying polynomial ring
##
##  The underlying ring / the indeterminate of the underlying polynomial ring
##  of the family <fam>.
##
DeclareAttribute( "UnderlyingRing", IsFamily );
DeclareAttribute( "UnderlyingIndeterminate", IsFamily );

#############################################################################
##
#P  IsZxZ . . . . . . . . . . . . . . . . . . . . .  Z^2 = Z x Z = Integers^2
##
DeclareProperty( "IsZxZ", IsObject );

#############################################################################
##
#O  AllResidues( <R>, <m> ) . . . . the residues (mod <m>) in canonical order
#F  AllResidueClassesModulo( [ <R>, ] <m> ) . . the residue classes (mod <m>)
#O  NumberOfResidues( <R>, <m> ) . . . . . . the number of residues (mod <m>)
#O  NrResidues( <R>, <m> ) . . . . . . . . . . . . . . . . . . . . - ditto - 
##
##  Returns
##   - a sorted list of all residues, resp.
##   - a sorted list of all residue classes, resp.
##   - the number of residues
##  modulo <m> in the ring <R>.
##
DeclareOperation( "AllResidues", [ IsRing, IsRingElement ] );
DeclareOperation( "AllResidues", [ IsRowModule, IsMatrix ] );
DeclareOperation( "NumberOfResidues", [ IsRing, IsRingElement ] );
DeclareOperation( "NumberOfResidues", [ IsRowModule, IsMatrix ] );
DeclareSynonym( "NrResidues", NumberOfResidues );
DeclareGlobalFunction( "AllResidueClassesModulo" );

#############################################################################
##
#F  All2x2IntegerMatricesInHNFWithDeterminantUpTo( <maxdet> )
##
##  Returns a list of all 2 x 2 integer matrices in Hermite normal form
##  whose determinant is less than or equal to <maxdet>.
##
DeclareGlobalFunction( "All2x2IntegerMatricesInHNFWithDeterminantUpTo" );

#############################################################################
##
#O  IsSublattice( <L1>, <L2> )
#O  Superlattices( <L> )
##
##  Lattices are represented by integer matrices in Hermite normal form.
##
DeclareOperation( "IsSublattice", [ IsMatrix, IsMatrix ] );
DeclareOperation( "Superlattices", [ IsMatrix ] );

#############################################################################
##
#A  SizeOfSmallestResidueClassRing( <R> )
##
DeclareAttribute( "SizeOfSmallestResidueClassRing", IsRing );

#############################################################################
##
#F  RingToString( <R> ) . . . how the ring <R> is printed by `View'/`Display'
##
##  The return value of this function determines the way the ring <R> is
##  printed by the methods for `View'/`Display' for residue class unions.
##
DeclareGlobalFunction( "RingToString" );

#############################################################################
##
#O  AsUnionOfFewClasses( <U> ) . . . . .  write <U> as a union of few classes
##
##  Returns a partition of <U> into 'few' residue classes, up to the finite
##  sets of included / excluded elements.
## 
DeclareOperation( "AsUnionOfFewClasses", [ IsResidueClassUnion ] );

#############################################################################
##
#O  SplittedClass( <cl>, <t> ) . . partition of <cl> into <t> residue classes
##
##  Returns a partition of the residue class <cl> into <t> residue classes
##  with the same modulus.
##
DeclareOperation( "SplittedClass", [ IsResidueClassUnion, IsRingElement ] );
DeclareOperation( "SplittedClass", [ IsResidueClassUnion, IsRowVector ] );

#############################################################################
##
#O  PartitionsIntoResidueClasses( <R>, <length> )
#O  PartitionsIntoResidueClasses( <R>, <length>, <primes> )
##
##  Returns a list of all partitions of the ring <R> into <length> residue
##  classes.
##
DeclareOperation( "PartitionsIntoResidueClasses", [ IsRing, IsPosInt ] );
DeclareOperation( "PartitionsIntoResidueClasses", [ IsRing, IsPosInt,
                                                    IsList ] );

#############################################################################
##
#O  RandomPartitionIntoResidueClasses( <R>, <length>, <primes> )
##
##  Returns a random partition of the ring <R> into <length> residue classes
##  whose moduli have only prime factors in <primes>.
##
DeclareOperation( "RandomPartitionIntoResidueClasses",
                  [ IsRing, IsPosInt, IsList ] );

#############################################################################
##
#O  CoverByResidueClasses( <S>, <moduli> ) . .  cover of S by residue classes
#O  CoversByResidueClasses( <S>, <moduli> )
##
##  One, respectively all, covers of the set <S> by residue classes with
##  moduli <moduli>.
##  If no such cover exists, the former operation returns `fail'.
##
DeclareOperation( "CoverByResidueClasses", [ IsListOrCollection, IsList ] );
DeclareOperation( "CoversByResidueClasses", [ IsListOrCollection, IsList ] );

#############################################################################
##
#A  Density( <S> ) . .  natural density of the set <S> in the underlying ring
##
DeclareAttribute( "Density", IsResidueClassUnion );

#############################################################################
##
#F  DisplayAsGrid( <U> ) .  display the residue class union <U> as ASCII grid
##
DeclareGlobalFunction( "DisplayAsGrid" );

#############################################################################
##
#E  resclass.gd . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
