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
#C  IsUnionOfResidueClassesOfZ . .  unions of residue classes and finite sets
##
##  The category of unions of residue classes of the integers and finite sets
##  of integers.
##
DeclareCategory( "IsUnionOfResidueClassesOfZ",
                 IsDomain and IsListOrCollection );

#############################################################################
##
#C  IsUnionOfResidueClassesOfZ_pi . unions of residue classes and finite sets
##
##  The category of unions of residue classes of some ring $\Z_\pi$ and
##  finite subsets of of this ring. 
##
DeclareCategory( "IsUnionOfResidueClassesOfZ_pi",
                 IsDomain and IsListOrCollection );

#############################################################################
##
#C  IsUnionOfResidueClassesOfZorZ_pi . unions of res. classes and finite sets
##
##  The union of the categories `IsUnionOfResidueClassesOfZ' and
##  `IsUnionOfResidueClassesOfZ_pi'.
##
DeclareCategory( "IsUnionOfResidueClassesOfZorZ_pi",
                 IsDomain and IsListOrCollection );

#############################################################################
##
#C  IsUnionOfResidueClassesOfGFqx . . .  unions of res. classes and fin. sets
##
##  The category of unions of residue classes of some ring GF($q$)[$x$] and
##  finite subsets of of this ring. 
##
DeclareCategory( "IsUnionOfResidueClassesOfGFqx",
                 IsDomain and IsListOrCollection );

#############################################################################
##
#F  ResidueClassUnionsFamily( <R> ) . family of all residue class unions of R
##
DeclareGlobalFunction( "ResidueClassUnionsFamily" );

#############################################################################
##
#V  ZResidueClassUnionsFamily . . . . .  family of all res. class unions of Z
##
DeclareGlobalVariable( "ZResidueClassUnionsFamily" );

#############################################################################
##
#F  Z_piResidueClassUnionsFamily( <R> )
##
##  Family of unions of residue classes of $\Z_\pi$ and finite subsets of
##  this ring, where the set $\pi$ is given by the list <primes>.
##
DeclareGlobalFunction( "Z_piResidueClassUnionsFamily" );

#############################################################################
##
#F  GFqxResidueClassUnionsFamily( <R> )
##
##  Family of unions of residue classes of the ring $R$ = GF($q$)[$x$] and
##  finite subsets of this ring.
##
DeclareGlobalFunction( "GFqxResidueClassUnionsFamily" );

#############################################################################
##
#F  AllGFqPolynomialsModDegree( <q>, <d>, <x> ) . residues in canonical order
##
##  Returns a sorted list of all residues modulo a polynomial of degree <d>
##  over GF(<q>) in the variable <x>.
##  This gives also the ordering in which the coefficients of a modular rcwa
##  mapping are stored; thus, if <f> is a modular rcwa mapping over
##  GF(<q>)[x] with coefficients list <c>, whose modulus <m> has degree <d>,
##  then <f> maps a polynomial <P> with <P> mod <m> = <r> to
##
##  ( <c>[`Position'(<res>,<r>)][1] * <P> + <c>[`Position'(<res>,<r>)][2] ) /
##    <c>[`Position'(<res>,<r>)][3],
##
##  where <res> denotes the list of residues returned by this function.
##
DeclareGlobalFunction( "AllGFqPolynomialsModDegree" );

#############################################################################
##
#F  UnderlyingRing( <obj> ) . . . . . . . . . . . . . . . . . underlying ring
##
##  The underlying ring of the object <obj>.
##
DeclareAttribute( "UnderlyingRing", IsObject );

#############################################################################
##
#F  UnderlyingIndeterminate( <fam> ) . . indet. of underlying polynomial ring
##
##  The indeterminate of the underlying polynomial ring of the family <fam>.
##
DeclareAttribute( "UnderlyingIndeterminate", IsFamily );

#############################################################################
##
#R  IsResidueClassUnionSparseRep . . .  `sparse' rep. of residue class unions
##
##  Representation of unions of residue classes of the integers, a
##  semilocalization $\Z_\pi$ of the integers or a univariate polynomial ring
##  GF($q$)[$x$] over a finite field.
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
#O  ResidueClassUnionCons( <R>, <m>, <r>, <included>, <excluded> )
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
#F  ResidueClass( <R>, <m>, <r> ) . . . . . . . . . . .  single residue class
##
##  The residue class <r> ( mod <m> ) of the ring <R>.
##
DeclareGlobalFunction( "ResidueClass" );

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
#O  Modulus( <obj> ) . . . . modulus of a residue class union or other object
##
DeclareOperation( "Modulus", [ IsObject ] );

#############################################################################
##
#O  Residues( <U> ) . . . . . . . . . . . . . residues of residue class union
##
DeclareOperation( "Residues", [ IsUnionOfResidueClasses ] );

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
#O  AsUnionOfFewClasses( <U> ) . . . . .  write <U> as a union of few classes
##
DeclareOperation( "AsUnionOfFewClasses", [ IsUnionOfResidueClasses ] );

#############################################################################
##
#O  Complement( <U> ) . . . . . . . . . . . complement of residue class union
##
if   not IsBound( Complement ) # In case CRISP is not loaded.
then DeclareOperation( "Complement", [ IsObject ] ); fi;

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
