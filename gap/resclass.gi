#############################################################################
##
#W  resclass.gi             GAP4 Package `ResClasses'             Stefan Kohl
##
#H  @(#)$Id$
##
##  This file contains implementations of methods and functions dealing with
##  unions of residue classes and finite sets.
##
Revision.resclass_gi :=
  "@(#)$Id$";

RingToString := function ( R )
  if IsIntegers(R) then return "Z"; else return String(R); fi;
end;
MakeReadOnlyGlobal( "RingToString");

# Internal variables storing the residue class unions families used in the
# current GAP session.

BindGlobal( "Z_PI_RESIDUE_CLASS_UNIONS_FAMILIES", [] );
BindGlobal( "GF_Q_X_RESIDUE_CLASS_UNIONS_FAMILIES", [] );

# Shorthand for commonly used filter.

BindGlobal( "IsResidueClassUnionInSparseRep",
            IsUnionOfResidueClasses and IsResidueClassUnionSparseRep );

# Implications between types of unions of residue classes.

InstallTrueMethod( IsUnionOfResidueClasses,
                   IsUnionOfResidueClassesOfZorZ_pi );
InstallTrueMethod( IsUnionOfResidueClassesOfZorZ_pi,
                   IsUnionOfResidueClassesOfZ );
InstallTrueMethod( IsUnionOfResidueClassesOfZorZ_pi,
                   IsUnionOfResidueClassesOfZ_pi );
InstallTrueMethod( IsUnionOfResidueClasses,
                   IsUnionOfResidueClassesOfGFqx );

InstallTrueMethod( IsUnionOfResidueClassesWithFixedRepresentatives,
                   IsUnionOfResidueClassesOfZorZ_piWithFixedRepresentatives);
InstallTrueMethod( IsUnionOfResidueClassesOfZorZ_piWithFixedRepresentatives,
                   IsUnionOfResidueClassesOfZWithFixedRepresentatives );
InstallTrueMethod( IsUnionOfResidueClassesOfZorZ_piWithFixedRepresentatives,
                   IsUnionOfResidueClassesOfZ_piWithFixedRepresentatives );
InstallTrueMethod( IsUnionOfResidueClassesWithFixedRepresentatives,
                   IsUnionOfResidueClassesOfGFqxWithFixedRepresentatives );

#############################################################################
##
#V  ZResidueClassUnionsFamily . . the family of all residue class unions of Z
##
InstallValue( ZResidueClassUnionsFamily,
              NewFamily( "ResidueClassUnionsFamily( Integers )",
                         IsUnionOfResidueClassesOfZ,
                         CanEasilySortElements, CanEasilySortElements ) );
SetUnderlyingRing( ZResidueClassUnionsFamily, Integers );
SetElementsFamily( ZResidueClassUnionsFamily, FamilyObj( 1 ) );

#############################################################################
##
#V  ZResidueClassUnionsWithFixedRepresentativesFamily .  same with fixed reps
##
InstallValue( ZResidueClassUnionsWithFixedRepresentativesFamily,
              NewFamily( "ResidueClassUnionsFamily( Integers, true )",
                         IsUnionOfResidueClassesWithFixedRepresentatives,
                         CanEasilySortElements, CanEasilySortElements ) );
SetUnderlyingRing( ZResidueClassUnionsWithFixedRepresentativesFamily,
                   Integers );
SetElementsFamily( ZResidueClassUnionsWithFixedRepresentativesFamily,
                   ZResidueClassUnionsWithFixedRepresentativesFamily );

#############################################################################
##
#F  Z_piResidueClassUnionsFamily( <R> , <fixedreps> )
##
InstallGlobalFunction( Z_piResidueClassUnionsFamily,

  function ( R, fixedreps )

    local  fam, cat, name;

    fam := First( Z_PI_RESIDUE_CLASS_UNIONS_FAMILIES,
                  fam ->     UnderlyingRing( fam ) = R
                         and PositionSublist( fam!.NAME,
                                              String(fixedreps) ) <> fail );
    if fam <> fail then return fam; fi;
    if   not fixedreps
    then cat := IsUnionOfResidueClassesOfZ_pi;
    else cat := IsUnionOfResidueClassesOfZ_piWithFixedRepresentatives; fi;
    name := Concatenation( "ResidueClassUnionsFamily( ",
                           String( R ),", ",String(fixedreps)," )" );
    fam := NewFamily( name, cat,
                      CanEasilySortElements, CanEasilySortElements );
    SetUnderlyingRing( fam, R );
    if not fixedreps then SetElementsFamily( fam, FamilyObj( 1 ) );
                     else SetElementsFamily( fam, fam ); fi;
    MakeReadWriteGlobal( "Z_PI_RESIDUE_CLASS_UNIONS_FAMILIES" );
    Add( Z_PI_RESIDUE_CLASS_UNIONS_FAMILIES, fam );
    MakeReadOnlyGlobal( "Z_PI_RESIDUE_CLASS_UNIONS_FAMILIES" );

    return fam;
  end );

#############################################################################
##
#F  GFqxResidueClassUnionsFamily( <R>, <fixedreps> )
##
InstallGlobalFunction( GFqxResidueClassUnionsFamily,

  function ( R, fixedreps )

    local  fam, cat, name, x;

    x := IndeterminatesOfPolynomialRing( R )[ 1 ];
    fam := First( GF_Q_X_RESIDUE_CLASS_UNIONS_FAMILIES,
                  fam -> UnderlyingRing( fam ) = R
                         and PositionSublist( fam!.NAME,
                                              String(fixedreps) ) <> fail );
    if fam <> fail then return fam; fi;
    if   not fixedreps
    then cat := IsUnionOfResidueClassesOfGFqx;
    else cat := IsUnionOfResidueClassesOfGFqxWithFixedRepresentatives; fi;
    name := Concatenation( "ResidueClassUnionsFamily( ",
                           String( R ),", ",String(fixedreps)," )" );
    fam := NewFamily( name, cat,
                      CanEasilySortElements, CanEasilySortElements );
    SetUnderlyingIndeterminate( fam, x );
    SetUnderlyingRing( fam, R );
    if not fixedreps then SetElementsFamily( fam, FamilyObj( x ) );
                     else SetElementsFamily( fam, fam ); fi;
    MakeReadWriteGlobal( "GF_Q_X_RESIDUE_CLASS_UNIONS_FAMILIES" );
    Add( GF_Q_X_RESIDUE_CLASS_UNIONS_FAMILIES, fam );
    MakeReadOnlyGlobal( "GF_Q_X_RESIDUE_CLASS_UNIONS_FAMILIES" );

    return fam;
  end );

#############################################################################
##
#F  ResidueClassUnionsFamily( <R> [ , <fixedreps> ]  )
##
InstallGlobalFunction( ResidueClassUnionsFamily,

  function ( arg )

    local  R, fixedreps;

    if   not Length(arg) in [1,2]
    or   Length(arg) = 2 and not arg[2] in [true,false]
    then Error("Usage: ResidueClassUnionsFamily( <R> [ , <fixedreps> ]\n");
    fi;
    R := arg[1];
    if Length(arg) = 2 then fixedreps := arg[2]; else fixedreps := false; fi;
    if   IsIntegers( R )
    then if   not fixedreps
         then return ZResidueClassUnionsFamily;
         else return ZResidueClassUnionsWithFixedRepresentativesFamily; fi;
    elif IsZ_pi( R )
    then return Z_piResidueClassUnionsFamily( R, fixedreps );
    elif IsUnivariatePolynomialRing( R ) and IsFiniteFieldPolynomialRing( R )
    then return GFqxResidueClassUnionsFamily( R, fixedreps );
    else Error("Sorry, residue class unions of ",R,
               " are not yet implemented.\n");
    fi;
  end );

#############################################################################
##
#M  String( <R> ) . . . . . for univariate polynomial ring over finite field
##
InstallMethod( String,
               Concatenation("for univariate polynomial rings ",
                             "over finite fields (ResClasses)"),
               true, [ IsUnivariatePolynomialRing ], 0,

  function ( R )

    local  F, q, x, IndNr, IndName;

    F := CoefficientsRing(R); q := Size(F);
    if not IsFinite(F) or F <> GF(q) then TryNextMethod(); fi;
    x := IndeterminatesOfPolynomialRing(R)[1];
    IndNr := IndeterminateNumberOfUnivariateLaurentPolynomial(x);
    IndName := IndeterminateName(FamilyObj(x),IndNr);
    if   IndName = fail
    then IndName := Concatenation("x_",String(IndNr)); fi;
    return Concatenation( "GF(", String(q), ")[", IndName, "]" );
  end );

#############################################################################
##
#M  ViewObj( <R> ) . . . . . for univariate polynomial ring over finite field
##
InstallMethod( ViewObj,
               Concatenation("for univariate polynomial rings ",
                             "over finite fields (ResClasses)"),
               true, [ IsUnivariatePolynomialRing ], 100,

  function ( R )

    local  F, q;

    F := CoefficientsRing(R); q := Size(F);
    if not IsFinite(F) or F <> GF(q) then TryNextMethod(); fi;
    Print(String(R));
  end );

# Buffer for storing already computed polynomial residue systems.

BindGlobal( "POLYNOMIAL_RESIDUE_CACHE", [] );

AllGFqPolynomialsModDegree := function ( q, d, x )

  local  erg, mon, gflist;

  if   d = 0
  then return [ Zero( x ) ];
  elif     IsBound( POLYNOMIAL_RESIDUE_CACHE[ q ] )
       and IsBound( POLYNOMIAL_RESIDUE_CACHE[ q ][ d ] )
  then return ShallowCopy( POLYNOMIAL_RESIDUE_CACHE[ q ][ d ] );
  else gflist := AsList( GF( q ) );
       mon := List( gflist, el -> List( [ 0 .. d - 1 ], i -> el * x^i ) );
       erg := List( Tuples( GF( q ), d ),
                    t -> Sum( List( [ 1 .. d ],
                                    i -> mon[ Position( gflist, t[ i ] ) ]
                                            [ d - i + 1 ] ) ) );
       MakeReadWriteGlobal( "POLYNOMIAL_RESIDUE_CACHE" );
       if not IsBound( POLYNOMIAL_RESIDUE_CACHE[ q ] )
       then POLYNOMIAL_RESIDUE_CACHE[ q ] := [ ]; fi;
       POLYNOMIAL_RESIDUE_CACHE[ q ][ d ] := Immutable( erg );
       MakeReadOnlyGlobal( "POLYNOMIAL_RESIDUE_CACHE" );
       return erg;
  fi;
end;
MakeReadOnlyGlobal( "AllGFqPolynomialsModDegree" );

#############################################################################
##
#F  AllResidues( <R>, <m> ) . . . . the residues (mod <m>) in canonical order
##
InstallGlobalFunction( AllResidues,

  function ( R, m )

    local  q, d, x;

    if   IsIntegers(R) or IsZ_pi(R)
    then return [0..StandardAssociate(R,m)-1];
    else q := Size(CoefficientsRing(R));
         d := DegreeOfLaurentPolynomial(m);
         x := IndeterminatesOfPolynomialRing(R)[1];
         return AllGFqPolynomialsModDegree(q,d,x);
    fi;
  end );

#############################################################################
##
#F  AllResidueClassesModulo( [ <R>, ] <m> ) . . the residue classes (mod <m>)
##
InstallGlobalFunction( AllResidueClassesModulo,

  function ( arg )

    local  R, m;

    if   Length(arg) = 2
    then R := arg[1]; m := arg[2];
    else m := arg[1]; R := DefaultRing(m); fi;
    if IsZero(m) or not m in R then return fail; fi;
    return List(AllResidues(R,m),r->ResidueClass(R,m,r));
  end );

#############################################################################
##
#F  AllResidueClassesWithFixedRepresentativesModulo( [ <R>, ] <m> )
#F  AllResidueClassesWithFixedRepsModulo( [ <R>, ] <m> )
##
InstallGlobalFunction( AllResidueClassesWithFixedRepresentativesModulo,

  function ( arg )

    local  R, m;

    if   Length(arg) = 2
    then R := arg[1]; m := arg[2];
    else m := arg[1]; R := DefaultRing(m); fi;
    if IsZero(m) or not m in R then return fail; fi;
    return List(AllResidues(R,m),r->ResidueClassWithFixedRep(R,m,r));
  end );

# Bring the residue class union <U> to normalized, reduced form.

ReduceResidueClassUnion := function ( U )

  local  R, m, r, mRed, mRedBuf, rRed, rRedBuf, valid, fact, p;

  R := UnderlyingRing(FamilyObj(U));
  m := StandardAssociate(R,U!.m);  mRed := m;
  r := List( U!.r, n -> n mod m ); rRed := r;
  fact := Set(Factors(R,m));
  for p in fact do
    repeat
      mRedBuf := mRed; rRedBuf := ShallowCopy(rRed);
      mRed := mRed/p;
      rRed := Set(List( rRedBuf, n -> n mod mRed ));
      if   IsIntegers(R) or IsZ_pi(R)
      then valid := Length(rRed) = Length(rRedBuf)/p;
      else valid := Length(rRed) = Length(rRedBuf)/
                    Size(CoefficientsRing(R))^DegreeOfLaurentPolynomial(p);
      fi;
    until not valid or not IsZero(mRed mod p) or IsOne(mRed);
    if not valid then mRed := mRedBuf; rRed := rRedBuf; fi;
  od;
  U!.m := mRed; U!.r := Immutable(rRed);
  U!.included := Immutable(Set(Filtered( U!.included,
                                         n -> not (n mod mRed in rRed) )));
  U!.excluded := Immutable(Set(Filtered( U!.excluded,
                                         n -> n mod mRed in rRed )));
  if rRed = [] then U := Difference(U!.included,U!.excluded); fi;
end;
MakeReadOnlyGlobal( "ReduceResidueClassUnion" );

#############################################################################
##
#M  ResidueClassUnionCons( <filter>, <R>, <m>, <r>, <included>, <excluded> )
##
InstallMethod( ResidueClassUnionCons,
               "sparse rep., for Z, Z_pi and GF(q)[x] (ResClasses)",
               ReturnTrue, [ IsUnionOfResidueClasses, IsRing, IsRingElement,
                             IsList, IsList, IsList ], 0,

  function ( filter, R, m, r, included, excluded )

    local  Result, both, fam, type, rep, pos;

    if not ( IsIntegers( R ) or IsZ_pi( R )
             or (     IsFiniteFieldPolynomialRing( R )
                  and IsUnivariatePolynomialRing( R ) ) )
    then TryNextMethod( ); fi;
    m := StandardAssociate( R, m );
    r := Set( List( r, n -> n mod m ) );
    both := Intersection( included, excluded );
    included := Set( Difference( included, both ) );
    excluded := Set( Difference( excluded, both ) );
    if r = [] then return Difference(included,excluded); fi;
    fam := ResidueClassUnionsFamily( R );
    if   IsIntegers( R )       then type := IsUnionOfResidueClassesOfZ;
    elif IsZ_pi( R )           then type := IsUnionOfResidueClassesOfZ_pi;
    elif IsPolynomialRing( R ) then type := IsUnionOfResidueClassesOfGFqx;
    fi;
    Result := Objectify( NewType( fam,
                                  type and IsResidueClassUnionSparseRep ),
                         rec( m := m, r := r,
                              included := included, excluded := excluded ) );
    SetSize( Result, infinity ); SetIsFinite( Result, false );
    SetIsEmpty( Result, false );
    rep := r[1]; pos := 1;
    while rep in excluded do
      pos := pos + 1;
      rep := r[pos mod Length(r) + 1] + Int(pos/Length(r)) * m;
    od;
    if   included <> [ ] and rep > Minimum( included ) 
    then rep := Minimum( included ); fi;
    SetRepresentative( Result, rep );
    ReduceResidueClassUnion( Result );
    if IsOne( Result!.m ) and Result!.r = [ Zero( R ) ]
      and [ Result!.included, Result!.excluded ] = [ [ ], [ ] ]
    then return R; else return Result; fi;
  end );

#############################################################################
##
#M  ResidueClassUnionWithFixedRepresentativesCons( <filter>, <R>, <classes> )
##
InstallMethod( ResidueClassUnionWithFixedRepresentativesCons,
               "for Z, Z_pi and GF(q)[x] (ResClasses)", ReturnTrue,
               [ IsUnionOfResidueClassesWithFixedRepresentatives,
                 IsRing, IsList ], 0,

  function ( filter, R, classes )

    local  Result, both, fam, type, rep, pos;

    if not ( IsIntegers( R ) or IsZ_pi( R )
             or (     IsFiniteFieldPolynomialRing( R )
                  and IsUnivariatePolynomialRing( R ) ) )
    then TryNextMethod( ); fi;
    fam := ResidueClassUnionsFamily( R, true );
    if   IsIntegers( R )
    then type := IsUnionOfResidueClassesOfZWithFixedRepresentatives;
    elif IsZ_pi( R )
    then type := IsUnionOfResidueClassesOfZ_piWithFixedRepresentatives;
    elif IsPolynomialRing( R )
    then type := IsUnionOfResidueClassesOfGFqxWithFixedRepresentatives; fi;
    Result := Objectify( NewType( fam,
                                  type and IsFixedRepResidueClassUnionRep ),
                         rec( classes := AsSortedList( classes ) ) );
    if classes <> [] then
      SetSize( Result, infinity ); SetIsFinite( Result, false );
      SetIsEmpty( Result, false );
      SetRepresentative( Result, classes[1][2] );
    else
      SetSize( Result, 0 ); SetIsEmpty( Result, true );
    fi;
    return Result;
  end );

#############################################################################
##
#F  ResidueClassUnion( <R>, <m>, <r> ) . . . . . . . union of residue classes
#F  ResidueClassUnion( <R>, <m>, <r>, <included>, <excluded> )
##
InstallGlobalFunction( ResidueClassUnion,

  function ( arg )

    local  Result, R, m, r, included, excluded;

    if not ( Length(arg) in [3,5] and IsRing(arg[1]) and arg[2] in arg[1]
             and IsList(arg[3]) and IsSubset(arg[1],arg[3])
             and (Length(arg) = 3 or (IsList(arg[4]) and IsList(arg[5])
                  and IsSubset(arg[1],arg[4]) and IsSubset(arg[1],arg[5]))) )
    then Error("usage: ResidueClassUnion( <R>, <m>, <r> [, <included>",
               ", <excluded>] ),\nfor details see manual.\n"); return fail;
    fi;
    R := arg[1]; m := arg[2]; r := Set(arg[3]);
    if   Length(arg) = 5
    then included := Set(arg[4]); excluded := Set(arg[5]);
    else included := [];          excluded := []; fi;
    return ResidueClassUnionCons( IsUnionOfResidueClasses, R, m, r,
                                  included, excluded );
  end );

#############################################################################
##
#F  ResidueClassUnionWithFixedRepresentatives( <R>, <classes> )
#F  ResidueClassUnionWithFixedRepresentatives( <classes> )
#F  ResidueClassUnionWithFixedReps( <R>, <classes> )
#F  ResidueClassUnionWithFixedReps( <classes> )
##
InstallGlobalFunction( ResidueClassUnionWithFixedRepresentatives,

  function ( arg )

    local  R, classes, usage;

    usage := Concatenation("usage: ResidueClassUnionWithFixedRepresenta",
                           "tives( [ <R>, ] <classes> )\n");
    if not Length(arg) in [1,2] then Error(usage); return fail; fi;
    if Length(arg) = 2 then R := arg[1];   classes := arg[2];
                       else R := Integers; classes := arg[1]; fi;
    if   not IsIntegers(R) and not IsZ_pi(R) and not IsPolynomialRing(R)
      or not IsList(classes) or not ForAll(classes,IsList)
      or not ForAll(classes,cl->Length(cl)=2)
      or not IsSubset(R,Flat(classes))
      or IsZero(Product(List(classes,cl->cl[1])))
    then Error(usage); return fail; fi;
    return ResidueClassUnionWithFixedRepresentativesCons(
             IsUnionOfResidueClassesWithFixedRepresentatives, R, classes );
  end );

#############################################################################
##
#F  ResidueClass( <R>, <m>, <r> ) . . . . . . . . . . .  residue class of <R>
#F  ResidueClass( <m>, <r> )  . . . . . . . . . residue class of the integers
#F  ResidueClass( <r>, <m> )  . . . . . . . . . . . . . . . . . . .  ( dito )
##
InstallGlobalFunction( ResidueClass,

  function ( arg )

    local  R, m, r;

    if Length(arg) = 3 then
      R := arg[1]; m := arg[2]; r := arg[3];
      if   not ( IsRing(R) and m in R and r in R )
      then Error( "usage: see ?ResidueClass\n"); fi;
    elif Length(arg) = 2 then
      if   not (ForAll(arg,IsInt) and Minimum(arg) >= 0 and Maximum(arg) > 0)
      then Error( "usage: see ?ResidueClass\n"); fi;
      R := Integers; m := Maximum(arg); r := Minimum(arg);
    else
     Error( "usage: see ?ResidueClass\n");
    fi;
    return ResidueClassUnion( R, m, [ r ] );
  end );

#############################################################################
##
#F  ResidueClassWithFixedRepresentative( <R>, <m>, <r> )  same with fixed rep
#F  ResidueClassWithFixedRepresentative( <m>, <r> )
#F  ResidueClassWithFixedRep( <R>, <m>, <r> )
#F  ResidueClassWithFixedRep( <m>, <r> )
##
InstallGlobalFunction( ResidueClassWithFixedRepresentative,

  function ( arg )

    local  R, m, r, usage;

    usage := Concatenation("usage: ResidueClassWithFixedRepresentative",
                           "( [ <R>, ] <m>, <r> ) for a ring <R> and ",
                           "elements <m> and <r>.\n");
    if not Length(arg) in [2,3] then Error(usage); return fail; fi;
    if Length(arg) = 3 then R := arg[1];   m := arg[2]; r := arg[3];
                       else R := Integers; m := arg[1]; r := arg[2]; fi;
    if not ( IsRing(R) and m in R and r in R )
    then Error(usage); return fail; fi;
    return ResidueClassUnionWithFixedRepresentatives( R, [ [ m, r ] ] );
  end );

#############################################################################
##
#M  Modulus( <U> ) . . . . . . . . . . . . . . . . .  for residue class union
##
InstallMethod( Modulus,
               "for residue class unions (ResClasses)", true,
               [ IsResidueClassUnionInSparseRep ], 0, U -> U!.m );

#############################################################################
##
#M  Modulus( <U> ) . . . . . . . . .  for residue class union with fixed reps
##
InstallMethod( Modulus,
               "for residue class unions with fixed reps (ResClasses)", true,
               [ IsUnionOfResidueClassesWithFixedRepresentatives ], 0,
               U -> Lcm( UnderlyingRing( FamilyObj( U ) ),
                         List( U!.classes, cl -> cl[1] ) ) );

#############################################################################
##
#M  Modulus( <R> ) . . . . . . . . . . . . . . . . . . . . . .  for base ring
##
InstallOtherMethod( Modulus,
                    "for base ring (ResClasses)", true,
                    [ IsRing ], 0, R -> One( R ) );

#############################################################################
##
#M  Residues( <U> ) . . . . . . . . . . . . . . . . . for residue class union
##
InstallMethod( Residues,
               "for residue class unions (ResClasses)", true,
               [ IsResidueClassUnionInSparseRep ], 0, U -> U!.r );

#############################################################################
##
#M  Residues( <R> ) . . . . . . . . . . . . . . . . . . . . . . for base ring
##
InstallOtherMethod( Residues,
                    "for base ring (ResClasses)", true,
                    [ IsRing ], 0, R -> [ Zero( R ) ] );

#############################################################################
##
#M  Residues( <l> ) . . . . . . . . . . . . . . . for finite list of elements
##
InstallOtherMethod( Residues,
                    "for finite list of elements (ResClasses)", true,
                    [ IsList ], 0, l -> [  ] );

#############################################################################
##
#M  IncludedElements( <U> ) . . . . . . . . . . . . . for residue class union
##
InstallMethod( IncludedElements,
               "for residue class unions (ResClasses)", true,
               [ IsResidueClassUnionInSparseRep ], 0, U -> U!.included );

#############################################################################
##
#M  IncludedElements( <R> ) . . . . . . . . . . . . . . . . . . for base ring
##
InstallOtherMethod( IncludedElements,
                    "for base ring (ResClasses)", true, [ IsRing ], 0,
                    R -> [ ] );

#############################################################################
##
#M  IncludedElements( <l> ) . . . . . . . . . . . for finite list of elements
##
InstallOtherMethod( IncludedElements,
                    "for finite list of elements (ResClasses)", true,
                    [ IsList ], 0, l -> l );

#############################################################################
##
#M  ExcludedElements( <U> ) . . . . . . . . . . . . . for residue class union
##
InstallMethod( ExcludedElements,
               "for residue class unions (ResClasses)", true,
               [ IsResidueClassUnionInSparseRep ], 0, U -> U!.excluded );

#############################################################################
##
#M  ExcludedElements( <R> ) . . . . . . . . . . . . . . . . . . for base ring
##
InstallOtherMethod( ExcludedElements,
                    "for base ring (ResClasses)", true, [ IsRing ], 0,
                    R -> [ ] );

#############################################################################
##
#M  ExcludedElements( <l> ) . . . . . . . . . . . for finite list of elements
##
InstallOtherMethod( ExcludedElements,
                    "for finite list of elements (ResClasses)", true,
                    [ IsList ], 0, l -> [ ] );

#############################################################################
##
#M  Classes( <U> ) . . . . . . . . .  for residue class union with fixed reps
##
InstallMethod( Classes,
               "for residue class unions with fixed reps (ResClasses)", true,
               [ IsUnionOfResidueClassesWithFixedRepresentatives ], 0,
               U -> U!.classes );

#############################################################################
##
#M  AsUnionOfFewClasses( <U> ) . . . . . .  for pure residue class union of Z
##
InstallMethod( AsUnionOfFewClasses,
               "for pure residue class unions of Z (ResClasses)", true,
               [ IsUnionOfResidueClassesOfZ ], 0,

  function ( U )

    local  S, Si, remaining, m, res, div, d, pos;

    m := Modulus(U); res := Residues(U);
    S := []; remaining := U;
    div := DivisorsInt(m);
    for d in div do
      if m mod d <> 0 then continue; fi; pos := 1;
      while pos <= Length(res) do
        if IsSubset(res,List([1..m/d],i->(i-1)*d+(res[pos] mod d))) then
          Si := ResidueClass(Integers,d,res[pos]);
          Add(S,Si); remaining := Difference(remaining,Si);
          if IsList(remaining) then break; fi;
          m := Modulus(remaining); res := Residues(remaining); pos := 0;
          if m mod d <> 0 then break; fi;
        fi;
        pos := pos + 1;
      od;
      if IsList(remaining) then break; fi;
    od;
    return S;
  end );

#############################################################################
##
#M  AsUnionOfFewClasses( [  ] ) . . . . . . . . . . . . . . . . for empty set
##
InstallOtherMethod( AsUnionOfFewClasses,
                    "for empty set (ResClasses)", true,
                    [ IsList and IsEmpty ], 0, l -> [  ] );

#############################################################################
##
#M  AsUnionOfFewClasses( Integers ) . . . . . . . . . . . . . . . . . . for Z
##
InstallOtherMethod( AsUnionOfFewClasses,
                    "for Z (ResClasses)", true, [ IsIntegers ], 0,
                    ints -> [ Integers ] );

#############################################################################
##
#M  SplittedClass( <cl>, <t> ) . . write <cl> as union of <t> residue classes
##
InstallMethod( SplittedClass,
               "for residue class of Z or Z_pi (ResClasses)", true,
               [ IsUnionOfResidueClassesOfZorZ_pi, IsPosInt ], 0,

  function ( cl, t )

    local  R, r, m;

    R := UnderlyingRing(FamilyObj(cl));
    if Length(Residues(cl)) > 1 or (IsZ_pi(R) and not
       IsSubset(Union(NoninvertiblePrimes(R),[1]),Set(Factors(t))))
    then return fail; fi;
    r := Residues(cl)[1]; m := Modulus(cl);
    return List([0..t-1],k->ResidueClass(R,t*m,k*m+r));
  end );

#############################################################################
##
#M  AsListOfClasses( <U> ) . . . . .  for residue class union with fixed reps
##
InstallMethod( AsListOfClasses,
               "for residue class unions with fixed reps (ResClasses)", true,
               [ IsUnionOfResidueClassesWithFixedRepresentatives ], 0,

  U -> SortedList( List( Classes( U ),
                   cl -> ResidueClassWithFixedRepresentative(
                   UnderlyingRing( FamilyObj( U ) ), cl[1], cl[2] ) ) ) );

#############################################################################
##
#M  AsOrdinaryUnionOfResidueClasses( <U> )
##
InstallMethod( AsOrdinaryUnionOfResidueClasses,
               "for residue class unions with fixed reps (ResClasses)",
               true, [ IsUnionOfResidueClassesWithFixedRepresentatives ], 0,

  function ( U )

    local  R, cl;

    R := UnderlyingRing(FamilyObj(U));
    return Union(List(Classes(U),cl->ResidueClass(R,cl[1],cl[2])));
  end );

#############################################################################
##
#M  IsOverlappingFree( <U> ) . . . .  for residue class union with fixed reps
##
InstallMethod( IsOverlappingFree,
               "for residue class unions with fixed reps (ResClasses)",
               true, [ IsUnionOfResidueClassesWithFixedRepresentatives ], 0,
               U ->    Density( U )
                     = Density( AsOrdinaryUnionOfResidueClasses( U ) ) );

#############################################################################
##
#M  ViewObj( <U> ) . . . . . . . . . . . . . . . . .  for residue class union
##
InstallMethod( ViewObj,
               "for residue class unions (ResClasses)", true,
               [ IsUnionOfResidueClasses ], 0,

  function ( U )

    local  R, m, r, included, excluded, PrintFiniteSet, n, cl, endval, short;

    PrintFiniteSet := function ( S )
      if   Length(String(S)) <= 32
      then Print(S);
      else Print("<set of cardinality ",Length(S),">"); fi;
    end;

    short := RESCLASSES_VIEWING_FORMAT = "short";
    R := UnderlyingRing(FamilyObj(U)); m := Modulus(U); r := Residues(U);
    included := IncludedElements(U); excluded := ExcludedElements(U);
    if   IsIntegers(R) and Length(r) < 20 and m < 1000
    then cl := AsUnionOfFewClasses(U); fi;
    if   IsIntegers(R) and (IsBound(cl) and Length(cl) < 6 or Length(r) < 6)
      or Length(r) = 1
    then
      if IsOne(m) then
        Print(RingToString(R)," \\ "); PrintFiniteSet(excluded);
      else
        if Length(r) > 1 then
          if not short then
            if included <> [] or excluded <> [] then Print("("); fi;
            Print("Union of the residue classes ");
          fi;
          if IsBound(cl) then
            endval := Length(cl) - 1;
            for n in [1..endval] do
              Print(Residues(cl[n])[1],"(",Modulus(cl[n]),")");
              if n < endval then
                if short then Print(" U "); else Print(", "); fi;
              fi;
            od;
            if short then Print(" U "); else Print(" and "); fi;
          else
            for n in [1..Length(r)-1] do
              Print(r[n],"(",m,")");
              if n < Length(r) - 1 then
                if short then Print(" U "); else Print(", "); fi;
              fi;
            od;
            if short then Print(" U "); else Print(" and "); fi;
          fi;
        else
          if not short then
            if included <> [] or excluded <> [] then Print("("); fi;
            Print("The residue class ");
          fi;
        fi;
        if   IsIntegers(R) or IsZ_pi(R)
        then if   IsBound(cl)
             then Print(Residues(cl[Length(cl)])[1],"(",
                        Modulus(cl[Length(cl)]),")");
             else Print(r[Length(r)],"(",m,")"); fi;
        else Print(r[Length(r)]," ( mod ",m," )"); fi;
        if not short then Print(" of ",RingToString(R)); fi;
        if included <> [] then
          if short then Print(" U "); else Print(") U "); fi;
          PrintFiniteSet(included);
        fi;
        if excluded <> [] then
          if   short or included <> [] then Print(" \\ ");
          else Print(") \\ "); fi;
          PrintFiniteSet(excluded);
        fi;
      fi;
    else
      Print("<union of ",Length(r)," residue classes (mod ",m,")");
      if not short then Print(" of ",RingToString(R)); fi;
      Print(">");
      if included <> [] then Print(" U ");  PrintFiniteSet(included); fi;
      if excluded <> [] then Print(" \\ "); PrintFiniteSet(excluded); fi;
    fi;
  end ); 

#############################################################################
##
#M  ViewObj( <U> ) . . . . . . . . .  for residue class union with fixed reps
##
InstallMethod( ViewObj,
               "for residue class unions with fixed reps (ResClasses)", true,
               [ IsUnionOfResidueClassesWithFixedRepresentatives ], 0,

  function ( U )

    local  R, classes, l, i, short;

    short := RESCLASSES_VIEWING_FORMAT = "short";
    R := UnderlyingRing(FamilyObj(U));
    classes := Classes(U); l := Length(classes);
    if l = 0 or l > 8 or IsPolynomialRing(R) and l > 3 then
      if l = 0 then
        if not short then
          Print("Empty union of residue classes of ",RingToString(R),
                " with fixed representatives");
        else
          Print("[]");
        fi;
      else
        Print("<union of ",l," residue classes");
        if   not short
        then Print(" of ",RingToString(R)," with fixed representatives>");
        else Print(" with fixed rep's>"); fi;
      fi;
    else
      for i in [1..l] do
        if i > 1 then Print(" U "); fi;
        Print("[",classes[i][2],"/",classes[i][1],"]");
      od;
    fi;
  end );

#############################################################################
##
#M  String( <U> ) . . . . . . . . . . . . . . . . . . for residue class union
##
InstallMethod( String,
               "for residue class unions (ResClasses)", true,
               [ IsUnionOfResidueClasses ], 0,

  function ( U )

    local  s, R, m, r, included, excluded;

    R := UnderlyingRing(FamilyObj(U)); m := Modulus(U); r := Residues(U);
    included := IncludedElements(U); excluded := ExcludedElements(U);
    s := Concatenation("ResidueClassUnion( ",String(R),", ",
                       String(Modulus(U)),", ",String(Residues(U)));
    if   included <> [] or excluded <> []
    then s := Concatenation(s,", ",String(included),", ",String(excluded));
    fi;
    s := Concatenation(s," )");
    return s;
  end );

#############################################################################
##
#M  String( <U> ) . . . . . . . . . . for residue class union with fixed reps
##
InstallMethod( String,
               "for residue class unions with fixed reps (ResClasses)", true,
               [ IsUnionOfResidueClassesWithFixedRepresentatives ], 0,

  function ( U )

    local  R, classes;

    R := UnderlyingRing(FamilyObj(U)); classes := Classes(U);
    if   Length(classes) = 1
    then return Concatenation("ResidueClassWithFixedRepresentative( ",
                              String(R),", ",String(classes[1][1]),", ",
                              String(classes[1][2])," )");
    else return Concatenation("ResidueClassUnionWithFixedRepresentatives( ",
                              String(R),", ",String(classes)," )");
    fi;
  end );

#############################################################################
##
#M  PrintObj( <U> ) . . . . . . . . . . . . . . . . . for residue class union
##
InstallMethod( PrintObj,
               "for residue class unions (ResClasses)", true,
               [ IsUnionOfResidueClasses ], 0,

 function ( U )

    local  R, m, r, included, excluded, n;

    R := UnderlyingRing(FamilyObj(U)); m := Modulus(U); r := Residues(U);
    included := IncludedElements(U); excluded := ExcludedElements(U);
    Print("ResidueClassUnion( ",String(R),", ",Modulus(U),", ",Residues(U));
    if   included <> [] or excluded <> []
    then Print(", ",included,", ",excluded); fi;
    Print(" )");
  end );

#############################################################################
##
#M  PrintObj( <U> ) . . . . . . . . . for residue class union with fixed reps
##
InstallMethod( PrintObj,
               "for residue class unions with fixed reps (ResClasses)", true,
               [ IsUnionOfResidueClassesWithFixedRepresentatives ], 0,

  function ( U )

    local  R, classes;

    R := UnderlyingRing(FamilyObj(U)); classes := Classes(U);
    if Length(classes) = 1 then
      Print("ResidueClassWithFixedRepresentative( ",String(R),", ",
            classes[1][1],", ",classes[1][2]," )");
    else
      Print("ResidueClassUnionWithFixedRepresentatives( ",String(R),", ",
            classes," )");
    fi;
  end );

# Display a list of ring elements.

DisplayArray := function ( l )

  local ellng, elperline, sign, i;

  Print("\n\n");
  ellng     := Maximum(List(l,n->Length(String(n)))) + 1;
  elperline := Int((SizeScreen()[1]-3)/ellng);
  if IsRat(l[1]) then sign := 1; else sign := -1; fi;
  for i in [1..Length(l)] do
    Print(String(l[i],sign*ellng));
    if i mod elperline = 0 and i < Length(l) then Print("\n"); fi;
  od;
  Print("\n\n");
end;
MakeReadOnlyGlobal( "DisplayArray" );

#############################################################################
##
#M  Display( <U> ) . . . . . . . . . . . . . . . . .  for residue class union
##
InstallMethod( Display,
               "for residue class unions (ResClasses)", true,
               [ IsUnionOfResidueClasses ], 0,

  function ( U )

    local  R, m, r, included, excluded, plin, plex;

    R := UnderlyingRing(FamilyObj(U)); m := Modulus(U); r := Residues(U);
    included := IncludedElements(U); excluded := ExcludedElements(U);
    if Length(included) > 1 then plin := "s"; else plin := ""; fi;
    if Length(excluded) > 1 then plex := "s"; else plex := ""; fi;
    if IsOne(m) then
      Print(RingToString(R)," \\ ",excluded,"\n"); return;
    elif Length(r) = 1 then
      if [included,excluded] <> [[],[]] then Print("\n"); fi;
      Print("The residue class ",r[1]," ( mod ",m," )");
      Print(" of ",RingToString(R),"\n");
      if [included,excluded] <> [[],[]] then Print("\n"); fi;
    else
      Print("\nThe union of the residue classes r ( mod ",m," ) ");
      Print("of ",RingToString(R));
      Print(" for r ="); DisplayArray(r);
    fi;
    if   included <> []
    then Print("and the element",plin); DisplayArray(included); fi;
    if   excluded <> []
    then Print("without the element",plex); DisplayArray(excluded); fi;
  end );

#############################################################################
##
#M  Display( <U> ) . . . . . . . . .  for residue class union with fixed reps
##
InstallMethod( Display,
               "for residue class unions with fixed reps (ResClasses)", true,
               [ IsUnionOfResidueClassesWithFixedRepresentatives ], 0,

  function ( U )

    local  R, classes, l, i;

    R := UnderlyingRing(FamilyObj(U));
    classes := Classes(U); l := Length(classes);
    if l = 0 then View(U); else
      for i in [1..l] do
          if i > 1 then Print(" U "); fi;
          Print("[",classes[i][2],"/",classes[i][1],"]");
      od;
    fi;
    Print("\n");
  end );

#############################################################################
##
#M  \=( <U1>, <U2> ) . . . . . . . . . . . . . . . . for residue class unions
##
InstallMethod( \=,
               "for two residue class unions (ResClasses)", IsIdenticalObj,
               [ IsResidueClassUnionInSparseRep,
                 IsResidueClassUnionInSparseRep ], 0,

  function ( U1, U2 )
    return U1!.m = U2!.m and U1!.r = U2!.r
           and U1!.included = U2!.included and U1!.excluded = U2!.excluded;
  end );

#############################################################################
##
#M  \=( <U1>, <U2> ) . . . . . . . . for residue class unions with fixed reps
##
InstallMethod( \=,
               "for two residue class unions with fixed reps (ResClasses)",
               IsIdenticalObj,
               [ IsUnionOfResidueClassesWithFixedRepresentatives,
                 IsUnionOfResidueClassesWithFixedRepresentatives ], 0,

  function ( U1, U2 )
    return U1!.classes = U2!.classes;
  end );

#############################################################################
##
#M  \<( <U1>, <U2> ) . . . . . . . . . . . . . . . . for residue class unions
##
##  Total ordering of residue class unions (for tech. purposes, only).
##
InstallMethod( \<,
               "for two residue class unions (ResClasses)", IsIdenticalObj,
               [ IsResidueClassUnionInSparseRep,
                 IsResidueClassUnionInSparseRep ], 0,

  function ( U1, U2 )
    if   U1!.m <> U2!.m then return U1!.m < U2!.m;
    elif U1!.r <> U2!.r then return U1!.r < U2!.r;
    elif U1!.included <> U2!.included then return U1!.included < U2.included;
    else return U1!.excluded < U2!.excluded; fi;
  end );

#############################################################################
##
#M  \<( <U1>, <U2> ) . . . . . . . . for residue class unions with fixed reps
##
##  Total ordering of residue class unions with fixed representatives
##  (for tech. purposes, only).
##
InstallMethod( \<,
               "for two residue class unions with fixed reps (ResClasses)",
               IsIdenticalObj,
               [ IsUnionOfResidueClassesWithFixedRepresentatives,
                 IsUnionOfResidueClassesWithFixedRepresentatives ], 0,
               function ( U1, U2 ) return U1!.classes < U2!.classes; end );

#############################################################################
##
#M  \in( <n>, <U> ) . . . . . . . .  for ring element and residue class union
##
InstallMethod( \in,
               "for ring element and residue class union (ResClasses)",
               ReturnTrue, [ IsRingElement, IsResidueClassUnionInSparseRep ],
               0,

  function ( n, U )
    if not n in UnderlyingRing(FamilyObj(U)) then return false; fi;
    if   n in U!.included then return true;
    elif n in U!.excluded then return false;
    else return n mod U!.m in U!.r; fi;
  end );

#############################################################################
##
#M  \in( <cl>, <U> )  for residue class & residue class union with fixed reps
##
InstallMethod( \in,
               Concatenation("for residue class & residue class union ",
                             "with fixed reps (ResClasses)"), ReturnTrue,
               [ IsUnionOfResidueClassesWithFixedRepresentatives,
                 IsUnionOfResidueClassesWithFixedRepresentatives ], 0,

  function ( cl, U )
    if Length(cl!.classes) > 1 then TryNextMethod(); fi;
    return cl!.classes[1] in U!.classes;
  end );

#############################################################################
##
#M  Multiplicity( <n>, <U> )
##
InstallMethod( Multiplicity,
               Concatenation("for ring element and residue class union ",
                             "with fixed reps (ResClasses)"), ReturnTrue,
               [ IsRingElement,
                 IsUnionOfResidueClassesWithFixedRepresentatives ], 0,

  function ( n, U )
    if not n in UnderlyingRing(FamilyObj(U)) then return 0; fi;
    return Number(U!.classes,cl->n mod cl[1] = cl[2]);
  end );

#############################################################################
##
#M  Density( <l> ) . . . . . . . . . . . . . . . . . . . . . . for empty list
##
InstallOtherMethod( Density,
                    "for empty list (ResClasses)", true,
                    [ IsList and IsEmpty ], 0, l -> 0 );

#############################################################################
##
#M  Density( <l> ) . . . . . . . . . . . . . . .  for finite list of elements
##
InstallOtherMethod( Density,
                   "for finite list of elements (ResClasses)", true,
                   [ IsList and IsCollection ], 0,

  function ( l )
    if   not IsFinite(DefaultRing(l[1]))
    then return 0; else TryNextMethod(); fi;
  end );

#############################################################################
##
#M  Density( <R> ) . . . . . . . . . . . . . . . . . . .  for whole base ring
##
InstallOtherMethod( Density,
                    "for whole base ring (ResClasses)", true,
                    [ IsRing ], 0, R -> 1 );

#############################################################################
##
#M  Density( <U> ) . . . . . . . . . . . . . . . . . for residue class unions
##
InstallMethod( Density,
               "for residue class unions (ResClasses)", true,
               [ IsResidueClassUnionInSparseRep ], 0,

  function ( U )
    return Length(U!.r)/
           Length(AllResidues(UnderlyingRing(FamilyObj(U)),U!.m));
  end );

#############################################################################
##
#M  Density( <U> ) . . . . . . . . .  for residue class union with fixed reps
##
InstallOtherMethod( Density,
                   "for residue class unions with fixed reps (ResClasses)",
                   true, [ IsUnionOfResidueClassesWithFixedRepresentatives ],
                   0,

  function ( U )

    local  R;

    R := UnderlyingRing(FamilyObj(U));
    return Sum(List(U!.classes,c->1/Length(AllResidues(R,c[1]))));
  end );

#############################################################################
##
#M  Union2( <U1>, <U2> ) . . . . . . . . . . . . . . for residue class unions
##
InstallMethod( Union2,
               "for two residue class unions (ResClasses)", IsIdenticalObj,
               [ IsResidueClassUnionInSparseRep,
                 IsResidueClassUnionInSparseRep ], 0,

  function ( U1, U2 )

    local  R, m1, m2, m, r1, r2, r, included, excluded, allres;

    R := UnderlyingRing(FamilyObj(U1));
    m1 := U1!.m; m2 := U2!.m; m := Lcm(R,m1,m2);
    r1 := U1!.r; r2 := U2!.r;
    included := Union(U1!.included,U2!.included);
    excluded := Difference(Union(Difference(U1!.excluded,U2),
                                 Difference(U2!.excluded,U1)),included);
    allres := AllResidues(R,m);
    r := Filtered(allres,n->n mod m1 in r1 or n mod m2 in r2);
    return ResidueClassUnion(R,m,r,included,excluded);
  end );

#############################################################################
##
#M  Union2( <U1>, <U2> ) . . . . . . for residue class unions with fixed reps
##
InstallMethod( Union2,
               "for two residue class unions with fixed reps (ResClasses)",
               IsIdenticalObj,
               [ IsUnionOfResidueClassesWithFixedRepresentatives,
                 IsUnionOfResidueClassesWithFixedRepresentatives ], 0,

  function ( U1, U2 )

    local  R;

    R := UnderlyingRing(FamilyObj(U1));
    return ResidueClassUnionWithFixedRepresentatives(R,
             Concatenation(Classes(U1),Classes(U2)));
  end );

#############################################################################
##
#M  Intersection2( <U1>, <U2> ) . . . . . . . . . .  for residue class unions
##
InstallMethod( Intersection2,
               "for two residue class unions (ResClasses)", IsIdenticalObj,
               [ IsResidueClassUnionInSparseRep,
                 IsResidueClassUnionInSparseRep ], 0,

  function ( U1, U2 )

    local  R, m1, m2, m, r1, r2, r, included, excluded, gcd, allres;

    R := UnderlyingRing(FamilyObj(U1));
    m1 := U1!.m; m2 := U2!.m; m := Lcm(R,m1,m2);
    r1 := U1!.r; r2 := U2!.r;
    included := Union(U1!.included,U2!.included);
    included := Filtered(included,n -> n in U1!.included and n mod m2 in r2
                                    or n in U2!.included and n mod m1 in r1);
    included := Union(included,Intersection(U1!.included,U2!.included));
    excluded := Union(U1!.excluded,U2!.excluded);
    if IsIntegers(R) and m * Gcd(m1,m2) > 100 * Length(r1) * Length(r2) then
      gcd := Gcd(m1,m2);
      r := List(Filtered(Cartesian(r1,r2),t1->(t1[1]-t1[2]) mod gcd = 0),
                t2->ChineseRem([m1,m2],t2));
    else
      allres := AllResidues(R,m);
      r := Filtered(allres,n->n mod m1 in r1 and n mod m2 in r2);
    fi;
    return ResidueClassUnion(R,m,r,included,excluded);
  end );

#############################################################################
##
#M  Intersection2( <U1>, <U2> ) . .  for residue class unions with fixed reps
##
InstallMethod( Intersection2,
               "for two residue class unions with fixed reps (ResClasses)",
               IsIdenticalObj,
               [ IsUnionOfResidueClassesWithFixedRepresentatives,
                 IsUnionOfResidueClassesWithFixedRepresentatives ], 0,

  function ( U1, U2 )

    local  R, cls, cls1, cls2, int, cl;

    R := UnderlyingRing(FamilyObj(U1));
    cls1 := Classes(U1); cls2 := Classes(U2); int := Intersection(cls1,cls2);
    cls := Concatenation(List(int,cl->ListWithIdenticalEntries(
           Minimum(Number(cls1,cl1->cl1=cl),Number(cls2,cl2->cl2=cl)),cl)));
    return ResidueClassUnionWithFixedRepresentatives( R, cls );
  end );

#############################################################################
##
#M  Difference( <U1>, <U2> ) . . . . . . . . . . . . for residue class unions
##
InstallMethod( Difference,
               "for two residue class unions (ResClasses)", IsIdenticalObj,
               [ IsResidueClassUnionInSparseRep,
                 IsResidueClassUnionInSparseRep ], 0,

  function ( U1, U2 )

    local  R, m1, m2, m, r1, r2, r, included, excluded, allres;

    R := UnderlyingRing(FamilyObj(U1));
    m1 := U1!.m; m2 := U2!.m; m := Lcm(R,m1,m2);
    r1 := U1!.r; r2 := U2!.r;
    included := Union(U1!.included,U2!.excluded);
    included := Filtered(included,
                         n -> n in U1!.included and not n mod m2 in r2
                           or n in U2!.excluded and n mod m1 in r1);
    excluded := Union(U1!.excluded,U2!.included);
    allres := AllResidues(R,m);
    r := Filtered(allres,n->n mod m1 in r1 and not n mod m2 in r2);
    return ResidueClassUnion(R,m,r,included,excluded);
  end );

#############################################################################
##
#M  Difference( <U1>, <U2> ) . . . . for residue class unions with fixed reps
##
InstallMethod( Difference,
               "for two residue class unions with fixed reps (ResClasses)",
               IsIdenticalObj,
               [ IsUnionOfResidueClassesWithFixedRepresentatives,
                 IsUnionOfResidueClassesWithFixedRepresentatives ], 0,

  function ( U1, U2 )

    local  Multiple, R, cls, classes, numbers, diffcls;

    Multiple := function ( cl, k )
      if   k < 0
      then return ListWithIdenticalEntries(-k,[cl[1],cl[1]-cl[2]]);
      else return ListWithIdenticalEntries(k,cl); fi;
    end;

    R       := UnderlyingRing(FamilyObj(U1));
    cls     := [Classes(U1),Classes(U2)];
    classes := Set(Union(cls));
    numbers := List(classes,cl1->List(cls,list->Number(list,cl2->cl2=cl1)));
    numbers := TransposedMat(numbers); numbers := numbers[1] - numbers[2];
    diffcls := Concatenation(List([1..Length(numbers)],
                                  i->Multiple(classes[i],numbers[i])));
    return ResidueClassUnionWithFixedRepresentatives( R, diffcls );
  end );

#############################################################################
##
#M  Union2( <U>, <S> ) . . . . . . . . for residue class union and finite set
##
InstallMethod( Union2,
               "for residue class union and finite set (ResClasses)",
               ReturnTrue, [ IsResidueClassUnionInSparseRep, IsList ], 0,

  function ( U, S )
    if not IsSubset(UnderlyingRing(FamilyObj(U)),S) then TryNextMethod(); fi;
    return ResidueClassUnion(UnderlyingRing(FamilyObj(U)),U!.m,U!.r,
                             Union(U!.included,S),Difference(U!.excluded,S));
  end );

#############################################################################
##
#M  Union2( <S>, <U> ) . . . . . . . . for finite set and residue class union
##
InstallMethod( Union2,
               "for finite set and residue class union (ResClasses)",
               ReturnTrue, [ IsList, IsUnionOfResidueClasses ], 0,
               function ( S, U ) return Union2( U, S ); end );

#############################################################################
##
#M  Union2( <S1>, <S2> ) . . . . . . . . . . . . . . . . . for set and subset
##
InstallMethod( Union2,
               "for set and subset (ResClasses)", ReturnTrue,
               [ IsListOrCollection, IsListOrCollection ], 20,

  function ( S1, S2 )
    if not ForAny([S1,S2],IsRing)   
        or ForAny([S1,S2],IsUnionOfResidueClassesWithFixedRepresentatives)
    then TryNextMethod(); fi;
    if   IsSubset(S1,S2) then return S1;
    elif IsSubset(S2,S1) then return S2;
    else TryNextMethod(); fi;
  end );

#############################################################################
##
#M  Intersection2( <U>, <S> ) . . . .  for residue class union and finite set
##
InstallMethod( Intersection2,
               "for residue class union and finite set (ResClasses)",
               ReturnTrue, [ IsResidueClassUnionInSparseRep, IsList ], 0,

  function ( U, S )
    if not IsSubset(UnderlyingRing(FamilyObj(U)),S) then TryNextMethod(); fi;
    return Filtered( Set(S), n -> n in U!.included
                        or ( n mod U!.m in U!.r and not n in U!.excluded ) );
  end );

#############################################################################
##
#M  Intersection2( <S>, <U> ) . . . .  for finite set and residue class union
##
InstallMethod( Intersection2,
               "for finite set and residue class union (ResClasses)",
               ReturnTrue, [ IsList, IsUnionOfResidueClasses ], 0,
               function ( S, U ) return Intersection2( U, S ); end );

#############################################################################
##
#M  Intersection2( <U>, <R> ) . . . . . for residue class union and base ring
##
InstallMethod( Intersection2,
               "for residue class union and base ring (ResClasses)",
               ReturnTrue, [ IsUnionOfResidueClasses, IsRing ], 0,

  function ( U, R )
    if not UnderlyingRing(FamilyObj(U)) = R then TryNextMethod(); fi;
    return U;
  end );

#############################################################################
##
#M  Intersection2( <R>, <U> ) . . . . . for base ring and residue class union
##
InstallMethod( Intersection2,
               "for base ring and residue class union (ResClasses)",
               ReturnTrue, [ IsRing, IsUnionOfResidueClasses ], 0,
               function ( R, U ) return Intersection2( U, R ); end );

#############################################################################
##
#M  Intersection2( <S1>, <S2> ) . . . . . . . . . . . . .  for set and subset
##
InstallMethod( Intersection2,
               "for set and subset (ResClasses)", ReturnTrue,
               [ IsListOrCollection, IsListOrCollection ], 0,

  function ( S1, S2 )
    if not ForAny([S1,S2],IsRing)  
        or ForAny([S1,S2],IsUnionOfResidueClassesWithFixedRepresentatives)
    then TryNextMethod(); fi;
    if   IsSubset(S1,S2) then return S2;
    elif IsSubset(S2,S1) then return S1;
    else TryNextMethod(); fi;
  end );

#############################################################################
##
#M  Intersection2( <S>, <S_> ) . . . . . . . . . . for two times the same set
##
InstallMethod( Intersection2,
               "for two times the same set (ResClasses)", ReturnTrue,
               [ IsListOrCollection, IsListOrCollection ], SUM_FLAGS,

  function ( S, S_ )
    if IsIdenticalObj(S,S_) then return S; else TryNextMethod(); fi;
  end );

#############################################################################
##
#M  Difference( <U>, <S> ) . . . . . . for residue class union and finite set
##
InstallMethod( Difference,
               "for residue class union and finite set (ResClasses)",
               ReturnTrue, [ IsResidueClassUnionInSparseRep, IsList ], 100,

  function ( U, S )
    if not IsSubset(UnderlyingRing(FamilyObj(U)),S) then TryNextMethod(); fi;
    return ResidueClassUnion(UnderlyingRing(FamilyObj(U)),U!.m,U!.r,
                             Difference(U!.included,S),Union(U!.excluded,S));
  end );

#############################################################################
##
#M  Difference( <S>, <U> ) . . . . . . for finite set and residue class union
##
InstallMethod( Difference,
               "for finite set and residue class union (ResClasses)",
               ReturnTrue, [ IsList, IsUnionOfResidueClasses ], 0,

  function ( S, U )
    return Filtered( Set( S ), n -> not n in U );
  end );

#############################################################################
##
#M  Difference( <R>, <U> ) . . . . . .  for base ring and residue class union
##
InstallMethod( Difference,
               "for base ring and residue class union (ResClasses)",
               ReturnTrue, [ IsRing, IsUnionOfResidueClasses ], 0,

  function ( R, U )

    local  m;

    m := Modulus(U);
    if not UnderlyingRing(FamilyObj(U)) = R then TryNextMethod(); fi;
    return ResidueClassUnion(R,m,Difference(AllResidues(R,m),Residues(U)),
                             ExcludedElements(U),IncludedElements(U));
  end );

#############################################################################
##
#M  Difference( <U>, <R> ) . . . . . .  for residue class union and base ring
##
InstallMethod( Difference,
               "for residue class union and base ring (ResClasses)",
               ReturnTrue, [ IsUnionOfResidueClasses, IsRing ], 0,

  function ( U, R )
    if not UnderlyingRing(FamilyObj(U)) = R then TryNextMethod(); fi;
    return [];
  end );

#############################################################################
##
#M  Difference( <R>, <S> ) . . . . . . . . . . . . .  for ring and finite set
##
InstallMethod( Difference,
               "for ring and finite set (ResClasses)", ReturnTrue,
               [ IsRing, IsList ], 0,

  function ( R, S )
    S := Set(S); if not IsSubset(R,S) then TryNextMethod(); fi;
    return ResidueClassUnion(R,One(R),[Zero(R)],[],S);
  end );

#############################################################################
##
#M  Difference( <S>, <S_> ) . . . . . . . . . . .  for two times the same set
##
InstallMethod( Difference,
               "for two times the same set (ResClasses)", ReturnTrue,
               [ IsListOrCollection, IsListOrCollection ], SUM_FLAGS,

  function ( S, S_ )
    if IsIdenticalObj(S,S_) then return []; else TryNextMethod(); fi;
  end );

#############################################################################
##
#M  IsSubset( <U>, <l> ) . . . . . . for residue class union and element list
##
InstallMethod( IsSubset,
               "for residue class union and element list (ResClasses)",
               ReturnTrue, [ IsUnionOfResidueClasses, IsList ], 0,

  function ( U, l )
    return ForAll( Set( l ), n -> n in U );
  end );

#############################################################################
##
#M  IsSubset( <U1>, <U2> ) . . . . . . . . . . . . . for residue class unions
##
InstallMethod( IsSubset,
               "for two residue class unions (ResClasses)", IsIdenticalObj,
               [ IsResidueClassUnionInSparseRep,
                 IsResidueClassUnionInSparseRep ], 0,

  function ( U1, U2 )

    local  R, m1, m2, m, r1, r2, r, allres1, allres2, allres;

    R := UnderlyingRing(FamilyObj(U1));
    m1 := U1!.m; m2 := U2!.m; m := Lcm(R,m1,m2);
    r1 := U1!.r; r2 := U2!.r;
    if not IsSubset(U1,U2!.included) or Intersection(U1!.excluded,U2) <> []
    then return false; fi;
    allres  := AllResidues(R,m);
    allres1 := Filtered(allres,n->n mod m1 in r1);
    allres2 := Filtered(allres,n->n mod m2 in r2);
    return IsSubset(allres1,allres2);
  end );

#############################################################################
##
#M  IsSubset( <U1>, <U2> ) . . . . . for residue class unions with fixed reps
##
InstallMethod( IsSubset,
               "for two residue class unions with fixed reps (ResClasses)",
               IsIdenticalObj,
               [ IsUnionOfResidueClassesWithFixedRepresentatives,
                 IsUnionOfResidueClassesWithFixedRepresentatives ], 0,

  function ( U1, U2 )

    local  R, cls, classes, numbers;

    R       := UnderlyingRing(FamilyObj(U1));
    cls     := [Classes(U1),Classes(U2)];
    classes := Set(Union(cls));
    numbers := List(classes,cl1->List(cls,list->Number(list,cl2->cl2=cl1)));
    return ForAll(numbers,num->num[1]>=num[2]); 
  end );

#############################################################################
##
#M  IsSubset( <R>, <U> ) . . . . . . .  for base ring and residue class union
##
InstallMethod( IsSubset,
               "for base ring and residue class union (ResClasses)",
               ReturnTrue, [ IsRing, IsUnionOfResidueClasses ], 0,

  function ( R, U )
    if   R = UnderlyingRing(FamilyObj(U))
    then return true; else TryNextMethod(); fi;
  end );

#############################################################################
##
#M  IsSubset( <U>, <R> ) . . . . . . .  for residue class union and base ring
##
InstallMethod( IsSubset,
               "for residue class union and base ring (ResClasses)",
               ReturnTrue, [ IsUnionOfResidueClasses, IsRing ], 0,

  function ( U, R )
    if   R = UnderlyingRing(FamilyObj(U))
    then return U = R; else TryNextMethod(); fi;
  end );

#############################################################################
##
#M  \+( <U>, <x> ) . . . . . . . . . for residue class union and ring element
##
InstallOtherMethod( \+,
                    "for residue class union and ring element (ResClasses)",
                    ReturnTrue, [ IsUnionOfResidueClasses, IsRingElement ],
                    0,

  function ( U, x )

    local  R;

    R := UnderlyingRing(FamilyObj(U));
    if not x in R then TryNextMethod(); fi;
    return ResidueClassUnion(R,Modulus(U),
                             List(Residues(U),r -> (r + x) mod Modulus(U)),
                             List(IncludedElements(U),el -> el+x),
                             List(ExcludedElements(U),el -> el+x));
  end );

#############################################################################
##
#M  \+( <x>, <U> ) . . . . . . . . . for ring element and residue class union
##
InstallOtherMethod( \+,
                    "for ring element and residue class union (ResClasses)",
                    ReturnTrue, [ IsRingElement, IsUnionOfResidueClasses ],
                    0, function ( x, U ) return U + x; end );

#############################################################################
##
#M  \+( <U>, <x> ) . for residue class union with fixed reps and ring element
##
InstallOtherMethod( \+,
                    Concatenation( "for residue class union with fixed ",
                                   "reps and ring element (ResClasses)" ),
                    ReturnTrue,
                    [ IsUnionOfResidueClassesWithFixedRepresentatives,
                    IsRingElement ], 0,

  function ( U, x )

    local  R;

    R := UnderlyingRing(FamilyObj(U));
    return ResidueClassUnionWithFixedRepresentatives( R,
             List(Classes(U),cl->[cl[1],cl[2]+x]) );
  end );

#############################################################################
##
#M  \+( <x>, <U> ) . for residue class union with fixed reps and ring element
##
InstallOtherMethod( \+,
                    Concatenation( "for ring element and residue class ",
                                   "union with fixed reps (ResClasses)" ),
                    ReturnTrue,
                    [ IsRingElement,
                      IsUnionOfResidueClassesWithFixedRepresentatives ], 0,
                    function ( x, U ) return U + x; end );

#############################################################################
##
#M  \+( <R>, <x> ) . . . . . . . . . . . . . . for base ring and ring element
##
InstallOtherMethod( \+,
                    "for base ring and ring element (ResClasses)",
                    ReturnTrue, [ IsRing, IsRingElement ], 0,
                    
  function ( R, x )
    if not x in R then TryNextMethod(); fi;
    return R;
  end );

#############################################################################
##
#M  \+( <x>, <R> ) . . . . . . . . . . . . . . for ring element and base ring
##
InstallOtherMethod( \+,
                    "for ring element and base ring (ResClasses)",
                    ReturnTrue, [ IsRingElement, IsRing ], 0,
                    function ( x, R ) return R + x; end );

#############################################################################
##
#M  \-( <U>, <x> ) . . . . . . . . . for residue class union and ring element
##
InstallOtherMethod( \-,
                    "for residue class union and ring element (ResClasses)",
                    ReturnTrue, [ IsListOrCollection, IsRingElement ],
                    0, function ( U, x ) return U + (-x); end );

#############################################################################
##
#M  \-( <x>, <U> ) . . . . . . . . . for ring element and residue class union
##
InstallOtherMethod( \-,
                    "for ring element and residue class union (ResClasses)",
                    ReturnTrue, [ IsRingElement, IsListOrCollection ],
                    0, function ( x, U ) return (-U) + x; end );

#############################################################################
##
#M  \-( <U>, <x> ) . for residue class union with fixed reps and ring element
##
InstallOtherMethod( \-,
                    Concatenation( "for residue class union with fixed ",
                                   "reps and ring element (ResClasses)" ),
                    ReturnTrue,
                    [ IsUnionOfResidueClassesWithFixedRepresentatives,
                      IsRingElement ], 0,
                    function ( U, x ) return U + (-x); end );

#############################################################################
##
#M  \-( <x>, <U> ) . for residue class union with fixed reps and ring element
##
InstallOtherMethod( \-,
                    Concatenation( "for ring element and residue class ",
                                   "union with fixed reps (ResClasses)" ),
                    ReturnTrue,
                    [ IsRingElement,
                      IsUnionOfResidueClassesWithFixedRepresentatives ], 0,
                    function ( x, U ) return (-U) + x; end );

#############################################################################
##
#M  AdditiveInverseOp( <U> ) . . . . . . . . . . . .  for residue class union
##
InstallOtherMethod( AdditiveInverseOp,
                    "for residue class union (ResClasses)", ReturnTrue,
                    [ IsUnionOfResidueClasses ], 0,
                    
  U -> ResidueClassUnion(UnderlyingRing(FamilyObj(U)),Modulus(U),
                         List(Residues(U),r -> (-r) mod Modulus(U)),
                         List(IncludedElements(U),el -> -el),
                         List(ExcludedElements(U),el -> -el)) );

#############################################################################
##
#M  AdditiveInverseOp( <U> ) . . . .  for residue class union with fixed reps
##
InstallOtherMethod( AdditiveInverseOp,
                    "for residue class union with fixed reps (ResClasses)",
                    ReturnTrue,
                    [ IsUnionOfResidueClassesWithFixedRepresentatives ], 0,

  function ( U )

    local  R, invclasses;

    R := UnderlyingRing(FamilyObj(U));
    invclasses := List(Classes(U),cl->[-cl[1],-cl[2]]);
    return ResidueClassUnionWithFixedRepresentatives(R,invclasses);
  end );

#############################################################################
##
#M  AdditiveInverseOp( <R> ) . . . . . . . . . . . . . . . . .  for base ring
##
InstallOtherMethod( AdditiveInverseOp,
                    "for base ring (ResClasses)", ReturnTrue, [ IsRing ], 0,
                    R -> R );

#############################################################################
##
#M  \*( <U>, <x> ) . . . . . . . . . for residue class union and ring element
##
InstallOtherMethod( \*,
                    "for residue class union and ring element (ResClasses)",
                    ReturnTrue, [ IsUnionOfResidueClasses, IsRingElement ],
                    0,

  function ( U, x )

    local  R;

    R := UnderlyingRing(FamilyObj(U));
    if not x in R then TryNextMethod(); fi;
    if IsZero(x) then return [Zero(R)]; fi;
    return ResidueClassUnion(R,x*Modulus(U),
                             List(Residues(U),r -> x*r),
                             List(IncludedElements(U),el -> x*el),
                             List(ExcludedElements(U),el -> x*el));
  end );

#############################################################################
##
#M  \*( <x>, <U> ) . . . . . . . . . for ring element and residue class union
##
InstallOtherMethod( \*,
                    "for ring element and residue class union (ResClasses)",
                    ReturnTrue, [ IsRingElement, IsUnionOfResidueClasses ],
                    0, function ( x, U ) return U * x; end );

#############################################################################
##
#M  \*( <U>, <x> ) . for residue class union with fixed reps and ring element
##
InstallOtherMethod( \*,
                    Concatenation( "for residue class union with fixed ",
                                   "reps and ring element (ResClasses)" ),
                                   ReturnTrue,
                    [ IsUnionOfResidueClassesWithFixedRepresentatives,
                      IsRingElement ], 0,

  function ( U, x )

    local  R;

    R := UnderlyingRing(FamilyObj(U));
    return ResidueClassUnionWithFixedRepresentatives( R, Classes(U) * x );
  end );

#############################################################################
##
#M  \*( <x>, <U> ) . for residue class union with fixed reps and ring element
##
InstallOtherMethod( \*,
                    Concatenation( "for ring element and residue class ",
                                   "union with fixed reps (ResClasses)" ),
                    ReturnTrue,
                    [ IsRingElement,
                      IsUnionOfResidueClassesWithFixedRepresentatives ], 0,
                    function ( x, U ) return U * x; end );

#############################################################################
##
#M  \*( <R>, <x> ) . . . . . . . . . . . . . . for base ring and ring element
##
InstallOtherMethod( \*,
                    "for base ring and ring element (ResClasses)",
                    ReturnTrue, [ IsRing, IsRingElement ], 0,
                    
  function ( R, x )
    if not x in R then TryNextMethod(); fi;
    return ResidueClass(R,x,Zero(x));
  end );

#############################################################################
##
#M  \*( <x>, <R> ) . . . . . . . . . . . . . . for ring element and base ring
##
InstallOtherMethod( \*,
                    "for ring element and base ring (ResClasses)",
                    ReturnTrue, [ IsRingElement, IsRing ], 0,
                    function ( x, R ) return R * x; end );

#############################################################################
##
#M  \/( <U>, <x> ) . . . . . . . . . for residue class union and ring element
##
InstallOtherMethod( \/,
                    "for residue class union and ring element (ResClasses)",
                    ReturnTrue, [ IsUnionOfResidueClasses, IsRingElement ],
                    0,

  function ( U, x )

    local  R, m;

    R := UnderlyingRing(FamilyObj(U)); m := Modulus(U);
    if not x in R or not m/x in R
       or not ForAll(Residues(U),r -> r/x in R)
       or not ForAll(IncludedElements(U),el -> el/x in R) 
    then TryNextMethod(); fi;
    return ResidueClassUnion(R,m/x,List(Residues(U),r -> r/x),
                             List(IncludedElements(U),el -> el/x),
                             List(ExcludedElements(U),el -> el/x));
  end );

#############################################################################
##
#M  \/( <U>, <x> ) . for residue class union with fixed reps and ring element
##
InstallOtherMethod( \/,
                    Concatenation( "for residue class union with fixed ",
                                   "reps and ring element (ResClasses)" ),
                    ReturnTrue,
                    [ IsUnionOfResidueClassesWithFixedRepresentatives,
                      IsRingElement ], 0,

  function ( U, x )

    local  R, quotclasses;

    R := UnderlyingRing(FamilyObj(U));
    quotclasses := Classes(U)/x;
    if not IsSubset(R,Flat(quotclasses)) then TryNextMethod(); fi;
    return ResidueClassUnionWithFixedRepresentatives( R, quotclasses );
  end );

#############################################################################
##
#M  \/( [ ], <x> ) . . . . . . . . . . . . .  for empty list and ring element
##
InstallOtherMethod( \/,
                    "for empty list and ring element (ResClasses)",
                    ReturnTrue, [ IsList and IsEmpty, IsRingElement ], 0,

  function ( empty, x )
    return [ ];
  end );

#############################################################################
##
#M  RepresentativeStabilizingRefinement( <U>, <k> )
##
InstallMethod( RepresentativeStabilizingRefinement,
               Concatenation( "for residue class union of Z with fixed reps",
                              " and positive integer (ResClasses)" ),
               ReturnTrue,
               [ IsUnionOfResidueClassesOfZWithFixedRepresentatives,
                 IsPosInt ], 0,

  function ( U, k )

    local  classes;

    classes := Concatenation(List(Classes(U),
                                  cl->List([0..k-1],
                                           i->[k*cl[1],i*cl[1]+cl[2]])));
    return ResidueClassUnionWithFixedRepresentatives(Integers,classes);
  end );

#############################################################################
##
#M  RepresentativeStabilizingRefinement( <U>, 0 )
##
InstallMethod( RepresentativeStabilizingRefinement,
               Concatenation( "for residue class union of Z with fixed reps",
                              " and 0 (simplify) (ResClasses)" ),
               ReturnTrue,
               [ IsUnionOfResidueClassesOfZWithFixedRepresentatives,
                 IsInt and IsZero ], 0,

  function ( U, k )

    local  cls, olds, mods, parts, part, mp, rp, kp, m, complete, c,
           progression, replacement, found, cl, pos;

    cls := ShallowCopy(Classes(U));
    if Length(cls) < 2 then return U; fi;
    repeat
      olds  := ShallowCopy(cls);
      mods  := Set(List(cls,cl->cl[1]));
      parts := List(mods,m->Filtered(cls,cl->cl[1]=m));
      found := false;
      for part in parts do
        mp := part[1][1]; rp := Set(part,cl->cl[2]);
        for kp in Intersection([2..Length(rp)],DivisorsInt(mp)) do
          m := mp/kp;
          complete := First(Collected(rp mod m),c->c[2]=kp);
          if complete <> fail then
            progression := Set(Filtered(part,t->t[2] mod m = complete[1]));
            replacement := [[m,progression[1][2]]];
            while progression <> [] do
              cl := progression[1];
              pos := Position(cls,cl);
              Unbind(cls[pos]);
              cls := SortedList(cls); 
              Unbind(progression[1]);
              progression := SortedList(progression);
            od;
            cls := Concatenation(cls,replacement);
            found := true;
          fi;
          if found then break; fi;
        od;
        if found then break; fi;
      od;
    until cls = olds;
    return ResidueClassUnionWithFixedRepresentatives(Integers,cls);
  end );

#############################################################################
##
#M  DELTA( <U> ) . . . . . . . . for residue class union of Z with fixed reps
##
InstallMethod( DELTA,
               "for residue class unions of Z with fixed reps (ResClasses)",
               true, [ IsUnionOfResidueClassesOfZWithFixedRepresentatives ],
               0,

  function ( U )
    return Sum(List(Classes(U),c->c[2]/c[1])) - Length(Classes(U))/2;
  end );

#############################################################################
##
#M  DELTA( <U> ) . . . . . . . . . . . . . . . . for residue class union of Z
##
InstallOtherMethod( DELTA,
                    "for residue class unions of Z (ResClasses)",
                    true, [ IsUnionOfResidueClassesOfZ ], 0,

  function ( U )

    local  delta;

    if   IsEmpty(U)    then return 0;
    elif IsIntegers(U) then return 1/2; else
      delta :=   Sum(Residues(U))/Modulus(U)
             - Length(Residues(U))/2 + Modulus(U);
      return delta - Int(delta);
    fi;
  end );

#############################################################################
##
#M  RHO( <U> ) . . . . . . . . . for residue class union of Z with fixed reps
##
InstallMethod( RHO,
               "for residue class unions of Z with fixed reps (ResClasses)",
               true, [ IsUnionOfResidueClassesOfZWithFixedRepresentatives ],
               0,

  function ( U )

    local  delta;

    delta := DELTA(U)/2;
    return E(DenominatorRat(delta))^NumeratorRat(delta);
  end );

#############################################################################
##
#M  RHO( <U> ) . . . . . . . . . . . . . . . . . for residue class union of Z
##
InstallOtherMethod( RHO,
                    "for residue class unions of Z (ResClasses)",
                    true, [ IsUnionOfResidueClassesOfZ ], 0,

  function ( U )

    local  delta;

    if IsEmpty(U) or IsIntegers(U) then return 1; else
      delta := DELTA(U)/2;
      delta := delta - Int(delta);
      if delta >= 1/2 then delta := delta - 1/2; fi;
      return E(DenominatorRat(delta))^NumeratorRat(delta);
    fi;
  end );

#############################################################################
##
#M  Iterator( <U> ) . . . . . . . . . . . . . . . . . for residue class union
##
InstallMethod( Iterator,
               "for residue class unions (ResClasses)", true,
               [ IsResidueClassUnionInSparseRep ], 0,

  function ( U )
    return Objectify( NewType( IteratorsFamily,
                                   IsIterator
                               and IsMutable
                               and IsResidueClassUnionsIteratorRep ),
                      rec( structure    := U,
                           counter      := 0,
                           classpos     := 1,
                           m_count      := 0,
                           element      := fail,
                           rem_included := ShallowCopy( U!.included ) ) );
  end );

#############################################################################
##
#M  NextIterator( <iter> ) . . . . . for iterator of residue class union of Z
##
InstallMethod( NextIterator,
               "for iterators of residue class unions (ResClasses)", true,
               [     IsIterator and IsMutable
                 and IsResidueClassUnionsIteratorRep ], 0,

  function ( iter )

    local  U, next, R, m, r, excluded;

    U := iter!.structure;
    if IsUnionOfResidueClassesOfZ(U) then
      if iter!.rem_included <> [] then
        next := iter!.rem_included[1];
        RemoveSet(iter!.rem_included,next);
        iter!.counter := iter!.counter + 1;
        return next;
      else
        m := Modulus(U); r := Residues(U);
        excluded := ExcludedElements(U);
        repeat
          if iter!.classpos > Length(r) then
            iter!.classpos := 1;
            iter!.m_count := iter!.m_count + 1;
          fi;
          if iter!.element <> fail and iter!.element >= 0 then
            next := (-iter!.m_count-1) * m + r[iter!.classpos];
            iter!.classpos := iter!.classpos + 1;
          else
            next := iter!.m_count * m + r[iter!.classpos];
          fi;
          iter!.element := next;
          iter!.counter := iter!.counter  + 1;
        until not next in excluded;
        return next;
      fi;
    else TryNextMethod(); fi;
  end );

#############################################################################
##
#M  IsDoneIterator( <iter> ) . . . . . .  for iterator of residue class union
##
InstallMethod( IsDoneIterator,
               "for iterators of residue class unions (ResClasses)", true,
               [ IsIterator and IsResidueClassUnionsIteratorRep ], 0,
               ReturnFalse );

#############################################################################
##
#M  ShallowCopy( <iter> ) . . . . . . . . for iterator of residue class union
##
InstallMethod( ShallowCopy,
               "for iterators of residue class unions (ResClasses)", true,
               [ IsIterator and IsResidueClassUnionsIteratorRep ], 0,

  iter -> Objectify( Subtype( TypeObj( iter ), IsMutable ),
                     rec( structure    := iter!.structure,
                          counter      := iter!.counter,
                          classpos     := iter!.classpos,
                          m_count      := iter!.m_count,
                          element      := iter!.element,
                          rem_included := iter!.rem_included ) ) );

#############################################################################
##
#M  ViewObj( <iter> ) . . . . . . . . . . for iterator of residue class union
##
InstallMethod( ViewObj,
               "for iterators of residue class unions (ResClasses)", true,
               [ IsIterator and IsResidueClassUnionsIteratorRep ], 0,

  function ( iter )

    local  R;

    R := UnderlyingRing(FamilyObj(iter!.structure));
    Print("<iterator of a residue class union of ");
    if IsIntegers(R) then Print("Z>"); else Print(String(R),">"); fi;
  end );

#############################################################################
##
#E  resclass.gi . . . . . . . . . . . . . . . . . . . . . . . . . . ends here