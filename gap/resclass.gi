#############################################################################
##
#W  resclass.gi             GAP4 Package `ResClasses'             Stefan Kohl
##
#H  @(#)$Id$
##
##  This file contains implementations of methods and functions for computing
##  with unions of residue classes +/- finite sets ('residue class unions').
##
Revision.resclass_gi :=
  "@(#)$Id$";

#############################################################################
##
#S  Implications between the categories of residue class unions. ////////////
##
#############################################################################

InstallTrueMethod( IsResidueClassUnion,
                   IsResidueClassUnionOfZorZ_pi );
InstallTrueMethod( IsResidueClassUnionOfZorZ_pi,
                   IsResidueClassUnionOfZ );
InstallTrueMethod( IsResidueClassUnionOfZorZ_pi,
                   IsResidueClassUnionOfZ_pi );
InstallTrueMethod( IsResidueClassUnion,
                   IsResidueClassUnionOfZxZ );
InstallTrueMethod( IsResidueClassUnion,
                   IsResidueClassUnionOfGFqx );

# Shorthand for commonly used filter.

BindGlobal( "IsResidueClassUnionInResidueListRep",
            IsResidueClassUnion and IsResidueClassUnionResidueListRep );

#############################################################################
##
#S  The families of residue class unions. ///////////////////////////////////
##
#############################################################################

# Internal variables for caching the families of residue class unions
# used in the current GAP session.

BindGlobal( "Z_RESIDUE_CLASS_UNIONS_FAMILIES", [] );
BindGlobal( "Z_PI_RESIDUE_CLASS_UNIONS_FAMILIES", [] );
BindGlobal( "GF_Q_X_RESIDUE_CLASS_UNIONS_FAMILIES", [] );

#############################################################################
##
#F  ZResidueClassUnionsFamily( <fixedreps> )
##
InstallGlobalFunction( ZResidueClassUnionsFamily,

  function ( fixedreps )

    local  fam, pos;

    if not fixedreps then pos := 1; else pos := 2; fi;
    if   IsBound( Z_RESIDUE_CLASS_UNIONS_FAMILIES[ pos ] )
    then return   Z_RESIDUE_CLASS_UNIONS_FAMILIES[ pos ]; fi;
    if not fixedreps then
      fam := NewFamily( "ResidueClassUnionsFamily( Integers )",
                        IsResidueClassUnionOfZ,
                        CanEasilySortElements, CanEasilySortElements );
      SetUnderlyingRing( fam, Integers );
      SetElementsFamily( fam, FamilyObj( 1 ) );
    else
      fam := NewFamily( "ResidueClassUnionsFamily( Integers, true )",
                        IsUnionOfResidueClassesWithFixedRepresentatives,
                        CanEasilySortElements, CanEasilySortElements );
      SetUnderlyingRing( fam, Integers );
      SetElementsFamily( fam, fam );
    fi;
    MakeReadWriteGlobal( "Z_RESIDUE_CLASS_UNIONS_FAMILIES" );
    Z_RESIDUE_CLASS_UNIONS_FAMILIES[ pos ] := fam;
    MakeReadOnlyGlobal( "Z_RESIDUE_CLASS_UNIONS_FAMILIES" );
    return fam;
  end );

#############################################################################
##
#V  ZxZResidueClassUnionsFamily . . family of all residue class unions of Z^2
##
##  GAP does not view Z^2 as a ring, but rather as a row module.
##  Anyway it is viewed as the underlying ring of the family, since it
##  mathematically is a ring and since this avoids a case distinction
##  in many places in the code.
##
BindGlobal( "ZxZResidueClassUnionsFamily",
            NewFamily( "ResidueClassUnionsFamily( Integers^2 )",
                       IsResidueClassUnionOfZxZ,
                       CanEasilySortElements, CanEasilySortElements ) );
SetUnderlyingRing( ZxZResidueClassUnionsFamily, Integers^2 );
SetElementsFamily( ZxZResidueClassUnionsFamily, ZxZResidueClassUnionsFamily );

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
    then cat := IsResidueClassUnionOfZ_pi;
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
    then cat := IsResidueClassUnionOfGFqx;
    else cat := IsUnionOfResidueClassesOfGFqxWithFixedRepresentatives; fi;
    name := Concatenation( "ResidueClassUnionsFamily( ",
                           ViewString( R ),", ",String(fixedreps)," )" );
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
    then Error("Usage: ResidueClassUnionsFamily( <R> [ , <fixedreps> ] )\n");
    fi;
    R := arg[1];
    if Length(arg) = 2 then fixedreps := arg[2]; else fixedreps := false; fi;
    if   IsIntegers( R )
    then return ZResidueClassUnionsFamily( fixedreps );
    elif IsZxZ( R ) then return ZxZResidueClassUnionsFamily;
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
#M  IsZxZ . . . . . . . . . . . . . . . . . . . . .  Z^2 = Z x Z = Integers^2
##
InstallMethod( IsZxZ, "general method (ResClasses)", true,
               [ IsObject ], 0, R -> R = Integers^2 );

#############################################################################
##
#S  Residues / residue classes (mod m). /////////////////////////////////////
##
#############################################################################

# Buffer for storing already computed polynomial residue systems.

BindGlobal( "POLYNOMIAL_RESIDUE_CACHE", [] );

BindGlobal( "AllGFqPolynomialsModDegree",

  function ( q, d, x )

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
  end );

#############################################################################
##
#M  AllResidues( <R>, <m> ) . . . . . . . . . . . .  for Z, Z_pi and GF(q)[x]
##
InstallMethod( AllResidues,
               "for Z, Z_pi and GF(q)[x] (ResClasses)", ReturnTrue,
               [ IsRing, IsRingElement ], 0,

  function ( R, m )

    local  q, d, x;

    if   IsIntegers(R) or IsZ_pi(R)
    then return [0..StandardAssociate(R,m)-1]; fi;
    if IsUnivariatePolynomialRing(R) and Characteristic(R) <> 0 then
      q := Size(CoefficientsRing(R));
      d := DegreeOfLaurentPolynomial(m);
      x := IndeterminatesOfPolynomialRing(R)[1];
      return AllGFqPolynomialsModDegree(q,d,x);
    fi;
    TryNextMethod();
  end );

#############################################################################
##
#M  AllResidues( Integers^2, <L> ) . . . . . . . . . . . . for lattice in Z^2
##
InstallOtherMethod( AllResidues,
                    "for lattice in Z^2 (ResClasses)", true,
                    [ IsRowModule, IsMatrix ], 0,

  function ( ZxZ, L )
    if not IsZxZ(ZxZ) or not IsSubset(ZxZ,L) then TryNextMethod(); fi;
    L := HermiteNormalFormIntegerMat(L);
    return Cartesian([0..L[1][1]-1],[0..L[2][2]-1]);
  end );

#############################################################################
##
#M  NumberOfResidues( <R>, <m> ) . . . . . . . . . . for Z, Z_pi and GF(q)[x]
##
InstallMethod( NumberOfResidues,
               "for Z, Z_pi and GF(q)[x] (ResClasses)", ReturnTrue,
               [ IsRing, IsRingElement ], 0,

  function ( R, m )

    local  q, d, x;

    if IsIntegers(R) or IsZ_pi(R) then return StandardAssociate(R,m); fi;
    if IsUnivariatePolynomialRing(R) and Characteristic(R) <> 0 then
      q := Size(CoefficientsRing(R));
      d := DegreeOfLaurentPolynomial(m);
      return q^d;
    fi;
    TryNextMethod();
  end );

#############################################################################
##
#M  NumberOfResidues( Integers^2, <L> ) . . . . . . . . .  for lattice in Z^2
##
InstallOtherMethod( NumberOfResidues,
                    "for lattice in Z^2 (ResClasses)", ReturnTrue,
                    [ IsRowModule, IsMatrix ], 0,

  function ( ZxZ, L )
    if not IsZxZ(ZxZ) or not IsSubset(ZxZ,L) then TryNextMethod(); fi;
    return AbsInt(DeterminantMat(L));
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
    if IsRing(R) and IsZero(m) or not m in R then return fail; fi;
    return List(AllResidues(R,m),r->ResidueClass(R,m,r));
  end );

#############################################################################
##
#M  SizeOfSmallestResidueClassRing( <R> ) . . . . .  for Z, Z_pi and GF(q)[x]
##
InstallMethod( SizeOfSmallestResidueClassRing,
               "for Z, Z_pi and GF(q)[x] (ResClasses)", true, [ IsRing ], 0,

  function ( R )
    if   IsIntegers(R) then return 2;
    elif IsZ_pi(R) then return Minimum(NoninvertiblePrimes(R));
    elif IsFiniteFieldPolynomialRing(R) then return Characteristic(R);
    else TryNextMethod(); fi;
  end );

#############################################################################
##
#M  SizeOfSmallestResidueClassRing( Integers^2 ) . . . . . . . . . .  for Z^2
##
InstallOtherMethod( SizeOfSmallestResidueClassRing,
                    "for Z^2 (ResClasses)", true, [ IsRowModule ], 0,

  function ( ZxZ )
    if IsZxZ(ZxZ) then return 2; else TryNextMethod(); fi;
  end );

#############################################################################
##
#M  \mod . . . . . . . . . . . . . . . . . . . . . . . for vector and lattice
##
InstallMethod( \mod, "for vector and lattice (ResClasses)", ReturnTrue,
               [ IsRowVector, IsMatrix ], 0,
               function ( v, L ) return VectorModLattice( v, L ); end );

#############################################################################
##
#S  Construction of residue class unions. ///////////////////////////////////
##
#############################################################################

#############################################################################
##
#M  ResidueClassUnionCons( <filter>, <R>, <m>, <r>, <included>, <excluded> )
##
InstallMethod( ResidueClassUnionCons,
               "residue list rep., for Z, Z_pi and GF(q)[x] (ResClasses)",
               ReturnTrue, [ IsResidueClassUnion, IsRing, IsRingElement,
                             IsList, IsList, IsList ], 0,

  function ( filter, R, m, r, included, excluded )

    local  ReduceResidueClassUnion, Result, both, fam, type, rep, pos;

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
      U!.included := Immutable(Set(Filtered(U!.included,
                                            n -> not (n mod mRed in rRed))));
      U!.excluded := Immutable(Set(Filtered(U!.excluded,
                                            n -> n mod mRed in rRed)));
      if rRed = [] then U := Difference(U!.included,U!.excluded); fi;
    end;

    if not ( IsIntegers( R ) or IsZ_pi( R )
             or (     IsFiniteFieldPolynomialRing( R )
                  and IsUnivariatePolynomialRing( R ) ) )
    then TryNextMethod( ); fi;
    m := StandardAssociate( R, m );
    r := Set( r, n -> n mod m );
    both := Intersection( included, excluded );
    included := Set( Difference( included, both ) );
    excluded := Set( Difference( excluded, both ) );
    if r = [] then return Difference(included,excluded); fi;
    fam := ResidueClassUnionsFamily( R );
    if   IsIntegers( R )       then type := IsResidueClassUnionOfZ;
    elif IsZ_pi( R )           then type := IsResidueClassUnionOfZ_pi;
    elif IsPolynomialRing( R ) then type := IsResidueClassUnionOfGFqx;
    fi;
    Result := Objectify( NewType( fam, type and
                                  IsResidueClassUnionResidueListRep ),
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
#M  ResidueClassUnionCons( <filter>, Integers^2,
##                         <L>, <r>, <included>, <excluded> )
##
InstallMethod( ResidueClassUnionCons,
               "residue list rep., for Z^2 (ResClasses)",
               ReturnTrue, [ IsResidueClassUnion, IsRowModule, IsMatrix,
                             IsList, IsList, IsList ], 0,

  function ( filter, ZxZ, L, r, included, excluded )

    local  ReduceResidueClassUnion, result, both, rep, pos;

    ReduceResidueClassUnion := function ( U )

      local  L, r, LRed, LRedBuf, rRed, rRedBuf, valid, factx, facty, p, q;

      L := HermiteNormalFormIntegerMat(U!.m);  LRed := L;
      r := List(U!.r,v->v mod L); rRed := r;
      factx := Set(Factors(L[2][2]));
      facty := Set(Factors(Gcd(L[1])));
      for p in factx do
        for q in facty do
          repeat
            LRedBuf := LRed; rRedBuf := ShallowCopy(rRed);
            LRed := [LRed[1]/q,LRed[2]/p];
            rRed := Set(List(rRedBuf,v->v mod LRed));
            valid := Length(rRed) = Length(rRedBuf)/(p*q);
          until not valid or not IsZero([LRed[1] mod q, LRed[2] mod p])
                or IsOne(LRed);
          if not valid then LRed := LRedBuf; rRed := rRedBuf; fi;
        od;
      od;
      U!.m := LRed; U!.r := Immutable(rRed);
      U!.included := Immutable(Set(Filtered(U!.included,
                                            v->not v mod LRed in rRed)));
      U!.excluded := Immutable(Set(Filtered(U!.excluded,
                                            v->v mod LRed in rRed)));
      if rRed = [] then U := Difference(U!.included,U!.excluded); fi;
    end;

    if not IsZxZ( ZxZ ) then TryNextMethod( ); fi;
    L := HermiteNormalFormIntegerMat( L );
    r := Set( r, v -> v mod L );
    both := Intersection( included, excluded );
    included := Set( Difference( included, both ) );
    excluded := Set( Difference( excluded, both ) );
    if r = [] then return Difference(included,excluded); fi;
    result := Objectify( NewType( ResidueClassUnionsFamily( ZxZ ),
                                  IsResidueClassUnionOfZxZ and
                                  IsResidueClassUnionResidueListRep ),
                         rec( m := L, r := r,
                              included := included, excluded := excluded ) );
    SetSize( result, infinity ); SetIsFinite( result, false );
    SetIsEmpty( result, false );
    if included <> [ ] then rep := included[ 1 ]; else
      rep := r[1]; pos := 1;
      while rep in excluded do
        pos := pos + 1;
        rep := r[pos mod Length(r) + 1] + Int(pos/Length(r)) * L[1];
      od;
    fi;
    SetRepresentative( result, rep );
    ReduceResidueClassUnion( result );
    if AbsInt( DeterminantMat( result!.m ) ) = 1
      and result!.r = [ [ 0, 0 ] ]
      and [ result!.included, result!.excluded ] = [ [ ], [ ] ]
    then return ZxZ; else return result; fi;
  end );

#############################################################################
##
#F  ResidueClassUnion( <R>, <m>, <r> ) . . . . . . . union of residue classes
#F  ResidueClassUnion( <R>, <m>, <r>, <included>, <excluded> )
##
InstallGlobalFunction( ResidueClassUnion,

  function ( arg )

    if not (     Length(arg) in [3,5]
             and (    IsRing(arg[1]) and arg[2] in arg[1]
                   or     IsRowModule(arg[1]) and IsMatrix(arg[2])
                      and IsSubset(arg[1],arg[2])
                      and not IsZero(DeterminantMat(arg[2])) )
             and IsList(arg[3]) and IsSubset(arg[1],arg[3])
             and (    Length(arg) = 3 or IsList(arg[4]) and IsList(arg[5])
                  and IsSubset(arg[1],arg[4]) and IsSubset(arg[1],arg[5])) )
    then Error("usage: ResidueClassUnion( <R>, <m>, <r> [, <included>",
               ", <excluded>] ),\nfor details see manual.\n"); return fail;
    fi;
    return CallFuncList( ResidueClassUnionNC, arg );
  end );

#############################################################################
##
#F  ResidueClassUnionNC( <R>, <m>, <r> ) . . . . . . union of residue classes
#F  ResidueClassUnionNC( <R>, <m>, <r>, <included>, <excluded> )
##
InstallGlobalFunction( ResidueClassUnionNC,

  function ( arg )

    local  R, m, r, included, excluded;

    R := arg[1]; m := arg[2]; r := Set(arg[3]);
    if   Length(arg) = 5
    then included := Set(arg[4]); excluded := Set(arg[5]);
    else included := [];          excluded := []; fi;
    return ResidueClassUnionCons( IsResidueClassUnion, R, m, r,
                                  included, excluded );
  end );

#############################################################################
##
#F  ResidueClass( <R>, <m>, <r> ) . . . . . . . . . . .  residue class of <R>
#F  ResidueClass( <m>, <r> )  . residue class of the default ring of <m>, <r>
#F  ResidueClass( <r>, <m> )  . . . . . . . . . . . . . . . . . . .  ( dito )
##
InstallGlobalFunction( ResidueClass,

  function ( arg )

    local  R, m, r, d, cl, usage;

    usage := "usage: see ?ResidueClass\n";
    if   Length( arg ) = 3 then
      R := arg[1]; m := arg[2]; r := arg[3];
      if   not (    IsRing(R) and m in R and r in R and not IsZero(m)
                 or IsRowModule(R) and IsMatrix(m) and IsSubset(R,m)
                    and not IsZero(DeterminantMat(m)) )
      then Error( usage ); return fail; fi;
    elif Length( arg ) = 2 then
      if ForAll( arg, IsRingElement ) then
        R := DefaultRing( arg ); 
        m := Maximum( arg ); r := Minimum( arg );
        if IsZero( m ) then Error( usage ); return fail; fi;
      else
        if IsMatrix( arg[1] ) then m := arg[1]; r := arg[2];
                              else m := arg[2]; r := arg[1]; fi;
        if   not ( IsMatrix(m) and IsVector(r) )
        then Error( usage ); return fail; fi;
        d := Length(r);
        R := DefaultRing(r)^d;
        if not (     DimensionsMat(m) = [d,d] and IsSubset(R,m)
                 and RankMat(m) = d and r in R )
        then Error( usage ); return fail; fi;
      fi;
    elif Length( arg ) = 1 then
      if   IsList( arg[1] )
      then return CallFuncList( ResidueClass, arg[1] );
      else Error( usage ); return fail; fi;
    else
      Error( usage ); return fail;
    fi;
    cl := ResidueClassUnionNC( R, m, [ r ] );
    SetIsResidueClass( cl, true );
    return cl;
  end );

#############################################################################
##
#F  ResidueClassNC( <R>, <m>, <r> ) . . . . . . . . . .  residue class of <R>
#F  ResidueClassNC( <m>, <r> )  residue class of the default ring of <m>, <r>
#F  ResidueClassNC( <r>, <m> )  . . . . . . . . . . . . . . . . . .  ( dito )
##
InstallGlobalFunction( ResidueClassNC,

  function ( arg )

    local  R, m, r, d, cl;

    if   Length( arg ) = 3 then
      R := arg[1]; m := arg[2]; r := arg[3];
    elif Length( arg ) = 2 then
      if ForAll(arg,IsRingElement) then
        R := DefaultRing( arg ); 
        m := Maximum( arg ); r := Minimum( arg );
      else
        if IsMatrix(arg[1]) then m := arg[1]; r := arg[2];
                            else m := arg[2]; r := arg[1]; fi;
        d := Length(r);
        R := DefaultRing(r)^d;
      fi;
    elif Length( arg ) = 1 then return CallFuncList(ResidueClassNC,arg[1]);
    else return fail; fi;
    cl := ResidueClassUnionNC( R, m, [ r ] );
    SetIsResidueClass( cl, true );
    return cl;
  end );

#############################################################################
##
#M  IsResidueClass( <obj> ) . . . . . . . . . . . . . . . . .  general method
##
InstallMethod( IsResidueClass,
               "general method (ResClasses)", true, [ IsObject ], 0,

  function ( obj )
    if IsRing(obj) then return true; fi;
    if    IsResidueClassUnion(obj) and Length(Residues(obj)) = 1
      and IncludedElements(obj) = [] and ExcludedElements(obj) = []
    then return true; fi;
    return false;
  end );

#############################################################################
##
#S  ExtRepOfObj / ObjByExtRep for residue class unions. /////////////////////
##
#############################################################################

#############################################################################
##
#M  ExtRepOfObj( <U> ) . . . . . . . . . . . . . . . for residue class unions
##
InstallMethod( ExtRepOfObj,
               "for residue class unions (ResClasses)",
               true, [ IsResidueClassUnion ], 0,
               U -> [ Modulus( U ), ShallowCopy( Residues( U ) ),
                      ShallowCopy( IncludedElements( U ) ),
                      ShallowCopy( ExcludedElements( U ) ) ] );

#############################################################################
##
#M  ObjByExtRep( <fam>, <l> ) . . . . . . . reconstruct a residue class union
##
InstallMethod( ObjByExtRep,
               "reconstruct a residue class union (ResClasses)",
               ReturnTrue, [ IsFamily, IsList ], 0,

  function ( fam, l )

    local  R;

    if not HasUnderlyingRing(fam) or Length(l) <> 4 then TryNextMethod(); fi;
    R := UnderlyingRing(fam);
    if fam <> ResidueClassUnionsFamily(R) then TryNextMethod(); fi;
    return ResidueClassUnion(R,l[1],l[2],l[3],l[4]);
  end );

#############################################################################
##
#S  Accessing the components of a residue class union object. ///////////////
##
#############################################################################

#############################################################################
##
#M  Modulus( <U> ) . . . . . . . . . . . . . . . . . for residue class unions
##
InstallMethod( Modulus,
               "for residue class unions (ResClasses)", true,
               [ IsResidueClassUnionInResidueListRep ], 0, U -> U!.m );

#############################################################################
##
#M  Modulus( [  ] ) . . . . . . . . . . . . . . . . . . . . for the empty set
##
##  Since the empty list carries no information about the objects it does not
##  contain, this method silently assumes that these are supposed to be
##  integers, and returns 0.
##
InstallOtherMethod( Modulus,
                    "for the empty set (ResClasses)", true,
                    [ IsList and IsEmpty ], 0, empty -> 0 );

#############################################################################
##
#M  Modulus( <l> ) . . . . . . . . . . . . . . . . . . . . .  for finite sets
##
InstallOtherMethod( Modulus,
                    "for finite sets (ResClasses)", true,
                    [ IsList ], 0, l -> Zero( l[ 1 ] ) );

#############################################################################
##
#M  Modulus( <R> ) . . . . . . . . . . . . . . .  for the base ring / -module
##
InstallOtherMethod( Modulus, "for the base ring (ResClasses)", true,
                    [ IsRing ], 0, One );
InstallOtherMethod( Modulus, "for the base module (ResClasses)", true,
                    [ IsRowModule ], 0, R -> AsList( Basis( R ) ) );


#############################################################################
##
#M  Residue( <cl> ) . . . . . . . . . . . . . . . . . . . . .  default method
##
InstallMethod( Residue,
               "default method for residue classes (ResClasses)", true,
               [ IsResidueClass ], 0, cl -> Residues(cl)[1] );

#############################################################################
##
#M  Residue( <R> ) . . . . . . . . . . . . . . .  for the base ring / -module
##
InstallOtherMethod( Residue, "for the base ring (ResClasses)", true,
                    [ IsRing ], 0, Zero );
InstallOtherMethod( Residue, "for the base module (ResClasses)", true,
                    [ IsRowModule ], 0, Zero );

#############################################################################
##
#M  Residues( <U> ) . . . . . . . . . . . . . . . .  for residue class unions
##
InstallMethod( Residues,
               "for residue class unions (ResClasses)", true,
               [ IsResidueClassUnionInResidueListRep ], 0, U -> U!.r );

#############################################################################
##
#M  Residues( <R> ) . . . . . . . . . . . . . . . for the base ring / -module
##
InstallOtherMethod( Residues, "for the base ring (ResClasses)", true,
                    [ IsRing ], 0, R -> [ Zero( R ) ] );
InstallOtherMethod( Residues, "for the base module (ResClasses)", true,
                    [ IsRowModule ], 0, R -> [ Zero( R ) ] );

#############################################################################
##
#M  Residues( <l> ) . . . . . . . . . . . . . .  for finite lists of elements
##
InstallOtherMethod( Residues,
                    "for finite lists of elements (ResClasses)", true,
                    [ IsList ], 0, l -> [  ] );

#############################################################################
##
#M  IncludedElements( <U> ) . . . . . . . . . . . .  for residue class unions
##
InstallMethod( IncludedElements,
               "for residue class unions (ResClasses)", true,
               [ IsResidueClassUnionInResidueListRep ], 0,
               U -> U!.included );

#############################################################################
##
#M  IncludedElements( <R> ) . . . . . . . . . . . for the base ring / -module
##
InstallOtherMethod( IncludedElements, "for the base ring (ResClasses)",
                    true, [ IsRing ], 0, R -> [ ] );
InstallOtherMethod( IncludedElements, "for the base module (ResClasses)",
                    true, [ IsRowModule ], 0, R -> [ ] );

#############################################################################
##
#M  IncludedElements( <l> ) . . . . . . . . . .  for finite lists of elements
##
InstallOtherMethod( IncludedElements,
                    "for finite lists of elements (ResClasses)", true,
                    [ IsList ], 0, l -> l );

#############################################################################
##
#M  ExcludedElements( <U> ) . . . . . . . . . . . .  for residue class unions
##
InstallMethod( ExcludedElements,
               "for residue class unions (ResClasses)", true,
               [ IsResidueClassUnionInResidueListRep ], 0,
               U -> U!.excluded );

#############################################################################
##
#M  ExcludedElements( <R> ) . . . . . . . . . . . for the base ring / -module
##
InstallOtherMethod( ExcludedElements, "for the base ring (ResClasses)",
                    true, [ IsRing ], 0, R -> [ ] );
InstallOtherMethod( ExcludedElements, "for the base module (ResClasses)",
                    true, [ IsRowModule ], 0, R -> [ ] );

#############################################################################
##
#M  ExcludedElements( <l> ) . . . . . . . . . .  for finite lists of elements
##
InstallOtherMethod( ExcludedElements,
                    "for finite list of elements (ResClasses)", true,
                    [ IsList ], 0, l -> [ ] );

#############################################################################
##
#S  Testing residue class unions for equality. //////////////////////////////
##
#############################################################################

#############################################################################
##
#M  \=( <U1>, <U2> ) . . . . . . . . . . . . . . . . for residue class unions
##
InstallMethod( \=,
               "for two residue class unions (ResClasses)", IsIdenticalObj,
               [ IsResidueClassUnionInResidueListRep,
                 IsResidueClassUnionInResidueListRep ], 0,

  function ( U1, U2 )
    return U1!.m = U2!.m and U1!.r = U2!.r
           and U1!.included = U2!.included and U1!.excluded = U2!.excluded;
  end );

#############################################################################
##
#M  \=( <D>, <l> ) . . . . . .  for an infinite domain and a list of elements
#M  \=( <l>, <D> ) . . . . . .  for a list of elements and an infinite domain
##
InstallMethod( \=,
               "for an infinite domain and a list of elements (ResClasses)", 
               IsIdenticalObj, [ IsDomain, IsList and IsFinite ], 0,
               function ( D, l )
                 if   not IsFinite( D ) then return false;
                 else TryNextMethod(  ); fi;
               end );
InstallMethod( \=,
               "for a list of elements and an infinite domain (ResClasses)", 
               IsIdenticalObj, [ IsList and IsFinite, IsDomain ], 0,
               function ( l, D ) return D = l; end );

#############################################################################
##
#M  \<( <U1>, <U2> ) . . . . . . . . . . . . . . . . for residue class unions
##
##  A total ordering of residue class unions (for technical purposes, only).
##
InstallMethod( \<,
               "for two residue class unions (ResClasses)", IsIdenticalObj,
               [ IsResidueClassUnionInResidueListRep,
                 IsResidueClassUnionInResidueListRep ], 0,

  function ( U1, U2 )
    if   U1!.m <> U2!.m then return U1!.m < U2!.m;
    elif U1!.r <> U2!.r then return U1!.r < U2!.r;
    elif U1!.included <> U2!.included
    then return U1!.included < U2!.included;
    else return U1!.excluded < U2!.excluded; fi;
  end );

#############################################################################
##
#M  \<( <U>, <R> ) . . . . . .  for a residue class union and a ring / module
#M  \<( <R>, <U> ) . . . . . .  for a ring / module and a residue class union
#M  \<( <l>, <R> ) . . . .  for a finite list of elements and a ring / module
#M  \<( <R>, <l> ) . . . .  for a ring / module and a finite list of elements
#M  \<( <l>, <U> ) .  for a finite list of elements and a residue class union
#M  \<( <U>, <l> ) .  for a residue class union and a finite list of elements
##
InstallMethod( \<, "for a residue class union and a ring (ResClasses)",
               ReturnTrue, [ IsResidueClassUnion, IsRing ], 0, ReturnFalse );
InstallMethod( \<, "for a residue class union and a module (ResClasses)",
               ReturnTrue, [ IsResidueClassUnion, IsRowModule ], 0,
               ReturnFalse );
InstallMethod( \<, "for a ring and a residue class union (ResClasses)",
               ReturnTrue, [ IsRing, IsResidueClassUnion ], 0, ReturnTrue );
InstallMethod( \<, "for a module and a residue class union (ResClasses)",
               ReturnTrue, [ IsRowModule, IsResidueClassUnion ], 0,
               ReturnTrue );
InstallMethod( \<, "for a list of elements and a ring (ResClasses)",
               IsIdenticalObj, [ IsList, IsRing ], 0, ReturnFalse );
InstallMethod( \<, "for a list of elements and a module (ResClasses)",
               IsIdenticalObj, [ IsList, IsRowModule ], 0, ReturnFalse );
InstallMethod( \<, "for a ring and a list of elements (ResClasses)",
               IsIdenticalObj, [ IsRing, IsList ], 0, ReturnTrue );
InstallMethod( \<, "for a module and a list of elements (ResClasses)",
               IsIdenticalObj, [ IsRowModule, IsList ], 0, ReturnTrue );
InstallMethod( \<, "for a list and a residue class union (ResClasses)",
               ReturnTrue, [ IsList, IsResidueClassUnion ], 0, ReturnFalse );
InstallMethod( \<, "for a residue class union and a list (ResClasses)",
               ReturnTrue, [ IsResidueClassUnion, IsList ], 0, ReturnTrue );

#############################################################################
##
#S  Testing for membership in a residue class union. ////////////////////////
##
#############################################################################

#############################################################################
##
#M  \in( <n>, <U> ) . . . . . .  for a ring element and a residue class union
##
InstallMethod( \in,
               "for a ring element and a residue class union (ResClasses)",
               ReturnTrue,
               [ IsObject, IsResidueClassUnionInResidueListRep ], 0,

  function ( n, U )
    if not n in UnderlyingRing(FamilyObj(U)) then return false; fi;
    if   n in U!.included then return true;
    elif n in U!.excluded then return false;
    else return n mod U!.m in U!.r; fi;
  end );

#############################################################################
##
#S  Density and subset relations. ///////////////////////////////////////////
##
#############################################################################

#############################################################################
##
#M  Density( <l> ) . . . . . . . . . . . . . . . . . . . .  for the empty set
##
InstallOtherMethod( Density,
                    "for the empty set (ResClasses)", true,
                    [ IsList and IsEmpty ], 0, l -> 0 );

#############################################################################
##
#M  Density( <l> ) . . . . . . . . . . . . . . . for a finite set of elements
##
InstallOtherMethod( Density,
                   "for a finite set of elements (ResClasses)", true,
                   [ IsList and IsCollection ], 0,

  function ( l )
    if   not IsFinite(DefaultRing(l[1]))
    then return 0; else TryNextMethod(); fi;
  end );

#############################################################################
##
#M  Density( <R> ) . . . . . . . . . . . .  for the whole base ring / -module
##
InstallOtherMethod( Density, "for the whole base ring (ResClasses)", true,
                    [ IsRing ], 0, R -> 1 );
InstallOtherMethod( Density, "for the whole base module (ResClasses)", true,
                    [ IsRowModule ], 0, R -> 1 );

#############################################################################
##
#M  Density( <U> ) . . . . . . . . . . . . . . . . . for residue class unions
##
InstallMethod( Density,
               "for residue class unions (ResClasses)", true,
               [ IsResidueClassUnionInResidueListRep ], 0,

  function ( U )
    return Length(U!.r)/NumberOfResidues(UnderlyingRing(FamilyObj(U)),U!.m);
  end );

#############################################################################
##
#M  IsSubset( <U>, <l> ) . . .  for a residue class union and an element list
##
InstallMethod( IsSubset,
               "for residue class union and element list (ResClasses)",
               ReturnTrue, [ IsResidueClassUnion, IsList ], 0,

  function ( U, l )
    return ForAll( Set( l ), n -> n in U );
  end );

#############################################################################
##
#M  IsSubset( <U1>, <U2> ) . . . . . . . . . . . . . for residue class unions
##
InstallMethod( IsSubset,
               "for two residue class unions (ResClasses)", IsIdenticalObj,
               [ IsResidueClassUnionInResidueListRep,
                 IsResidueClassUnionInResidueListRep ], 0,

  function ( U1, U2 )

    local  R, m1, m2, m, r1, r2, r, allres1, allres2, allres;

    R := UnderlyingRing(FamilyObj(U1));
    m1 := U1!.m; m2 := U2!.m;
    r1 := U1!.r; r2 := U2!.r;
    if   not IsSubset(U1,U2!.included) or Intersection(U1!.excluded,U2) <> []
    then return false; fi;
    if   IsRing(R)      then m := Lcm(R,m1,m2);
    elif IsRowModule(R) then m := LatticeIntersection(m1,m2); fi;
    allres  := AllResidues(R,m);
    allres1 := Filtered(allres,n->n mod m1 in r1);
    allres2 := Filtered(allres,n->n mod m2 in r2);
    return IsSubset(allres1,allres2);
  end );

#############################################################################
##
#M  IsSubset( <R>, <U> ) . . . .  for the base ring and a residue class union
##
InstallMethod( IsSubset,
               "for the base ring and a residue class union (ResClasses)",
               ReturnTrue, [ IsDomain, IsResidueClassUnion ], 0,

  function ( R, U )
    if   R = UnderlyingRing(FamilyObj(U))
    then return true; else TryNextMethod(); fi;
  end );

#############################################################################
##
#M  IsSubset( <U>, <R> ) . . . .  for a residue class union and the base ring
##
InstallMethod( IsSubset,
               "for residue class union and base ring (ResClasses)",
               ReturnTrue, [ IsResidueClassUnion, IsDomain ], 0,

  function ( U, R )
    if   R = UnderlyingRing(FamilyObj(U))
    then return U = R; else TryNextMethod(); fi;
  end );

#############################################################################
##
#M  IsSubset( Integers, Rationals ) . . . . . . . . . . . . . . . for Z and Q
#M  IsSubset( Z_pi( <pi> ), Rationals ) . . . . . . . . . . .  for Z_pi and Q
##
InstallMethod( IsSubset, "for Integers and Rationals (ResClasses)",
               ReturnTrue, [ IsIntegers, IsRationals ], 0, ReturnFalse );
InstallMethod( IsSubset, "for Z_pi and Rationals (ResClasses)",
               ReturnTrue, [ IsZ_pi, IsRationals ], 0, ReturnFalse );

#############################################################################
##
#S  Computing unions, intersections and differences. ////////////////////////
##
#############################################################################

#############################################################################
##
#M  Union2( <U1>, <U2> ) . . . . . . . . . . . . . . for residue class unions
##
InstallMethod( Union2,
               "for two residue class unions (ResClasses)", IsIdenticalObj,
               [ IsResidueClassUnionInResidueListRep,
                 IsResidueClassUnionInResidueListRep ], 0,

  function ( U1, U2 )

    local  R, m1, m2, m, r1, r2, r, included, excluded,
           r1exp, r2exp, allres;

    R := UnderlyingRing(FamilyObj(U1));
    m1 := U1!.m; m2 := U2!.m;
    r1 := U1!.r; r2 := U2!.r;
    if   IsRing(R)      then m := Lcm(R,m1,m2);
    elif IsRowModule(R) then m := LatticeIntersection(m1,m2); fi;
    included := Union(U1!.included,U2!.included);
    excluded := Difference(Union(Difference(U1!.excluded,U2),
                                 Difference(U2!.excluded,U1)),included);
    if IsIntegers(R) then
      r1exp := Concatenation(List([0..m/m1-1],i->i*m1+r1));
      r2exp := Concatenation(List([0..m/m2-1],i->i*m2+r2));
      r     := Union(r1exp,r2exp);
    else
      allres := AllResidues(R,m);
      r := Filtered(allres,n->n mod m1 in r1 or n mod m2 in r2);
    fi;
    return ResidueClassUnionNC(R,m,r,included,excluded);
  end );

#############################################################################
##
#M  Union2( <U>, <S> ) . . . . . . for a residue class union and a finite set
##
InstallMethod( Union2,
               "for a residue class union and a finite set (ResClasses)",
               ReturnTrue, [ IsResidueClassUnionInResidueListRep, IsList ],
               0,

  function ( U, S )
    if not IsSubset(UnderlyingRing(FamilyObj(U)),S) then TryNextMethod(); fi;
    return ResidueClassUnionNC(UnderlyingRing(FamilyObj(U)),U!.m,U!.r,
                               Union(U!.included,S),
                               Difference(U!.excluded,S));
  end );

#############################################################################
##
#M  Union2( <S>, <U> ) . . . . . . for a finite set and a residue class union
##
InstallMethod( Union2,
               "for a finite set and a residue class union (ResClasses)",
               ReturnTrue, [ IsList, IsResidueClassUnion ], 0,
               function ( S, U ) return Union2( U, S ); end );

#############################################################################
##
#M  Union2( <U>, <R> ) . . . . .  for a residue class union and the base ring
##
InstallMethod( Union2,
               "for a residue class union and the base ring (ResClasses)",
               ReturnTrue, [ IsResidueClassUnion, IsDomain ], 0,

  function ( U, R )
    if not UnderlyingRing(FamilyObj(U)) = R then TryNextMethod(); fi;
    return R;
  end );

#############################################################################
##
#M  Union2( <R>, <U> ) . . . . .  for the base ring and a residue class union
##
InstallMethod( Union2,
               "for the base ring and a residue class union (ResClasses)",
               ReturnTrue, [ IsDomain, IsResidueClassUnion ], 0,
               function ( R, U ) return Union2( U, R ); end );

#############################################################################
##
#M  Union2( <S>, <R> ) . . . . . . . . . . for a finite set and the base ring
##
InstallMethod( Union2,
               "for a finite set and the base ring (ResClasses)",
               ReturnTrue, [ IsList, IsDomain ], 0,

  function ( S, R )
    if not IsSubset(R,S) then TryNextMethod(); fi;
    return R;
  end );

#############################################################################
##
#M  Union2( <R>, <S> ) . . . . . . . . . . for the base ring and a finite set
##
InstallMethod( Union2,
               "for the base ring and a finite set (ResClasses)",
               ReturnTrue, [ IsDomain, IsList ], 0,
               function ( R, S ) return Union2( S, R ); end );

#############################################################################
##
#M  Union2( <R>, <R_> ) . . . . . . . . . . . . . for two times the same ring
##
InstallMethod( Union2,
               "for two times the same ring (ResClasses)", ReturnTrue,
               [ IsDomain, IsDomain ], SUM_FLAGS,

  function ( R, R_ )
    if   not ForAll([R,R_],IsRing) and not ForAll([R,R_],IsRowModule)
    then TryNextMethod(); fi;
    if   IsIdenticalObj(R,R_) or R = R_
    then return R; else TryNextMethod(); fi;
  end );

#############################################################################
##
#M  Intersection2( <U1>, <U2> ) . . . . . . . . . .  for residue class unions
##
InstallMethod( Intersection2,
               "for two residue class unions (ResClasses)", IsIdenticalObj,
               [ IsResidueClassUnionInResidueListRep,
                 IsResidueClassUnionInResidueListRep ], 0,

  function ( U1, U2 )

    local  R, m1, m2, m, r1, r2, r, included, excluded,
           gcd, rescsd, res, res1, res2, pairs, pair, allres,
           crt, bruteforce;

    R := UnderlyingRing(FamilyObj(U1));
    m1 := U1!.m; m2 := U2!.m;
    r1 := U1!.r; r2 := U2!.r;
    if   IsRing(R)      then m := Lcm(R,m1,m2);
    elif IsRowModule(R) then m := LatticeIntersection(m1,m2); fi;
    included := Union(U1!.included,U2!.included);
    included := Filtered(included,n -> n in U1!.included and n mod m2 in r2
                                    or n in U2!.included and n mod m1 in r1);
    included := Union(included,Intersection(U1!.included,U2!.included));
    excluded := Union(U1!.excluded,U2!.excluded);
    crt        := ValueOption("CRTIntersection")        = true;
    bruteforce := ValueOption("BruteForceIntersection") = true;
    if IsIntegers(R)
      and (crt or m > 10^6 or m1 * m2 > 100 * Length(r1) * Length(r2))
      and not bruteforce
    then
      gcd    := Gcd(m1,m2);
      rescsd := Intersection(Set(r1 mod gcd),Set(r2 mod gcd));
      r1 := Filtered(r1,res->res mod gcd in rescsd);
      r2 := Filtered(r2,res->res mod gcd in rescsd);
      pairs := Concatenation(List(r1,res1->List(Filtered(r2,
                 res->(res1-res) mod gcd = 0),res2->[res1,res2])));
      r := Set(List(pairs,pair->ChineseRem([m1,m2],pair)));
    else
      allres := AllResidues(R,m);
      r := Filtered(allres,n->n mod m1 in r1 and n mod m2 in r2);
    fi;
    return ResidueClassUnionNC(R,m,r,included,excluded);
  end );

#############################################################################
##
#M  Intersection2( <U>, <S> ) . .  for a residue class union and a finite set
##
InstallMethod( Intersection2,
               "for a residue class union and a finite set (ResClasses)",
               ReturnTrue, [ IsResidueClassUnionInResidueListRep, IsList ],
               0,

  function ( U, S )
    if not IsSubset(UnderlyingRing(FamilyObj(U)),S) then TryNextMethod(); fi;
    return Filtered( Set(S), n -> n in U!.included
                        or ( n mod U!.m in U!.r and not n in U!.excluded ) );
  end );

#############################################################################
##
#M  Intersection2( <S>, <U> ) . .  for a finite set and a residue class union
##
InstallMethod( Intersection2,
               "for a finite set and a residue class union (ResClasses)",
               ReturnTrue, [ IsList, IsResidueClassUnion ], 0,
               function ( S, U ) return Intersection2( U, S ); end );

#############################################################################
##
#M  Intersection2( <U>, <R> ) . . for a residue class union and the base ring
##
InstallMethod( Intersection2,
               "for a residue class union and the base ring (ResClasses)",
               ReturnTrue, [ IsResidueClassUnion, IsDomain ], 0,

  function ( U, R )
    if not UnderlyingRing(FamilyObj(U)) = R then TryNextMethod(); fi;
    return U;
  end );

#############################################################################
##
#M  Intersection2( <R>, <U> ) . . for the base ring and a residue class union
##
InstallMethod( Intersection2,
               "for the base ring and a residue class union (ResClasses)",
               ReturnTrue, [ IsDomain, IsResidueClassUnion ], 0,
               function ( R, U ) return Intersection2( U, R ); end );

#############################################################################
##
#M  Intersection2( <R>, <R_> ) . . . . . . . . .  for two times the same ring
##
InstallMethod( Intersection2,
               "for two times the same ring (ResClasses)", ReturnTrue,
               [ IsListOrCollection, IsListOrCollection ], SUM_FLAGS,

  function ( R, R_ )
    if IsIdenticalObj(R,R_) then return R; else
      if   ForAll([R,R_],IsZ_pi) or ForAll([R,R_],IsRowModule)
      then if R = R_ then return R; fi; fi; 
      TryNextMethod();
    fi;
  end );

#############################################################################
##
#M  Intersection2( <set>, [ ] ) . . . . . . . . . for a set and the empty set
#M  Intersection2( [ ], <set> ) . . . . . . . . . for the empty set and a set
##
InstallMethod( Intersection2, "for a set and the empty set (ResClasses)",
               ReturnTrue, [ IsListOrCollection, IsList and IsEmpty ], 0,
               function ( S, empty ) return [  ]; end );
InstallMethod( Intersection2, "for the empty set and a set (ResClasses)",
               ReturnTrue, [ IsList and IsEmpty, IsListOrCollection ], 0,
               function ( empty, S ) return [  ]; end );

#############################################################################
##
#M  Difference( <U1>, <U2> ) . . . . . . . . . . . . for residue class unions
##
InstallMethod( Difference,
               "for two residue class unions (ResClasses)", IsIdenticalObj,
               [ IsResidueClassUnionInResidueListRep,
                 IsResidueClassUnionInResidueListRep ], 0,

  function ( U1, U2 )

    local  R, m1, m2, m, r1, r2, r, included, excluded, allres, expres;

    R := UnderlyingRing(FamilyObj(U1));
    m1 := U1!.m; m2 := U2!.m;
    r1 := U1!.r; r2 := U2!.r;
    if   IsRing(R)      then m := Lcm(R,m1,m2);
    elif IsRowModule(R) then m := LatticeIntersection(m1,m2); fi;
    included := Union(U1!.included,U2!.excluded);
    included := Filtered(included,
                         n -> n in U1!.included and not n mod m2 in r2
                           or n in U2!.excluded and n mod m1 in r1);
    excluded := Union(U1!.excluded,U2!.included);
    if IsIntegers(R) then
      expres := Concatenation(List([0..m/m1-1],i->i*m1+r1));
      r      := Filtered(expres,n -> not n mod m2 in r2);
    else
      allres := AllResidues(R,m);
      r      := Filtered(allres,n -> n mod m1 in r1 and not n mod m2 in r2);
    fi;
    return ResidueClassUnionNC(R,m,r,included,excluded);
  end );

#############################################################################
##
#M  Difference( <U>, <S> ) . . . . for a residue class union and a finite set
##
InstallMethod( Difference,
               "for a residue class union and a finite set (ResClasses)",
               ReturnTrue, [ IsResidueClassUnionInResidueListRep, IsList ],
               100,

  function ( U, S )
    if not IsSubset(UnderlyingRing(FamilyObj(U)),S) then TryNextMethod(); fi;
    return ResidueClassUnionNC(UnderlyingRing(FamilyObj(U)),U!.m,U!.r,
                               Difference(U!.included,S),
                               Union(U!.excluded,S));
  end );

#############################################################################
##
#M  Difference( <S>, <U> ) . . . . for a finite set and a residue class union
##
InstallMethod( Difference,
               "for a finite set and a residue class union (ResClasses)",
               ReturnTrue, [ IsList, IsResidueClassUnion ], 0,

  function ( S, U )
    return Filtered( Set( S ), n -> not n in U );
  end );

#############################################################################
##
#M  Difference( <R>, <U> ) . . .  for the base ring and a residue class union
##
InstallMethod( Difference,
               "for the base ring and a residue class union (ResClasses)",
               ReturnTrue, [ IsDomain, IsResidueClassUnion ], 0,

  function ( R, U )

    local  m;

    m := Modulus(U);
    if not UnderlyingRing(FamilyObj(U)) = R then TryNextMethod(); fi;
    return ResidueClassUnion(R,m,Difference(AllResidues(R,m),Residues(U)),
                             ExcludedElements(U),IncludedElements(U));
  end );

#############################################################################
##
#M  Difference( <U>, <R> ) . . .  for a residue class union and the base ring
##
InstallMethod( Difference,
               "for a residue class union and the base ring (ResClasses)",
               ReturnTrue, [ IsResidueClassUnion, IsDomain ], 0,

  function ( U, R )
    if not UnderlyingRing(FamilyObj(U)) = R then TryNextMethod(); fi;
    return [];
  end );

#############################################################################
##
#M  Difference( <R>, <S> ) . . . . . . . . . . .  for a ring and a finite set
##
InstallMethod( Difference,
               "for a ring and a finite set (ResClasses)", ReturnTrue,
               [ IsDomain, IsList ], 0,

  function ( R, S )
    if   not IsSubset(R,S) or not (IsRing(R) or IsRowModule(R))
    then TryNextMethod(); fi;
    return ResidueClassUnionNC(R,One(R),[Zero(R)],[],Set(S));
  end );

#############################################################################
##
#M  Difference( <D>, <S> ) . . . . . . . . . . . for a ring and the empty set
##
InstallMethod( Difference, "for a domain and the empty set (ResClasses)",
               ReturnTrue, [ IsDomain, IsList and IsEmpty ], SUM_FLAGS,
               function ( D, S ) return D; end );

#############################################################################
##
#M  Difference( <R>, <R_> ) . . . . . . . . . . . for two times the same ring
##
InstallMethod( Difference,
               "for two times the same ring (ResClasses)", ReturnTrue,
               [ IsDomain, IsDomain ], SUM_FLAGS,

  function ( R, R_ )
    if R = R_ then return []; else TryNextMethod(); fi;
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
#S  Applying arithmetic operations to the elements of a residue class union.
##
#############################################################################

#############################################################################
##
#M  \+( <U>, <x> ) . . . . . . . for a residue class union and a ring element
##
InstallOtherMethod( \+,
                    "for residue class union and ring element (ResClasses)",
                    IsCollsElms, [ IsResidueClassUnion, IsObject ], 0,

  function ( U, x )

    local  R;

    R := UnderlyingRing(FamilyObj(U));
    if not x in R then TryNextMethod(); fi;
    return ResidueClassUnion(R,Modulus(U),
                             List(Residues(U),r -> (r + x) mod Modulus(U)),
                             List(IncludedElements(U),el->el+x),
                             List(ExcludedElements(U),el->el+x));
  end );

#############################################################################
##
#M  \+( <x>, <U> ) . . . . . . . for a ring element and a residue class union
##
InstallOtherMethod( \+,
                    "for ring element and residue class union (ResClasses)",
                    IsElmsColls, [ IsObject, IsResidueClassUnion ], 0,
                    function ( x, U ) return U + x; end );

#############################################################################
##
#M  \+( <R>, <x> ) . . . . . . . . . . . for the base ring and a ring element
##
InstallOtherMethod( \+,
                    "for the base ring and a ring element (ResClasses)",
                    IsCollsElms, [ IsDomain, IsObject ], SUM_FLAGS,
                    
  function ( R, x )
    if not IsRing(R) and not IsRowModule(R) then TryNextMethod(); fi;
    if not x in R then TryNextMethod(); fi;
    return R;
  end );

#############################################################################
##
#M  \+( <x>, <R> ) . . . . . . . . . . . for a ring element and the base ring
##
InstallOtherMethod( \+,
                    "for a ring element and the base ring (ResClasses)",
                    ReturnTrue, [ IsObject, IsDomain ], SUM_FLAGS,
                    function ( x, R ) return R + x; end );

#############################################################################
##
#M  AdditiveInverseOp( <U> ) . . . . . . . . . . . . for residue class unions
##
InstallOtherMethod( AdditiveInverseOp,
                    "for residue class unions (ResClasses)",
                    true, [ IsResidueClassUnion ], 0,

  U -> ResidueClassUnion(UnderlyingRing(FamilyObj(U)),Modulus(U),
                         List(Residues(U),r -> (-r) mod Modulus(U)),
                         List(IncludedElements(U),el -> -el),
                         List(ExcludedElements(U),el -> -el)) );

#############################################################################
##
#M  \-( <U>, <x> ) . . . . . . . for a residue class union and a ring element
##
InstallOtherMethod( \-,
                    "for residue class union and ring element (ResClasses)",
                    IsCollsElms, [ IsListOrCollection, IsObject ], SUM_FLAGS,
                    function ( U, x ) return U + (-x); end );

#############################################################################
##
#M  \-( <x>, <U> ) . . . . . . . for a ring element and a residue class union
##
InstallOtherMethod( \-,
                    "for ring element and residue class union (ResClasses)",
                    IsElmsColls, [ IsObject, IsListOrCollection ], SUM_FLAGS,
                    function ( x, U ) return (-U) + x; end );

#############################################################################
##
#M  AdditiveInverseOp( <R> ) . . . . . . . . . . . . . . .  for the base ring
##
InstallOtherMethod( AdditiveInverseOp,
                    "for base ring (ResClasses)", true,
                    [ IsRing ], 0, R -> R );
InstallOtherMethod( AdditiveInverseOp,
                    "for base module (ResClasses)", true,
                    [ IsRowModule ], 0, R -> R );

#############################################################################
##
#M  \*( <U>, <x> ) . . . . . . . for a residue class union and a ring element
##
InstallOtherMethod( \*,
                    "for residue class union and ring element (ResClasses)",
                    ReturnTrue, [ IsResidueClassUnion, IsRingElement ], 0,

  function ( U, x )

    local  R;

    R := UnderlyingRing(FamilyObj(U));
    if IsRing(R) and not x in R then TryNextMethod(); fi;
    if   IsRowModule(R) and not x in LeftActingDomain(R)
      and not x in GL(Dimension(R),LeftActingDomain(R))
    then TryNextMethod(); fi;
    if IsZero(x) then return [Zero(R)]; fi;
    return ResidueClassUnionNC(R,x*Modulus(U),
                               List(Residues(U),r->x*r),
                               List(IncludedElements(U),el->x*el),
                               List(ExcludedElements(U),el->x*el));
  end );

#############################################################################
##
#M  \*( <x>, <U> ) . . . . . . . for a ring element and a residue class union
##
InstallOtherMethod( \*,
                    "for ring element and residue class union (ResClasses)",
                    ReturnTrue, [ IsRingElement, IsResidueClassUnion ], 0,
                    function ( x, U ) return U * x; end );

#############################################################################
##
#M  \*( <R>, <x> ) . . . . . . . . . . . for the base ring and a ring element
##
InstallOtherMethod( \*,
                    "for the base ring and a ring element (ResClasses)",
                    IsCollsElms, [ IsRing, IsRingElement ], 0,
                    
  function ( R, x )
    if not IsIntegers(R) and not IsZ_pi(R)
       and not (     IsUnivariatePolynomialRing(R)
                 and IsFiniteFieldPolynomialRing(R)) or not x in R
    then TryNextMethod(); fi;
    if IsZero(x) then return [Zero(R)]; 
                 else return ResidueClass(R,x,Zero(x)); fi;
  end );

#############################################################################
##
#M  \*( <R>, <x> ) . . . . . . . .  for the base module and a scalar / matrix
##
InstallOtherMethod( \*,
                    "for the base module and a scalar / matrix (ResClasses)",
                    ReturnTrue, [ IsRowModule, IsRingElement ], 0,
                    
  function ( R, x )
    if   x in LeftActingDomain(R)
    then return ResidueClassNC(R,x*One(GL(Dimension(R),
                                          LeftActingDomain(R))),[0,0]);
    elif x in GL(Dimension(R),LeftActingDomain(R))
    then return ResidueClassNC(R,x,Zero(R));
    else TryNextMethod(); fi;
  end );

#############################################################################
##
#M  \*( <x>, <R> ) . . . . . . . . . . . for a ring element and the base ring
##
InstallOtherMethod( \*,
                    "for a ring element and the base ring (ResClasses)",
                    ReturnTrue, [ IsRingElement, IsDomain ],
                    SUM_FLAGS, function ( x, R ) return R * x; end );

#############################################################################
##
#M  \/( <U>, <x> ) . . . . . . . for a residue class union and a ring element
##
InstallOtherMethod( \/,
                    "for residue class union and ring element (ResClasses)",
                    ReturnTrue, [ IsResidueClassUnion, IsRingElement ], 0,

  function ( U, x )

    local  R, m;

    R := UnderlyingRing(FamilyObj(U)); m := Modulus(U);
    if not x in R or not m/x in R
       or not ForAll(Residues(U),r->r/x in R)
       or not ForAll(IncludedElements(U),el->el/x in R) 
    then TryNextMethod(); fi;
    return ResidueClassUnion(R,m/x,List(Residues(U),r->r/x),
                             List(IncludedElements(U),el->el/x),
                             List(ExcludedElements(U),el->el/x));
  end );

#############################################################################
##
#M  \/( [ ], <x> ) . . . . . . . . . . . for the empty set and a ring element
##
InstallOtherMethod( \/,
                    "for the empty set and a ring element (ResClasses)",
                    ReturnTrue, [ IsList and IsEmpty, IsRingElement ], 0,

  function ( empty, x )
    return [ ];
  end );

#############################################################################
##
#S  Splitting residue classes. //////////////////////////////////////////////
##
#############################################################################

#############################################################################
##
#M  SplittedClass( <cl>, <t> ) . . . . . . . for residue classes of Z or Z_pi
##
InstallMethod( SplittedClass,
               "for residue classes of Z or Z_pi (ResClasses)", ReturnTrue,
               [ IsResidueClassUnionOfZorZ_pi, IsPosInt ], 0,

  function ( cl, t )

    local  R, r, m;

    R := UnderlyingRing(FamilyObj(cl));
    if not IsResidueClass(cl)
       or (IsZ_pi(R) and not IsSubset(Union(NoninvertiblePrimes(R),[1]),
                                      Set(Factors(t))))
    then return fail; fi;
    r := Residue(cl); m := Modulus(cl);
    return List([0..t-1],k->ResidueClass(R,t*m,k*m+r));
  end );

#############################################################################
##
#M  SplittedClass( <cl>, <t> ) . . . . . . .  for residue classes of GF(q)[x]
##
InstallMethod( SplittedClass,
               "for residue classes of GF(q)[x] (ResClasses)", ReturnTrue,
               [ IsResidueClassUnionOfGFqx, IsPosInt ], 0,

  function ( cl, t )

    local  R, r, m1, m2, m;

    if t = 1 then return [cl]; fi;
    R := UnderlyingRing(FamilyObj(cl));
    if   not IsResidueClass(cl) or SmallestRootInt(t) <> Characteristic(R)
    then return fail; fi;
    r  := Residue(cl);
    m1 := Modulus(cl);
    m2 := IndeterminatesOfPolynomialRing(R)[1]^LogInt(t,Characteristic(R));
    m  := m1 * m2;
    return List(AllResidues(R,m2),k->ResidueClass(R,m,k*m1+r));
  end );

#############################################################################
##
#M  SplittedClass( <R>, <t> ) . . . . . . . . . . . . for Z, Z_pi or GF(q)[x]
##
InstallOtherMethod( SplittedClass,
                    "for Z, Z_pi or GF(q)[x] (ResClasses)", ReturnTrue,
                    [ IsRing, IsPosInt ], 0,

  function ( R, t )

    local  m;

    if IsOne(t) then return [R]; fi;
    if    not IsIntegers(R) and not IsZ_pi(R)
      and not IsFiniteFieldPolynomialRing(R)
    then TryNextMethod(); fi;
    if IsZ_pi(R)
      and not IsSubset(Union(NoninvertiblePrimes(R),[1]),Set(Factors(t)))
    then return fail; fi;
    if IsFiniteFieldPolynomialRing(R)
      and SmallestRootInt(t) <> Characteristic(R)
    then return fail; fi;
    if IsIntegers(R) or IsZ_pi(R) then m := t; else
      m := IndeterminatesOfPolynomialRing(R)[1]^LogInt(t,Characteristic(R));
    fi;
    return AllResidueClassesModulo(R,m);
  end );

#############################################################################
##
#M  SplittedClass( <cl>, <m2> ) .  for a residue class of GF(q)[x] and a pol.
##
InstallOtherMethod( SplittedClass,
                    Concatenation("for a residue class of GF(q)[x] and a ",
                                  "polynomial (ResClasses)"), ReturnTrue,
                    [ IsResidueClassUnionOfGFqx, IsPolynomial ], 0,

  function ( cl, m2 )

    local  R, r, m1, m;

    R := UnderlyingRing(FamilyObj(cl));
    if not IsResidueClass(cl) or not m2 in R then return fail; fi;
    if IsOne(m2) then return [cl]; fi;
    r  := Residue(cl);
    m1 := Modulus(cl);
    m  := m1 * m2;
    return List(AllResidues(R,m2),k->ResidueClass(R,m,k*m1+r));
  end );

#############################################################################
##
#M  SplittedClass( <R>, <m> ) . . . . . . . . . for GF(q)[x] and a polynomial
##
InstallOtherMethod( SplittedClass,
                    "for GF(q)[x] and a polynomial (ResClasses)", ReturnTrue,
                    [ IsFiniteFieldPolynomialRing, IsPolynomial ], 0,

  function ( R, m )
    if not m in R then return fail; fi;
    return AllResidueClassesModulo(R,m);
  end );

#############################################################################
##
#S  Computing partitions into residue classes. //////////////////////////////
##
#############################################################################

#############################################################################
##
#M  AsUnionOfFewClasses( <U> ) . . . . . . . .  for pure residue class unions
##
InstallMethod( AsUnionOfFewClasses,
               "for pure residue class unions (ResClasses)", true,
               [ IsResidueClassUnion ], 0,

  function ( U )

    local  R, cls, cl, remaining, m, res, res_d, div, d, r;

    R := UnderlyingRing(FamilyObj(U));
    m := Modulus(U); res := Residues(U);
    if Length(res) = 1 then return [ ResidueClass(R,m,res[1]) ]; fi;
    cls := []; remaining := U;
    div := List(Combinations(Factors(R,m)),Product);
    div[1] := One(R); div := Set(div);
    for d in div do
      if   not IsZero(m mod d) or NumberOfResidues(R,m/d) > Length(res)
      then continue; fi;
      res_d := Set(res mod d);
      for r in res_d do
        if IsSubset(res,List(AllResidues(R,m/d),s->r+s*d)) then
          cl := ResidueClass(R,d,r);
          Add(cls,cl); remaining := Difference(remaining,cl);
          if IsList(remaining) then break; fi;
          m := Modulus(remaining); res := Residues(remaining);
          if   not IsZero(m mod d) or Length(res) < NumberOfResidues(R,m/d)
          then break; fi;
        fi;
      od;
      if IsList(remaining) then break; fi;
    od;
    return cls;
  end );

#############################################################################
##
#M  AsUnionOfFewClasses( <U> ) . . . . . . for pure residue class unions of Z
##
InstallMethod( AsUnionOfFewClasses,
               "for pure residue class unions of Z, 2 (ResClasses)", true,
               [ IsResidueClassUnionOfZ ], 20,

  function ( U )

    local  cls, cl, remaining, m, res, res_d, div, d, r;

    m := Modulus(U); res := Residues(U);
    if Length(res) = 1 then return [ ResidueClass(Integers,m,res[1]) ]; fi;
    cls := []; remaining := U;
    div := DivisorsInt(m);
    for d in div do
      if m mod d <> 0 or m/d > Length(res) then continue; fi;
      res_d := Set(res mod d);
      for r in res_d do
        if IsSubset(res,[r,r+d..r+(m/d-1)*d]) then
          cl := ResidueClass(Integers,d,r);
          Add(cls,cl); remaining := Difference(remaining,cl);
          if IsList(remaining) then break; fi;
          m := Modulus(remaining); res := Residues(remaining);
          if m mod d <> 0 or Length(res) < m/d then break; fi;
        fi;
      od;
      if IsList(remaining) then break; fi;
    od;
    return cls;
  end );

#############################################################################
##
#M  AsUnionOfFewClasses( <l> ) . . . . . . . . .  for finite sets of elements
##
InstallOtherMethod( AsUnionOfFewClasses,
                    "for finite sets of elements (ResClasses)", true,
                    [ IsList ], 0, l -> [  ] );

#############################################################################
##
#M  AsUnionOfFewClasses( <R> ) . . . . . . . . . . . . . . . . . . . for ring
##
InstallOtherMethod( AsUnionOfFewClasses,
                    "for a ring (ResClasses)", true, [ IsRing ], 0,
                    R -> [ R ] );

#############################################################################
##
#M  PartitionsIntoResidueClasses( <R>, <length> ) . . . . . .  general method
##
InstallMethod( PartitionsIntoResidueClasses,
               "general method (ResClasses)",
               ReturnTrue, [ IsRing, IsPosInt ], 0,

  function ( R, length )

    local  Recurse, partitions, primes, p, q, x;

    Recurse := function ( P, pos, p )

      local  remaining_primes;

      P[pos] := SplittedClass(P[pos],p);
      if P[pos] = fail then return; fi;
      P := Flat(P);

      if Length(P) = length then Add(partitions,Set(P)); return; fi;

      if   IsIntegers(R) or IsZ_pi(P)
      then remaining_primes := Intersection(primes,[2..length-Length(P)+1]);
      else remaining_primes := Filtered(primes,p->NumberOfResidues(R,p)
                                               <= length-Length(P)+1);
      fi;

      for p in remaining_primes do
        for pos in [1..Length(P)] do
          Recurse(ShallowCopy(P),pos,p);
        od;
      od;
    end;

    if length = 1 then return [[R]]; fi;

    partitions := [];

    if   IsIntegers(R)
    then primes := Filtered([2..length],IsPrime);
    elif IsZ_pi(R)
    then primes := Intersection([2..length],NoninvertiblePrimes(R));
    elif IsUnivariatePolynomialRing(R) and IsFiniteFieldPolynomialRing(R)
    then x := IndeterminatesOfPolynomialRing(R)[1];
         q := Size(CoefficientsRing(R));
         primes := Filtered(AllResidues(R,x^(LogInt(length,q)+1)),
                            p -> IsIrreducibleRingElement(R,p));
    else TryNextMethod(); fi;

    for p in primes do Recurse([R],1,p); od;

    return Set(partitions);
  end );

#############################################################################
##
#M  RandomPartitionIntoResidueClasses( <R>, <length>, <primes> )  for Z, Z_pi
##
InstallMethod( RandomPartitionIntoResidueClasses,
               "for Z or Z_pi (ResClasses)", ReturnTrue,
               [ IsRing, IsPosInt, IsList ], 0,

  function ( R, length, primes )

    local  P, diff, p, parts, part, i, j;

    if   not (IsIntegers(R) or IsZ_pi(R))
      or not ForAll(primes,IsInt) or not ForAll(primes,IsPrime)
    then TryNextMethod(); fi;
    if   IsZ_pi(R)
    then primes := Intersection(primes,NoninvertiblePrimes(R)); fi;
    parts := Filtered(Partitions(length-1),
                      part->IsSubset(primes-1,part));
    if IsEmpty(parts) then return fail; fi;
    part := Random(parts);
    P    := [ R ];
    for i in [1..Length(part)] do
      p    := part[i] + 1;
      j    := Random([1..Length(P)]);
      P[j] := SplittedClass(P[j],p);
      P    := Flat(P);
    od;
    return Set(P);
  end );

#############################################################################
##
#M  RandomPartitionIntoResidueClasses( <R>, <length>, <primes> ) for GF(q)[x]
##
InstallMethod( RandomPartitionIntoResidueClasses,
               "for GF(q)[x] (ResClasses)", ReturnTrue,
               [ IsFiniteFieldPolynomialRing, IsPosInt, IsList ], 0,

  function ( R, length, primes )

    local  P, splitparts, parts, part, p, i, j;

    if not IsSubset(R,primes) then TryNextMethod(); fi;
    splitparts := List(primes,p->NumberOfResidues(R,p)-1);
    parts := Filtered(Partitions(length-1),part->IsSubset(splitparts,part));
    if IsEmpty(parts) then return fail; fi;
    part := Random(parts);
    P    := [ R ];
    for i in [1..Length(part)] do
      p    := Random(Filtered(primes, q->NumberOfResidues(R,q) = part[i]+1));
      j    := Random([1..Length(P)]);
      P[j] := SplittedClass(P[j],p);
      P    := Flat(P);
    od;
    return Set(P);
  end );

#############################################################################
##
#S  The invariants Delta and Rho. ///////////////////////////////////////////
##
#############################################################################

#############################################################################
##
#M  Delta( <U> ) . . . . . . . . . . . . . . .  for residue class unions of Z
##
InstallMethod( Delta,
               "for residue class unions of Z (ResClasses)",
               true, [ IsResidueClassUnionOfZ ], 0,

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
#M  Rho( <U> ) . . . . . . . . . . . . . . . .  for residue class unions of Z
##
InstallMethod( Rho,
               "for residue class unions of Z (ResClasses)",
               true, [ IsResidueClassUnionOfZ ], 0,

  function ( U )

    local  delta;

    if IsEmpty(U) or IsIntegers(U) then return 1; else
      delta := Delta(U)/2;
      delta := delta - Int(delta);
      if delta >= 1/2 then delta := delta - 1/2; fi;
      return E(DenominatorRat(delta))^NumeratorRat(delta);
    fi;
  end );

#############################################################################
##
#S  Iterators for residue class unions. /////////////////////////////////////
##
#############################################################################

#############################################################################
##
#M  Iterator( <U> ) . . . . . . . . . . . . . . . .  for residue class unions
##
InstallMethod( Iterator,
               "for residue class unions (ResClasses)", true,
               [ IsResidueClassUnionInResidueListRep ], 0,

  function ( U )
    return Objectify( NewType( IteratorsFamily,
                                   IsIterator
                               and IsMutable
                               and IsResidueClassUnionsIteratorRep ),
                      rec( U            := U,
                           counter      := 0,
                           classpos     := 1,
                           m_count      := 0,
                           element      := fail,
                           rem_included := ShallowCopy( U!.included ) ) );
  end );

#############################################################################
##
#M  NextIterator( <iter> ) . . . . . .  for iterators of residue class unions
##
InstallMethod( NextIterator,
               "for iterators of residue class unions (ResClasses)", true,
               [     IsIterator and IsMutable
                 and IsResidueClassUnionsIteratorRep ], 0,

  function ( iter )

    local  U, next, R, m, r, excluded;

    U := iter!.U;
    if IsResidueClassUnionOfZ(U) then
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
#M  IsDoneIterator( <iter> ) . . . . .  for iterators of residue class unions
##
InstallMethod( IsDoneIterator,
               "for iterators of residue class unions (ResClasses)", true,
               [ IsIterator and IsResidueClassUnionsIteratorRep ], 0,
               ReturnFalse );

#############################################################################
##
#M  ShallowCopy( <iter> ) . . . . . . . for iterators of residue class unions
##
InstallMethod( ShallowCopy,
               "for iterators of residue class unions (ResClasses)", true,
               [ IsIterator and IsResidueClassUnionsIteratorRep ], 0,

  iter -> Objectify( Subtype( TypeObj( iter ), IsMutable ),
                     rec( U            := iter!.U,
                          counter      := iter!.counter,
                          classpos     := iter!.classpos,
                          m_count      := iter!.m_count,
                          element      := iter!.element,
                          rem_included := iter!.rem_included ) ) );

#############################################################################
##
#M  ViewObj( <iter> ) . . . . . . . . . for iterators of residue class unions
##
InstallMethod( ViewObj,
               "for iterators of residue class unions (ResClasses)", true,
               [ IsIterator and IsResidueClassUnionsIteratorRep ], 0,

  function ( iter )

    local  R;

    R := UnderlyingRing(FamilyObj(iter!.U));
    Print("<iterator of a residue class union of ",RingToString(R),">");
  end );

#############################################################################
##
#S  Viewing, printing and displaying residue class unions. //////////////////
##
#############################################################################

#############################################################################
##
#F  RingToString( <R> ) . . . how the ring <R> is printed by `View'/`Display'
##
InstallGlobalFunction( RingToString,
  function ( R )
    if   IsIntegers(R) then return "Z";
    elif IsZxZ(R)      then return "Z^2";
                       else return ViewString(R); fi;
  end );

#############################################################################
##
#M  ViewObj( <U> ) . . . . . . . . . . . . . . . . . for residue class unions
##
InstallMethod( ViewObj,
               "for residue class unions (ResClasses)", true,
               [ IsResidueClassUnion ], 0,

  function ( U )

    local  R, m, r, included, excluded, PrintFiniteSet, n, cl, endval,
           short, bound, display;

    PrintFiniteSet := function ( S )
      if   Length(String(S)) <= 32 or display
      then Print(S);
      else Print("<set of cardinality ",Length(S),">"); fi;
    end;

    short   := RESCLASSES_VIEWING_FORMAT = "short";
    display := ValueOption("RC_DISPLAY") = true;
    R := UnderlyingRing(FamilyObj(U)); m := Modulus(U); r := Residues(U);
    included := IncludedElements(U); excluded := ExcludedElements(U);
    if display or Length(r) <= 20 then cl := AsUnionOfFewClasses(U); fi;
    if   (display or Length(r) > NumberOfResidues(R,m) - 20)
      and Length(r) > NumberOfResidues(R,m)/2
      and included = [] and excluded = []
      and Length(AsUnionOfFewClasses(Difference(R,U))) <= 3
    then
      Print(RingToString(R)," \\ "); View(Difference(R,U));
      return;
    fi;
    if   IsIntegers(R) or IsZ_pi(R)
    then bound := 5; elif short then bound := 3; else bound := 1; fi;
    if display or (IsBound(cl) and Length(cl) <= bound) then
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
              Print(Residue(cl[n]));
              if   IsIntegers(R) or IsZ_pi(R)
              then Print("(",Modulus(cl[n]),")");
              elif short then Print("(mod ",Modulus(cl[n]),")");
              else Print(" ( mod ",Modulus(cl[n])," )"); fi;
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
             then Print(Residue(cl[Length(cl)]),"(",
                        Modulus(cl[Length(cl)]),")");
             else Print(r[Length(r)],"(",m,")"); fi;
        else if   IsBound(cl)
             then if   short
                  then Print(Residue(cl[Length(cl)]),"(mod ",
                             Modulus(cl[Length(cl)]),")");
                  else Print(Residue(cl[Length(cl)])," ( mod ",
                             Modulus(cl[Length(cl)])," )"); fi;
             else if   short
                  then Print(r[Length(r)],"(mod ",m,")");
                  else Print(r[Length(r)]," ( mod ",m," )"); fi;
             fi;
        fi;
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
#M  String( <U> ) . . . . . . . . . . . . . . . . .  for residue class unions
##
InstallMethod( String,
               "for residue class unions (ResClasses)", true,
               [ IsResidueClassUnion ], 0,

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
#M  PrintObj( <U> ) . . . . . . . . . . . . . . . .  for residue class unions
##
InstallMethod( PrintObj,
               "for residue class unions (ResClasses)", true,
               [ IsResidueClassUnion ], 0,

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
#M  Display( <U> ) . . . . . . . . . . . . . .  for residue class unions of Z
##
InstallMethod( Display,
               "for residue class unions of Z (ResClasses)", true,
               [ IsResidueClassUnion ], 0,
               function ( U ) ViewObj(U:RC_DISPLAY); Print("\n"); end );

#############################################################################
##
#E  resclass.gi . . . . . . . . . . . . . . . . . . . . . . . . . . ends here