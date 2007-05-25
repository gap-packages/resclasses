#############################################################################
##
#W  general.g              GAP4 Package `ResClasses'              Stefan Kohl
##
#H  @(#)$Id$
##
##  This file contains a couple of methods which might perhaps later be moved
##  into the GAP Library.
##
Revision.general_g :=
  "@(#)$Id$";

#############################################################################
##
#M  String( <obj> ) . . . . . . default method, returns the output by `Print'
##
InstallMethod( String,
               "default method, returns the output by `Print' (ResClasses)",
               true, [ IsObject ], 0,

  function( obj )

    local  str, out;

    str := "";
    out := OutputTextString( str, true );
    PrintTo( out, obj );
    CloseStream(out);
    return str;
  end );

#############################################################################
##
#M  ViewString( <obj> ) . default method - use `Name' or dispatch to `String'
##
InstallMethod( ViewString,
               Concatenation("default method - use `Name' or dispatch to ",
                             "`String' (ResClasses)"), true, [ IsObject ], 0,

  function ( obj )
    if HasName(obj) then return Name(obj); else return String(obj); fi;
  end );

#############################################################################
##
#M  ViewString( <R> ) . . . . . . . . . . . . . . . . . for a polynomial ring
##
InstallMethod( ViewString,
               "for polynomial rings (ResClasses)", true, 
               [ IsPolynomialRing ], 0,

  R -> Concatenation(String(LeftActingDomain(R)),
                     Filtered(String(IndeterminatesOfPolynomialRing(R)),
                              ch -> ch <> ' ')) );

#############################################################################
##
#M  ViewObj( <R> ) . . . . . . . . . . . . . . . . . .  for a polynomial ring
##
InstallMethod( ViewObj,
               "for polynomial rings (ResClasses)", true,
               [ IsPolynomialRing ], 100,
               function( R ) Print( ViewString(R) ); end );

#############################################################################
##
#F  RingToString( <R> ) . . . how the ring <R> is printed by `View'/`Display'
##
##  The return value of this function determines the way the ring <R> is
##  printed by the methods for `View'/`Display' for residue class unions.
##
BindGlobal( "RingToString",
  function ( R )
    if IsIntegers(R) then return "Z"; else return ViewString(R); fi;
  end );

#############################################################################
##
#M  Intersection2( <C1>, <C2> ) . . . . . . . . . . . . .  GAP Library bugfix
##
InstallMethod( Intersection2,
    "for two collections in the same family, the second being a list",
    IsIdenticalObj,
    [ IsCollection, IsCollection and IsList ], 1,
    function ( C1, C2 )
    local   I, elm;
    if ( HasIsFinite( C1 ) or CanComputeSize( C1 ) ) and IsFinite( C1 ) then
        I := ShallowCopy( AsSSortedList( C1 ) );
        IntersectSet( I, C2 );
    else
        I := [];
        for elm in C2 do
            if elm in C1 then
                AddSet( I, elm );
            fi;
        od;
    fi;
    return I;
    end );

InstallMethod( Intersection2,
    "for two collections in the same family, the first being a list",
    IsIdenticalObj,
    [ IsCollection and IsList, IsCollection ], 1,
    function ( C1, C2 )
    local   I, elm;
    if ( HasIsFinite( C2 ) or CanComputeSize( C2 ) ) and IsFinite( C2 ) then
        I := ShallowCopy( AsSSortedList( C2 ) );
        IntersectSet( I, C1 );
    else
        I := [];
        for elm in C1 do
            if elm in C2 then
                AddSet( I, elm );
            fi;
        od;
    fi;
    return I;
    end );

#############################################################################
##
#V  One-character global variables
##
##  For the convenience of the reader, the manual uses one-character global
##  variables such as `R' for a ring and `U' for a union of residue classes.
##  Further, for the convenience of the user these variables should remain
##  available for redefinition in an interactive session. Therefore these
##  variables must not be readonly -- of course with the natural exception
##  of the `traditional' identifiers `E', `X' and `Z'.
##
BindGlobal( "FREE_ONE_LETTER_GLOBALS",
  List( "ABCDFGHIJKLMNOPQRSTUVWYabcdefghijklmnopqrstuvwxyz", ch -> [ch] ) );
for ch in FREE_ONE_LETTER_GLOBALS do
  if IsReadOnlyGlobal(ch) then MakeReadWriteGlobal(ch); fi;
od;

#############################################################################
##
#E  general.g . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here