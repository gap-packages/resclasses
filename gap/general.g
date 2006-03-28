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
#E  general.g . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here