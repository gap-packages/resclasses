#############################################################################
##
#W  general.g              GAP4 Package `ResClasses'              Stefan Kohl
##
#H  @(#)$Id$
##
##  This file contains a couple of functions and methods which are not
##  directly related to computations with residue classes, and which might
##  perhaps later be moved into the GAP Library.
##
Revision.general_g :=
  "@(#)$Id$";

#############################################################################
##
#F  SendEmail( <sendto>, <copyto>, <subject>, <text> ) . . . .  send an email
##
##  Sends an e-mail with subject <subject> and body <text> to the addresses
##  in the list <sendto>, and copies it to those in the list <copyto>.
##  The first two arguments must be lists of strings, and the latter two must
##  be strings.
##
BindGlobal( "SendEmail",

  function ( sendto, copyto, subject, text )

    local  sendmail, inp;

    sendto   := JoinStringsWithSeparator( sendto, "," );
    copyto   := JoinStringsWithSeparator( copyto, "," );
    sendmail := Filename( DirectoriesSystemPrograms(  ), "mail" );
    inp      := InputTextString( text );
    return Process( DirectoryCurrent(  ), sendmail, inp, OutputTextNone(  ),
                    [ "-s", subject, "-c", copyto, sendto ] );
  end );

#############################################################################
##
#F  EmailLogFile( <addresses> )  send current logfile by email to <addresses>
##
##  Sends the current logfile by e-mail to <addresses>, if GAP is in logging
##  mode and one is working under UNIX, and does nothing otherwise.
##  The argument <addresses> must be either a list of email addresses or
##  a single e-mail address. Long log files are abbreviated, i.e. if the log
##  file is larger than 64KB, then any output is truncated at 1KB, and if the
##  log file is still longer than 64KB afterwards, it is truncated at 64KB.
##
BindGlobal( "EmailLogFile", 

  function ( addresses )

    local  filename, logfile, selection, pos1, pos2;

    if ARCH_IS_UNIX() and IN_LOGGING_MODE <> false then
      if IsString(addresses) then addresses := [addresses]; fi;
      filename := USER_HOME_EXPAND(IN_LOGGING_MODE);
      logfile  := ReadAll(InputTextFile(filename));
      if Length(logfile) > 2^16 then # Abbreviate output in long logfiles.
        selection := ""; pos1 := 1;
        repeat
          pos2 := PositionSublist(logfile,"gap> ",pos1);
          if pos2 = fail then pos2 := Length(logfile) + 1; fi;
          Append(selection,logfile{[pos1..Minimum(pos1+1024,pos2-1)]});
          if pos1 + 1024 < pos2 - 1 then
            Append(selection,
                   logfile{[pos1+1025..Position(logfile,'\n',pos1+1024)]});
            Append(selection,"                                    ");
            Append(selection,"[ ... ]\n");
          fi;
          pos1 := pos2;
        until pos2 >= Length(logfile);
        logfile := selection;
        if Length(logfile) > 2^16 then logfile := logfile{[1..2^16]}; fi;
      fi;
      return SendEmail(addresses,[],Concatenation("GAP logfile ",filename),
                       logfile);
    fi;
  end );

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