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
#S  SendEmail and EmailLogFile. /////////////////////////////////////////////
##
#############################################################################

#############################################################################
##
#F  SendEmail( <sendto>, <copyto>, <subject>, <text> ) . . . . send an e-mail
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
#F  EmailLogFile( <addresses> ) . . .  send log file by e-mail to <addresses>
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
#S  A simple caching facility. //////////////////////////////////////////////
##
#############################################################################

#############################################################################
##
#F  SetupCache( <name>, <size> )
##
##  Creates an empty cache named <name> for at most <size> values.
##
BindGlobal( "SetupCache",
  function ( name, size )
    BindGlobal(name,[[size,-1,fail]]);
  end );

#############################################################################
##
#F  PutIntoCache( <name>, <key>, <value> )
##
##  Puts the entry <value> with key <key> into the cache named <name>.
##
BindGlobal( "PutIntoCache",

  function ( name, key, value )

    local  cache, pos, i;

    cache := ValueGlobal(name);
    MakeReadWriteGlobal(name);
    pos := Position(List(cache,t->t[1]),key,1);
    if pos = fail then Add(cache,[key,0,value]);
                  else cache[pos][2] := 0; fi;
    for i in [2..Length(cache)] do
      cache[i][2] := cache[i][2] + 1;
    od;
    Sort(cache,function(t1,t2) return t1[2]<t2[2]; end);
    if   Length(cache) > cache[1][1]+1
    then cache := cache{[1..cache[1][1]+1]}; fi;
    MakeReadOnlyGlobal(name);
  end );

#############################################################################
##
#F  FetchFromCache( <name>, <key> )
##
##  Picks the entry with key <key> from the cache named <name>.
##  Returns fail if no such entry is present.
##
BindGlobal( "FetchFromCache",

  function ( name, key )

    local  cache, pos, i;

    cache := ValueGlobal(name);
    pos   := Position(List(cache,t->t[1]),key,1);
    if IsInt(pos) then
      MakeReadWriteGlobal(name);
      cache[pos][2] := 0;
      for i in [2..Length(cache)] do
        cache[i][2] := cache[i][2] + 1;
      od;
      MakeReadOnlyGlobal(name);
      return cache[pos][3];
    fi;
    return fail;
  end );

#############################################################################
##
#S  Some trivial methods which are missing in the GAP Library. //////////////
##
#############################################################################

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
#F  BlankFreeString( <obj> ) . . . . . . . . . . . . . .string without blanks
##
BindGlobal( "BlankFreeString",

  function ( obj )

    local  str;

    str := String(obj);
    RemoveCharacters(str," ");
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
#M  IsRowModule .  return `false' for objects which are not free left modules 
##
InstallOtherMethod( IsRowModule,
                    Concatenation("return `false' for objects which are ",
                                  "not free left modules (ResClasses)"),
                    true, [ IsObject ], 0,

  function ( obj )
    if not IsFreeLeftModule(obj) then return false; else TryNextMethod(); fi;
  end );

#############################################################################
##
#M  \in( <g>, GL( <n>, Integers ) )
##
InstallMethod( \in,
               "for matrix and GL(n,Z) (ResClasses)", IsElmsColls,
               [ IsMatrix, IsNaturalGLnZ ],

  function ( g, GLnZ )
    return DimensionsMat(g) = DimensionsMat(One(GLnZ))
       and ForAll(Flat(g),IsInt) and DeterminantMat(g) in [-1,1];
  end );

#############################################################################
##
#S  GAP Library bugfix(es). /////////////////////////////////////////////////
##
#############################################################################

#############################################################################
##
#M  Intersection2( <C1>, <C2> ) . . . . . . . . . . . . .  GAP Library bugfix
##
InstallMethod( Intersection2,
               "for two coll's in the same family, the second being a list",
               IsIdenticalObj, [ IsCollection, IsCollection and IsList ], 1,

  function ( C1, C2 )

    local  I, elm;

    if ( HasIsFinite( C1 ) or CanComputeSize( C1 ) ) and IsFinite( C1 ) then
      I := ShallowCopy( AsSSortedList( C1 ) ); IntersectSet( I, C2 );
    else
      I := []; for elm in C2 do if elm in C1 then AddSet( I, elm ); fi; od;
    fi;
    return I;
  end );

InstallMethod( Intersection2,
               "for two coll's in the same family, the first being a list",
               IsIdenticalObj, [ IsCollection and IsList, IsCollection ], 1,

  function ( C1, C2 )

    local  I, elm;

    if ( HasIsFinite( C2 ) or CanComputeSize( C2 ) ) and IsFinite( C2 ) then
      I := ShallowCopy( AsSSortedList( C2 ) ); IntersectSet( I, C1 );
    else
      I := []; for elm in C1 do if elm in C2 then AddSet( I, elm ); fi; od;
    fi;
    return I;
  end );

#############################################################################
##
#S  Miscellanea. ////////////////////////////////////////////////////////////
##
#############################################################################

#############################################################################
##
#V  ENTIRE_LIST_OR_RECORD_VIEWING_THRESHOLD
#F  SetEntireListOrRecordViewingThreshold( <threshold> )
##
BindGlobal( "ENTIRE_LIST_OR_RECORD_VIEWING_THRESHOLD", 10000 );
BindGlobal( "SetEntireListOrRecordViewingThreshold",
  function ( threshold )
    if   not IsPosInt(threshold) and not IsInfinity(threshold)
    then return fail; fi;
    MakeReadWriteGlobal("ENTIRE_LIST_OR_RECORD_VIEWING_THRESHOLD");
    ENTIRE_LIST_OR_RECORD_VIEWING_THRESHOLD := threshold;
    MakeReadOnlyGlobal("ENTIRE_LIST_OR_RECORD_VIEWING_THRESHOLD");
    return threshold;
  end );

#############################################################################
##
#M  View( <list> ) . . . . . . . . . . . . . . . . . . . . . . for long lists
##
InstallMethod( ViewObj,
               "for long lists (ResClasses)", true, [ IsList ], SUM_FLAGS,

  function ( list )

    local  pos;

    if not TNUM_OBJ_INT(list) in [FIRST_LIST_TNUM..LAST_LIST_TNUM]
      or Length(list) < 1000
      or MemoryUsage(list) < ENTIRE_LIST_OR_RECORD_VIEWING_THRESHOLD
    then TryNextMethod(); fi;
    if not IsString(list) then
      Print("<",TNUM_OBJ(list)[2]," [ ");
      for pos in [1..10] do View(list[pos]); Print(", "); od;
      Print("... ], of length ",Length(list),">");
    else
      Print("<string of length ",Length(list),", starting ");
      View(list{[1..20]}); Print(">");
    fi;
  end );

#############################################################################
##
#M  View( <rec> ) . . . . . . . . . . . . .  for records with many components
##
InstallMethod( ViewObj,
               "for records with many components (ResClasses)", true,
               [ IsRecord ], SUM_FLAGS,

  function ( record )

    local  names;

    names := RecNames(record);
    if   MemoryUsage(record) < ENTIRE_LIST_OR_RECORD_VIEWING_THRESHOLD
      or Length(names) < 10
    then TryNextMethod(); fi;
    Print("<record with components ");
    View(names); Print(">");
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