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
#M  String( <M> ) . . . . . . . . . . . . . . . . . . . . . . for row modules
##
InstallMethod( String,
               "for row modules", true, [ IsRowModule ], 0,
  M -> Concatenation(List(["( ",LeftActingDomain(M),"^",
                                DimensionOfVectors(M)," )"],String)) );

#############################################################################
##
#M  ViewString( <M> ) . . . . . . . . . . . . . . . . . . . . for row modules
##
InstallMethod( ViewString, "for row modules", true, [ IsRowModule ], 0,
               String );

#############################################################################
##
#F  BlankFreeString( <obj> ) . . . . . . . . . . . . .  string without blanks
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
#M  String( <R> ) . . . . . . . . . . . . . . . . . . . for a polynomial ring
##
InstallMethod( String,
               "for a polynomial ring", true, [ IsPolynomialRing ],
               RankFilter(IsFLMLOR),

  function ( R )

    local  s, i, f;

    s := Concatenation("PolynomialRing( ",String(LeftActingDomain(R)),", [");
    f:=false;
    for i in IndeterminatesOfPolynomialRing(R) do
      if f then Append(s,", ");fi;
      s := Concatenation(s,"\"",String(i),"\"");
      f:=true;
    od;
    Append(s,"] )");
    return s;
  end );

#############################################################################
##
#M  ViewString( <R> ) . . . . . . . . . . . . . . . . . for a polynomial ring
##
InstallMethod( ViewString,
               "for a polynomial ring", true, [ IsPolynomialRing ],
               RankFilter(IsFLMLOR),
  R -> Concatenation(String(LeftActingDomain(R)),
                     BlankFreeString(IndeterminatesOfPolynomialRing(R))) );

#############################################################################
##
#M  ViewString( <obj> ) . . . . . . . . . . . . . . . . for objects with name
##
InstallMethod( ViewString, "for objects with name", true,
               [ IsObject and HasName ], 0, Name );

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
#M  \in( <g>, SL( <n>, Integers ) )
##
InstallMethod( \in,
               "for matrix and SL(n,Z) (ResClasses)", IsElmsColls,
               [ IsMatrix, IsNaturalSLnZ ],

  function ( g, SLnZ )
    return DimensionsMat(g) = DimensionsMat(One(SLnZ))
       and ForAll(Flat(g),IsInt) and DeterminantMat(g) = 1;
  end );

#############################################################################
##
#S  Miscellanea. ////////////////////////////////////////////////////////////
##
#############################################################################

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