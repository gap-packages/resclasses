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
#F  InstallLinearOrder( <domains> )
##
DeclareGlobalFunction( "InstallLinearOrder" );
InstallGlobalFunction( InstallLinearOrder,

  function ( domains )

    local  pairs, pair, names, desc, descrev;

    pairs := Combinations([1..Length(domains)],2);
    for pair in pairs do
      names := List([1..2],i->NameFunction(domains[pair[i]]));
      desc    := Concatenation("for ",names[1]," and ",names[2]);
      descrev := Concatenation("for ",names[2]," and ",names[1]);
      InstallMethod( IsSubset, desc, ReturnTrue,
                     [domains[pair[2]],domains[pair[1]]], 0, ReturnTrue );
      InstallMethod( IsSubset, descrev, ReturnTrue,
                     [domains[pair[1]],domains[pair[2]]], 0, ReturnFalse );
      InstallMethod( \=, desc, ReturnTrue,
                     [domains[pair[2]],domains[pair[1]]], 0, ReturnFalse );
      InstallMethod( \=, descrev, ReturnTrue,
                     [domains[pair[1]],domains[pair[2]]], 0, ReturnFalse );
    od;
  end );

#############################################################################
##
##  Some orderings.
##
InstallLinearOrder( [ IsPositiveIntegers, IsNonnegativeIntegers, IsIntegers,
                      IsRationals, IsGaussianRationals ] );
InstallLinearOrder( [ IsPositiveIntegers, IsNonnegativeIntegers, IsIntegers,
                      IsGaussianIntegers, IsGaussianRationals ] );

#############################################################################
##
#M  ViewString( <rat> ) . . . . . . . . . . . . . . . . . . .  for a rational
#M  ViewString( <z> ) . . . . . . . . . . . . . .  for a finite field element
#M  ViewString( <s> ) . . . . . . . . . . . . . . . . . . . . .  for a string
#M  ViewString( <obj> ) . .  for an object with name (added to lib/object.gi)
#M  ViewString( <M> ) . . .  for a full row module (added to lib/modulrow.gi)
##
InstallMethod( ViewString, "for a rational (ResClasses)", true,
               [ IsRat ], 0, String );
InstallMethod( ViewString, "for a finite field element (ResClasses)", true,
               [ IsFFE and IsInternalRep ], 0, String );
InstallMethod( ViewString, "for a string (ResClasses)", true,
               [ IsString ], 0, String );
InstallMethod( ViewString, "for an object with name", true,
               [ HasName ], 0 , Name );
InstallMethod( ViewString, "for a full row module", true,
               [ IsFreeLeftModule and IsFullRowModule ], 0, String );

#############################################################################
##
#M  ViewString( <R> ) . . .  for a polynomial ring (added to lib/ringpoly.gi)
##
InstallMethod( ViewString,
               "for a polynomial ring", true, [ IsPolynomialRing ],
               RankFilter(IsFLMLOR),
  R -> Concatenation(String(LeftActingDomain(R)),
                     Filtered(String(IndeterminatesOfPolynomialRing(R)),
                              ch->ch<>' ')) );

#############################################################################
##
#M  ViewString( <P> ) . . . . for a univariate polynomial over a finite field
##
InstallMethod( ViewString,
               "for univariate polynomial over finite field (ResClasses)",
               true, [ IsUnivariatePolynomial ], 0,

  function ( P )

    local  str, R, F, F_el, F_elints, lngs1, lngs2, i;

    R := DefaultRing(P);
    F := LeftActingDomain(R);
    if not IsPrimeField(F) then
      if IsFinite(F) then return String(P);
                     else TryNextMethod(); fi;
    fi;

    F_el     := List(AsList(F),String);
    F_elints := List(List(AsList(F),Int),String);
    lngs1    := -List(F_el,Length);
    lngs2    := ShallowCopy(lngs1);
    SortParallel(lngs1,F_el);
    SortParallel(lngs2,F_elints);

    str := String(P);

    for i in [1..Length(F_el)] do
      str := ReplacedString(str,F_el[i],F_elints[i]);
    od;

    return str;
  end );

#############################################################################
##
#M  String( <M> ) . . . . . . . . . . . . . . . . . . .  for full row modules
##
##  Added to lib/modulrow.gi.
##
InstallMethod( String, "for full row modules", true,
               [ IsFreeLeftModule and IsFullRowModule ], 0,
  M -> Concatenation(List(["( ",LeftActingDomain(M),"^",
                                DimensionOfVectors(M)," )"], String)) );

#############################################################################
##
#M  String( <R> ) . . . . . . . . . . . . . . . . . . . for a polynomial ring
##
##  Added to lib/ringpoly.gi.
##
InstallMethod( String,
               "for a polynomial ring", true, [ IsPolynomialRing ],
               RankFilter(IsFLMLOR),
               R -> Concatenation("PolynomialRing( ",
                                   String(LeftActingDomain(R)),", ",
                                   String(IndeterminatesOfPolynomialRing(R)),
                                  " )") );

#############################################################################
##
#M  IsRowModule .  return `false' for objects which are not free left modules 
##
##  Added to lib/modulrow.gi.
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
##  Added to lib/grpramat.gi.
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
##  Added to lib/grpramat.gi.
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
#M  \^( <p>, <G> ) . . . . . . . orbit of a point under the action of a group
##
##  Returns the orbit of the point <p> under the action of the group <G>,
##  with respect to the action OnPoints.
##
##  The following cases are handled specially:
##
##    - if <p> is an element of <G>, then the method returns the
##      conjugacy class of <G> which contains <p>, and
##    - if <p> is a subgroup of <G>, then the method returns the
##      conjugacy class of subgroups of <G>  which contains <p>.
##
InstallOtherMethod( \^, "orbit of a point under the action of a group",
                    ReturnTrue, [ IsObject, IsGroup ], 0,

  function ( p, G )
    if   p in G then return ConjugacyClass(G,p);
    elif IsGroup(p) and IsSubgroup(G,p)
    then return ConjugacyClassSubgroups(G,p);
    else return Orbit(G,p,OnPoints); fi;
  end );

#############################################################################
##
#S  A tool for telling GAP about linear order relations (inclusions, etc.) //
##  between certain objects (particularly domains). /////////////////////////
##
#############################################################################

#############################################################################
##
#F  LinearOrder( <rel>, <domains> )
##
DeclareGlobalFunction( "LinearOrder" );
InstallGlobalFunction( LinearOrder,

  function ( rel, domains )

    local  pairs, pair;

    pairs := Combinations([1..Length(domains)],2);
    for pair in pairs do
      InstallMethod( rel,
                     Concatenation( "for ",NameFunction(domains[pair[2]]),
                                    " and ",NameFunction(domains[pair[1]]) ),
                     ReturnTrue, [domains[pair[1]],domains[pair[2]]], 0,
                     ReturnFalse );
      InstallMethod( rel,
                     Concatenation( "for ",NameFunction(domains[pair[1]]),
                                    " and ",NameFunction(domains[pair[2]]) ),
                     ReturnTrue, [domains[pair[2]],domains[pair[1]]], 0,
                     ReturnTrue );
    od;
  end );

#############################################################################
##
#S  Miscellanea. ////////////////////////////////////////////////////////////
##
#############################################################################

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
#F  IntOrInfinityToLaTeX( n )
##
BindGlobal( "IntOrInfinityToLaTeX",
  function( n )
    if   IsInt(n)      then return String(n);
    elif IsInfinity(n) then return "\\infty";
    else return fail; fi;
  end );

#############################################################################
##
#V  One-character global variables ...
##
##  ... should not be overwritten when reading test files, e.g., although
##  one-letter variable names are used in test files frequently.
##  This is just the list of their names.
##
##  The actual caching is done by `ResClassesDoThingsToBeDoneBeforeTest' and
##  `ResClassesDoThingsToBeDoneAfterTest'.
##
BindGlobal( "ONE_LETTER_GLOBALS",
  List( "ABCDFGHIJKLMNOPQRSTUVWYabcdefghijklmnopqrstuvwxyz", ch -> [ch] ) );

#############################################################################
##
#O  Make `Float', `IsFloat' etc. available again if changed to `MacFloat'
##
if not IsBound(Float) then
  DeclareSynonym( "Float", MacFloat );
  DeclareSynonym( "IsFloat", IsMacFloat );
fi;

#############################################################################
##
#E  general.g . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here