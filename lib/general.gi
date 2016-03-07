#############################################################################
##
#W  general.gi             GAP4 Package `ResClasses'              Stefan Kohl
##
##  This file contains a couple of functions and methods which are not
##  directly related to computations with residue classes, and which might
##  perhaps later be moved into the GAP Library or elsewhere.
##
#############################################################################

#############################################################################
##
#S  Some trivial methods which are missing in the GAP Library. //////////////
##
#############################################################################

#############################################################################
##
#M  ViewString( <P> ) . . . . for a univariate polynomial over a finite field
##
InstallMethod( ViewString,
               "for univariate polynomial over finite field (ResClasses)",
               true, [ IsUnivariatePolynomial ], 0,

  function ( P )

    local  str, R, F, coeffs, coeffstrings, coeffintstrings, i;

    if   ValueGlobal("GF_Q_X_RESIDUE_CLASS_UNIONS_FAMILIES") = []
    then TryNextMethod(); fi;

    str := String(P);

    R := DefaultRing(P);
    F := LeftActingDomain(R);
    if not IsFinite(F) then TryNextMethod(); fi;
    if not IsPrimeField(F) then return str; fi;

    coeffs := CoefficientsOfUnivariateLaurentPolynomial(P)[1];
    coeffs := Concatenation([Zero(F),One(F)],coeffs);
    SortParallel(List(coeffs,c->-Length(String(c))),coeffs);
    coeffstrings    := List(coeffs,String);
    coeffintstrings := List(List(coeffs,Int),String);

    for i in [1..Length(coeffstrings)] do
      str := ReplacedString(str,coeffstrings[i],coeffintstrings[i]);
    od;

    return str;
  end );

#############################################################################
##
#S  Some utility functions. /////////////////////////////////////////////////
##
#############################################################################

#############################################################################
##
#F  PositionsSublist( <list>, <sub> )
##
InstallMethod( PositionsSublist, "default method",
               ReturnTrue, [ IsList, IsList ],

  function ( list, sub )

    local  positions, pos;

    pos := 0; positions := [];
    repeat
      pos := PositionSublist(list,sub,pos);
      if pos <> fail then Add(positions,pos); fi;
    until pos = fail;
    return positions;
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
InstallGlobalFunction( SetupCache,
                       function ( name, size )
                         BindGlobal(name,[[size,-1,fail]]);
                       end );

#############################################################################
##
#F  PutIntoCache( <name>, <key>, <value> )
##
InstallGlobalFunction( PutIntoCache,

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
InstallGlobalFunction( "FetchFromCache",

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
#S  Creating timestamped logfiles. //////////////////////////////////////////
##
#############################################################################

#############################################################################
##
#F LogToDatedFile( <directory> )
##
InstallGlobalFunction( LogToDatedFile,

  function ( arg )

    local  name, directory, gettimeofday, dmy;

    if IsBoundGlobal("IO_gettimeofday") then
      gettimeofday := ValueGlobal("IO_gettimeofday");
    else
      Error("the function `LogToDatedFile' is available only if the ",
            "IO package\nis installed and compiled.");
      return fail;
    fi;
    if   Length(arg) >= 1 and IsString(arg[1])
    then directory := arg[1];
    else directory := "/user/GAP/log/"; fi;
    dmy := DMYhmsSeconds(gettimeofday().tv_sec);
    name := Concatenation(directory,
                          String(dmy[3]),"-",
                          String(dmy[2]+100){[2..3]},"-",
                          String(dmy[1]+100){[2..3]}," ",
                          String(dmy[4]+100){[2..3]},"-",
                          String(dmy[5]+100){[2..3]},"-",
                          String(dmy[6]+100){[2..3]},".log");
    if IN_LOGGING_MODE <> false then LogTo(); fi;
    LogTo(name);
    return name;
  end );

#############################################################################
##
#S  SendEmail, EmailLogFile and DownloadFile ////////////////////////////////
##
#############################################################################

#############################################################################
##
#F  SendEmail( <sendto>, <copyto>, <subject>, <text> ) . . . . send an e-mail
##
InstallGlobalFunction( SendEmail,

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
InstallGlobalFunction( EmailLogFile, 

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
#F  DownloadFile( <url> ) . . . . . . . . . download a file from the internet
##
InstallGlobalFunction( DownloadFile,

  function ( url )

    local  Download, host, path, slashpos, r;

    if   IsBoundGlobal("SingleHTTPRequest")
    then Download := ValueGlobal("SingleHTTPRequest");
    else Info(InfoWarning,1,"DownloadFile: the IO package is not loaded.");
         return fail;
    fi;
    url := ReplacedString(url,"http://","");
    slashpos := Position(url,'/');
    host := url{[1..slashpos-1]};
    path := url{[slashpos..Length(url)]};
    r := Download(host,80,"GET",path,rec(),false,false);
    if r.statuscode = 0 then
      Info(InfoWarning,1,"Downloading ",url," failed: ",r.status);
      return fail;
    fi;
    return r.body;
  end );

#############################################################################
##
#E  general.gi . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
