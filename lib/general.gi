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
#S  Multiplication with infinity. ///////////////////////////////////////////
##
#############################################################################

#############################################################################
##
#M  \*( <n>, infinity ) . . . . . . . . .  for positive rational and infinity
#M  \*( infinity, <n> ) . . . . . . . . .  for infinity and positive rational
#M  \*( infinity, infinity )  . . . . . . . . . . . for infinity and infinity
##
InstallMethod( \*, "for positive rational and infinity (RCWA)",
               ReturnTrue, [ IsPosRat, IsInfinity ], 0,
               function ( n, infty ) return infinity; end );
InstallMethod( \*, "for infinity and positive rational (RCWA)",
               ReturnTrue, [ IsInfinity, IsPosRat ], 0,
               function ( infty, n ) return infinity; end );
InstallMethod( \*, "for infinity and infinity (RCWA)",
               ReturnTrue, [ IsInfinity, IsInfinity ], 0,
               function ( infty1, infty2 ) return infinity; end );

#############################################################################
##
#S  List operations. ////////////////////////////////////////////////////////
##
#############################################################################

#############################################################################
##
#M  PositionsSublist( <list>, <sub> )
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
#M  EquivalenceClasses( <list>, <relation> )
#M  EquivalenceClasses( <list>, <classinvariant> )
##
##  Returns a list of equivalence classes on <list> under <relation>
##  or a list of equivalence classes on <list> given by <classinvariant>,
##  respectively.
##
##  The argument <relation> must be a function which takes as arguments
##  two entries of <list> and returns either true or false, and which
##  describes an equivalence relation on <list>.
##  The argument <classinvariant> must be a function which takes as argument
##  an element of <list> and returns a class invariant.
##  
InstallOtherMethod( EquivalenceClasses,
                    "for a list and a relation or a class invariant (RCWA)",
                    ReturnTrue, [ IsList, IsFunction ], 0,

  function ( list, relation )

    local  classes, invs, longestfirst, byinvs, elm, pos, inserted, count;

    if IsEmpty(list) then return []; fi;

    longestfirst := function(c1,c2) return Length(c1) > Length(c2); end;
    byinvs := function(c1,c2) return relation(c1[1]) < relation(c2[1]); end;

    if   NumberArgumentsFunction(relation) = 1 then
      invs    := List(list,relation);
      classes := List(Set(invs),inv->list{Positions(invs,inv)});
      Sort(classes,byinvs);
    elif NumberArgumentsFunction(relation) = 2 then
      classes := [[list[1]]]; count := 0;
      for elm in list{[2..Length(list)]} do
        inserted := false; count := count + 1;
        for pos in [1..Length(classes)] do
          if relation(elm,classes[pos][1]) then
            Add(classes[pos],elm);
            inserted := true;
            break;
          fi;
        od;
        if   not inserted
        then classes := Concatenation(classes,[[elm]]); fi;
        if   count mod 100 = 0 # rough performance heuristics ...
        then Sort(classes,longestfirst); fi;
      od;
      Sort(classes,longestfirst);
    else TryNextMethod(); fi;

    return classes;
  end );

#############################################################################
##
#S  Utility functions for groups and their elements. ////////////////////////
##
#############################################################################

#############################################################################
##
#F  LaTeXStringWord( <w> ) . . . . . . . . . .  LaTeX string for a group word
##
InstallGlobalFunction( "LaTeXStringWord",

  function ( w )

    local  s, i;

    s := String(w);
    s := ReplacedString(s,"^","^{");
    for i in [0..9] do
      s := ReplacedString(s,Concatenation(String(i),"*"),
                            Concatenation(String(i),"}*"));
      s := ReplacedString(s,Concatenation(String(i),")"),
                            Concatenation(String(i),"})"));
    od;
    s := ReplacedString(s,"*","");
    if s[Length(s)] in DIGITS then Append(s,"}"); fi;
    for i in [2..9] do
      s := ReplacedString(s,Concatenation("{",String(i),"}"),String(i));
    od;
    return s;
  end );

#############################################################################
##
#S  Functions to generate small graphs. /////////////////////////////////////
##
#############################################################################

#############################################################################
##
#F  AllGraphs( <n> ) . . . .  all graphs with <n> vertices, up to isomorphism
##
InstallMethod( AllGraphs,
               "for given number of vertices", true, [ IsPosInt ], 0,
               n -> List( GraphClasses( n ), Representative ) );

#############################################################################
##
#F  GraphClasses( <n> )  isomorphism classes of graphs with vertices 1,2,..,n
##
InstallMethod( GraphClasses,
               "for given number of vertices", true, [ IsPosInt ], 0,

  function ( n )

    local  classes;

    classes := ShallowCopy(Orbits(SymmetricGroup(n),
                                  Combinations(Combinations([1..n],2)),
                                  function(Gamma,g)
                                    return Set(Gamma,k->OnSets(k,g));
                                  end));
    SortParallel(List(classes,cl->Length(cl[1])),classes);
    return classes;
  end );

#############################################################################
##
#F  IdGraphNC( <graph>, <classes> ) . . identify isomorphism class of <graph>
##
InstallMethod( IdGraphNC,
               "for a graph and a list of classes of graphs", ReturnTrue,
               [ IsList, IsList ], 0,

  function ( graph, classes )

    local  vertexnums, i;

    vertexnums := Set(Flat(graph));
    graph      := Set(graph,edge->List(edge,v->Position(vertexnums,v)));
    return First([1..Length(classes)],
                 i ->    Length(graph) = Length(classes[i][1])
                     and graph in classes[i]);
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
      filename := UserHomeExpand(IN_LOGGING_MODE);
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
#S  Routines for bitmap pictures. ///////////////////////////////////////////
##
#############################################################################

#############################################################################
##
#F  SaveAsBitmapPicture( <picture>, <filename> ) . . . .  save bitmap picture
##
InstallGlobalFunction( SaveAsBitmapPicture,

  function ( picture, filename )

    local  AppendHex, Append16Bit, Append32Bit, str, colored,
           height, width, fullwidth, length, offset, vec8, pix,
           chunk, fill, x, y, n, i;

    Append16Bit := function ( n )
      Add(str,CHAR_INT(n mod 256)); Add(str,CHAR_INT(Int(n/256)));
    end;

    Append32Bit := function ( n )
      Add(str,CHAR_INT(n mod 256)); n := Int(n/256);
      Add(str,CHAR_INT(n mod 256)); n := Int(n/256);
      Add(str,CHAR_INT(n mod 256)); n := Int(n/256);
      Add(str,CHAR_INT(n));
    end;

    if not IsMatrix(picture) or not IsString(filename)
      or (not IsInt(picture[1][1]) and not picture[1][1] in GF(2))
    then Error("usage: SaveAsBitmapPicture( <picture>, <filename> )\n"); fi;

    colored := IsInt(picture[1][1]);
    height  := Length(picture);
    width   := Length(picture[1]);
    if colored then fullwidth := width + (width mod 4)/3;
    elif width mod 32 <> 0 then
      fullwidth := width + 32 - width mod 32;
      fill := List([1..fullwidth-width],i->Zero(GF(2)));
      ConvertToGF2VectorRep(fill);
      picture := List(picture,line->Concatenation(line,fill));
    else fullwidth := width; fi;
    str := "BM";
    if colored then offset := 54; length := 3 * fullwidth * height + offset;
               else offset := 62; length := (fullwidth * height)/8 + offset;
    fi;
    for n in [length,0,offset,40,width,height] do Append32Bit(n); od;
    Append16Bit(1);
    if colored then
      Append16Bit(24);
      for i in [1..6] do Append32Bit(0); od;
      for y in [1..height] do
        for x in [1..width] do
          pix := picture[y][x];
          Add(str,CHAR_INT(pix mod 256)); pix := Int(pix/256);
          Add(str,CHAR_INT(pix mod 256)); pix := Int(pix/256);
          Add(str,CHAR_INT(pix));
        od;
        for i in [1..width mod 4] do Add(str,CHAR_INT(0)); od;
      od;
    else # monochrome picture
      Append16Bit(1);
      for i in [1..6] do Append32Bit(0); od;
      Append32Bit(0); Append32Bit(2^24-1);
      vec8 := List([0..255],i->CoefficientsQadic(i+256,2){[8,7..1]})*Z(2)^0;
      for i in [1..256] do ConvertToGF2VectorRep(vec8[i]); od;
      for y in [1..height] do
        for x in [1,9..fullwidth-7] do
          Add(str,CHAR_INT(PositionSorted(vec8,picture[y]{[x..x+7]})-1));
        od;
      od;
    fi;
    FileString(filename,str);
  end );

#############################################################################
##
#F  LoadBitmapPicture( <filename> ) . . . . . . . . . . . load bitmap picture
##
InstallGlobalFunction( LoadBitmapPicture,

  function ( filename )

    local  str, picture, height, width, fullwidth, vec8, chunk, x, y, i;

    if   not IsString(filename)
    then Error("usage: LoadBitmapPicture( <filename> )\n"); fi;

    str    := StringFile(filename);
    if str = fail then Error("file not found"); return fail; fi;
    width  := List(str{[19..22]},INT_CHAR) * List([0..3],i->256^i);
    height := List(str{[23..26]},INT_CHAR) * List([0..3],i->256^i);
    if INT_CHAR(str[29]) = 24 then # 24-bit RGB picture
      fullwidth := width + (width mod 4)/3;
      picture := List([1..height],
                      y->List([1..Int(fullwidth)],
                              x->List(str{[55+3*(fullwidth*(y-1)+x-1)..
                                           55+3*(fullwidth*(y-1)+x-1)+2]},
                                      INT_CHAR)
                                *[1,256,65536]));
    else # monochrome picture
      if width mod 32 = 0 then fullwidth := width;
                          else fullwidth := width + 32 - width mod 32; fi;
      vec8 := List([0..255],i->CoefficientsQadic(i+256,2){[8,7..1]})*Z(2)^0;
      for i in [1..256] do ConvertToGF2VectorRep(vec8[i]); od;
      picture := List([1..height],y->Concatenation(List([1,9..fullwidth-7],
                     x->vec8[INT_CHAR(str[63+(fullwidth*(y-1)+x-1)/8])+1])));
    fi;
    if width = fullwidth then return picture;
                         else return picture{[1..height]}{[1..width]}; fi;
  end );

#############################################################################
##
#F  DrawLineNC( <pic>, <x1>, <y1>, <x2>, <y2>, <color>, <width> )
##
InstallGlobalFunction( DrawLineNC,

  function ( pic, x1, y1, x2, y2, color, width )

    local  w, h, x, y, ym, d, c, b1, b2, switched, tmp, i, j;

    w := Length(pic[1]); h := Length(pic);
    if AbsInt(x2-x1) < AbsInt(y2-y1) then
      tmp := x1; x1 := y1; y1 := tmp;
      tmp := x2; x2 := y2; y2 := tmp;
      switched := true;
    else switched := false; fi;
    d := (y2-y1)/(x2-x1);
    c := Sqrt(Float(((x2-x1)^2+(y2-y1)^2)/(x2-x1)^2))/2;
    for x in [Minimum(x1,x2)..Maximum(x1,x2)] do
      ym := y1+(x-x1)*d;
      b1 := ym-c*width;
      b2 := ym+c*width;
      for y in [Int(b1)..Int(b2+0.5)] do
        if switched then
          if x < 1 or x > h or y < 1 or y > w then continue; fi;
        else
          if y < 1 or y > h or x < 1 or x > w then continue; fi;
        fi;
        if 1.0*y > b1 and 1.0*y < b2 then
          if switched then pic[x][y] := color; else pic[y][x] := color; fi;
        else
          if Random([1..100]) < Int(Minimum(100*(b1-y),100*(y-b2))) then
            if switched then pic[x][y] := color; else pic[y][x] := color; fi;
          fi;
        fi;
      od;
    od;
  end );

#############################################################################
##
#F  DrawGrid( <U>, <range_y>, <range_x>, <filename> )
##
InstallGlobalFunction( DrawGrid,

  function ( U, range_y, range_x, filename )

    local  grid, x, y, one, offset_x, offset_y, colors, color, pos;

    if   not (   IsResidueClassUnionOfZxZ(U)
              or IsList(U) and ForAll(U,IsResidueClassUnionOfZxZ))
      or not IsRange(range_y) or not IsRange(range_x)
      or not IsString(filename)
    then
      Error("usage: DrawGrid( <U>, <range_y>, <range_x>, <filename> )\n");
      return fail;
    fi;

    offset_x := -Minimum(range_x) + 1;
    offset_y := -Minimum(range_y) + 1;

    if IsResidueClassUnionOfZxZ(U) then

      grid     := NullMat(Length(range_y),Length(range_x),GF(2));
      one      := One(GF(2));

      for y in range_y do for x in range_x do
        if not [y,x] in U then grid[y+offset_y][x+offset_x] := one; fi;
      od; od;

    else

      colors := [[255,0,0],[0,255,0],[0,0,255],[255,255,0],[255,0,255],
                 [0,255,255],[255,128,128],[128,255,128],[128,128,255]]
              * [65536,256,1];

      grid := NullMat(Length(range_y),Length(range_x));

      for y in range_y do
        for x in range_x do
          pos := First([1..Length(U)],k->[y,x] in U[k]);
          if   pos = fail then color := 0;
          elif pos > Length(colors) then color := 2^24-1;
          else color := colors[pos]; fi;
          grid[y+offset_y][x+offset_x] := color;
        od;
      od;

    fi;

    SaveAsBitmapPicture( grid, filename );

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
#S  Other utilities. ////////////////////////////////////////////////////////
##
#############################################################################

#############################################################################
##
#F  AssignGlobalNC( <name>, <value> ) .  forced assignment to global variable
##
InstallGlobalFunction( AssignGlobalNC,

  function ( name, value )
    if IsReadOnlyGlobal(name) then MakeReadWriteGlobal(name); fi;
    if IsBoundGlobal(name)    then UnbindGlobal(name);        fi;
    BindGlobal(name,value);
  end );

#############################################################################
##
#F  GetOption( <option>, <default> [, <filter> ] )
##
InstallGlobalFunction( GetOption,

  function ( arg )

    local  value, option, filter, default;

    if not Length(arg) in [2,3] or not IsString(arg[1]) then return fail; fi;
    option  := arg[1];
    default := arg[2];
    if Length(arg) = 2 then
      filter := IsObject;
    else
      filter := arg[3];
      if not IsFunction(filter) then return fail; fi;
    fi;
    value := ValueOption(option);
    if   value <> fail and filter(value) = true
    then return value;
    else return default; fi;
  end );

#############################################################################
##
#S  Package-specific customizations. ////////////////////////////////////////
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
#E  general.gi . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
