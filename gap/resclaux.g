#############################################################################
##
#W  resclaux.g             GAP4 Package `ResClasses'              Stefan Kohl
##
#H  @(#)$Id$
##
##  This file contains auxiliary functions for the ResClasses package.
##
Revision.resclaux_g :=
  "@(#)$Id$";

# Missing `String' method for Integers.

InstallMethod( String, "for Integers", true, [ IsIntegers ], 0,
               Ints -> "Integers" );

# The string representation of One(GF(2)) is subject to change to "Z(2)^0".

if not CompareVersionNumbers( VERSION, "4r4" ) then
  InstallMethod( String, "for One(GF(2))", true, [ IsFFE ], SUM_FLAGS,
  function ( ffe )
    if ffe = Z(2) then return "Z(2)^0"; else TryNextMethod(); fi;
  end );
fi;

#############################################################################
##
##  Reverse monomial ordering in GAP 4.3. The leading term should be printed
##  first. These functions are taken from ratfun.gi resp. ratfunul.gi in the
##  library, and will be included in GAP 4.4.
##
if not CompareVersionNumbers( VERSION, "4r4" ) then

  MakeReadWriteGlobal( "ExtRepOfPolynomial_String" );

  ExtRepOfPolynomial_String := function(arg)

  local fam,ext,zero,one,mone,i,j,ind,bra,str,s,b,c, mbra,le;

    fam:=arg[1]; ext:=arg[2]; bra:=false; mbra:= false; str:="";
    zero := fam!.zeroCoefficient; one := fam!.oneCoefficient; mone := -one;
    le:=Length(ext); if le=0 then return String(zero); fi;
    for i in [ le-1,le-3..1] do
      if i<le-1 then
        # this is the second summand, so arithmetic will occur
        bra:=true;
      fi;
      if ext[i+1]=one then
        if i<le-1 then Add(str,'+'); fi; c:=false;
      elif ext[i+1]=mone then Add(str,'-'); c:=false;
      else
        s:=String(ext[i+1]); b:=false;
        if not (IsRat(ext[i+1]) or IsFFE(ext[i+1])) then
	  # do 1-st level arithmetics occur in s?
          # we could do better by checking bracketing as well, but this
	  # would be harder.
          j:=2;
          while j<=Length(s) do
            if s[j]='+' or s[j]='-' then b:=true; j:=Length(s)+1; fi;
            j:=j+1;
          od;
	  if b then s:=Concatenation("(",s,")"); fi;
        fi;
        if i<le-1 and s[1]<>'-' then Add(str,'+'); fi;
        Append(str,s); c:=true;
      fi;
      if Length(ext[i])<2 then
        # trivial monomial. Do we have to add a '1'?
        if c=false then Append(str,String(one)); fi;
      else
        if c then Add(str,'*'); mbra:= true; fi;
        for j  in [ 1, 3 .. Length(ext[i])-1 ]  do
	  if 1 < j  then Add(str,'*'); mbra:= true; fi;
          ind:=ext[i][j];
          if HasIndeterminateName(fam,ind) then
            Append(str,IndeterminateName(fam,ind));
          else Append(str,"x_"); Append(str,String(ind)); fi;
          if 1 <> ext[i][j+1] then
            Add(str,'^'); Append(str,String(ext[i][j+1]));
	  fi;
        od;
      fi;
    od;
    if    ( bra and Length( arg ) >= 3 and arg[3] = true )
       or ( mbra and Length( arg ) = 4 and arg[4] = true ) then
      str:=Concatenation("(",str,")");
    fi;
    return str;
  end;

  MakeReadOnlyGlobal( "ExtRepOfPolynomial_String" );

  MakeReadWriteGlobal( "DoPrintUnivariateLaurent" );

  DoPrintUnivariateLaurent := function(fam,cofs,val,ind)

  local zero,one,mone,i,c,name,lc;

    if HasIndeterminateName(fam,ind) then
      name:=IndeterminateName(fam,ind);
    else
      name:=Concatenation("x_",String(ind));
    fi;
    zero := fam!.zeroCoefficient; one := fam!.oneCoefficient;
    mone := -one;
    if Length(cofs)=0 then Print(zero); fi;
    lc:=Length(cofs);
    for i  in [ lc,lc-1..1 ]  do
      if cofs[i] <> zero  then
        # print a '+' if necessary
        c := "*";
        if i <lc  then
          if IsRat(cofs[i]) then
            if   cofs[i] = one  then Print( "+" ); c := "";
            elif cofs[i] > 0    then Print( "+", cofs[i] );
            elif cofs[i] = mone then Print( "-" ); c := "";
            else Print( cofs[i] );
            fi;
          elif cofs[i] = one  then Print( "+" ); c := "";
          elif cofs[i] = mone then Print( "-" ); c := "";
          else Print( "+", cofs[i] );
          fi;
        elif cofs[i] = one  then c := "";
        elif cofs[i] = mone then Print("-"); c := "";
        else Print(cofs[i]);
        fi;
        if i+val <> 1 then
          Print( c, name );
          if i+val <> 2 then Print( "^", i+val-1 ); fi;
        elif cofs[i] = one  then Print(one);
        elif cofs[i] = mone then Print(one);
        fi;
      fi;
    od;
  end;

  MakeReadOnlyGlobal( "DoPrintUnivariateLaurent" );

fi;

#############################################################################
##
#F  ColorPrompt( b ) . . . . . . . . . . . . . . . . . . . the coloring stuff
##
##  This encloses Frank L\"ubeck's code for coloring GAP prompts, user input
##  and online help texts.
##  Coloring can be switched off by setting the option `BlackAndWhite' when
##  loading ResClasses.
##
if not CompareVersionNumbers( VERSION, "4r4" ) then

  STDOUT := OutputTextUser();;
  PrintPromptHook:=CPROMPT;; EndLineHook:=function() end;;

  if not IsBound( ColorPrompt ) then  

    ColorPrompt := function( b )
  
      if b = false then
        Unbind(PrintPromptHook); Unbind(EndLineHook); return;
      fi;

      # print the prompt

      PrintPromptHook := function( )
  
        local cp;
  
        cp := CPROMPT();
        if cp = "gap> " then cp := "gap> "; fi;
        # different color for brk...> prompts
        if Length(cp)>0 and cp[1] = 'b' then
          WriteAll(STDOUT, "\033[1m\033[31m");
        else
          WriteAll(STDOUT, "\033[1m\033[34m");
        fi;
        # use this instead of Print such that the column counter for the 
        # command line editor is correct
        PRINT_CPROMPT(cp);
        # another color for input
        WriteAll(STDOUT, "\033[0m\033[31m");
      end;

      # reset attributes before going to the next line

      EndLineHook := function()
        WriteAll(STDOUT, "\033[0m");
      end;
    end;
    MakeReadOnlyGlobal( "ColorPrompt" );

  fi;

  Unbind(PrintPromptHook); Unbind(EndLineHook);

  ColorPrompt(true);
  ANSI_COLORS := true; # switch on coloring of online help texts

fi;

#############################################################################
##
#F  ResClassesBuildManual( ) . . . . . . . . . . . . . . . . build the manual
##
##  This function builds the manual of the ResClasses package in the file
##  formats &LaTeX;, DVI, Postscript, PDF and HTML.
##
##  This is done using the GAPDoc package by Frank L\"ubeck and
##  Max Neunh\"offer.
##
ResClassesBuildManual := function ( )

  local  ResClassesDir;

  ResClassesDir := Concatenation( DIRECTORIES_LIBRARY.pkg[1]![1],
                                  "resclasses/" );
  MakeGAPDocDoc( Concatenation( ResClassesDir, "doc/" ), "resclasses.xml",
                 [ "../gap/resclaux.g", "../gap/z_pi.gd", "../gap/z_pi.gi",
                   "../gap/resclass.gd", "../gap/resclass.gi" ],
                   "ResClasses", "../../../" );
end;
MakeReadOnlyGlobal( "ResClassesBuildManual" );

#############################################################################
##
#F  ResClassesTest(  ) . . . . . . . . . . . . . . . . . . .  read test files
##
##  Performs tests of the ResClasses package.
##
##  This function makes use of an adaptation of the test file `tst/testall.g'
##  of the {\GAP}-library to this package. 
##
ResClassesTest := function (  )

  local  dir;

  dir := Concatenation( DIRECTORIES_LIBRARY.pkg[1]![1], "resclasses/tst/" );
  Read( Concatenation( dir, "testall.g" ) );
end;
MakeReadOnlyGlobal( "ResClassesTest" );

#############################################################################
##
#E  rcwaaux.g . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
