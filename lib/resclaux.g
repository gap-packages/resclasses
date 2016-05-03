#############################################################################
##
#W  resclaux.g             GAP4 Package `ResClasses'              Stefan Kohl
##
##  This file contains auxiliary functions for the ResClasses package.
##
#############################################################################

RESCLASSES_WARNINGLEVEL_BACKUP := InfoLevel( InfoWarning );
RESCLASSES_ASSERTIONLEVEL_BACKUP := AssertionLevel();
RESCLASSES_VIEWINGFORMAT_BACKUP := "long";

#############################################################################
##
#S  Test utilities. /////////////////////////////////////////////////////////
##
#############################################################################

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
#F  ResClassesDoThingsToBeDoneBeforeTest(  )
#F  ResClassesDoThingsToBeDoneAfterTest(  )
##
BindGlobal( "ResClassesDoThingsToBeDoneBeforeTest",

  function (  )
    RESCLASSES_WARNINGLEVEL_BACKUP := InfoLevel(InfoWarning);;
    SetInfoLevel(InfoWarning,0);
    SetAssertionLevel(0);
    RESCLASSES_VIEWINGFORMAT_BACKUP := RESCLASSES_VIEWINGFORMAT;;
    ResidueClassUnionViewingFormat("long");
    CallFuncList(HideGlobalVariables,ONE_LETTER_GLOBALS);
  end );

BindGlobal( "ResClassesDoThingsToBeDoneAfterTest",

  function (  )
    CallFuncList(UnhideGlobalVariables,ONE_LETTER_GLOBALS);
    ResidueClassUnionViewingFormat(RESCLASSES_VIEWINGFORMAT_BACKUP);
    SetAssertionLevel(RESCLASSES_ASSERTIONLEVEL_BACKUP);
    SetInfoLevel(InfoWarning,RESCLASSES_WARNINGLEVEL_BACKUP);
  end );

#############################################################################
##
#F  ResClassesTest(  ) . . . . . . . . . . . . . . . . . . .  read test files
##
##  Performs tests of the ResClasses package.
##
BindGlobal( "ResClassesTest",

  function (  )
    RESCLASSES_ASSERTIONLEVEL_BACKUP := AssertionLevel();
    return TestDirectory( DirectoriesPackageLibrary( "resclasses", "tst" ) );
  end );

#############################################################################
##
#S  Building the manual and testing the examples. ///////////////////////////
##
#############################################################################

#############################################################################
##
#F  RemoveTemporaryPackageFiles( <package> )
##
##  Cleans up temporary files and repository data of package <package>.
##  Here <package> is assumed to be either "resclasses" or "rcwa".
##
BindGlobal( "RemoveTemporaryPackageFiles",

  function ( package )

    local  packagedir, docdir, file;

    packagedir := GAPInfo.PackagesInfo.(package)[1].InstallationPath;
    if   ".hg" in DirectoryContents(packagedir)
    then RemoveDirectoryRecursively(Concatenation(packagedir,"/.hg")); fi;
    docdir := Concatenation(packagedir,"/doc");
    for file in DirectoryContents(docdir) do
      if   ForAny([".aux",".bbl",".bib",".blg",".brf",".idx",".ilg",
                   ".log",".out",".pnr",".tex"],
                  ext->PositionSublist(file,ext) <> fail)
      then RemoveFile(Concatenation(docdir,"/",file)); fi; 
    od;
  end );

#############################################################################
##
#F  ResClassesBuildManual( ) . . . . . . . . . . . . . . . . build the manual
##
##  This function builds the manual of the ResClasses package in the file
##  formats &LaTeX;, DVI, Postscript, PDF and HTML.
##  This is done using the GAPDoc package by Frank Lübeck and Max Neunhöffer.
##
BindGlobal( "ResClassesBuildManual",

  function ( )

    local  ResClassesDir, i;

    ResClassesDir := GAPInfo.PackagesInfo.("resclasses")[1].InstallationPath;
    for i in [1..3] do
      Print("\nCompiling ResClasses manual: pass number ",i,"(3) . . .\n\n");
      MakeGAPDocDoc( Concatenation( ResClassesDir, "/doc/" ), "main.xml",
                     [ "../lib/resclaux.g",
                       "../lib/general.gd", "../lib/general.gi",
                       "../lib/z_pi.gd", "../lib/z_pi.gi",
                       "../lib/resclass.gd", "../lib/resclass.gi",
                       "../lib/fixedrep.gd", "../lib/fixedrep.gi" ],
                       "ResClasses", "../../../" );
    od;
    RemoveTemporaryPackageFiles("resclasses");
  end );

#############################################################################
##
#F  ResClassesTestExamples( ) . . . .  test examples in the ResClasses manual
##
##  Tests the examples in the manual of the ResClasses package.
##
BindGlobal( "ResClassesTestExamples",

  function ( )

    local  path;

    ResClassesDoThingsToBeDoneBeforeTest();
    path := GAPInfo.PackagesInfo.("resclasses")[1].InstallationPath;
    RunExamples(ExtractExamples(Concatenation(path,"/doc"),
                                "main.xml",[],"Chapter"),
                rec( width := 75, compareFunction := "uptowhitespace" ) );
    ResClassesDoThingsToBeDoneAfterTest();
  end );

#############################################################################
##
#S  Other. //////////////////////////////////////////////////////////////////
##
#############################################################################

#############################################################################
##
#F  ConvertPackageFilesToUNIXLineBreaks( <package> )
##
##  Converts the text files of package <package> from Windows- to UNIX line
##  breaks. Here <package> is assumed to be either "resclasses" or "rcwa".
##
BindGlobal( "ConvertPackageFilesToUNIXLineBreaks",

  function ( package )

    local  packagedir, RecodeFile, ProcessDirectory;

    RecodeFile := function ( file )

      local  str;

      str := StringFile(file);
      if PositionSublist(str,"\r\n") <> fail then
        str := ReplacedString(str,"\r\n","\n");
        FileString(file,str);
      fi;
    end;

    ProcessDirectory := function ( dir )

      local  files, file;

      files := DirectoryContents(dir);
      for file in files do
        if file in [".",".."] then continue; fi;
        if  ForAny([".txt",".g",".gd",".gi",".xml",".tst"],
                   ext->PositionSublist(file,ext) <> fail)
          or file in ["README","CHANGES","version"]
        then RecodeFile(Concatenation(dir,"/",file));
        elif file in ["data","3ctsgroups6","3ctsgroups9","4ctsgroups6",
                      "ctproducts","doc","examples","lib","paper","tst"]
        then ProcessDirectory(Concatenation(dir,"/",file)); fi;
      od;
    end;

    packagedir := GAPInfo.PackagesInfo.(package)[1].InstallationPath;
    ProcessDirectory(packagedir);
  end );

#############################################################################
##
#E  resclaux.g . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
