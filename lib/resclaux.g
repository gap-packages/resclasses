#############################################################################
##
#W  resclaux.g             GAP4 Package `ResClasses'              Stefan Kohl
##
##  This file contains some auxiliary functions for the ResClasses package.
##
#############################################################################

BindGlobal( "RESCLASSES_VIEWINGFORMAT", "long" );
RESCLASSES_VIEWINGFORMAT_BUFFER := RESCLASSES_VIEWINGFORMAT;
RESCLASSES_WARNINGLEVEL_BUFFER := InfoLevel( InfoWarning );

#############################################################################
##
#F  ResidueClassUnionViewingFormat( format ) . short <--> long viewing format
##
BindGlobal( "ResidueClassUnionViewingFormat",

  function ( format )

    if   not format in [ "short", "long" ]
    then Error( "viewing formats other than \"short\" and \"long\" ",
                "are not supported.\n");
    fi;
    MakeReadWriteGlobal( "RESCLASSES_VIEWINGFORMAT" );
    RESCLASSES_VIEWINGFORMAT := format;
    MakeReadOnlyGlobal( "RESCLASSES_VIEWINGFORMAT" );
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
#F  ResClassesDoThingsToBeDoneBeforeTest(  )
#F  ResClassesDoThingsToBeDoneAfterTest(  )
##
BindGlobal( "ResClassesDoThingsToBeDoneBeforeTest",

  function (  )
    RESCLASSES_WARNINGLEVEL_BUFFER := InfoLevel(InfoWarning);;
    SetInfoLevel(InfoWarning,0);
    RESCLASSES_VIEWINGFORMAT_BUFFER := RESCLASSES_VIEWINGFORMAT;;
    ResidueClassUnionViewingFormat("long");
    CallFuncList(HideGlobalVariables,ONE_LETTER_GLOBALS);
  end );

BindGlobal( "ResClassesDoThingsToBeDoneAfterTest",

  function (  )
    CallFuncList(UnhideGlobalVariables,ONE_LETTER_GLOBALS);
    ResidueClassUnionViewingFormat(RESCLASSES_VIEWINGFORMAT_BUFFER);
    SetInfoLevel(InfoWarning,RESCLASSES_WARNINGLEVEL_BUFFER);
  end );

#############################################################################
##
#F  ResClassesTest(  ) . . . . . . . . . . . . . . . . . . .  read test files
##
##  Performs tests of the ResClasses package.
##
##  This function makes use of an adaptation of the test file `tst/testall.g'
##  of the {\GAP}-library to this package. 
##
BindGlobal( "ResClassesTest",

  function (  )

    local  ResClassesDir, dir;

    ResClassesDir := GAPInfo.PackagesInfo.("resclasses")[1].InstallationPath;
    dir := Concatenation( ResClassesDir, "/tst/" );
    Read( Concatenation( dir, "testall.g" ) );
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
#F  ResClassesBuildManual( ) . . . . . . . . . . . . . . . . build the manual
##
##  This function builds the manual of the ResClasses package in the file
##  formats &LaTeX;, DVI, Postscript, PDF and HTML.
##
##  This is done using the GAPDoc package by Frank L\"ubeck and
##  Max Neunh\"offer.
##
BindGlobal( "ResClassesBuildManual",

  function ( )

    local  ResClassesDir;

    ResClassesDir := GAPInfo.PackagesInfo.("resclasses")[1].InstallationPath;
    MakeGAPDocDoc( Concatenation( ResClassesDir, "/doc/" ), "main.xml",
                   [ "../lib/resclaux.g",
                     "../lib/general.gd", "../lib/general.gi",
                     "../lib/z_pi.gd", "../lib/z_pi.gi",
                     "../lib/resclass.gd", "../lib/resclass.gi",
                     "../lib/fixedrep.gd", "../lib/fixedrep.gi" ],
                     "ResClasses", "../../../" );
  end );

#############################################################################
##
#F  ConvertPackageFilesToUNIXLineBreaks( <package> )
##
##  Converts the text files of package <package> from Windows- to
##  UNIX line breaks. Here <package> is assumed to be either "resclasses"
##  or "rcwa".
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
        elif file in ["data","doc","examples","lib","paper","tst","timings"]
        then ProcessDirectory(Concatenation(dir,"/",file)); fi;
      od;
    end;

    packagedir := GAPInfo.PackagesInfo.(package)[1].InstallationPath;
    ProcessDirectory(packagedir);
  end );

#############################################################################
##
#F  RemoveTemporaryPackageFiles( <package> )
##
##  Cleans up temporary files and repository data of package <package>.
##  Here <package> is assumed to be either "resclasses" or "rcwa".
##
BindGlobal( "RemoveTemporaryPackageFiles",

  function ( package )

    local  packagedir, docdir, timingsdir, file;

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
    if "timings" in DirectoryContents(Concatenation(packagedir,"/tst")) then
      timingsdir := Concatenation(packagedir,"/tst/timings");
      for file in DirectoryContents(timingsdir) do
        if   PositionSublist(file,".runtimes") <> fail
        then RemoveFile(Concatenation(timingsdir,"/",file)); fi;
      od;
    fi;
  end );

#############################################################################
##
#E  resclaux.g . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here