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
  end );

BindGlobal( "ResClassesDoThingsToBeDoneAfterTest",

  function (  )
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
#S  Testing the examples. ///////////////////////////////////////////////////
##
#############################################################################

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
#E  resclaux.g . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
