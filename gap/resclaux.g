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
  StefansManualLayout( "ResClasses" );
  MyMakeGAPDocDoc( Concatenation( ResClassesDir, "doc/" ), "resclasses.xml",
                   [ "../gap/resclaux.g", "../gap/z_pi.gd", "../gap/z_pi.gi",
                     "../gap/resclass.gd", "../gap/resclass.gi" ],
                     "ResClasses", "../../../" );
  ResetManualLayout( );
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
#E  resclaux.g . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
