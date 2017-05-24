#############################################################################
##
#W  other.tst              GAP4 Package `ResClasses'              Stefan Kohl
##
##  This file contains automated tests of ResClasses' utility functions, as
##  far as they are suitable for automated testing and not tested elsewhere.
##
#############################################################################

gap> START_TEST( "other.tst" );
gap> ResClassesDoThingsToBeDoneBeforeTest();
gap> PushOptions(rec(abc:=3)); 
gap> GetOption("abc",10,IsPosInt);
3
gap> GetOption("abc",10,IsString); 
10
gap> GetOption("abc",10);         
3
gap> GetOption("abcd",10,IsPosInt);
10
gap> GetOption("abcd",10);         
10
gap> PopOptions();
gap> ResClassesDoThingsToBeDoneAfterTest();
gap> STOP_TEST( "other.tst", 25000000 );

#############################################################################
##
#E  other.tst . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
