#############################################################################
##
#W  testall.g             GAP4 Package `ResClasses'               Stefan Kohl
##                                                                
#############################################################################

LoadPackage("resclasses");
TestDirectory(DirectoriesPackageLibrary( "resclasses", "tst" ),
              rec(exitGAP := true));

#############################################################################
##
#E  testall.g . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
