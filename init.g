#############################################################################
##
#W  init.g               GAP4 Package `ResClasses'                Stefan Kohl
##
#H  @(#)$Id$
##
#Y  Copyright (C) 2003 by Stefan Kohl, Fachbereich Mathematik,
#Y  Universit\"at Stuttgart, Germany
##

SetInfoLevel( InfoWarning, 0 );

DeclarePackage( "resclasses", "1.0",
  
  function ()
    if   CompareVersionNumbers( VERSION, "4r3" )
    then if   TestPackageAvailability( "gapdoc", "0.99" ) = fail
         then Info( InfoWarning, 1, 
                    "Package `ResClasses' needs the GAPDoc package." );
              return false;
         else return true; fi;
    else Info( InfoWarning, 1,
               "Package `ResClasses' needs at least GAP 4.3.");
         return false;
    fi;
  end );

# Load the GAPDoc package, if this has not been done so far.

if IsList( TestPackageAvailability( "gapdoc", "0.99" ) ) then
  OLD_BANNER := BANNER; MakeReadWriteGlobal( "BANNER" ); BANNER := false;
  LoadPackage( "gapdoc" );
  BANNER := OLD_BANNER; MakeReadOnlyGlobal( "BANNER" );
fi;

DeclarePackageAutoDocumentation( "resclasses", "doc" );

# Read the declaration part of the package.

ReadPkg( "resclasses", "banner.g" );
ReadPkg( "resclasses", "gap/z_pi.gd" );
ReadPkg( "resclasses", "gap/resclass.gd" );

SetInfoLevel( InfoWarning, 1 );

#############################################################################
##
#E  init.g . . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
