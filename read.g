#############################################################################
##
#W  read.g               GAP4 Package `ResClasses'                Stefan Kohl
##
#H  @(#)$Id$
##
#Y  Copyright (C) 2003 by Stefan Kohl, Fachbereich Mathematik,
#Y  Universit\"at Stuttgart, Germany
##

# Read the implementation part of the package.

SetInfoLevel( InfoWarning, 0 );

ReadPkg( "resclasses", "gap/compat43.g" );
ReadPkg( "resclasses", "gap/manstyle.g" );
ReadPkg( "resclasses", "gap/resclaux.g" );
ReadPkg( "resclasses", "gap/z_pi.gi" );
ReadPkg( "resclasses", "gap/resclass.gi" );

SetInfoLevel( InfoWarning, 1 );

#############################################################################
##
#E  read.g . . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
