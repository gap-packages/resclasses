#############################################################################
##
#W  read.g               GAP4 Package `ResClasses'                Stefan Kohl
##
#H  @(#)$Id$
##

# Read the implementation part of the package.

SetInfoLevel( InfoWarning, 0 );

if   not CompareVersionNumbers( VERSION, "4r4" )
then ReadPkg( "resclasses", "gap/compat43.g" ); fi;

ReadPkg( "resclasses", "gap/resclaux.g" );
ReadPkg( "resclasses", "gap/z_pi.gi" );
ReadPkg( "resclasses", "gap/resclass.gi" );

SetInfoLevel( InfoWarning, 1 );

#############################################################################
##
#E  read.g . . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here

