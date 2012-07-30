#############################################################################
##
#W  general.gd             GAP4 Package `ResClasses'              Stefan Kohl
##
##  This file contains declarations of a couple of functions and methods
##  which are not directly related to computations with residue classes, and
##  which might perhaps later be moved into the GAP Library.
##
#############################################################################

#############################################################################
##
#F  DifferencesList( <list> ) . . . . differences of consecutive list entries
#F  QuotientsList( <list> ) . . . . . . quotients of consecutive list entries
#F  FloatQuotientsList( <list> )  . . . . . . . . . . . . dito, but as floats
##
DeclareGlobalFunction( "DifferencesList" );
DeclareGlobalFunction( "QuotientsList" );
DeclareGlobalFunction( "FloatQuotientsList" );

#############################################################################
##
#O  IsCommuting( <a>, <b> ) .  checks whether two group elements etc. commute
##
DeclareOperation( "IsCommuting", [ IsMultiplicativeElement,
                                   IsMultiplicativeElement ] );

#############################################################################
##
#F  SetupCache( <name>, <size> )
#F  PutIntoCache( <name>, <key>, <value> )
#F  FetchFromCache( <name>, <key> )
##
##  A simple caching facility:
##
##  - The function `SetupCache' creates an empty cache named <name> for
##    at most <size> values.
##  - The function `PutIntoCache' puts the entry <value> with key <key>
##    into the cache named <name>.
##  - The function `FetchFromCache' picks the entry with key <key> from
##    the cache named <name>, and returns fail if no such entry exists.
##
DeclareGlobalFunction( "SetupCache" );
DeclareGlobalFunction( "PutIntoCache" );
DeclareGlobalFunction( "FetchFromCache" );

#############################################################################
##
#F  SendEmail( <sendto>, <copyto>, <subject>, <text> ) . . . . send an e-mail
##
##  Sends an e-mail with subject <subject> and body <text> to the addresses
##  in the list <sendto>, and copies it to those in the list <copyto>.
##  The first two arguments must be lists of strings, and the latter two must
##  be strings.
##
DeclareGlobalFunction( "SendEmail" );

#############################################################################
##
#F  EmailLogFile( <addresses> ) . . .  send log file by e-mail to <addresses>
##
##  Sends the current logfile by e-mail to <addresses>, if GAP is in logging
##  mode and one is working under UNIX, and does nothing otherwise.
##  The argument <addresses> must be either a list of email addresses or
##  a single e-mail address. Long log files are abbreviated, i.e. if the log
##  file is larger than 64KB, then any output is truncated at 1KB, and if the
##  log file is still longer than 64KB afterwards, it is truncated at 64KB.
##
DeclareGlobalFunction( "EmailLogFile" );

#############################################################################
##
#F  DownloadFile( <url> )
##
##  Downloads the file <url> and returns its contents as a string.
##  If an error occurs, the function prints a warning and returns `fail'.
##  The IO package is needed for using this function.
##
DeclareGlobalFunction( "DownloadFile" );

#############################################################################
##
#F  BlankFreeString( <obj> )  . . . . . . . . . . . . . string without blanks
#F  IntOrInfinityToLaTeX( n ) . LaTeX string representing integer or infinity
##
DeclareGlobalFunction( "BlankFreeString" );
DeclareGlobalFunction( "IntOrInfinityToLaTeX" );

#############################################################################
##
#E  general.gd . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here