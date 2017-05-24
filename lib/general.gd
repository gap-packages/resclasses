#############################################################################
##
#W  general.gd             GAP4 Package `ResClasses'              Stefan Kohl
##
##  This file contains declarations of a couple of functions and operations
##  which are not directly related to computations with residue classes, and
##  which might perhaps later be moved into the GAP Library or elsewhere.
##
#############################################################################

#############################################################################
##
#S  List operations. ////////////////////////////////////////////////////////
##
#############################################################################

#############################################################################
##
#O  PositionsSublist( <list>, <sub> )
##
##  Returns the list of indices in the list <list> at which a sublist equal
##  to <sub> starts.
##
DeclareOperation( "PositionsSublist",
                  [ IsListOrCollection, IsListOrCollection ] );

#############################################################################
##
#S  Routines to generate small graphs. //////////////////////////////////////
##
#############################################################################

#############################################################################
##
#F  AllGraphs( <n> ) . . . .  all graphs with <n> vertices, up to isomorphism
##
##  This function returns a list of all graphs with vertices 1, 2, ... , <n>,
##  up to isomorphism. The graphs are represented as lists of edges.
##
DeclareOperation( "AllGraphs", [ IsPosInt ] );

#############################################################################
##
#F  GraphClasses( <n> )  isomorphism classes of graphs with vertices 1,2,..,n
##
##  This function returns a list of isomorphism classes of graphs with
##  vertices 1, 2, ... , <n>, where the graphs are represented as lists of
##  edges.
##
DeclareOperation( "GraphClasses", [ IsPosInt ] );

#############################################################################
##
#F  IdGraphNC( <graph>, <classes> ) . . identify isomorphism class of <graph>
##
##  Finds the index i such that <graph> lies in the i-th class in the list
##  <classes>. The graph <graph> needs to be represented as a list of edges,
##  and <classes> needs to have the same format as the return value of
##  GraphClasses( n ) for some positive integer n. If the list <classes>
##  contains no class which contains <graph>, the return value is `fail'.
##  Argument checks are not done since they could be quite expensive in terms
##  of runtime.
##
DeclareOperation( "IdGraphNC", [ IsList, IsList ] );

#############################################################################
##
#S  Creating timestamped logfiles. //////////////////////////////////////////
##
#############################################################################

#############################################################################
##
#F  LogToDatedFile( <directory> )
##
## Opens a logfile in the specified directory whose name has the form of a
## timestamp, i.e. <year>-<month>-<day> <hour>-<minute>-<second>.log.
##
DeclareGlobalFunction( "LogToDatedFile" );

#############################################################################
##
#S  SendEmail, EmailLogFile and DownloadFile. ///////////////////////////////
##
#############################################################################

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
#S  Routines for bitmap pictures. ///////////////////////////////////////////
##
#############################################################################

#############################################################################
##
#F  SaveAsBitmapPicture( <picture>, <filename> )
##
##  Writes the pixel matrix <picture> to a bitmap- (bmp-) picture file
##  named <filename>. The filename should include the entire pathname.
##
##  The argument <picture> can be a GF(2) matrix, in which case a monochrome
##  picture file is generated. In this case, zeros stand for black pixels and
##  ones stand for white pixels.
##
##  The argument <picture> can also be an integer matrix, in which case
##  a 24-bit True Color picture file is generated. In this case, the entries
##  of the matrix are supposed to be integers n = 65536*red+256*green+blue in
##  the range 0,...,2^24-1 specifying the RGB values of the colors of the
##  pixels.
##
DeclareGlobalFunction( "SaveAsBitmapPicture" );

#############################################################################
##
#F  LoadBitmapPicture( <filename> )
##
##  Loads the bitmap picture file <filename> created by `SaveAsBitmapPicture'
##  back into GAP. The function returns the pixel matrix <picture>, as it has
##  been passed as first argument to `SaveAsBitmapPicture'.
##  The file <filename> must be an uncompressed monochrome
##  or 24-bit True Color bitmap file.
##
DeclareGlobalFunction( "LoadBitmapPicture" );

#############################################################################
##
#F  DrawLineNC( <pic>, <x1>, <y1>, <x2>, <y2>, <color>, <width> )
##
##  Draws a line on picture <pic> from (<x1>,<y1>) to (<x2>,<y2>),
##  with color <color> and of width <width>.
##
DeclareGlobalFunction( "DrawLineNC");

#############################################################################
##
#F  DrawGrid( <U>, <range_y>, <range_x>, <filename> )
##
##  Draws a picture of the residue class union <U> of Z^2 or the partition
##  <U> of Z^2 into residue class unions, respectively.
##
DeclareGlobalFunction( "DrawGrid" );

#############################################################################
##
#S  Other. //////////////////////////////////////////////////////////////////
##
#############################################################################

#############################################################################
##
#F  AssignGlobalNC( <name>, <value> ) .  forced assignment to global variable
##
DeclareGlobalFunction( "AssignGlobalNC" );

#############################################################################
##
#F  GetOption( <option>, <default> [, <filter> ] )
##
##  Returns the value of the option <option> if it is set and fulfils the
##  filter <filter>, and <default> otherwise. If not specified, <filter>
##  defaults to IsObject.
##
DeclareGlobalFunction( "GetOption" );

#############################################################################
##
#F  LaTeXStringWord( <w> ) . . . . . . . . . .  LaTeX string for a group word
##
DeclareGlobalFunction( "LaTeXStringWord" );

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
##  Note that the implementation is not efficient enough for larger appli-
##  cations, and therefore these functions are not documented.
##
DeclareGlobalFunction( "SetupCache" );
DeclareGlobalFunction( "PutIntoCache" );
DeclareGlobalFunction( "FetchFromCache" );

#############################################################################
##
#E  general.gd . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
