####################################################################################################
##
##  PkgInfo.g                        GAP4 Package `ResClasses'                           Stefan Kohl
##  
#H  @(#)$Id$
##
#Y  Copyright (C) 2002 by Stefan Kohl, Fachbereich Mathematik,
#Y  Universit\"at Stuttgart, Germany
##
##  Preliminary, not yet to be distributed !!!
##
##  None of the files on my webpage referred to from here is currently existing.
##

SetPackageInfo( rec(

PkgName          := "ResClasses",
Version          := "1.0",
Date             := "20/02/2003",
ArchiveURL       := "http://www.cip.mathematik.uni-stuttgart.de/~kohlsn/resclasses/resclasses-1.0",
ArchiveFormats   := ".zoo",
Persons          := [
                      rec( LastName      := "Kohl",
                           FirstNames    := "Stefan",
                           IsAuthor      := true,
                           IsMaintainer  := true,
                           Email         := "kohl@mathematik.uni-stuttgart.de",
                           WWWHome       := "http://www.cip.mathematik.uni-stuttgart.de/~kohlsn/",
                           PostalAddress := Concatenation("Stefan Kohl\n",
                                                          "Institut für Geometrie und Topologie\n",
                                                          "Pfaffenwaldring 57\n",
                                                          "Universität Stuttgart\n",
                                                          "70550 Stuttgart\n",
                                                          "Germany"),
                           Place         := "Stuttgart / Germany",
                           Institution   := "University of Stuttgart"
                         )
                    ],
Status           := "dev",
CommunicatedBy   := "",
AcceptDate       := "",
README_URL       := "http://www.cip.mathematik.uni-stuttgart.de/~kohlsn/resclasses/README.resclasses",
PkgInfoURL       := "http://www.cip.mathematik.uni-stuttgart.de/~kohlsn/resclasses/PkgInfo.g",
AbstractHTML     := "This package allows to do computations with unions of residue classes.",
PackageWWWHome   := "http://www.cip.mathematik.uni-stuttgart.de/~kohlsn/resclasses.html",
PackageDoc       := rec(
                         BookName  := "ResClasses",
                         Archive   := Concatenation("http://www.cip.mathematik.uni-stuttgart.de/",
                                                    "~kohlsn/resclasses/resclasses-1.0doc-win.zip"),
                         HTMLStart := "doc/manual.html",
                         PDFFile   := "doc/manual.pdf",
                         SixFile   := "doc/manual.six",
                         LongTitle := "Computations with Residue Classes",
                         AutoLoad  := true
                       ),
Dependencies     := rec(
                         GAP                    := ">=4.3",
                         NeededOtherPackages    := [ ["GAPDoc",">=0.99"] ],
                         SuggestedOtherPackages := [ ],
                         ExternalConditions     := [ ]
                       ),
AvailabilityTest := ReturnTrue,
BannerString     := Concatenation( "\nLoading ResClasses ", ~.Version,
                                   " (Computations with Residue Classes)",
                                   "\nby Stefan Kohl, kohl@mathematik.uni-stuttgart.de\n\n" ),
AutoLoad         := true,
TestFile         := "tst/testall.g",
Keywords         := [ "residue classes", "integers", "number theory" ]

) );

####################################################################################################
##
#E  PkgInfo.g  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
