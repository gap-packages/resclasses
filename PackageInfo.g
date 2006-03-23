####################################################################################################
##
##  PackageInfo.g                      GAP4 Package `ResClasses'                         Stefan Kohl
##  
#H  @(#)$Id$
##

SetPackageInfo( rec(

PackageName      := "ResClasses",
Subtitle         := "Set-Theoretic Computations with Residue Classes",
Version          := "2.2.2",
Date             := "10/03/2006",
ArchiveURL       := Concatenation("http://www.cip.mathematik.uni-stuttgart.de/~kohlsn/",
                                  "resclasses/resclasses-2.2.1"),
ArchiveFormats   := ".tar.gz",
Persons          := [
                      rec( LastName      := "Kohl",
                           FirstNames    := "Stefan",
                           IsAuthor      := true,
                           IsMaintainer  := true,
                           Email         := "kohl@mathematik.uni-stuttgart.de",
                           WWWHome       := "http://www.cip.mathematik.uni-stuttgart.de/~kohlsn/",
                           PostalAddress := Concatenation("Institut für Geometrie und Topologie\n",
                                                          "Pfaffenwaldring 57\n",
                                                          "Universität Stuttgart\n",
                                                          "70550 Stuttgart\n",
                                                          "Germany"),
                           Place         := "Stuttgart / Germany",
                           Institution   := "University of Stuttgart"
                         )
                    ],
Status           := "deposited",
README_URL       := Concatenation("http://www.cip.mathematik.uni-stuttgart.de/",
                                  "~kohlsn/resclasses/README.resclasses"),
PackageInfoURL   := "http://www.cip.mathematik.uni-stuttgart.de/~kohlsn/resclasses/PackageInfo.g",
AbstractHTML     := Concatenation("This package permits computing with set-theoretic ",
                                  "unions of residue classes of&nbsp;Z and a few other rings. ",
                                  "In particular it provides methods for computing unions, ",
                                  "intersections and differences of these sets."),
PackageWWWHome   := "http://www.cip.mathematik.uni-stuttgart.de/~kohlsn/resclasses.html",
PackageDoc       := rec(
                         BookName         := "ResClasses",
                         ArchiveURLSubset := ["doc"],
                         HTMLStart        := "doc/chap0.html",
                         PDFFile          := "doc/manual.pdf",
                         SixFile          := "doc/manual.six",
                         LongTitle        := Concatenation("Computations with Residue Classes ",
                                                           "and their Set-Theoretic Unions"),
                         Autoload         := true
                       ),
Dependencies     := rec(
                         GAP                    := ">=4.4",
                         NeededOtherPackages    := [ ["GAPDoc",">=0.999"] ],
                         SuggestedOtherPackages := [ ],
                         ExternalConditions     := [ ]
                       ),
AvailabilityTest := ReturnTrue,
BannerString     := Concatenation( "\nLoading ResClasses ", ~.Version,
                                   " (Computations with Residue Classes)",
                                   "\nby Stefan Kohl, kohl@mathematik.uni-stuttgart.de\n\n" ),
Autoload         := true,
TestFile         := "tst/testall.g",
Keywords         := [ "residue classes", "integers", "number theory" ]

) );

####################################################################################################
##
#E  PackageInfo.g  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here