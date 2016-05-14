####################################################################################################
##
##  PackageInfo.g                      GAP4 Package `ResClasses'                         Stefan Kohl
##
####################################################################################################

SetPackageInfo( rec(

PackageName      := "ResClasses",
Subtitle         := "Set-Theoretic Computations with Residue Classes",
Version          := "4.5.0",
Date             := "14/05/2016",
ArchiveURL       := Concatenation("http://www.gap-system.org/DevelopersPages/StefanKohl/",
                                  "resclasses/resclasses-4.5.0"),
ArchiveFormats   := ".tar.gz", # "-win.zip" when providing text files with Windows line breaks
Persons          := [
                      rec( LastName      := "Kohl",
                           FirstNames    := "Stefan",
                           IsAuthor      := true,
                           IsMaintainer  := true,
                           Email         := "stefan@mcs.st-and.ac.uk",
                           WWWHome       := "http://www.gap-system.org/DevelopersPages/StefanKohl/"
                         )
                    ],
Status           := "deposited",
PackageWWWHome   := "http://www.gap-system.org/DevelopersPages/StefanKohl/resclasses.html",
README_URL       := Concatenation("http://www.gap-system.org/DevelopersPages/StefanKohl/",
                                  "resclasses/README.resclasses"),
PackageInfoURL   := Concatenation("http://www.gap-system.org/DevelopersPages/StefanKohl/",
                                  "resclasses/PackageInfo.g"),
AbstractHTML     := Concatenation("This package permits to compute with set-theoretic ",
                                  "unions of residue classes of&nbsp;Z and a few other rings. ",
                                  "In particular it provides methods for computing unions, ",
                                  "intersections and differences of these sets."),
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
                         GAP                    := ">=4.8.3",
                         NeededOtherPackages    := [ ["GAPDoc",">=1.5.1"], ["Polycyclic",">=2.11"],
                                                     ["utils", ">=0.39"] ],
                         SuggestedOtherPackages := [ ["IO",">=4.4.5"] ],
                         ExternalConditions     := [ ]
                       ),
AvailabilityTest := ReturnTrue,
BannerString     := Concatenation( "\nLoading ResClasses ", ~.Version,
                                   " (Computations with Residue Classes)",
                                   "\nby Stefan Kohl, stefan@mcs.st-and.ac.uk\n\n" ),
TestFile         := "tst/testall.g",
Keywords         := [ "residue classes", "integers", "number theory" ]

) );

####################################################################################################
##
#E  PackageInfo.g  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
