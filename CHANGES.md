Version history of the ResClasses package

## Unreleased

- Added methods for `UnionOp' for residue class unions.

## 4.7.4 (2025-09-24)

- Require GAP >= 4.11.1

- Various janitorial changes.

## 4.7.3 (2022-07-29)

- Fixed a bug in the 3-argument version of `PartitionsIntoResidueClasses':
  If the length of the partitions to be computed was smaller than the biggest
  prime specified in the third argument, the third argument was just ignored.

- Changed IsZ_pi from a property to a category.

- Remove several internal helper functions.

- Other minor adjustments.

## 4.7.2 (2019-03-24)

- Clarified that this package is licensed under GPL 2 or later.

- Moved package homepage to <https://gap-packages.github.io/resclasses>.

- Fixed some issues where loading ResClasses could inadvertently alter
  the behavior of GAP library functions.

- Improved compatibility with future GAP releases (4.11 and later).

- Other minor adjustments.

## 4.7.1 (2017-12-18)

- The email address stefan@gap-system.org does not seem to work any more
  since a few days, so it has been changed back to stefan@mcs.st-and.ac.uk.

## 4.7.0 (2017-12-05)

- Several trivial methods for `SparseRep', `StandardRep' and `Classes' have been
  added (these methods are now needed by RCWA).

- A utility function `GetOption' has been added.

## 4.6.0 (2017-02-12)

- A utility function `LaTeXStringWord' has been added.

- The method for multiplying the empty list with a matrix has been made
  superfluous and removed.

## 4.5.0 (2016-05-14)

- Operations `CoverByResidueClasses' and `CoversByResidueClasses' have been
  introduced, and methods to compute covers of the integers by residue classes
  with prescribed moduli have been implemented.

- Utility functions `DrawLineNC' and `AssignGlobalNC' have been added.

## 4.4.2 (2016-03-09)

- Minor adjustments.

## 4.4.0 (2016-03-08)

- The utility functions and -operations `SaveAsBitmapPicture',
  `LoadBitmapPicture', `DrawGrid', `EquivalenceClasses', `GraphClasses',
  `AllGraphs' and `IdGraph' have been moved from the RCWA package here,
  and `IdGraph' has been renamed to `IdGraphNC'.

- The operations `PositionsSublist', `GraphClasses', `AllGraphs' and `IdGraphNC'
  have been documented.

- A few minor adjustments have been made.

## 4.3.0 (2016-03-04)

- The utility functions and -operations `DifferencesList', `QuotientsList',
  `FloatQuotientsList', `RandomCombination', `IsCommuting', `BlankFreeString',
  `QuotesStripped' and `IntOrInfinityToLaTeX' have been moved to the
  Utils package. Therefore Utils is now a needed package of ResClasses.

## 4.2.0 (2016-03-01)

- The test suite has been amended for better coverage of the code by the tests.

- The undocumented and unused function `ResidueClassesIntersectionType'
  has been removed.

- IO is now a suggested package of ResClasses.

- Minor issues with file permissions etc. have been resolved.

## 4.1.2 (2016-02-10)

- An auxiliary function `LogToDatedFile' has been added.

## 4.0.0 (2016-01-25)

- A class list ("sparse") representation for residue class unions has been
  implemented; this yields an enormous speedup for computations with unions
  of few residue classes some of which have big modulus.

- A performance problem has been fixed: the list of residues
  of a residue class union should know that it is a set.

- Utility operations `PositionsSublist' and `RandomCombination' have been added.

- The tests are now done using `TestDirectory'.

## 3.4.0 (2014-07-21)

- The file resclasses/doc/manual.js has been added.

- An auxiliary function `NextProbablyPrimeInt' has been added.

## 3.3.2 (2013-11-18)

- A potential performance problem in a `ViewString' method
  for polynomials over finite fields has been removed.

## 3.3.1 (2013-10-28)

- Minor adjustments to changes in the GAP Library.
  Note that only RCWA in version 3.6.0 or newer works with this version
  of ResClasses. -- So if you still have an old RCWA, you need to upgrade.

## 3.3.0 (2012-12-12)

- The operation `PartitionsIntoResidueClasses' has now an optional
  third argument `Primes'. Also it interprets now an option "distinct"
  to restrict the output to partitions which differ by the moduli
  of the involved residue classes.

- A bug has been fixed which caused [  ]/`<ring element>` to return its
  first argument instead of a copy of it.

## 3.2.0 (2012-11-01)

- A function `All2x2IntegerMatricesInHNFWithDeterminantUpTo' has been added.
  This function returns a list of all 2 x 2 integer matrices in Hermite
  normal form whose determinant is less than or equal to a specified value.

- The directory resclasses/gap/ has been renamed to resclasses/lib/,
  in order to follow the same naming convention as for the subdirectories
  of the GAP root directory.

- Utility functions `ResClassesTestExamples',
  `ConvertPackageFilesToUNIXLineBreaks' and `RemoveTemporaryPackageFiles'
  have been added.

## 3.1.1 (2012-05-23)

- The `DefaultRingByGenerators' method for lists of integer matrices
  has been removed, since a method covering this case is now provided by
  the GAP Library. For this reason, ResClasses now needs GAP in version
  at least 4.5.4.

## 3.1.0 (2012-05-07)

- Added a `DisplayString' method for residue class unions.

- Removed a number of trivial methods from general.g which are
  now in the GAP Library, to avoid unnecessary code duplication.
  Also split general.g into general.gd and general.gi.

- Declared an operation `IsCommuting'.

- Added a convenience method which allows enclosing the arguments of `Comm'
  into list brackets.

- Added a helper function `DownloadFile' (needs the IO package).

## 3.0.4 (2011-11-01)

- Removed an `Intersection2' method for Integers^2 and a list of
  pairs of integers which returned a residue class union object,
  to get rid of a possible source of incompatibilities with other
  packages.

## 3.0.3 (2011-10-25)

- Fixed a `ViewString' method which changed line breaks in printing of
  rationals with large numerators / denominators.

## 3.0.2 (2011-10-18)

- Residue class unions of GF(p)[x] and polynomials with coefficients in
  GF(p)[x] are `View'ed in a much nicer way.

- A number of smaller improvements has been made.

- The CVS revision entries have been removed from the source files.

- ResClasses needs now GAP 4.5.2 and GAPDoc 1.4.

## 3.0.0 (2011-05-08)

The main change is that ResClasses now supports computations with
unions of residue classes of Z^2.

Further changes are:

 - A function `ResidueClassesIntersectionType' has been added.

 - The required versions of GAP, GAPDoc and Polycyclic have been incremented
   to 4.4.12, 1.1 and 2.6, respectively.

## 2.5.3 (2007-09-26)

- The line length used in the examples has been reduced to 75 in order to
  improve their appearance in the online help with standard settings.

## 2.5.2 (2007-08-13)

- Functions `SendEmail' and `EmailLogFile' have been added.

## 2.5.1 (2007-06-01)

- Some minor adjustments have been made, and ResClasses needs now the
  recently released version 1.0 of GAPDoc.

## 2.5.0 (2007-05-25)

- An operation `PartitionsIntoResidueClasses' has been added.
  This operation returns a sorted list of all partitions of a given ring
  into a given number of residue classes.

- The index of the manual has been improved.

- Methods for `ExtRepOfObj' and `ObjByExtRep' for residue class unions
  have been added.

- A property `IsResidueClassWithFixedRepresentative' has been introduced.

- A couple of auxiliary trivial methods have been added.

- A bug in `RepresentativeStabilizingRefinement' has been fixed --
  the simplification did not work in case of negative moduli.

- The bibliography format has been changed from BibTeX to GAPDoc XML.

## 2.4.4 (2007-03-07)

- Now ResClasses needs GAPDoc 0.99999.

## 2.4.3 (2007-03-06)

- A quirk with the style file for the HTML version of the manual has
  been fixed.

## 2.4.2 (2007-03-05)

- A bug in the `View' method for residue class unions of polynomial rings
  has been fixed.

## 2.4.1 (2007-02-12)

- Minor changes.

## 2.4.0 (2007-02-09)

- The support for residue class unions of polynomial rings GF(q)[x] has
  been improved. For example there are now methods for `SplittedClass'
  and `RandomPartitionIntoResidueClasses' which are applicable to residue
  classes of GF(q)[x] resp. to GF(q)[x] itself. Further the `Viev'ing and
  `Display' format for residue class unions of GF(q)[x] has been
  improved.

- A "cleanup" of the source files has taken place -- many comments have
  been improved, and the declarations / implementations have been rearranged
  and grouped together in sections by "topic".

- The two-argument form ResidueClass( `<r>`, `<m>` ) is now also available
  for rings other than Z.

- The documentation now uses consistently the term "residue class union"
  for the objects returned by `ResidueClassUnion' (instead of "union of
  residue classes").

## 2.3.7 (2007-01-29)

- Synonyms `IsResidueClassUnion[OfZ|OfZ_pi|OfZorZ_pi|OfGFqx]' for the five
  categories `IsUnionOfResidueClasses[OfZ|OfZ_pi|OfZorZ_pi|OfGFqx]' have
  been defined.

- The documentation has been improved in a few places.

## 2.3.6 (2007-01-26)

- A problem w.r.t. residue classes vs. ideals has been fixed.
  This problem has caused a `no method found' message when trying
  to form a principal ideal by multiplying a ring by one of its
  elements.

- A missing `Difference' method for two times the same ring has been added.

- For consistency, the representation `IsResidueClassUnionSparseRep' has
  been renamed to `IsResidueClassUnionResidueListRep'. This change will not
  affect the use of this package.

- A bug in the \< method for residue class unions has been fixed.
  This bug caused an error message when sorting lists of residue class
  unions (in rare cases).

## 2.3.5 (2006-09-29)

- A method for `AsUnionOfFewClasses' for unions of residue classes of the
  rings Z_pi and GF(q)[x] has been added.

- The function `AllResidues' has been turned into an operation, and
  an operation `NumberOfResidues' has been introduced.

- The documentation has been slightly improved.

## 2.3.4 (2006-09-20)

- An operation `RandomPartitionIntoResidueClasses' has been introduced.

## 2.3.3 (2006-08-11)

- A few minor internal changes have been made.

## 2.3.2 (2006-06-09)

- A few trivial methods have been added, and some undocumented
  global functions have been made local.

## 2.3.1 (2006-06-08)

- For convenience, an operation `Residue' has been introduced.

## 2.3.0 (2006-05-30)

The main change is that the manual has been revised.

Further a couple of trivial methods have been added, some of which
are needed by RCWA.

## 2.2.2 (2006-03-28)

- A couple of minor output formatting improvements have been made.

## 2.2.1 (2006-03-10)

- For convenience, `ResidueClass' now accepts also a list [r,m] as argument.

- The output of `Display' for unions of residue classes has been improved
  a bit further.

## 2.2.0 (2006-02-14)

- The output of the `View' and `Display' methods for unions of residue
  classes has been improved.

- `AsUnionOfFewClasses' has been made another bit more efficient.

## 2.1.2 (2006-01-12)

- A property `IsResidueClass' has been added.

## 2.1.1 (2005-08-12)

- An adaptation to an incompatible change in GAP 4.4.6 w.r.t.
  `InstallValue' for families has been made.

## 2.1.0 (2005-08-08)

- Significant performance improvements in `Union', `Intersection',
  `Difference' and `AsUnionOfFewClasses' for `sparse' unions of
  residue classes have been obtained.

## 2.0.6 (2005-05-25)

- The following name changes have been made:
  `DELTA' --> `Delta' and `RHO' --> `Rho'.

- A bug in `RepresentativeStabilizingRefinement' has been fixed.
  This bug caused wrong results in certain situations. Test code:

  gap> U := ResidueClassUnionWithFixedReps([[-1,3],[1,3],[3,3]]);;
  gap> U = RepresentativeStabilizingRefinement(U,0);
  true

## 2.0.5 (2005-05-17)

- The manual has been cleaned up and compacted.

- An operation `SplittedClass' has been added.

- Minor improvements in the manual have been made, and a few trivial
  methods covering pathological cases have been added.

## 2.0.2 (2005-02-14)

- A bug in a `Union2' method for unions of residue classes has been fixed
  (ExcludedElements were not treated correctly). This bug caused wrong
  results in some cases.

## 2.0 (2004-06-03)

- A function `AllResidueClassesModulo' has been added.

- Support for unions of residue classes with fixed representatives has been
  added. These domains behave different than `ordinary' unions of residue
  classes:

  1. They behave like sets of single residue classes with fixed
     representative rather than like sets of ring elements.

  2. The residue classes are not necessarily disjoint, thus the unions can
     be viewed as multisets of ring elements.

## 1.1

- The output routines have been improved.

- An attribute `Density' has been added.

## 1.0 (2004-01-23)
