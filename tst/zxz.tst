#############################################################################
##
#W  zxz.tst               GAP4 Package `ResClasses'               Stefan Kohl
##
#H  @(#)$Id$
##
##  This file contains automated tests of ResClasses' functionality for
##  computing with residue class unions of Z^2.
##
gap> START_TEST("$Id$");
gap> oldformat := RESCLASSES_VIEWING_FORMAT;;
gap> ResidueClassUnionViewingFormat("short");;
gap> CallFuncList(HideGlobalVariables,FREE_ONE_LETTER_GLOBALS);
gap> R := Integers^2;
( Integers^2 )
gap> One(R);
[ 1, 1 ]
gap> IsOne(last);
true
gap> One([-5,0]);
[ 1, 1 ]
gap> Difference(R,R);
[  ]
gap> Union(R,R);
( Integers^2 )
gap> Intersection(R,R);
( Integers^2 )
gap> Difference(R,[]);
( Integers^2 )
gap> Difference(R,[[0,0]]);
Z^2 \ [ [ 0, 0 ] ]
gap> Union(last,[[1,-1]]);
Z^2 \ [ [ 0, 0 ] ]
gap> Union(last,[[0,0]]);
( Integers^2 )
gap> R+[1,0];
( Integers^2 )
gap> Difference(R,[[0,0],[1,1]]);
Z^2 \ [ [ 0, 0 ], [ 1, 1 ] ]
gap> Difference(last,[[0,0],[1,1]]);
Z^2 \ [ [ 0, 0 ], [ 1, 1 ] ]
gap> Difference(last,[[5,-5],[4,9]]);
Z^2 \ <set of cardinality 4>
gap> Difference(last,R);
[  ]
gap> ResidueClassUnionViewingFormat("long");;
gap> 2*R;
The residue class (0,0)+(2,0)Z+(0,2)Z of Z^2
gap> 3*last;
The residue class (0,0)+(6,0)Z+(0,6)Z of Z^2
gap> S := Difference(Union(2*R,[[2,0],[1,6]]),[[-4,6]]);
(The residue class (0,0)+(2,0)Z+(0,2)Z of Z^2) U [ [ 1, 6 ] ] \ [ [ -4, 6 ] ]
gap> 2*S;
(The residue class (0,0)+(4,0)Z+(0,4)Z of Z^2) U [ [ 2, 12 ] ] \
[ [ -8, 12 ] ]
gap> -S;
(The residue class (0,0)+(2,0)Z+(0,2)Z of Z^2) U [ [ -1, -6 ] ] \
[ [ 4, -6 ] ]
gap> ResidueClassUnionViewingFormat("short");;
gap> 2*R;
(0,0)+(2,0)Z+(0,2)Z
gap> 3*last;
(0,0)+(6,0)Z+(0,6)Z
gap> S := Difference(Union(2*R,[[2,0],[1,3],[1,6]]),[[2,2],[-4,6]]);
(0,0)+(2,0)Z+(0,2)Z U [ [ 1, 3 ], [ 1, 6 ] ] \ [ [ -4, 6 ], [ 2, 2 ] ]
gap> Modulus(S);
[ [ 2, 0 ], [ 0, 2 ] ]
gap> Residues(S);
[ [ 0, 0 ] ]
gap> IncludedElements(S);
[ [ 1, 3 ], [ 1, 6 ] ]
gap> ExcludedElements(S);
[ [ -4, 6 ], [ 2, 2 ] ]
gap> Density(S);
1/4
gap> Density(R);
1
gap> Density(7*R);
1/49
gap> S = Union(S,S);
true
gap> 2*S;
(0,0)+(4,0)Z+(0,4)Z U [ [ 2, 6 ], [ 2, 12 ] ] \ [ [ -8, 12 ], [ 4, 4 ] ]
gap> -S;
(0,0)+(2,0)Z+(0,2)Z U [ [ -1, -6 ], [ -1, -3 ] ] \ [ [ -2, -2 ], [ 4, -6 ] ]
gap> CallFuncList(UnhideGlobalVariables,FREE_ONE_LETTER_GLOBALS);
gap> ResidueClassUnionViewingFormat(oldformat);
gap> STOP_TEST( "zxz.tst", 30000000 );

#############################################################################
##
#E  zxz.tst . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here