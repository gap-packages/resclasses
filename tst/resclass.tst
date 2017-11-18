#############################################################################
##
#W  resclass.tst            GAP4 Package `ResClasses'             Stefan Kohl
##
##  This file contains automated tests of ResClasses' functionality for
##  computing with residue class unions.
##
#############################################################################

gap> START_TEST( "resclass.tst" );
gap> ResClassesDoThingsToBeDoneBeforeTest();
gap> cl1 := ResidueClass(Integers,3,2);
The residue class 2(3) of Z
gap> cl2 := ResidueClass(Z_pi([2,5]),2,1);
The residue class 1(2) of Z_( 2, 5 )
gap> cl2 = ResidueClass(Integers,2,1);
false
gap> cl1 = ResidueClassNC(2,3);
true
gap> ExtRepOfObj(cl1);
[ 3, [ 2 ], [  ], [  ] ]
gap> Mod(Integers);
1
gap> Mod([1,2,3]);
0
gap> Mod([]);     
0
gap> Residues(Integers);
[ 0 ]
gap> Residues([1,2,3]);
[  ]
gap> Residues([]);      
[  ]
gap> SizeOfSmallestResidueClassRing(Integers);
2
gap> SizeOfSmallestResidueClassRing(Z_pi([3,7]));
3
gap> SizeOfSmallestResidueClassRing(PolynomialRing(GF(5),1));
5
gap> SizeOfSmallestResidueClassRing(Integers^2);             
2
gap> R := PolynomialRing(GF(7),1);;
gap> x := Indeterminate(GF(7),1);; SetName(x,"x");
gap> cl3 := ResidueClass(R,x+One(R),3*One(R));
The residue class 3 ( mod x+1 ) of GF(7)[x]
gap> DisplayString(cl3);
"3(x+1)"
gap> U1 := ResidueClassUnion(Integers,6,[2,4]);
Union of the residue classes 2(6) and 4(6) of Z
gap> Classes(U1);
[ [ 2, 6 ], [ 4, 6 ] ]
gap> Classes(SparseRep(U1));
[ [ 2, 6 ], [ 4, 6 ] ]
gap> SparseRep(SparseRep(U1)) = U1;
true
gap> StandardRep(StandardRep(U1)) = U1;
true
gap> U1 = SparseRep(U1);               
true
gap> U1 = [1,2,3];
false
gap> [1,2,3] = U1;
false
gap> U2 := ResidueClassUnion(Integers,5,[1,2],[3,8],[-4,1]);
(Union of the residue classes 1(5) and 2(5) of Z) U [ 3, 8 ] \ [ -4, 1 ]
gap> DisplayString(U2);
"1(5) U 2(5) U [ 3, 8 ] \\ [ -4, 1 ]"
gap> String(SparseRep(U2));
"ResidueClassUnion( Integers, [ [ 1, 5 ], [ 2, 5 ] ], [ 3, 8 ], [ -4, 1 ] )"
gap> Print(SparseRep(U2),"\n"); 
ResidueClassUnion( Integers, [ [ 1, 5 ], [ 2, 5 ] ], [ 3, 8 ], [ -4, 1 ] )
gap> Display(U2);             
(Union of the residue classes 1(5) and 2(5) of Z) U [ 3, 8 ] \ [ -4, 1 ]
gap> Display(SparseRep(U2));
(Union of the residue classes 1(5) and 2(5) of Z) U [ 3, 8 ] \ [ -4, 1 ]
gap> U1 < U2;
false
gap> SparseRep(U1) < SparseRep(U2);
false
gap> 7 in SparseRep(U2);
true
gap> 8 in SparseRep(U2);
true
gap> -4 in SparseRep(U2);
false
gap> 0 in SparseRep(U2); 
false
gap> U3 := ResidueClassUnion(R,x,[One(R),5*One(R),6*One(R)],
>                               [Zero(R)],[One(R)]);
<union of 3 residue classes (mod x) of GF(7)[x]> U [ 0 ] \ [ 1 ]
gap> U4 := ResidueClassUnion(Z_pi([2,3]),8,[3,5]);
Union of the residue classes 3(8) and 5(8) of Z_( 2, 3 )
gap> List([U1,U2,U3,U4],Modulus);
[ 6, 5, x, 8 ]
gap> List([cl1,U2,U3,U4],Residues);
[ [ 2 ], [ 1, 2 ], [ 1, -1, 5 ], [ 3, 5 ] ]
gap> List([U1,U2,U3,U4],IncludedElements);
[ [  ], [ 3, 8 ], [ 0 ], [  ] ]
gap> List([U1,U2,U3,U4],ExcludedElements);
[ [  ], [ -4, 1 ], [ 1 ], [  ] ]
gap> String(cl3);
"ResidueClassUnion( PolynomialRing( GF(7), [ x ] ), x+Z(7)^0, [ Z(7) ] )"
gap> String(U2);
"ResidueClassUnion( Integers, 5, [ 1, 2 ], [ 3, 8 ], [ -4, 1 ] )"
gap> String(U4);
"ResidueClassUnion( Z_pi( [ 2, 3 ] ), 8, [ 3, 5 ] )"
gap> Print(cl3,"\n");
ResidueClassUnion( PolynomialRing( GF(7), ["x"] ), x+Z(7)^0, [ Z(7) ] )
gap> Print(U2,"\n");
ResidueClassUnion( Integers, 5, [ 1, 2 ], [ 3, 8 ], [ -4, 1 ] )
gap> Print(U4,"\n");
ResidueClassUnion( Z_pi( [ 2, 3 ] ), 8, [ 3, 5 ] )
gap> Display(U2);
(Union of the residue classes 1(5) and 2(5) of Z) U [ 3, 8 ] \ [ -4, 1 ]
gap> Display(U3);
(Union of the residue classes 1 ( mod x ), -1 ( mod x ) and 5 ( mod x ) of GF(\
7)[x]) U [ 0 ] \ [ 1 ]
gap> Display(U4);
Union of the residue classes 3(8) and 5(8) of Z_( 2, 3 )
gap> 20 in cl1;
true
gap> -20 in cl1;
false
gap> 1/3 in cl2;
true
gap> x in U3;
false
gap> Zero(R) in U3;
true
gap> IsSubset(cl1,U1);
false
gap> IsSubset(cl1,SparseRep(U1));
false
gap> IsSubset(SparseRep(cl1),U1);
false
gap> IsSubset(SparseRep(cl1),SparseRep(U1));
false
gap> IsSubset(U4,ResidueClass(Z_pi([2,3]),16,11));
true
gap> IsSubset(U2,ResidueClass(7,10));
true
gap> IsSubset(U2,SparseRep(ResidueClass(7,10)));
true
gap> IsSubset(SparseRep(U2),SparseRep(ResidueClass(7,10)));
true
gap> IsSubset(SparseRep(U2),ResidueClass(7,10));
true
gap> U5 := ResidueClassUnion(Integers,15,[6,12],[8],[]);
(Union of the residue classes 6(15) and 12(15) of Z) U [ 8 ]
gap> IsSubset(U2,U5);                                      
true
gap> IsSubset(SparseRep(U2),SparseRep(U5));
true
gap> U6 := ResidueClassUnion(Integers,15,[6,12],[8,11,23],[]);
(Union of the residue classes 6(15) and 12(15) of Z) U [ 8, 11, 23 ]
gap> IsSubset(U2,U6);
false
gap> IsSubset(SparseRep(U2),SparseRep(U6));
false
gap> IsSubset(Integers,U5);
true
gap> IsSubset(U5,Integers);
false
gap> U7 := ResidueClassUnion(Integers,15,[6,12],[8,11,23],[21]);
(Union of the residue classes 6(15) and 12(15) of Z) U [ 8, 11, 23 ] \ [ 21 ]
gap> Delta(U7);               
1/5
gap> Delta(SparseRep(U7));
1/5
gap> List([0..6],k->Rho(ResidueClass(k,7)));
[ E(4), -E(28)^23, E(28)^11, -E(28)^27, -E(28)^15, E(28)^3, -E(28)^19 ]
gap> Product(last);
-E(4)
gap> Union(U7,U7) = U7;
true
gap> Union(U7,SparseRep(U7)) = U7;
true
gap> Union(SparseRep(U7),U7) = U7;
true
gap> Union(SparseRep(U7),SparseRep(U7)) = U7;
true
gap> Union(Difference(Integers,U7),U7);
Integers
gap> Union(Difference(Integers,U7),SparseRep(U7));
Integers
gap> Union(SparseRep(Difference(Integers,U7)),SparseRep(U7));
Integers
gap> Union(U7,Integers);
Integers
gap> Union(Integers,U7);
Integers
gap> Union(Integers,[1,2,3]);
Integers
gap> Union([1,2,3],Integers);
Integers
gap> Union(Integers,Integers);
Integers
gap> Union(Z_pi(2),Z_pi(2));  
Z_( 2 )
gap> Intersection(U2+1,U7);
(The residue class 12(15) of Z) U [ 8, 23 ] \ [ -3 ]
gap> Intersection(SparseRep(U2+1),U7);
(The residue class 12(15) of Z) U [ 8, 23 ] \ [ -3 ]
gap> Intersection(SparseRep(U2)+1,U7);
(The residue class 12(15) of Z) U [ 8, 23 ] \ [ -3 ]
gap> Intersection(U2+1,SparseRep(U7));
(The residue class 12(15) of Z) U [ 8, 23 ] \ [ -3 ]
gap> Intersection(SparseRep(U2)+1,SparseRep(U7));
(The residue class 12(15) of Z) U [ 8, 23 ] \ [ -3 ]
gap> Intersection(U7,[-50..50]);
[ -48, -39, -33, -24, -18, -9, -3, 6, 8, 11, 12, 23, 27, 36, 42 ]
gap> Intersection([-50..50],U7) = last;
true
gap> Difference(U7,U2);
[ 23 ]
gap> Difference(U2,U7);
(Union of the residue classes 1(15), 2(15), 7(15) and 11(15) of Z) U 
[ 3, 21 ] \ [ -4, 1, 11 ]
gap> Difference(SparseRep(U7),U2);
[ 23 ]
gap> Difference(SparseRep(U2),U7);
(Union of the residue classes 1(15), 2(15), 7(15) and 11(15) of Z) U 
[ 3, 21 ] \ [ -4, 1, 11 ]
gap> List([U1,U1/2,2*U1+7,U3,U4,[1,2,3],[1/2,2,3],[x],[],Integers,Z_pi(3),R],
>         Density);
[ 1/3, 2/3, 1/6, 3/7, 1/4, 0, 0, 0, 0, 1, 1, 1 ]
gap> Difference([2,4,7,8],cl1);
[ 4, 7 ]
gap> I := ResidueClassUnion(Integers,6,[1,5]);
Union of the residue classes 1(6) and 5(6) of Z
gap> J := ResidueClassUnion(Integers,5,[1,2,3,4]);
Z \ The residue class 0(5) of Z
gap> K := Union(I,J);
Z \ Union of the residue classes 0(10) and 15(30) of Z
gap> Residues(K);
[ 1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 16, 17, 18, 19, 21, 22, 23, 24, 
  25, 26, 27, 28, 29 ]
gap> L := Intersection(I,J);
<union of 8 residue classes (mod 30) of Z>
gap> Display(L);
Union of the residue classes 1(30), 7(30), 11(30), 13(30), 17(30), 19(30), 
23(30) and 29(30) of Z
gap> M := Difference(I,J);
Union of the residue classes 5(30) and 25(30) of Z
gap> N := Difference(J,I);
<union of 16 residue classes (mod 30) of Z (8 classes)>
gap> Display(N);
Union of the residue classes 2(10), 4(10), 6(10), 8(10), 3(30), 9(30), 
21(30) and 27(30) of Z
gap> Difference(Integers,[1,2,3]);
Z \ [ 1, 2, 3 ]
gap> Display(last);
Z \ [ 1, 2, 3 ]
gap> Difference(Z_pi([2,3,7]),[1/5,1/55]);
Z_( 2, 3, 7 ) \ [ 1/55, 1/5 ]
gap> O := Difference(Union(cl1,[1,3]),[2,5,8]);
(The residue class 2(3) of Z) U [ 1, 3 ] \ [ 2, 5, 8 ]
gap> P := Union(Difference(cl1,[-1]),[-3,0]);
(The residue class 2(3) of Z) U [ -3, 0 ] \ [ -1 ]
gap> Display(Union(O,P));
(The residue class 2(3) of Z) U [ -3, 0, 1, 3 ]
gap> Difference(O,P);
[ -1, 1, 3 ]
gap> Difference(P,O);
[ -3, 0, 2, 5, 8 ]
gap> Display(Union(cl1,[1..100]));
(The residue class 2(3) of Z) U [ 1, 3, 4, 6, 7, 9, 10, 12, 13, 15, 16, 18, 
  19, 21, 22, 24, 25, 27, 28, 30, 31, 33, 34, 36, 37, 39, 40, 42, 43, 45, 46, 
  48, 49, 51, 52, 54, 55, 57, 58, 60, 61, 63, 64, 66, 67, 69, 70, 72, 73, 75, 
  76, 78, 79, 81, 82, 84, 85, 87, 88, 90, 91, 93, 94, 96, 97, 99, 100 ]
gap> Display(Difference(cl1,[1..100]));
(The residue class 2(3) of Z) \ [ 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 
  35, 38, 41, 44, 47, 50, 53, 56, 59, 62, 65, 68, 71, 74, 77, 80, 83, 86, 89, 
  92, 95, 98 ]
gap> Q := ResidueClassUnion( Integers, 18, [ 2, 5, 8, 11, 14, 16, 17 ],
>                            [ 1, 3, 4, 10 ], [ 2, 5, 8, 16 ] );;
gap> IsSubset(Q,O);
true
gap> IsSubset(O,Q);
false
gap> U := ResidueClassUnion(Integers,3,[1],[0],[]);
(The residue class 1(3) of Z) U [ 0 ]
gap> V := ResidueClassUnion(Integers,3,[2],[0],[]);
(The residue class 2(3) of Z) U [ 0 ]
gap> Intersection(U,V);
[ 0 ]
gap> U := ResidueClassUnion(Integers,3,[1],[0],[1]);
(The residue class 1(3) of Z) U [ 0 ] \ [ 1 ]
gap> V := ResidueClassUnion(Integers,3,[1,2],[0],[]);
(Union of the residue classes 1(3) and 2(3) of Z) U [ 0 ]
gap> Display(Difference(V,U));
(The residue class 2(3) of Z) U [ 1 ]
gap> cl := List([1..25],i->ResidueClass(Integers,Primes[i],i));;
gap> cl_int := Intersection(cl);
The residue class 941584379775558526136539054851975983(23055679639455184247531\
02147331756070) of Z
gap> List(Primes{[1..25]},p->Representative(cl_int) mod p);
[ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 
  22, 23, 24, 25 ]
gap> it := Iterator(U2);
<iterator of a residue class union of Z>
gap> l := List([1..16],i->NextIterator(it));
[ 3, 8, 2, -3, 6, -9, 7, -8, 11, -14, 12, -13, 16, -19, 17, -18 ]
gap> it2 := ShallowCopy(it);
<iterator of a residue class union of Z>
gap> l := List([1..16],i->NextIterator(it2));
[ 21, -24, 22, -23, 26, -29, 27, -28, 31, -34, 32, -33, 36, -39, 37, -38 ]
gap> l := [];;
gap> for n in U2 do Add(l,n); if Length(l) > 100 then break; fi; od;
gap> Set(l) = Intersection(U2,[-124..126]);
true
gap> l := [];;
gap> for n in Difference(Integers,cl1) do
>      Add(l,n); if Length(l)>100 then break; fi;
>    od;
gap> Set(l) = Intersection(Difference(Integers,cl1),[-75..75]);
true
gap> (((4+2*(U2+7)*8)/2)*2-4)/16-7 = U2;
true
gap> -U2;
(Union of the residue classes 3(5) and 4(5) of Z) U [ -8, -3 ] \ [ -1, 4 ]
gap> -(-U2) = U2;
true
gap> Difference(ResidueClass(Integers,6,1),Integers);
[  ]
gap> Difference(Integers,ResidueClass(Integers,6,1));
Z \ The residue class 1(6) of Z
gap> Difference(Integers,Integers);
[  ]
gap> Intersection(Integers,ResidueClass(Integers,6,1));
The residue class 1(6) of Z
gap> Difference(Integers,[]) = Integers;
true
gap> Modulus(Integers);
1
gap> Modulus(Z_pi([2]));
1
gap> Modulus(R);
1
gap> Residues(Integers);
[ 0 ]
gap> Residues(Z_pi([2,7]));
[ 0 ]
gap> Residues(R);
[ 0 ]
gap> IncludedElements(Integers);
[  ]
gap> IncludedElements(R);
[  ]
gap> IncludedElements([1,2,3]);
[ 1, 2, 3 ]
gap> ExcludedElements(Integers);
[  ]
gap> ExcludedElements([1,2,3]);
[  ]
gap> 2*Integers;
The residue class 0(2) of Z
gap> Z_pi(2)*3;
Z_( 2 )
gap> Z_pi(2)*2;
The residue class 0(2) of Z_( 2 )
gap> x*R;
The residue class 0 ( mod x ) of GF(7)[x]
gap> R+One(R);
GF(7)[x]
gap> Integers+1;
Integers
gap> Z_pi(2)-1;
Z_( 2 )
gap> -Integers;
Integers
gap> 45-Integers;
Integers
gap> -Z_pi([2,3]);
Z_( 2, 3 )
gap> 1-Z_pi([2,3]);
Z_( 2, 3 )
gap> 1/3*Z_pi(2);
Z_( 2 )
gap> 4*Z_pi(2);
The residue class 0(4) of Z_( 2 )
gap> last+1;
The residue class 1(4) of Z_( 2 )
gap> Union(last,last2);
Union of the residue classes 0(4) and 1(4) of Z_( 2 )
gap> Union(ResidueClass(Integers,2,1),[1..100]);
(The residue class 1(2) of Z) U <set of cardinality 50>
gap> Difference(ResidueClass(Integers,2,1),[1..100]);
(The residue class 1(2) of Z) \ <set of cardinality 50>
gap> Difference(Union(ResidueClass(Integers,2,1),[1..100]),[-5..-1]);
(The residue class 1(2) of Z) U <set of cardinality 50> \ [ -5, -3, -1 ]
gap> AllResidueClassesModulo(3);
[ The residue class 0(3) of Z, The residue class 1(3) of Z, 
  The residue class 2(3) of Z ]
gap> AllResidueClassesModulo(Z_pi(2),4);
[ The residue class 0(4) of Z_( 2 ), The residue class 1(4) of Z_( 2 ), 
  The residue class 2(4) of Z_( 2 ), The residue class 3(4) of Z_( 2 ) ]
gap> AllResidueClassesModulo(Z_pi(2),3);
[ Z_( 2 ) ]
gap> R := PolynomialRing(GF(7),1);;
gap> x := Indeterminate(GF(7),1);; SetName(x,"x");
gap> AllResidueClassesModulo(R,x);
[ The residue class 0 ( mod x ) of GF(7)[x], 
  The residue class 1 ( mod x ) of GF(7)[x], 
  The residue class 3 ( mod x ) of GF(7)[x], 
  The residue class 2 ( mod x ) of GF(7)[x], 
  The residue class -1 ( mod x ) of GF(7)[x], 
  The residue class 4 ( mod x ) of GF(7)[x], 
  The residue class 5 ( mod x ) of GF(7)[x] ]
gap> AllResidueClassesModulo(R,One(R));
[ GF(7)[x] ]
gap> SplittedClass(ResidueClass(2,3),5);
[ The residue class 2(15) of Z, The residue class 5(15) of Z, 
  The residue class 8(15) of Z, The residue class 11(15) of Z, 
  The residue class 14(15) of Z ]
gap> SplittedClass(ResidueClass(Z_pi([2,3]),3,2),2);
[ The residue class 2(6) of Z_( 2, 3 ), The residue class 5(6) of Z_( 2, 3 ) ]
gap> SplittedClass(ResidueClass(Z_pi([2,3]),3,2),5);
fail
gap> Residue(ResidueClass(8,16));
8
gap> Residue(Integers);
0
gap> U := ResidueClassUnion(Z_pi(2),8,[1,2,3,5,7]);
Z_( 2 ) \ Union of the residue classes 0(4) and 6(8) of Z_( 2 )
gap> AsUnionOfFewClasses(U);
[ The residue class 1(2) of Z_( 2 ), The residue class 2(8) of Z_( 2 ) ]
gap> x := Indeterminate(GF(2));; SetName(x,"x");;
gap> R := PolynomialRing(GF(2),1);;
gap> U := ResidueClassUnion(R,x^3,[Zero(R),One(R),x,x^2,x^2+x]);
GF(2)[x] \ <union of 3 residue classes (mod x^3) of GF(2)[x] (2 classes)>
gap> AsUnionOfFewClasses(U);
[ The residue class 0 ( mod x ) of GF(2)[x], 
  The residue class 1 ( mod x^3 ) of GF(2)[x] ]
gap> SplittedClass(R,1);
[ GF(2)[x] ]
gap> SplittedClass(R,2);
[ The residue class 0 ( mod x ) of GF(2)[x], 
  The residue class 1 ( mod x ) of GF(2)[x] ]
gap> SplittedClass(R,3);
fail
gap> SplittedClass(R,x+1);
[ The residue class 0 ( mod x+1 ) of GF(2)[x], 
  The residue class 1 ( mod x+1 ) of GF(2)[x] ]
gap> Union(last);
GF(2)[x]
gap> cl := ResidueClass(R,x,Zero(R));;
gap> SplittedClass(cl,1);
[ The residue class 0 ( mod x ) of GF(2)[x] ]
gap> SplittedClass(cl,2);
[ The residue class 0 ( mod x^2 ) of GF(2)[x], 
  The residue class x ( mod x^2 ) of GF(2)[x] ]
gap> SplittedClass(cl,3);
fail
gap> SplittedClass(cl,x^2+x+1);
[ The residue class 0 ( mod x^3+x^2+x ) of GF(2)[x], 
  The residue class x ( mod x^3+x^2+x ) of GF(2)[x], 
  The residue class x^2 ( mod x^3+x^2+x ) of GF(2)[x], 
  The residue class x^2+x ( mod x^3+x^2+x ) of GF(2)[x] ]
gap> Union(last);
The residue class 0 ( mod x ) of GF(2)[x]
gap> cl := ResidueClass(1,x);
The residue class 1 ( mod x ) of GF(2)[x]
gap> cl = ResidueClass(Z(2),x);
true
gap> cl = ResidueClass(x,1); 
true
gap> ViewString(cl);
"1(x)"
gap> 2*cl;
[ 0 ]
gap> 3*cl;
The residue class 1 ( mod x ) of GF(2)[x]
gap> Z(2)*cl;
The residue class 1 ( mod x ) of GF(2)[x]
gap> 0*Z(2)*cl;   
[ 0 ]
gap> cl*3;
The residue class 1 ( mod x ) of GF(2)[x]
gap> cl*Z(2);
The residue class 1 ( mod x ) of GF(2)[x]
gap> cl*Z(2)*0;
[ 0 ]
gap> 2*R;
[ 0 ]
gap> 3*R;
GF(2)[x]
gap> Z(2)*R;
GF(2)[x]
gap> R*Z(2);
GF(2)[x]
gap> x := Indeterminate(GF(13),"x");;
gap> Z(13)*x^3+Z(13)^3*x^2+Z(13)*x+Z(13)^6;
2*x^3+8*x^2+2*x-1
gap> Z(13)*x^4+Z(13)^3*x^3+Z(13)*x^2+Z(13)^4*x+Z(13)^6;
2*x^4+8*x^3+2*x^2+3*x-1
gap> P := Z(13)^2*x+Z(13);
4*x+2
gap> List([1..4],n->P^n);
[ 4*x+2, 3*x^2+3*x+4, -x^3+5*x^2+9*x+8, 9*x^4+5*x^3+7*x^2+11*x+3 ]
gap> ResidueClassUnionViewingFormat("short");;
gap> PartitionsIntoResidueClasses(Integers,1);
[ [ Integers ] ]
gap> PartitionsIntoResidueClasses(Integers,2);
[ [ 0(2), 1(2) ] ]
gap> PartitionsIntoResidueClasses(Integers,3);
[ [ 0(2), 1(4), 3(4) ], [ 1(2), 0(4), 2(4) ], [ 0(3), 1(3), 2(3) ] ]
gap> PartitionsIntoResidueClasses(Integers,4);
[ [ 0(2), 1(4), 3(8), 7(8) ], [ 0(2), 3(4), 1(8), 5(8) ], 
  [ 0(2), 1(6), 3(6), 5(6) ], [ 1(2), 0(4), 2(8), 6(8) ], 
  [ 1(2), 2(4), 0(8), 4(8) ], [ 1(2), 0(6), 2(6), 4(6) ], 
  [ 0(3), 1(3), 2(6), 5(6) ], [ 0(3), 2(3), 1(6), 4(6) ], 
  [ 1(3), 2(3), 0(6), 3(6) ], [ 0(4), 1(4), 2(4), 3(4) ] ]
gap> PartitionsIntoResidueClasses(Z_pi(2),1);
[ [ Z_( 2 ) ] ]
gap> PartitionsIntoResidueClasses(Z_pi(2),2);
[ [ 0(2), 1(2) ] ]
gap> PartitionsIntoResidueClasses(Z_pi(2),3);
[ [ 0(2), 1(4), 3(4) ], [ 1(2), 0(4), 2(4) ] ]
gap> PartitionsIntoResidueClasses(Z_pi(2),4);
[ [ 0(2), 1(4), 3(8), 7(8) ], [ 0(2), 3(4), 1(8), 5(8) ], 
  [ 1(2), 0(4), 2(8), 6(8) ], [ 1(2), 2(4), 0(8), 4(8) ], 
  [ 0(4), 1(4), 2(4), 3(4) ] ]
gap> PartitionsIntoResidueClasses(R,1);
[ [ GF(2)[x] ] ]
gap> PartitionsIntoResidueClasses(R,2);
[ [ 0(x), 1(x) ], [ 0(x+1), 1(x+1) ] ]
gap> PartitionsIntoResidueClasses(R,3);
[ [ 0(x), 1(x^2), x+1(x^2) ], [ 0(x), 1(x^2+x), x+1(x^2+x) ], 
  [ 1(x), 0(x^2), x(x^2) ], [ 1(x), 0(x^2+x), x(x^2+x) ], 
  [ 0(x+1), 1(x^2+1), x(x^2+1) ], [ 0(x+1), 1(x^2+x), x(x^2+x) ], 
  [ 1(x+1), 0(x^2+1), x+1(x^2+1) ], [ 1(x+1), 0(x^2+x), x+1(x^2+x) ] ]
gap> 0 * Integers;
[ 0 ]
gap> Integers * 0;
[ 0 ]
gap> 0 * Z_pi(2,3);
[ 0 ]
gap> Zero(R) * R;
[ 0 ]
gap> [1,2,3] > ResidueClass(0,2);
true
gap> Integers < ResidueClass(0,2);
true
gap> [1,2,3] > Integers;
true
gap> Integers = [0];
false
gap> [0] = Integers;
false
gap> Integers + 3;
Integers
gap> 3 + Integers;   
Integers
gap> AsUnionOfFewClasses([1,2,3]);                
[  ]
gap> AsUnionOfFewClasses(Integers);
[ Integers ]
gap> PartitionsIntoResidueClasses(Integers,5,[3]);
[ [ 0(3), 1(3), 2(9), 5(9), 8(9) ], [ 0(3), 2(3), 1(9), 4(9), 7(9) ], 
  [ 1(3), 2(3), 0(9), 3(9), 6(9) ] ]
gap> Union(RandomPartitionIntoResidueClasses(Integers,5,[2,3]));
Integers
gap> S := ResidueClassUnion(Integers,6,[1,2,4],[3,9],[1,7]);
1(3) U 2(6) U [ 3, 9 ] \ [ 1, 7 ]
gap> l := ExtRepOfObj(S);
[ 6, [ 1, 2, 4 ], [ 3, 9 ], [ 1, 7 ] ]
gap> ObjByExtRep(FamilyObj(S),l);
1(3) U 2(6) U [ 3, 9 ] \ [ 1, 7 ]
gap> last = S;
true
gap> S := ResidueClassUnion(Z_pi(2,3),6,[1,2,4],[3,9],[1,7]);
1(3) U 2(6) U [ 3, 9 ] \ [ 1, 7 ]
gap> l := ExtRepOfObj(S);
[ 6, [ 1, 2, 4 ], [ 3, 9 ], [ 1, 7 ] ]
gap> ObjByExtRep(FamilyObj(S),l);
1(3) U 2(6) U [ 3, 9 ] \ [ 1, 7 ]
gap> last = S;
true
gap> UnderlyingRing(FamilyObj(last2));
Z_( 2, 3 )
gap> Length(PartitionsIntoResidueClasses(Integers,6));
160
gap> PartitionsIntoResidueClasses(Integers,6:distinct);
[ [ 0(2), 1(4), 3(8), 7(16), 15(32), 31(32) ], 
  [ 0(2), 1(4), 3(8), 7(24), 15(24), 23(24) ], 
  [ 0(2), 1(4), 3(12), 7(12), 11(24), 23(24) ], 
  [ 0(2), 1(4), 3(16), 7(16), 11(16), 15(16) ], 
  [ 0(2), 1(6), 3(6), 5(12), 11(24), 23(24) ], 
  [ 0(2), 1(6), 3(6), 5(18), 11(18), 17(18) ], 
  [ 0(2), 1(6), 3(12), 5(12), 9(12), 11(12) ], 
  [ 0(2), 1(8), 3(8), 5(8), 7(16), 15(16) ], 
  [ 0(2), 1(8), 5(8), 3(12), 7(12), 11(12) ], 
  [ 0(2), 1(10), 3(10), 5(10), 7(10), 9(10) ], 
  [ 0(3), 1(3), 2(6), 5(12), 11(24), 23(24) ], 
  [ 0(3), 1(3), 2(6), 5(18), 11(18), 17(18) ], 
  [ 0(3), 1(3), 2(9), 5(9), 8(18), 17(18) ], 
  [ 0(3), 1(3), 2(12), 5(12), 8(12), 11(12) ], 
  [ 0(3), 1(6), 2(6), 4(6), 5(12), 11(12) ], 
  [ 0(3), 1(6), 4(6), 2(9), 5(9), 8(9) ], 
  [ 0(4), 1(4), 2(4), 3(8), 7(16), 15(16) ], 
  [ 0(4), 1(4), 2(4), 3(12), 7(12), 11(12) ], 
  [ 0(4), 2(4), 1(6), 3(6), 5(12), 11(12) ], 
  [ 0(4), 2(4), 1(8), 3(8), 5(8), 7(8) ], 
  [ 0(4), 1(6), 3(6), 5(6), 2(8), 6(8) ], 
  [ 0(5), 1(5), 2(5), 3(5), 4(10), 9(10) ], 
  [ 0(6), 1(6), 2(6), 3(6), 4(6), 5(6) ] ]
gap> S := ResidueClassUnion(Integers,[[1,2],[7,72]]);
1(2)
gap> S = ResidueClass(1,2);
true
gap> 2*S;
2(4)
gap> S+1;
0(2)
gap> -S;
1(2)
gap> 2*S;
2(4)
gap> last/2;
1(2)
gap> last = ResidueClass(1,2);
true
gap> S := Union(S,ResidueClass(4,60));
1(2) U 4(60)
gap> S!.cls;
[ [ 1, 2 ], [ 4, 60 ] ]
gap> 2*S;
2(4) U 8(120)
gap> S*3;
3(6) U 12(180)
gap> last/3;
1(2) U 4(60)
gap> S;
1(2) U 4(60)
gap> -S;
1(2) U 56(60)
gap> Representative(last);
1
gap> S := Union(S,[1..8]);
1(2) U 4(60) U [ 2, 6, 8 ]
gap> S := Difference(S,[7..15]);
1(2) U 4(60) U [ 2, 6 ] \ [ 7, 9, 11, 13, 15 ]
gap> 2*S;
2(4) U 8(120) U [ 4, 12 ] \ [ 14, 18, 22, 26, 30 ]
gap> last/2;
1(2) U 4(60) U [ 2, 6 ] \ [ 7, 9, 11, 13, 15 ]
gap> last=S;
true
gap> S := Difference(S,ResidueClass(0,7));
<union of 186 residue classes (mod 420) (12 classes)> U [ 2, 6 ] \ 
[ 9, 11, 13, 15 ]
gap> S!.cls;
[ [ 1, 14 ], [ 3, 14 ], [ 5, 14 ], [ 9, 14 ], [ 11, 14 ], [ 13, 14 ], 
  [ 4, 420 ], [ 64, 420 ], [ 124, 420 ], [ 184, 420 ], [ 244, 420 ], 
  [ 304, 420 ] ]
gap> S0 := StandardRep(S);
<union of 186 residue classes (mod 420)> U [ 2, 6 ] \ [ 9, 11, 13, 15 ]
gap> S1 := Difference(Union(ResidueClass(1,2),ResidueClass(4,60)),ResidueClass(0,7));
<union of 186 residue classes (mod 420)>
gap> Difference(S0,S1);
[ 2, 6 ]
gap> IsList(last);
true
gap> Difference(S1,S0);
[ 9, 11, 13, 15 ]
gap> IsResidueClassUnionOfZInClassListRep(S0);
false
gap> IsResidueClassUnionOfZInClassListRep(S);
true
gap> IsResidueClassUnionOfZInClassListRep(S1);
false
gap> AsUnionOfFewClasses(S1);
[ 1(14), 3(14), 5(14), 9(14), 11(14), 13(14), 4(420), 64(420), 124(420), 
  184(420), 244(420), 304(420) ]
gap> S;
<union of 186 residue classes (mod 420) (12 classes)> U [ 2, 6 ] \ 
[ 9, 11, 13, 15 ]
gap> S := S*5+17;
<union of 186 residue classes (mod 2100) (12 classes)> U [ 27, 47 ] \ 
[ 62, 72, 82, 92 ]
gap> T := Difference(Integers,S);
<union of 1914 residue classes (mod 2100) (17 classes)> U [ 62, 72, 82, 92 
 ] \ [ 27, 47 ]
gap> Union(S,T);
Integers
gap> CoverByResidueClasses(Integers,[2,3,4,6,8,12]);
[ 0(2), 0(3), 1(4), 1(6), 3(8), 11(12) ]
gap> Union(last);
Integers
gap> CoversByResidueClasses(Integers,[2,3,3,6]);
[ [ 0(2), 0(3), 1(3), 5(6) ], [ 0(2), 0(3), 2(3), 1(6) ], 
  [ 0(2), 1(3), 2(3), 3(6) ], [ 1(2), 0(3), 1(3), 2(6) ], 
  [ 1(2), 0(3), 2(3), 4(6) ], [ 1(2), 1(3), 2(3), 0(6) ] ]
gap> List(last,Union);
[ Integers, Integers, Integers, Integers, Integers, Integers ]
gap> ResClassesDoThingsToBeDoneAfterTest();
gap> STOP_TEST( "resclass.tst", 140000000 );

#############################################################################
##
#E  resclass.tst . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
