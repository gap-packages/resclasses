gap> START_TEST( "bugfix.tst" );

# Verify that \< method for a list and a ring (and vice versa) is restricted
# to a few specific kinds of rings (and e.g. does not apply to finite fields).
# See <https://github.com/gap-packages/resclasses/issues/3>.
gap> GF(3) < [0*Z(3)];
false
gap> GF(3) < AsList(GF(3));
false

#
gap> STOP_TEST( "bugfix.tst", 1 );
