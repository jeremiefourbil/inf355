! Copyright (C) 2013 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: tools.test hanoi ;
IN: hanoi.tests

[ "10 to 20" ] [ 10 20 move ] unit-test
[ 1 1 ] [ 2 3 other 3 2 ] unit-test