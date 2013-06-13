! Copyright (C) 2013 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: kernel math.parser math sequences io ;
IN: hanoi

: move ( a b -- str ) [ 10 >base ] bi@ " to " glue ;

: other ( a b -- o ) + 6 swap - ;

: partial ( a b -- a b' ) [ dup ] dip other ;

: hanoi ( d a n -- ) dup 1 = 
                     [ drop move print ]
                     [ 1 - dup [ 2dup partial ] 2dip [ hanoi ] dip [ 2dup move print ] dip [ swap partial swap ] dip hanoi ]
                     if ;