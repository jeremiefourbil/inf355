! Copyright (C) 2013 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: kernel math io memoize sequences;
IN: td1

: mean ( x -- x ) [ sum ] [ length ] bi / ;

MEMO: fibo ( n -- n' ) 
  dup 2 < [ 
    drop 1
  ] [ 
    [ 1 - ] [ 2 - ] bi [ fibo ] bi@ +
  ] if ; 

: fact ( n -- n' ) dup 2 < [
    drop 1
  ] [ 
    [ 1 - ] keep [ fact ] dip *
  ] if ;