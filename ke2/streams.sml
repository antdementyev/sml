(*From lecture*)
type 'a lazy = unit -> 'a;

fun force(f:'a lazy) = f();

datatype 'a sequ = NIL | CONS of 'a * 'a sequ lazy;

fun filters p  NIL        = NIL
 |  filters p (CONS(x,r)) = 
            if p x then CONS(x, fn() => filters p (force r))
                         else filters p (force r);

(**)
fun getNextPair(a, b) =
    let
        val nextPair = 
            if b = 0 then 
                (0, a+1) 
            else
                (a+1, b-1)
    in
        CONS(nextPair, fn() => getNextPair(nextPair))
    end;
    
val nat_paare = getNextPair(~1,0);

(**)
fun hasNoNull (a,b) = a <> 0 andalso b <> 0; 

val nat_paare_nicht_null = filters hasNoNull nat_paare;

(*
fun from n = CONS (n, fn () => from (n+1));

val nats = from 0;

fun le x y = (y:int) <= x;
fun gr x y = (y:int) > x;

fun filter (p, [])   = []
 |  filter (p, x::l) = if p(x) then x::filter(p,l)
                               else    filter(p,l);

fun qsort []     = []
 |  qsort (x::l) = qsort (filter (le x) l) @ [x] @
                   qsort (filter (gr x) l);
*)