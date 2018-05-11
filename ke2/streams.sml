(*From lecture*)

type 'a lazy = unit -> 'a;

fun force(f:'a lazy) = f();

datatype 'a sequ = NIL | CONS of 'a * 'a sequ lazy;

fun filters p  NIL        = NIL
 |  filters p (CONS(x,r)) = 
            if p x then CONS(x, fn() => filters p (force r))
                         else filters p (force r);
                         
fun first 0 s            = []
 |  first n NIL          = []
 |  first n (CONS (x,r)) = x::first (n-1) (force r)

(*Implementieren Sie eine Funktion nat_paare, die einen Strom aller Paare natürlicher Zahlen
auflistet. Dabei soll die Zahl 0 als natürliche Zahl angesehen werden. Die Auflistung soll
fair sein, d.h. jedes Paar erscheint an endlicher Position in dem Strom.
Den Datentyp sequ können Sie als gegeben annehmen.
Hinweis: Eine faire Aufzählung aller Paare natürlicher Zahlen erhalten Sie z.B. bei
Sortierung nach der Größe der Summe der beiden Elemente sowie bei gleicher Summe nach der Größe
des ersten Elementes.*)

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
    
fun nat_paare() = getNextPair(~1,0);

first 7 (nat_paare());

(*Implementieren Sie eine Funktion nat_paare_nicht_null die eine faire Auflistung aller Paare
natürlicher Zahlen ohne 0 zurückliefert. nat_paare_nicht_null soll mit Hilfe der Funktion filters
aus dem Kurstext und nat_paare implementiert werden. Sie müssen filters und nat_paare also nicht
mehr definieren.*)

fun hasNoNull (a,b) = a <> 0 andalso b <> 0;

fun nat_paare_nicht_null() = filters hasNoNull (nat_paare());

first 7 (nat_paare_nicht_null());

(*Implementieren Sie eine Funktion brueche die einen Strom aller Paare (a, b) von natürlichen
Zahlen (ohne 0) zurückliefert, so dass a/b ein gekürzter Bruch ist.
sequ, nat_paare, nat_paare_nicht_null und filters können als gegeben angenommen werden.*)

(*Groesster gemeinsmer Teiler*)
fun ggt a b = if a = 0 then b else
              if b < a then ggt b a
                       else ggt a (b-a)

fun isSimpleFraction(a,b) = (ggt a b) = 1

fun brueche() = filters isSimpleFraction (nat_paare_nicht_null());

first 10 (brueche());