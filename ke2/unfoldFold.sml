(*From lecture*)

datatype 'a tree = NODE of 'a * 'a tree * 'a tree
                 | EMPTY;

(*Implementieren Sie eine Funktion mapt: ('a −> 'b) −> 'a tree −> 'b tree.
Diese erwartet eine Funktion f und einen Baum t als Eingabe und liefert einen Baum t' als Ausgabe,
in dem jedes Element aus der Anwendung von f auf das entsprechende Element in t hervorgegangen ist.
Die Funktion soll geschönfinkelt vorliegen.*)

fun mapt f EMPTY = EMPTY
 |  mapt f (NODE(a, left, right)) = NODE(f a, mapt f left, mapt f right)
 
(*Implementieren Sie eine Funktion count : ('a −> bool) −> 'a tree −> int.
Diese soll zu einem Prädikat p und einem Baum t die Anzahl der Elemente von t liefern,
die p erfüllen. Auch diese Funktion soll geschönfinkelt vorliegen.*)

fun count f EMPTY = 0
 |  count f (NODE(a, left, right)) =
        let
            val countA = if f a then 1 else 0
        in
            countA + count f left + count f right
        end

(*Optimieren Sie unter Verwendung des Unfold/Fold-Verfahrens die Funktionsdefinition
fun opt t = count (fn m => m = 0)
                  (mapt (fn n => n mod 2) t);
so dass nur ein Baumdurchlauf notwendig ist. Bitte beachten Sie, dass lediglich die Korrektheit
Ihrer Einsendung überprüft werden kann. Ob tatsächlich nur ein Baumdurchlauf ausgeführt wurde,
wird nicht getestet.*)

(*
Sei
    fcou = (fn m => m = 0)
    fmap = (fn n => n mod 2)
Dann
    fun opt t = count fcou (mapt fmap t)

fun opt EMPTY = count fcou (mapt fmap EMPTY)
              = count fcou EMPTY
              = 0
fun opt (NODE(a, left, right)) = count fcou (mapt fmap NODE(a, left, right))
                               = count fcou NODE(fmap a, mapt fmap left, mapt fmap right)
                               = let
                                     val countA = if fcou(fmap a) then 1 else 0
                                 in
                                     countA + count fcou (mapt fmap left) + count fcou (mapt fmap right)
                                 end
                               = countA + opt left + opt right
*)

fun opt EMPTY = 0
 |  opt (NODE(a, left, right)) =
        let
            fun fcou m = m = 0
            fun fmap n = n mod 2
            val countA = if (fcou(fmap a)) then 1 else 0
        in
            countA + opt left + opt right
        end
