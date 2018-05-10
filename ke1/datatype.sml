(*Multimengen sind Konstrukte, die wie Mengen verschiedene Elemente ohne Beruecksichtigung einer Reihenfolge enthalten.
Im Unterschied zu Mengen kann ein Element in einer Multimenge mehrfach vorkommen.
Eine solche Multimenge lässt sich rekursiv wie folgt auffassen: 
Eine Multimenge kann leer sein, aus einem Element bestehen oder durch die Vereinigung zweier Multimengen gegeben sein.
Definieren Sie einen polymorphen Datentyp multimenge, der Multimengen beliebiger vergleichbarer Typen (equality types)
in oben beschriebener Weise darstellt. Verwenden Sie das Symbol LEER für eine leere Multimenge,
das Symbol EINELEM für Multimengen mit einem Element und VEREIN für Vereinigungen zweier Multimengen.*)
datatype ''a multimenge = LEER
                        | EINELEM of ''a
                        | VEREIN of ''a multimenge * ''a multimenge
                        
(*Definieren Sie eine Funktion number, die eine Multimenge M und einen Wert w vom entsprechenden Argumenttyp erwartet
und die Anzahl der Vorkommen von w in M zurückliefert. Den Datetup multimenge können Sie dabei als gegeben annehmen.
Die Funktion sollte einen Tupel (w, M) entgegennehmen.*)
fun number(w, LEER) = 0
 |  number(w, EINELEM(element)) = 
        if w = element then 1 else 0
 |  number(w, VEREIN(m1, m2)) =
        number(w, m1) + number(w, m2)
        
(*Definieren Sie eine Funktion simplify, die aus der Repräsentation einer Multimenge sämtliche Vereinigungen
mit der leeren Menge eliminiert*)
fun containsLeer(LEER) = true
 |  containsLeer(EINELEM(_)) = false
 |  containsLeer(VEREIN(m1, m2)) = containsLeer(m1) orelse containsLeer(m2)

fun simplify(LEER) = LEER
 |  simplify(EINELEM(element)) = EINELEM(element)
 |  simplify(VEREIN(m1, LEER)) = simplify(m1)
 |  simplify(VEREIN(LEER, m2)) = simplify(m2)
 |  simplify(VEREIN(m1, m2)) = 
        let
            val result = VEREIN(simplify(m1), simplify(m2))
          in
            if containsLeer(result) then
                simplify(result)
            else
                result
        end
        
(*Definieren Sie eine Funktion delete_all, die alle Vorkommen eines gegebenen Wertes aus einer Multimenge löscht.*)
fun delete_all(value, LEER) = LEER
 |  delete_all(value, EINELEM(element)) = 
        if value = element then LEER else EINELEM(element)
 |  delete_all(value, VEREIN(m1, m2)) =
        VEREIN(delete_all(value, m1), delete_all(value, m2))
        
(*Definieren Sie eine Funktion delete_one, die genau ein Vorkommen eines Wertes aus einer Multimenge löscht,
falls das Element vorhanden war, und ansonsten die unveränderte Multimenge zurückliefert.
Die zuvor definierte Funktion number können Sie dabei verwenden.*)
fun delete_one(value, LEER) = LEER
 |  delete_one(value, EINELEM(element)) = 
        if value = element then LEER else EINELEM(element)
 |  delete_one(value, VEREIN(m1, m2)) =
        if number(value, m1) > 0 then
            VEREIN(delete_one(value, m1), m2)
        else
    	      if number(value, m2) > 0 then
                VEREIN(m1, delete_one(value, m2))
            else
                VEREIN(m1, m2)