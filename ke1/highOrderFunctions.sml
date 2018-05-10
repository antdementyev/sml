(*Implementieren Sie eine Funktion make_map_fn, die eine Funktion f1 als Argument erwartet 
und eine Funktion f2 zurückliefert,  so dass in allen Fällen, in denen
    map f1 l;
definiert ist, das gleiche Ergebnis durch
    foldr f2 [] l;
berechnet wird.*)
fun make_map_fn f1 =
    fn(x, y) => f1(x) :: y
    
(*Implementieren Sie eine Funktion make_filter_fn, die zu einer übergebenen Funktion f1 
eine Funktion f2 zurückliefert, so dass jeder korrekt auswertbare Aufruf
    filter (f1, l);
ersetzt werden kann durch
    foldr f2 [] l;  *)
fun make_filter_fn f1 =
    fn(x, y) => if f1(x) then x::y else y
    
(*Definieren Sie mittels der zuvor definierten Funktionen make_map_fn und make_filter_fn sowie der Funktion foldr 
Funktionen my_map und my_filter, die sich wie die im Kurs vorgestellten Funktionen map und filter verhalten.
Die zuvor implementierten Funktionen make_map_fn und make_filter_fn können Sie als gegeben annehmen.*)
fun my_map f1 l = foldr(make_map_fn f1) [] l
fun my_filter(f1, l) = foldr(make_filter_fn f1) [] l