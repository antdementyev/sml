(* Implementieren Sie eine Funktion v v mult die zwei (gleich lange) Listen von reellen Zahlen
erwartet. Die erste Liste soll als Zeilenvektor interpretiert werden, die zweite Liste als
Spaltenvektor. Als Ergebnis liefert v v mult das Vektorprodukt der beiden Vektoren zurueck. *)
fun v_v_mult [] [] = 0.0
 |  v_v_mult (x1::vector1) [] = 0.0
 |  v_v_mult [] (x2::vector2) = 0.0
 |  v_v_mult (x1::vector1) (x2::vector2) = x1 * x2 + v_v_mult vector1 vector2


(*Implementieren Sie eine Funktion m v mult, die im ersten Argument eine Matrix erwartet und
im zweiten Argument eine Liste von Zahlen, interpretiert als Spaltenvektor. m v mult liefert
als Ergebnis eine Liste von Zahlen die bei Interpretation als Spaltenvektor das Produkt der
beiden Argumente darstellt.*)
fun m_v_mult [] _ = []
 |  m_v_mult (line::matrix) vector = v_v_mult line vector :: m_v_mult matrix vector


(*Implementieren Sie eine Funktion m m mult, die zwei Matrizen in der oben gegebenen Darstellung
als Eingabe erwartet und die Produktmatrix in der gleichen Form zurueckliefert.*)
fun getNthElementFromList(vector, position) =
    if (position <= 0) then
        hd(vector)
    else
        getNthElementFromList(tl(vector), position - 1)

fun getNthColumnFromMatrix [] columnNumber =  []
 |  getNthColumnFromMatrix (line::matrix) columnNumber = getNthElementFromList(line, columnNumber) :: getNthColumnFromMatrix matrix columnNumber

fun transposeMatrix(matrix) =
    let
        val columnIndex = ref 0
        val transposedMatrix = ref []
      in
        while (!columnIndex < length(hd(matrix))) do (
            transposedMatrix := getNthColumnFromMatrix matrix (!columnIndex) :: !transposedMatrix;
            columnIndex := !columnIndex + 1
        );
        rev(!transposedMatrix)
    end;
    
fun multiplyLineByLine matrix1 [] = []
 |  multiplyLineByLine matrix1 (line::matrix2) =  m_v_mult matrix1 line :: multiplyLineByLine matrix1 matrix2

fun m_m_mult matrix1 matrix2 =
    let
        val matrix2 = transposeMatrix(matrix2)
        val transposedResult = multiplyLineByLine matrix1 matrix2
      in
        transposeMatrix(transposedResult)
    end