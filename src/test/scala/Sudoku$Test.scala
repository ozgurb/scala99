import org.scalatest.{BeforeAndAfterEach, FunSuite}

/**
 * Created by ozgurb on 22.05.2015.
 */
class SudokuTest extends FunSuite with BeforeAndAfterEach {
    // 0, boş kareyi temsil eder.
    val sdi = Vector(Vector(0, 0, 4, 8, 0, 0, 0, 1, 7),
                     Vector(6, 7, 0, 9, 0, 0, 0, 0, 0),
                     Vector(5, 0, 8, 0, 3, 0, 0, 0, 4),
                     Vector(3, 0, 0, 7, 4, 0, 1, 0, 0),
                     Vector(0, 6, 9, 0, 0, 0, 7, 8, 0),
                     Vector(0, 0, 1, 0, 6, 9, 0, 0, 5),
                     Vector(1, 0, 0, 0, 8, 0, 3, 0, 6),
                     Vector(0, 0, 0, 0, 0, 6, 0, 9, 1),
                     Vector(2, 4, 0, 0, 0, 1, 5, 0, 0))


   val sdo = Vector(Vector(9, 3, 4, 8, 2, 5, 6, 1, 7),
                    Vector(6, 7, 2, 9, 1, 4, 8, 5, 3),
                    Vector(5, 1, 8, 6, 3, 7, 9, 2, 4),
                    Vector(3, 2, 5, 7, 4, 8, 1, 6, 9),
                    Vector(4, 6, 9, 1, 5, 3, 7, 8, 2),
                    Vector(7, 8, 1, 2, 6, 9, 4, 3, 5),
                    Vector(1, 9, 7, 5, 8, 2, 3, 4, 6),
                    Vector(8, 5, 3, 4, 7, 6, 2, 9, 1),
                    Vector(2, 4, 6, 3, 9, 1, 5, 7, 8))

    test("Sudoku icin çözüm test ediliyor"){
        val coz = Sudoku.solution(sdi)
        assert(coz == sdo)
    }


}
