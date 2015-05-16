/**
 * Created by ozgurb on 16.05.2015.
 */

//p97
object Sudoku {
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










    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    def satir[T](i: Int, j: Int, mat: Vector[Vector[T]]) = mat(i)
    def sutun[T](i: Int, j: Int, mat: Vector[Vector[T]]) = {
        (for(k <- 0 until mat.length) yield mat(k)(j)).toVector
    }
    def kare[T](i: Int, j: Int, mat: Vector[Vector[T]]) = {
        def bound(n: Int) = {
            if(n < 3) 0 to 2
            else if(n < 6) 3 to 5
            else 6 to 8
        }
        (for(k <- bound(i); l <- bound(j)) yield mat(k)(l)).toVector

    }

    def mapMatris[T, Z](mat: Vector[Vector[T]])(f: (Int, Int, T) => Z) = {
        (for(i <- 0 until mat.length) yield {
            (for(j <- 0 until mat(i).length) yield f(i, j, mat(i)(j))).toVector
        }).toVector
    }

    def numbers(i: Int, j: Int, mat: Vector[Vector[Int]]) = {
        if(mat(i)(j) > 0) List(mat(i)(j))
        else {
            val str = satir(i, j, mat)
            val stn = sutun(i, j, mat)
            val kr = kare(i, j, mat)
            (for {
                i <- 1 to 9
                if((! str.contains(i)) && (! stn.contains(i)) && (! kr.contains(i)))
            } yield i).toList
        }
    }



    def solution(mat: Vector[Vector[Int]]) = {

        def brute1(mat2: Vector[Vector[Int]], i: Int, j: Int): Vector[Vector[Int]] = {

            def nloop(lst: List[Int]) :Vector[Vector[Int]] = {
                if (lst.isEmpty) Vector()
                else {
                    val ii = if (j == 8) i + 1 else i
                    val jj = if (j == 8) 0 else j + 1
                    val m1 = mapMatris(mat2)((i1, j1, x) => if (i == i1 && j == j1) lst.head else x)
                    if (i == 8 && j == 8) m1
                    else {
                        val m2 = brute1(m1, ii, jj)
                        if (m2 == Vector()) nloop(lst.tail) else m2
                    }
                }
            }
            nloop(numbers(i, j, mat2))
        }
        brute1(mat, 0, 0)
    }


}
