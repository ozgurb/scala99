/**
 * Created by ozgurb on 16.05.2015.
 */

//p97
object Sudoku {

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
