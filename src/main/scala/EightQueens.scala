/**
 * Created by josef on 16.05.2015.
 */

//p90
object EightQueens {
    def solution: List[List[(Int, Int)]] = {
        def isOk(accu: List[(Int, Int)], x: Int, y: Int): Boolean = {
            def isntOk(x1: Int, y1: Int, x2: Int, y2: Int): Boolean = {
                if (x1 == x2) true
                else if (x2 == y2) true
                else if (math.abs(x1 - x2) == math.abs(y1 - y2)) true
                else false
            }

            !accu.exists(z => isntOk(z._1, z._2, x, y))
        }

        def fill(n: Int, accu: List[(Int, Int)]): List[List[(Int, Int)]] = {
            if (n == 0) List(accu)
            else (for (i <- 1 to 8 if (isOk(accu, i, n))) yield fill(n - 1, (i, n) :: accu)).toList.flatten
        }
        fill(8, Nil)
    }


}
