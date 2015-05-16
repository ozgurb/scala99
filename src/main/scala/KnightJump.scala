

/**
 * Created by josef on 16.05.2015.
 */
object KnightJump {
    //tamam lanması gereken kısımlar var ve optimize edilip hızlandırılmalı
    case class Point(i: Int, j: Int)

    def hamle(p: Point, n: Int): List[Point] = {
        (for {
            i <- List(-2, -1, 1, 2)
            j <- List(-2, -1, 1, 2)
            if(math.abs(i) != math.abs(j))
            if(p.i + i < n + 1 && p.i + i > 0)
            if(p.j + j < n + 1 && p.j + j > 0)
        } yield Point(p.i + i, p.j + j)).toList

    }

    def coz(n: Int, accu: List[Point]) : List[Point] = {
        def rcloop(lst: List[Point]): List[Point]  = {
            if(lst.isEmpty) accu
            else {
                val t = coz(n, lst.head :: accu)
                if(t.length == n * n) t
                else rcloop(lst.tail)
            }
        }
        if(accu.length == n * n) accu
        else {

            val h = hamle(accu.head, n)
            val gecerli = h diff accu
            rcloop(gecerli)
        }
    }


}
