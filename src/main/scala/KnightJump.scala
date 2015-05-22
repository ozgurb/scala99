

/**
 * Created by ozgurb on 16.05.2015.
 */

//p91
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


    def tours(nn: Int, x: Int, y: Int) = {
        def coz(n: Int, accu: List[Point]) : List[Point] = {
            def nloop(lst: List[Point]): List[Point]  = {
                if(lst.isEmpty) Nil
                else {
                    val t = coz(n, lst.head :: accu)
                    if(t.length == n * n) t
                    else nloop(lst.tail)
                }
            }
            if(accu.length == n * n) accu
            else {

                val h = hamle(accu.head, n)
                val gecerli = h diff accu
                nloop(gecerli)
            }
        }

        require(x < nn + 1 && x > 0)
        require(y < nn + 1 && y > 0)
        coz(nn, List(Point(x, y))).reverse
    }

    def closedtours(nn: Int, x: Int, y: Int) = {
        def coz(n: Int, accu: List[Point], start: Point) : List[Point] = {
            def nloop(lst: List[Point]): List[Point]  = {
                if(lst.isEmpty) Nil
                else {
                    val t = coz(n, lst.head :: accu, start)
                    if(t.length == n * n) t
                    else nloop(lst.tail)
                }
            }
            if(accu.length == n * n) accu
            else {

                val h = hamle(accu.head, n)
                val gecerli = h diff accu
                val s = hamle(start, n)
                if((s diff accu) == Nil) Nil
                else nloop(gecerli)
            }
        }
        require(x < nn + 1 && x > 0)
        require(y < nn + 1 && y > 0)
        coz(nn, List(Point(x, y)), Point(x, y)).reverse
    }


}
