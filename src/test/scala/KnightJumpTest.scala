import org.scalatest.FunSuite

/**
 * Created by josef on 28.05.2015.
 */
class KnightJumpTest extends FunSuite {
    def isValid(p1: KnightJump.Point, p2: KnightJump.Point, n: Int): Boolean = {
        KnightJump.hamle(p1, n).exists(x => x == p2)
    }

    def allPoint(n: Int) = {
        (for {
            i <- 1 to n
            j <- 1 to n
        } yield KnightJump.Point(i, j)).toList
    }

    test("KnightJomp icin tour test ediliyor."){
        val allpoint = allPoint(7)
        val coz = KnightJump.tour(7, 1, 1)
        assert((coz diff allpoint) === Nil)
        val ccoz = ! coz.sliding(2).exists(x => ! isValid(x.head, x.last, 7))
        assert(ccoz)
    }

    test("KnightJomp icin closedtour test ediliyor."){
        val coz1 = KnightJump.closedtour(6, 1, 1)
        assert(isValid(coz1.head, coz1.last, 6))
        assert((allPoint(6) diff coz1) === Nil)
    }

}
