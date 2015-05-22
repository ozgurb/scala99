import org.scalatest.FunSuite

/**
 * Created by josef on 22.05.2015.
 */
class FullWordTest extends FunSuite {
    test("fullWord icin çözüm test ediliyor"){
        val coz = FullWord.fullWord(175)
        coz === "one-seven-five"
    }

}
