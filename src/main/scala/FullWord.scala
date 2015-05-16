/**
 * Created by josef on 16.05.2015.
 */
object FullWord {
    def fullWord(n: Int) : String = {
        def toStr(nn: Int): String = nn match {
            case 1 => "one"
            case 2 => "two"
            case 3 => "three"
            case 4 => "four"
            case 5 => "five"
            case 6 => "six"
            case 7 => "seven"
            case 8 => "eight"
            case 9 => "nine"
            case 0 => "zero"
            case _ => ""
        }

        def rakamlar(nn: Int): List[Int] = {
            if(nn < 10) List(nn)
            else {
                val kalan = nn % 10
                val yeni = (nn - kalan) / 10
                rakamlar(yeni) ::: List(kalan)
            }
        }

        val rr = rakamlar(n)
        rr.map(x => toStr(x)).mkString("-")
    }


}
