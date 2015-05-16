/**
 * Created by josef on 16.05.2015.
 */
object VonKoch {


    val lst1 = List(Set("a", "g"), Set("a", "b"), Set("a", "i"),  Set("a", "h"),
        Set("a", "c"), Set("c", "f"), Set("c", "d"), Set("d", "k"), Set("c", "e"),
        Set("e", "q"), Set("q", "m"), Set("q", "n"), Set("n", "p"))




    def solutions(glst: List[Set[String]]): List[(String, Int)] = {

        def hamleler(glst: List[Set[(String, Int)]]) = {
            val mlst = glst.map(x => List(x.head._2, x.last._2)).flatten
            val flst = mlst.filter(x => x > 0).toSet.toList
            (1 to glst.length + 1).toList diff flst

        }
        def isOk(glst: List[Set[(String, Int)]]) = {
            val nlst = (1 to glst.length + 1).toList
            val flst = glst.filter(x => x.head._2 > 0 && x.last._2 > 0)
            val mlst = flst.map(x => math.abs(x.head._2 - x.last._2))
            val dlst = mlst diff nlst
            if(dlst.isEmpty) true else false
        }
        def setNumber(glst: List[Set[(String, Int)]], str: String, n: Int) = {
            glst.map(x => x.map(y => if(y._1 == str) (str, n) else y))
        }
        def solutions1(lst_str: List[String], accu: List[Set[(String, Int)]]): List[Set[(String, Int)]] = {
            def nloop(nlst: List[Int]): List[Set[(String, Int)]] = {
                if(nlst.isEmpty) Nil
                else {
                    val accu1 = setNumber(accu, lst_str.head, nlst.head)
                    if(isOk(accu1)) {
                        val accu2 = solutions1(lst_str.tail, accu1)
                        if(accu2 == Nil) nloop(nlst.tail) else accu2
                    }
                    else Nil
                }
            }
            if(lst_str.isEmpty) accu
            else {
                val hlst = hamleler(accu)
                nloop(hlst)
            }
        }

        val mlst = glst.map(x => x.map(y => (y, 0)))
        val slst = glst.map(x => List(x.head, x.last)).flatten.toSet.toList
        val sl = solutions1(slst, mlst)
        sl.map(x => List(x.head, x.last)).flatten.toSet.toList

    }

}
