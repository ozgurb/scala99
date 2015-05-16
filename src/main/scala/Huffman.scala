/**
 * Created by josef on 16.05.2015.
 */

//p50
object Huffman {
    def huffman[T](list: List[(T, Int)]): List[(T, String)] = {

        def setMap(mmp: Map[Set[T], (Int, String)], e: Set[T], n: Int) = {
            mmp.map(x => if((x._1 diff e).size == 0) (x._1, (x._2._1, n.toString + x._2._2))
            else x)
        }
        def huffman1(mp: Map[Set[T], (Int, String)],
                     lst: List[(Set[T], Int)]): Map[Set[T], (Int, String)] = {
            if(lst.length < 2) mp
            else {
                val (x1 :: x2 :: xs) = lst
                val mp1 = setMap(mp, x1._1, 0)
                val mp2 = setMap(mp1, x2._1, 1)
                val ee = (x1._1 ++ x2._1, (x1._2 + x2._2, ""))
                val mp3 = mp2 + ee
                val ll = (x1._1 ++ x2._1, x1._2 + x2._2) :: xs
                val lll = ll.sortWith( _._2 < _._2)
                huffman1(mp3, lll)
            }
        }

        val lst1 = list.sortWith((x1, x2) => x1._2 < x2._2)
        val lst2 = lst1.map(x => (Set(x._1), x._2))
        val mp = (for(i <- lst2) yield (i._1, (i._2, ""))).toMap
        val code1 = huffman1(mp, lst2)
        val code2 = code1.filter(x => x._1.size == 1)
        code2.map(x => (x._1.head, x._2._2)).toList

    }

    val lst = List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5))
    val s = huffman(lst)

}
