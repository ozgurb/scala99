/**
 * Created by ozgurb on 16.05.2015.
 */

//p93
object ArithPuzzle {
    def solution(lst_int: List[Int]): List[String] = {
        val idlst = lst_int.toSet.toList
        val m = idlst.map { x =>
            val n = x
            val dlst = lst_int diff List(x)
            val slst = solution1(n, dlst, Nil)
            if(slst.isEmpty) ""
            else {
                val m = toString1(slst, dlst, Map())
                m(n) + " = " + n.toString
            }
        }
        m.filter(x => x != "")
    }

    def toString1(lst1: List[(Int, Int, Int)], lst2: List[Int],
                  mp: Map[Int, String]): Map[Int, String] = {

        def op1(nn1: Int, nn2: Int, op: Int): Int = {
            if(op == 1) nn1 + nn2
            else if(op == 2) nn1 - nn2
            else if(op == 3) nn1 * nn2
            else nn1 / nn2
        }

        if(lst1.isEmpty) mp
        else {
            val n1 = lst1.head._2
            val n2 = lst1.head._3
            val op = lst1.head._1
            val rop = op1(n1, n2, op)

            val sn1 = if(lst2.contains(n1)) n1.toString else mp(n1)
            val sn2 = if(lst2.contains(n2)) n2.toString else mp(n2)
            val sop = if(op == 1) "+"
            else if(op == 2) "-"
            else if(op == 3) "*"
            else "/"

            val llst2 = if(lst2.contains(n1)) lst2 diff List(n1) else lst2
            val lllst2 = if(llst2.contains(n2)) llst2 diff List(n2) else llst2

            val mmp = if(lst2.contains(n1) && lst2.contains(n2)) mp + ((rop, "(" + n1.toString + sop + n2.toString + ")"))
            else if(lst2.contains(n1)) (mp - n2) + ((rop, "(" + n1.toString + sop  + mp(n2) + ")"))
            else if(lst2.contains(n2)) (mp - n1) + ((rop, "(" + mp(n1)  + sop  + n2.toString + ")"))
            else  ((mp - n1) - n2) + ((rop, "(" + mp(n1) + sop + mp(n2) + ")"))

            toString1(lst1.tail, lllst2, mmp)

        }
    }




    def solution1(n: Int, lst1: List[Int],
                  accu: List[(Int, Int, Int)]): List[(Int, Int, Int)] = {

        def nloop(lst2: List[List[Int]]): List[(Int, Int, Int)] = {
            if(lst2.isEmpty) Nil
            else {
                val llst = lst1 diff lst2.head

                val top = lst2.head.head + lst2.head.last
                val cik1 = lst2.head.head - lst2.head.last
                val cik2 = lst2.head.last - lst2.head.head
                val carp = lst2.head.head * lst2.head.last
                val bol1 = if(lst2.head.last == 0 ) 0 else lst2.head.head / lst2.head.last
                val bol2 = if(lst2.head.head == 0 ) 0 else lst2.head.last / lst2.head.head
                val kosul1 = if(lst2.head.last != 0 && lst2.head.head % lst2.head.last == 0) true
                else false
                val kosul2 = if(lst2.head.head != 0 && lst2.head.last % lst2.head.head == 0) true
                else false

                val str_top =  (1, lst2.head.head, lst2.head.last)
                val str_cik1 = (2, lst2.head.head, lst2.head.last)
                val str_cik2 = (2, lst2.head.last, lst2.head.head)
                val str_carp = (3, lst2.head.head, lst2.head.last)
                val str_bol1 = (4, lst2.head.head, lst2.head.last)
                val str_bol2 = (4, lst2.head.last, lst2.head.head)

                if(llst.isEmpty){
                    if(top == n ) accu ::: List(str_top)
                    else if(cik1 == n) accu ::: List(str_cik1)
                    else if(cik2 == n) accu ::: List(str_cik2)
                    else if(carp == n) accu ::: List(str_carp)
                    else if(kosul1 && bol1 == n) accu ::: List(str_bol1) //bölünebilme duruumunu kontrol etmek laızm
                    else if(kosul2 && bol2 == n) accu ::: List(str_bol2) // burda da aynı şey geçerli
                    else Nil

                }
                else {
                    lazy val r1 = solution1(n, top :: llst, accu ::: List(str_top))
                    lazy val r2 = solution1(n, cik1 :: llst, accu ::: List(str_cik1))
                    lazy val r3 = solution1(n, cik2 :: llst, accu ::: List(str_cik2))
                    lazy val r4 = solution1(n, carp :: llst, accu ::: List(str_carp))
                    lazy val r5 = solution1(n, bol1 :: llst, accu ::: List(str_bol1))
                    lazy val r6 = solution1(n, bol2 :: llst, accu ::: List(str_bol2))


                    if(r1 != Nil) r1
                    else if(r2 != Nil) r2
                    else if(r3 != Nil) r3
                    else if(r4 != Nil) r4
                    else if(kosul1 && r5 != Nil) r5
                    else if(kosul2 && r6 != Nil) r6
                    else nloop(lst2.tail)

                }

            }
        }
        if(lst1.isEmpty) accu
        else {
            val clst = lst1.combinations(2) // iterator döner ya toList yada type iteratörlü seçilmeli
            nloop(clst.toList)
        }
    }


}
