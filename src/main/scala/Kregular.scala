/**
 * Created by ozgurb on 16.05.2015.
 */

//p94
object Kregular {
    def neighbors(n: Int, lst_edge: List[Set[Int]]) = {
        val neigh_edge = lst_edge.filter(x => x.contains(n))
        neigh_edge.map(x => if(x.head == n) x.last else x.head)

    }

    def isIsomorphic(glst1: List[Set[Int]], glst2: List[Set[Int]]) = {
        def isOk(lst1: List[Int], lst2: List[Int]) = {
            val mmap = (lst1 zip lst2).toMap
            def isright1(lst: List[Int]): Boolean = {
                if(lst.isEmpty) true
                else {
                    val s1 = neighbors(lst.head, glst1).toSet
                    val s2 = s1.map(x => mmap(x))
                    val s3 = neighbors(mmap(lst.head), glst2).toSet
                    if(s2 == s3) isright1(lst.tail) else false
                }
            }
            isright1(lst1)
        }

        val n1 = glst1.toSet.flatten.toList
        val n2 = glst2.toSet.flatten.toList
        val p = n2.permutations
        p.exists(x => isOk(n1, x))
    }

    def isRegular(n: Int, glst: List[Set[Int]]) = {
        val node = glst.toSet.flatten.toList
        def isRegular1(lst: List[Int]): Boolean = {
            if(lst.isEmpty) true
            else {
                if(neighbors(lst.head, glst).length == n) isRegular1(lst.tail) else false
            }
        }
        isRegular1(node)
    }

    def newFilter[T](lst: List[T])(f: (T, T) => Boolean) = {
        def newFilter1(llst: List[T], accu: List[T]): List[T] = {
            if(llst.isEmpty) accu
            else {
                val filter_lst = llst.filterNot(x => f(llst.head, x))
                newFilter1(filter_lst, llst.head :: accu)
            }
        }
        newFilter1(lst, Nil)
    }

    def solution(n: Int, k: Int) = {
        val lst_node = (1 to n).toList
        val lst_edge = lst_node.combinations(2).toList.map(x => x.toSet)
        val kk = (n * k) / 2
        val lst_regular = lst_edge.combinations(kk).filter(x => isRegular(k ,x)).toList
        val  lst_nonIsomorphic = newFilter(lst_regular)((x1, x2) => isIsomorphic(x1, x2))
        lst_nonIsomorphic

    }

    val coz = solution(6, 3)

}
