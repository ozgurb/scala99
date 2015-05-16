/**
 * Created by ozgur on 15.12.2014.
 */
package scala99
/**
 *Çoklu agac
 * */
class MTree[+T](val value: T, val children: List[MTree[T]]) {
    def this(value: T) = this(value, List())
    //override def toString = value.toString + children.map(_.toString + "^").mkString("")
    //overrpaide def toString = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"

    //p70
    override def toString = {
        def toString1(lt: List[MTree[T]]): String = if(lt.isEmpty) ""
        else toString2(lt.head) + "^" + toString1(lt.tail)
        def toString2(tr: MTree[T]): String = tr.value.toString + toString1(tr.children)
        toString2(this)
    }

    //p73
    def lispyTree: String = {
        if(children == Nil) value.toString
        else "(" + value.toString + " " + children.map( _.lispyTree).mkString(" ") + ")"
    }

    //p70C
    def nodeCount: Int = if (children == Nil) 1 else 1 + children.map( _.nodeCount).sum

    //p72
    def postorder: List[T] = if(children == Nil) List(value) else children.flatMap(x => x.postorder) ::: List(value)

    //p71
    def internalPathLength: Int = {
        def length1(mt: MTree[T], n: Int): Int = {
            if(mt.children == Nil) n
            else n + mt.children.map(x => length1(x, n + 1)).sum
        }

        length1(this, 0)
    }

}
/**
 * Çoklu ağac sınıfı içi companion object
 * */
object MTree {
    def apply[T](value: T) = new MTree(value, List())
    def apply[T](value: T, children: List[MTree[T]]) = new MTree(value, children)

    //p70
    def string2MTree(s: String): MTree[Char] = {

        // İlk karektere baglı stinglerin sayısı
        def listString(st: String): List[String] = {
            def makeList(str1: String, accu: String, level: Int): List[String] = {
                if (str1.isEmpty) List(accu)
                else if (level == 0 && accu != "") accu :: makeList(str1, "", 0)
                else str1.head match {
                    case '^' => makeList(str1.tail.toString, accu + '^'.toString, level - 1)
                    case c => makeList(str1.tail.toString, accu + c.toString, level + 1)
                }
            }
            def delEnd(str1: String): String = str1.last match {
                case '^' => delEnd(str1.init)
                case _ => str1
            }
            val lst = makeList(st.tail, "", 0)
            val lst1 = if(lst.last == "^") lst.init else lst
            lst1.map(delEnd( _ ))
        }

        if(s.length == 1) MTree(s.head)
        else MTree(s.head, listString(s).map(x => string2MTree(x)))
    }

    //p73
    def fromLispyString(s: String): MTree[String] = {
        def fromString1(str: String, accu1: List[String], accu2: String, n: Int): List[String] = {
            if(str.isEmpty) accu1 ::: List(accu2)
            else str.head match {
                case '(' => fromString1(str.tail, accu1, accu2 + "(", n - 1)
                case ')' => fromString1(str.tail, accu1, accu2 + ")", n + 1)
                case ' ' => if(n == -1) fromString1(str.tail, accu1 ::: List(accu2), "", n)
                else fromString1(str.tail, accu1, accu2 + " ", n)
                case c => fromString1(str.tail, accu1, accu2 + c.toString, n)
            }
        }

        def parse(str: String): List[String] = {
            if (str.head == '(') fromString1(str.tail.init, Nil, "", -1)
            else List(str)
        }

        def creat1(str: String): MTree[String] = {
            val lst = parse(str)
            if(lst.length == 1) MTree(lst.head, Nil)
            else MTree(lst.head, lst.tail.map(x => creat1(x)))
        }

        creat1(s)
    }
}


