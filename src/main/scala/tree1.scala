package scala99

/**
 * Binary agacı icin temel sinir
 * */
sealed abstract class Tree[+T] {
    def isSymmetric: Boolean
    def addValue[U >: T <% Ordered[U]](x: U): Tree[U]
    def leafCount: Int
    def leafList: List[T]
    def internalList: List[T]
    def atLevel(n: Int): List[T]
    def layoutBinaryTree: Tree[T]
    def layoutBinaryTree2: Tree[T]
    def layoutBinaryTree3: Tree[T]
    def preorder: List[T]
    def inorder: List[T]
}

/**
 * Nodeler icin sınıf tanımı
 * */
case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString = value.toString + "(" + left.toString + "," + right.toString + ")"

    //p55//////////////////////////////////////////////////////////////////
    def isSymmetric = {
        def isMirror(a: Tree[T], b: Tree[T]) = {
            def equal(x: Tree[T], y: Tree[T]): Boolean = {
                (x, y) match {
                    case (End, End) => true
                    case (xx: Node[T], yy: Node[T]) => equal(xx.left, yy.left) && equal(xx.right, yy.right)
                    case _ => false
                }
            }
            def crSmt(t: Tree[T]): Tree[T] = {
                t match {
                    case Node(v, lft, rght) => Node(v, crSmt(rght), crSmt(lft))
                    case _ => End
                }
            }

            equal(a, crSmt(b))
        }

        this match {
            case Node(v, lefft, right) => isMirror(left, right)
            case _ => false
        }
    }

    //p57/////////////////////////////////////////////////////////////////////////
    def addValue[U >: T <% Ordered[U]](x: U): Tree[U] = {
        if (x < this.value) Node(this.value, this.left.addValue(x), this.right)
        else if (x > this.value) Node(this.value, this.left, this.right.addValue(x))
        else this
    }

    //p61//////////////////////////////////////////////////////////////////////
    def leafCount = this match {
        case Node(v, End, End) => 1
        case Node(v, l, r) => l.leafCount + r.leafCount
        case _ => 0
    }

    //p61A///////////////////////////////////////////////////////////////
    def leafList = this match {
        case Node(v, End, End) => List(v)
        case Node(v, l, r) => l.leafList ::: r.leafList
        case _ => List()
    }

    //p62/////////////////////////////////////////////////////////////////
    def internalList = this match {
        case Node(v, End, End) => Nil
        case Node(v, l, r) => List(v) ::: l.internalList ::: r.internalList
        case _ => Nil
    }

    //62B////////////////////////////////////////////////////////////////////
    def atLevel(n: Int) = n match {
        case x if (x == 1) => List(this.value)
        case x if (x > 1) => this.left.atLevel(n - 1) ::: this.right.atLevel(n - 1)
        case _ => Nil
    }

    //p64//////////////////////////////////////////////////////////////////////////
    def layoutBinaryTree: Tree[T] = {
        def countNode(tr: Tree[T]): Int = tr match {
            case Node(v, l, r) => 1 + countNode(l) + countNode(r)
            case _ => 0
        }

        def creatTree(tr: Tree[T], leftCount: Int, seviye: Int): Tree[T] = tr match {
            case Node(v, l, r) => {
                val xx = countNode(l) + leftCount + 1
                new PositionedNode[T](v, creatTree(l, leftCount, seviye + 1), creatTree(r, xx, seviye + 1), xx, seviye)
            }
            case _ => End
        }

        creatTree(this, 0, 1)
    }

    //p65/////////////////////////////////////////////////////////////
    def layoutBinaryTree2: Tree[T] = {
        def derinlik(tr: Tree[T]): Int = tr match {
            case Node(v, l, r) => {
                val x = derinlik(l) + 1
                val y = derinlik(r) + 1
                if (x > y) x else y
            }
            case _ => 0
        }

        def ensol(tr: Tree[T]): Int = tr match {
            case Node(v, l, r) => ensol(l) + 1
            case _ => 0
        }

        val der = derinlik(this) - 1
        val uc = ensol(this) - 1
        val lst = (0 :: (for (i <- 0 until der) yield math.pow(2, i).toInt).toList).reverse
        val root = lst.take(uc).sum + 1

        def creatTree(tr: Tree[T], ref: Int, seviye: Int): Tree[T] = tr match {
            case Node(v, l, r) => {
                new PositionedNode[T](v, creatTree(l, ref - lst(seviye - 1), seviye + 1), creatTree(r, ref + lst(seviye - 1), seviye + 1), ref, seviye)
            }
            case _ => End
        }

        creatTree(this, root, 1)
    }

    //p66///////////////////////////////////////////////////////
    def layoutBinaryTree3: Tree[T] = {

        def derinlik(tr: Tree[T]): Int = tr match {
            case Node(v, l, r) => {
                val x = derinlik(l) + 1
                val y = derinlik(r) + 1
                if (x > y) x else y
            }
            case _ => 0
        }

        def atLevelOk(tr: Tree[T], n: Int): Boolean = {
            def atLevel1(tr1: Tree[T], n1: Int): List[PositionedNode[T]] = tr1 match {
                case t: PositionedNode[T] if (n1 == 1) => List(t)
                case t: PositionedNode[T] if (n1 > 1) => atLevel1(t.left, n1 - 1) ::: atLevel1(t.right, n1 - 1)
                case _ => Nil
            }
            def isOk(lst: List[PositionedNode[T]]): Boolean = lst match {
                case x1 :: x2 :: xs => (if (x1.x >= x2.x) false else true) && isOk(x2 :: xs)
                case _ => true
            }
            isOk(atLevel1(tr, n))
        }

        def redudant(tr: Tree[T], n: Int): Tree[T] = tr match {
            case End => tr
            case PositionedNode(v, l, r, x, y) => {
                val ll = l match {
                    case End => l
                    case PositionedNode(_, _, _, x1, _) => redudant(l, n - x + x1)
                }
                val rr = r match {
                    case End => r
                    case PositionedNode(_, _, _, x1, _) => redudant(r, n - x + x1)
                }
                PositionedNode(v, ll, rr, n, y)
            }
        }

        def setnode(tr: Tree[T], level: Int, n: Int): Tree[T] = {
            def setnode1(tr: Tree[T], accu: Int): Tree[T] = tr match {
                case End => tr
                case PositionedNode(v, l, r, x, y) if (accu == level) => {
                    val ll = l match {
                        case End => l
                        case _: PositionedNode[T] => redudant(l, x - n)
                    }
                    val rr = r match {
                        case End => r
                        case _: PositionedNode[T] => redudant(r, x + n)
                    }
                    PositionedNode(v, ll, rr, x, y)
                }
                case PositionedNode(v, l, r, x, y) => PositionedNode(v, setnode1(l, accu + 1), setnode1(r, accu + 1), x, y)
            }
            setnode1(tr, 1)
        }
        def atLevelAllOk(tr: Tree[T], level: Int) = (for (i <- level to derinlik(tr)) yield atLevelOk(tr, i)).foldRight(true)(_ && _)
        def creatTr(tr: Tree[T], level: Int, l: List[Int]): Tree[T] = {
            if (level == 0) tr
            else {
                val t = setnode(tr, level, l.head)
                if (atLevelAllOk(t, level)) creatTr(t, level - 1, l) else creatTr(tr, level, l.tail)
            }
        }
        def ensol(tr: Tree[T]): Int = tr match {
            case PositionedNode(_, End, _, x, _) => x
            case PositionedNode(_, l, _, _, _) => ensol(l)
        }
        val d = derinlik(this)
        val lst = (for (i <- 0 until d) yield math.pow(2, i).toInt).toList
        val tr1 = this.layoutBinaryTree2
        val tr2 = creatTr(tr1, d - 1, lst)
        redudant(tr2, tr2.asInstanceOf[PositionedNode[T]].x - ensol(tr2) + 1)
    }

    //p68///////////////////////////////////////////////////////////////////////
    def preorder: List[T] = this match {
        case Node(v, l, r) => List(v) ::: l.preorder ::: r.preorder
        case _ => Nil
    }

    //p68 devamı
    def inorder: List[T] = this match {
        case Node(v, l, r) => l.inorder ::: List(v) ::: r.inorder
        case _ => Nil
    }

}

/**
 * Agac veri yapısındaki bos dugumleri temsil eder
 * */
case object End extends Tree[Nothing] {
    override def toString = ""

    def isSymmetric = false

    def addValue[U <% Ordered[U]](x: U): Tree[U] = Node(x, End, End)

    def leafCount = 0

    def leafList = Nil

    def internalList = Nil

    def atLevel(n: Int) = Nil

    def layoutBinaryTree = End

    def layoutBinaryTree2 = End

    def layoutBinaryTree3 = End

    def preorder = Nil

    def inorder = Nil
}

/**
 * Nodelara poziyon bilgisi eklenmiş sinif yapısı
 * */
class PositionedNode[+T](override val value: T, override val left: Tree[T], override val right: Tree[T], val x: Int, val y: Int) extends Node[T](value, left, right) {
    override def toString = "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
}

/**
 * Usteki sinif icin companion  object
 * */
object PositionedNode {
    def unapply[T](obj: PositionedNode[T]): Option[(T, Tree[T], Tree[T], Int, Int)] = Some((obj.value, obj.left, obj.right, obj.x, obj.y))
    def apply[T](value: T, left: Tree[T], right: Tree[T], x: Int, y: Int) = new PositionedNode[T](value, left, right, x, y)
}

/**
 * Node sınıfı icin companion object
 * */
object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)

    //p55//////////////////////////////////////////////////
    def cBalanced[T](count: Int, value: T) = {

        def agac_node(nn: Int) = {
            def tam(nnn: Int): Stream[Int] = Stream.cons(nnn, tam(2 * nnn + 1))
            tam(1).takeWhile(_ <= nn).toList.last
        }

        def agac[T](v: T, lst: List[Int]): Tree[T] = lst match {
            case 0 :: 0 :: Nil => Node(v, End, End)
            case 0 :: 1 :: Nil => Node(v, End, Node(v, End, End))
            case 1 :: 0 :: Nil => Node(v, Node(v, End, End), End)
            case 1 :: 1 :: Nil => Node(v, Node(v, End, End), Node(v, End, End))
            case _ => Node(v, agac(v, lst.splitAt(lst.length / 2)._1), agac(v, lst.splitAt(lst.length / 2)._2))
        }

        def isDengeli(lst: List[Int]): Boolean = lst match {

            case Nil => false
            case a :: b :: Nil => true
            case _ => {
                val (lst1, lst2) = lst.splitAt(lst.length / 2)

                ((lst1.sum - lst2.sum).abs <= 1) && isDengeli(lst1) && isDengeli(lst2)
            }
        }
        val toplam_yaprak = agac_node(count) + 1
        val dolu_yaprak = count - agac_node(count)
        val bos_yaprak = toplam_yaprak - dolu_yaprak
        val lst = (for (i <- 1 to bos_yaprak) yield 0).toList ::: (for (i <- 1 to dolu_yaprak) yield 1).toList
        val lstt = lst.permutations.toList.filter(isDengeli(_))
        lstt.map(x => agac(value, x))

    }

    //p57//////////////////////////////////////////////////////////////////////////
    def fromList[T <% Ordered[T]](x: List[T]): Tree[T] = {
        def cr[T <% Ordered[T]](tr: Tree[T], lst: List[T]): Tree[T] = if (lst.length == 0) tr else cr(tr.addValue(lst.head), lst.tail)
        cr(End, x)
    }

    //p58////////////////////////////////////////////////////////////////////////////
    def symmetricBalancedTrees[T](n: Int, value: T): List[Tree[T]] = cBalanced(n, value).filter(_.isSymmetric)

    //p59//////////////////////////////////////////////////////////////////////////////
    def hbalTrees[T](count: Int, value: T) = {

        def agac[T](v: T, lst: List[Int]): Tree[T] = lst match {
            case 0 :: 0 :: Nil => Node(v, End, End)
            case 0 :: 1 :: Nil => Node(v, End, Node(v, End, End))
            case 1 :: 0 :: Nil => Node(v, Node(v, End, End), End)
            case 1 :: 1 :: Nil => Node(v, Node(v, End, End), Node(v, End, End))
            case _ => Node(v, agac(v, lst.splitAt(lst.length / 2)._1), agac(v, lst.splitAt(lst.length / 2)._2))
        }

        def perlist(one: Int, zero: Int) = (for (i <- 1 to one) yield 1).toList ::: (for (j <- 1 to zero) yield 0).toList
        val yaprak = math.pow(2, count - 1).toInt
        val lst = (for (i <- 1 to yaprak) yield perlist(i, yaprak - i)).toList.flatMap(_.permutations.toList)
        lst.map(x => agac(value, x))
    }

    //p60//////////////////////////////////////////////////////////////////////////////
    def hbalTreesWithNodes[T](count: Int, value: T) = {
        def agac_node(nn: Int) = {
            def tam(nnn: Int): Stream[Int] = Stream.cons(nnn, tam(2 * nnn + 1))
            tam(1).takeWhile(_ <= nn).toList.last
        }

        def agac[T](v: T, lst: List[Int]): Tree[T] = lst match {
            case 0 :: 0 :: Nil => Node(v, End, End)
            case 0 :: 1 :: Nil => Node(v, End, Node(v, End, End))
            case 1 :: 0 :: Nil => Node(v, Node(v, End, End), End)
            case 1 :: 1 :: Nil => Node(v, Node(v, End, End), Node(v, End, End))
            case _ => Node(v, agac(v, lst.splitAt(lst.length / 2)._1), agac(v, lst.splitAt(lst.length / 2)._2))
        }

        val toplam_yaprak = agac_node(count) + 1
        val dolu_yaprak = count - agac_node(count)
        val bos_yaprak = toplam_yaprak - dolu_yaprak
        val lst = (for (i <- 1 to bos_yaprak) yield 0).toList ::: (for (i <- 1 to dolu_yaprak) yield 1).toList
        lst.permutations.toList.map(x => agac(value, x))
    }

    //p63//////////////////////////////////////////////////////////////////////
    def completeBinaryTree[T](count: Int, value: T) = {
        def agac_node(nn: Int) = {
            def tam(nnn: Int): Stream[Int] = Stream.cons(nnn, tam(2 * nnn + 1))
            tam(1).takeWhile(_ <= nn).toList.last
        }

        def agac[T](v: T, lst: List[Int]): Tree[T] = lst match {
            case 0 :: 0 :: Nil => Node(v, End, End)
            case 0 :: 1 :: Nil => Node(v, End, Node(v, End, End))
            case 1 :: 0 :: Nil => Node(v, Node(v, End, End), End)
            case 1 :: 1 :: Nil => Node(v, Node(v, End, End), Node(v, End, End))
            case _ => Node(v, agac(v, lst.splitAt(lst.length / 2)._1), agac(v, lst.splitAt(lst.length / 2)._2))
        }

        val toplam_yaprak = agac_node(count) + 1
        val dolu_yaprak = count - agac_node(count)
        val bos_yaprak = toplam_yaprak - dolu_yaprak
        val lst = (for (i <- 1 to dolu_yaprak) yield 1).toList ::: (for (i <- 1 to bos_yaprak) yield 0).toList
        agac(value, lst)
    }

    //67//////////////////////////////////////////////////////////////////////
    def string2Tree(str: String): Tree[Char] = {
        def divTwo(lst: List[Char]) = {
            def divTwo1(ls: List[Char], accu1: List[Char], accu2: Int): (List[Char], List[Char]) = {
                ls.head match {
                    case ',' => if (accu2 == 0) (accu1, ls.tail)
                    else divTwo1(ls.tail, accu1 ::: List(','), accu2)
                    case '(' => divTwo1(ls.tail, accu1 ::: List('('), accu2 + 1)
                    case ')' => divTwo1(ls.tail, accu1 ::: List(')'), accu2 - 1)
                    case c => divTwo1(ls.tail, accu1 ::: List(c), accu2)
                }
            }
            divTwo1(lst.tail.init, Nil, 0)
        }

        def fill(lst: List[Char], ch: Char): List[Char] = lst match {
            case x1 :: x2 :: xs => {
                val s = x1.toString + x2.toString
                if (s == "(,") '(' :: ch :: ',' :: fill(xs, ch)
                else if (s == ",)") ',' :: ch :: ')' :: fill(xs, ch)
                else x1 :: fill(x2 :: xs, ch)
            }
            case _ => lst
        }

        def makeTree(lst: List[Char], ch: Char): Tree[Char] = lst match {
            case x :: Nil => if (x == ch) End else Node(x, End, End)
            case _ => {
                val v = lst.head
                val (l, r) = divTwo(lst.tail)
                Node(v, makeTree(l, ch), makeTree(r, ch))
            }
        }

        makeTree(fill(str.toList, '.'), '.')
    }

    //p68////////////////////////////////////////////////////////////////
    def preInTree(pre: List[Char], in: List[Char]): Tree[Char] = {
        def part2(l: List[Char], c: Char): (List[Char], List[Char]) = l match {
            case x :: xs => if (x == c) (Nil, xs)
            else {
                val (r1, r2) = part2(xs, c)
                (x :: r1, r2)
            }
            case Nil => (Nil, Nil)
        }

        (pre, in) match {
            case (Nil, Nil) => End
            case (p, i) => {
                val (r1, r2) = part2(i, p.head)
                val (s1, s2) = (p.tail.take(r1.length), p.tail.drop(r1.length))
                Node(p.head, preInTree(s1, r1), preInTree(s2, r2))
            }
        }
    }

    //p69//////////////////////////////////////////////////////////////////////////////
    def fromString(str: String):Tree[Char] = {
        def fromString1(s: List[Char]): (Tree[Char], List[Char]) =
            s.head match {
                case '.' => (End, s.tail)
                case c => {
                    val(t1, s1) = fromString1(s.tail)
                    val(t2, s2) = fromString1(s1)
                    (Node(c, t1, t2), s2)
                }
            }

        fromString1(str.toList)._1
    }

}

/**
 * Test icin prog
 * */
object Prog {
    def main(args: Array[String]) {
        //println(Node.cBalanced(3,"x"))
        //val tr = End.addValue(2)
        //val tr1 = tr.addValue(3)
        //val tr2 = tr1.addValue(0)

        //println(tr2)
        //println(Node.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric)
        //println(Node.fromList(List(3, 2, 5, 7, 4)).isSymmetric)
        //Node.symmetricBalancedTrees(5, "x").foreach(println _ )
        //println(Node.hbalTrees(3, "x"))
        //println(Node('x', Node('x'), End).leafCount)
        //println( Node('a', Node('b'), Node('c', Node('d'), Node('e'))).leafList)
        //println( Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList)
        //println(Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(2))
        //println(Node.hbalTreesWithNodes(4, "x"))
        //println(Node.completeBinaryTree(6, "x"))
        println(Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree)
        println(Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree2)
        println(Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree3)
        println(Node.preInTree(List('a', 'b', 'd', 'e', 'c', 'f', 'g'), List('d', 'b', 'e', 'a', 'c', 'g', 'f')))
        val str = "afg^^c^bd^e^^^"
        val mt = MTree.string2MTree(str)
        //println(mt)
        println(MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).toString)
        println(MTree("a", List(MTree("b", List(MTree("c"))))).lispyTree)
        println(MTree('a', List(MTree('f'))).nodeCount)
        println(MTree.fromLispyString("(a (f g) c (b d e))").lispyTree)
        println(MTree.string2MTree("afg^^c^bd^e^^").postorder)
        println(MTree.string2MTree("afg^^c^bd^e^^").internalPathLength)
        val grph =  Graph.fromString("[b-c, b-f, b-d, c-d, c-f, f-d, d-e, g]")
        val grph1 = Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]")
        println(grph.toTermForm)
        println(grph1.toAdjacentForm)
        println(grph1.toString)
        println(Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").toTermForm)
        println(Digraph.fromString("[s>r, t, u>r, s>u, u>s, v>u]").toAdjacentForm)
        println(Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").toAdjacentForm)
        println(grph.toTermForm)
        val paths = grph.findPaths("b", "f")
        println(paths)
        println(grph.findCycles("b"))
        println(Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").findCycles("f"))
        val paths1 = Digraph.fromString("[s>r, t, u>r, s>u, u>s, v>u]").findPaths("s", "r")
        println(grph.findCycles("g"))
        //println(paths1)
        Graph.term(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
            List(('a', 'b'), ('a', 'd'), ('b', 'c'), ('b', 'e'),
                ('c', 'e'), ('d', 'e'), ('d', 'f'), ('d', 'g'),
                ('e', 'h'), ('f', 'g'), ('g', 'h')))
        println(Graph.fromString("[a-b, b-c, a-c]").spanningTrees)
        println(Graph.fromStringLabel("[a-b/1, b-c/2, a-c/3]").minimalSpanningTree)
        val grrr = Graph.termLabel(
            List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
            List(('a', 'b', 5), ('a', 'd', 3), ('b', 'c', 2), ('b', 'e', 4),
                ('c', 'e', 6), ('d', 'e', 7), ('d', 'f', 4), ('d', 'g', 3),
                ('e', 'h', 5), ('f', 'g', 4), ('g', 'h', 1)))
        //println(grrr.minimalSpanningTree)
        println("color seysi")
        println(Graph.fromString("[a-b, b-c, a-c, a-d]").nodesByDegree)
        println(Graph.fromString("[a-b, b-c, a-c, a-d]").colorNodes)
        println(grrr.colorNodes)
        //println(Graph.fromString("[a-b, b-c, e, a-c, a-d]").nodesByDepthFrom("d"))
        //println(grrr.nodesByDepthFrom('a'))
        //println(Graph.fromString("[a-b, c]").splitGraph)//tostring gozden gecirilmeli
        println(Digraph.fromString("[a>b, c>a, d>b]").isBipartite) // yonlu icin dogru calismiyor
        println(Digraph.fromString("[a-b, c-a, d-b]").isBipartite)
        println(Graph.fromString("[a-b, b-c, c-a]").isBipartite)
        println(Graph.fromString("[a-b, b-c, d]").isBipartite)
        println(Graph.fromString("[a-b, b-c, d, e-f, f-g, g-e, h]").isBipartite)
        println("izomorfik test")
        println(Graph.fromString("[a-b]").isIsomorphicTo(Graph.fromString("[5-7]")))
        println("felan filan")

    }
}
