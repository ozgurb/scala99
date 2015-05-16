package scala99

abstract class GraphBase[T, U] {
    case class Edge(n1: Node, n2: Node, value: U) {
        def toTuple = (n1.value, n2.value, value)
    }
    case class Node(value: T) {
        var adj: List[Edge] = Nil
        // neighbors are all nodes adjacent to this node.
        def neighbors: List[Node] = adj.map(edgeTarget(_, this).get)
        def degree = adj.length
    }

    var nodes: Map[T, Node] = Map()
    var edges: List[Edge] = Nil

    // If the edge E connects N to another node, returns the other node,
    // otherwise returns None.
    def edgeTarget(e: Edge, n: Node): Option[Node]

    override def equals(o: Any) = o match {
        case g: GraphBase[_,_] => ((nodes.keys.toList diff g.nodes.keys.toList) == Nil) &&
          ((edges.map(_.toTuple) diff g.edges.map(_.toTuple)) == Nil)
        case _ => false
    }
    def addNode(value: T) = {
        val n = new Node(value)
        nodes = Map(value -> n) ++ nodes
        n
    }

    def toTermForm = (nodes.keySet.toList, edges.map( _.toTuple))

    def toAdjacentForm  = {
        val lst = nodes.values.toSet.toList
        lst.map( x => (x.value, x.adj.map( y => (this.edgeTarget(y, x).get.value, y.value ))) )
    }

    //p81
    def findPaths(v: T, d: T) = {

        def findPath1(vv: T, accu1: List[T], accu2: List[List[T]]): List[List[T]] = {
            val n = nodes(vv).neighbors.map(_.value)
            if(vv == d)  accu1 :: accu2
            else n.flatMap { x =>
                if(accu1.contains(x)) accu2
                else findPath1(x, accu1 ::: List(x), accu2)
            }
        }
        findPath1(v, List(v), Nil)
    }

    //p82
    def findCycles(n: T): List[List[T]] = {
        val ngh = nodes(n).neighbors.map(_.value)
        val pth = ngh.map(x => (x, findPaths(n, x).filterNot( _ == List(n, x))))
        pth.flatMap { x =>
            if(nodes(x._1).neighbors.map(_.value).contains(n)) // filter ile yap
                x._2.map( y => y ::: List(n))
            else Nil
        }
    }

    //p83
    def isConnected = ! nodes.keys.toList.tail.exists( x => findPaths(nodes.keys.toList.head, x) == Nil)
    def isTree =  ! nodes.keys.toList.exists(x => findCycles(x) != Nil)
    def spanningTrees = {
        val t = this.toTermForm
        val n = t._1
        val e = t._2
        if(n.length - 1 > e.length) Nil
        else {
            val com = e.combinations(n.length - 1).toList
            val sp1 = com.map( x => Graph.termLabel(n, x))
            val sp2 = sp1.filter( x => x.isConnected && x.isTree )
            sp2
        }
    }
    //p84 - ordered kısmı halledilmeli.....
    def minimalSpanningTree= {

        def prim1(accu1: List[Node], accu2: List[Edge]): List[Edge] = {
            if(accu2.length == nodes.size - 1) accu2
            else {
                val all_adj = (accu1 flatMap { n =>
                    val ee = n.adj.filter( x => ! accu1.contains(edgeTarget(x, n).get))
                    ee.map(x => (x, edgeTarget(x, n).get, x.value))
                }).sortWith((x1, x2) => x1._3.toString.toDouble < x2._3.toString.toDouble
                                      /* Ordered[U].compare(< x2._3)*/)
                prim1(all_adj.head._2 :: accu1, all_adj.head._1 :: accu2)
            }
        }
        val edgs = prim1(List(nodes.values.head), Nil).map(x => (x.n1.value, x.n2.value, x.value ))
        val nds = nodes.keys.toList
        Graph.termLabel(nds, edgs)
    }

    def isIsomorphicTo[R, S](g: Graph[R, S]) = {
        /* etkisiz kılanan versioyondaki hata duzeltilmesi yarar saglar
        def eqEdge(e1: Edge, e2: Edge) = (e1, e2) match {
            case (Edge(n1, n2, v1), Edge(nn1, nn2, vv)) =>  if(n1.adj.length == nn1.adj.length && n2.adj.length == nn2.adj.length)
                true
                else if(n1.adj.length == nn2.adj.length && n2.adj.length == nn1.adj.length)
                true
                else
                false
            case _ => false
        }
        val list_edges = g.edges
        (for(i <- list_edges) yield {
            list_edges.filter(x => eqEdge(x, i)).length == edges.filter(y => eqEdge(y, i)).length
        }).reduce( _ && _ )*/

        def isOk(v1: Vector[T], v2: Vector[R]) = {
            val mmap = (v1 zip v2) toMap
            def isright1(vv: Vector[T]): Boolean = {
                if(vv.isEmpty) true
                else {
                    val s1 = nodes(vv.head).neighbors.toSet
                    val s2 = s1.map(x => mmap(x.value))
                    val s3 = g.nodes(mmap(vv.head)).neighbors.map(_.value).toSet
                    if(s2 == s3) isright1(vv.tail) else false
                }
            }
            isright1(v1)
        }
        val p = g.nodes.keys.toVector.permutations
        val nd = nodes.keys.toVector
        p.exists(x => isOk(nd, x))
    }
    //p86
    def nodesByDegree = nodes.values.toList.sortWith( _.degree > _.degree)
    //p86
    def colorNodes = {
        def setColor(n: Int, lst: List[(Node, Int, List[Int])]): List[(Node, Int, List[Int])] = lst match {
            case x :: xs => if(x._3.contains(n) || x._2 > 0) x :: setColor(n, xs)
                else {
                    val aa = x._1.neighbors
                    val xss = xs.map(y => if(aa.contains(y._1)) (y._1, y._2, n :: y._3) else y)
                    (x._1, n, x._3) :: setColor(n, xss)
                }
            case Nil => Nil
        }

        def isOk(accu: List[(Node, Int, List[Int])]) = ! accu.exists(x => x._2 == 0)

        def colorNodes1(n: Int, accu: List[(Node, Int, List[Int])]): List[(Node, Int, List[Int])] = {
            val temp = setColor(n, accu)
            if(isOk(temp)) temp
            else colorNodes1(n + 1, temp)
        }

        val nn = this.nodesByDegree.map(x => (x, 0, Nil))
        colorNodes1(1, nn).map(x => (x._1, x._2))
    }
    //p87
    def nodesByDepthFrom(n: T): List[T] = {

        def nodesDepth(nn: Node, accu: List[Node]): List[Node] = {
            if(accu.contains(nn)) accu
            else {
                val ll = nn.neighbors
                nloop(ll, nn :: accu)
            }
        }
        def nloop(lst: List[Node], accu: List[Node]): List[Node] = {
            if(lst.isEmpty) accu
            else {
                val dd = nodesDepth(lst.head, accu)
                nloop(lst.tail, dd)
            }
        }

        nodesDepth(nodes(n), Nil).map( _.value)
    }
    //p88
    def   splitGraph = {
        def splitNode(lst: List[Node]): List[List[Node]]= {
            if(lst.isEmpty) Nil
            else {
                val nn = nodesByDepthFrom(lst.head.value).map(x => nodes(x))
                val dd = lst diff nn
                nn :: splitNode(dd)
            }
        }

        val nn = nodes.values.toList
        val ll = splitNode(nn)
        val lll = ll.map(x => (x.map( _.value),
          edges.filter(y => x.contains(y.n1)).map(z => (z.n1.value, z.n2.value, z.value))))
        lll.map(x => Graph.termLabel(x._1, x._2))

    }

    //p89
    def isBipartite = {
        //ayrik node icin de bir if kolu düşünülmeli
        ! colorNodes.exists(x => x._2 > 2)
    }

}

class Graph[T, U] extends GraphBase[T, U] {
    override def equals(o: Any) = o match {
        case g: Graph[_,_] => super.equals(g)
        case _ => false
    }

    def edgeTarget(e: Edge, n: Node): Option[Node] =
        if (e.n1 == n) Some(e.n2)
        else if (e.n2 == n) Some(e.n1)
        else None

    def addEdge(n1: T, n2: T, value: U) = {
        val e = new Edge(nodes(n1), nodes(n2), value)
        edges = e :: edges
        nodes(n1).adj = e :: nodes(n1).adj
        nodes(n2).adj = e :: nodes(n2).adj
    }


    override def toString = {
        val ee = edges.map(x => x.n1 :: x.n2 :: Nil).flatten.toSet
        val nn = nodes.values.toSet
        val dd = nn diff ee
        val s1 = edges.map( x => {
            val r1 = x.n1.value.toString + "-" + x.n2.value.toString
            val r2 = if(x.value.isInstanceOf[Unit]) "" else "/" + x.value.toString
            r1 + r2
        }).mkString(", ")
        val s2 = dd.map( _.value.toString).mkString(", ")
        if(s2 == "") "[" + s1 + "]" else  "[" + s1 + ", " + s2 + "]"
    }
}

class Digraph[T, U] extends GraphBase[T, U] {
    override def equals(o: Any) = o match {
        case g: Digraph[_,_] => super.equals(g)
        case _ => false
    }

    def edgeTarget(e: Edge, n: Node): Option[Node] =
        if (e.n1 == n) Some(e.n2)
        else None

    def addArc(source: T, dest: T, value: U) = {
        val e = new Edge(nodes(source), nodes(dest), value)
        edges = e :: edges
        nodes(source).adj = e :: nodes(source).adj
    }

    override def toString = {
        val ee = edges.map(x => x.n1 :: x.n2 :: Nil).flatten.toSet
        val nn = nodes.values.toSet
        val dd = nn diff ee
        val s1 = edges.map( x => {
            val r1 = x.n1.value.toString + ">" + x.n2.value.toString
            val r2 = if(x.value.isInstanceOf[Unit]) "" else "/" + x.value.toString
            r1 + r2
        }).mkString(", ")
        val s2 = dd.map( _.value.toString).mkString(", ")
        if(s2 == "") "[" + s1 + "]" else  "[" + s1 + ", " + s2 + "]"
    }
}

abstract class GraphObjBase {
    type GraphClass[T, U]
    def addLabel[T](edges: List[(T, T)]) =
        edges.map(v => (v._1, v._2, ()))
    def term[T](nodes: List[T], edges: List[(T,T)]) =
        termLabel(nodes, addLabel(edges))
    def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]): GraphClass[T, U]
    def addAdjacentLabel[T](nodes: List[(T, List[T])]) =
        nodes.map(a => (a._1, a._2.map((_, ()))))
    def adjacent[T](nodes: List[(T, List[T])]) =
        adjacentLabel(addAdjacentLabel(nodes))
    def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]): GraphClass[T, U]

    /**
     * Stringden graf oluşturmak icin genel bir fonksiyon.
     * @param s : Bu stringden graf oluşturur.
     * @param c1 : Bu karekter düğümleri bir birinden ayırmakta kullanılan karekterdir.
     * @param c2 : Bu karekter kenarlar bir degere sahipse onu ayırmakda kullanılan karekterdir.
     * @param label : Grafın yönlü olup olmadığını gösterir.
     * @return : Dönüş degeğeri bir grafdır.
     */
    protected def fromString1(s: String, c1: Char, c2: Char, label: Boolean) = {
        /**
         * Parametreden gelen stringin ön ve ardındaki boşlukları siler
         * @param str
         * @return
         */
        def delSpace(str: String): String = {
            val ss = str.head match {
                case ' ' => delSpace(str.tail)
                case _ => str
            }
            val sss = ss.last match {
                case ' ' => delSpace(ss.init)
                case _ => ss
            }
            sss
        }

        require(s.head == '[' && s.last == ']')
        val ss = s.tail.init
        val lst1 = ss.split(',').toList.map( delSpace( _ ))
        val lst2 = lst1.map( _.split(c1).toList)
        val lst_node= lst2.flatten.toSet.toList
        val lst_edge = lst2.filter( _.length == 2).map(x => (x.head, x.last))

        if(label == true){
            val lst_edge_label = lst_edge.map(x =>
            {
                val ll = x._2.split(c2).toList
                (x._1, ll.head, ll.last)
            } ).map(x => (x._1, x._2, x._3.toDouble))
            val lst_node_label = lst_node.map(x => {
                if(x.contains(c2.toString)) x.split(c2).head else x
            }).toSet.toList

            termLabel(lst_node_label, lst_edge_label)
        }
        else{
            term(lst_node, lst_edge)
        }


    }

}

object Graph extends GraphObjBase {
    type GraphClass[T, U] = Graph[T, U]

    def termLabel[T, U](nodes: List[T], edges: List[(T, T, U)]) = {
        val g = new Graph[T, U]
        nodes.map(g.addNode)
        edges.map(v => g.addEdge(v._1, v._2, v._3))
        g
    }

    def adjacentLabel[T, U](nodes: List[(T, List[(T, U)])]) = {
        val g = new Graph[T, U]
        for ((v, a) <- nodes) g.addNode(v)
        for ((n1, a) <- nodes; (n2, l) <- a) {
            if (!g.nodes(n1).neighbors.contains(g.nodes(n2)))
                g.addEdge(n1, n2, l)
        }
        g
    }

    def fromString(s: String) = fromString1(s, '-', '/', false)
    def fromStringLabel(s: String) = fromString1(s, '-', '/', true)
}



object Digraph extends GraphObjBase {
    type GraphClass[T, U] = Digraph[T, U]

    def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]) = {
        val g = new Digraph[T, U]
        nodes.map(g.addNode)
        edges.map(v => g.addArc(v._1, v._2, v._3))
        g
    }
    def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]) = {
        val g = new Digraph[T, U]
        for ((n, a) <- nodes) g.addNode(n)
        for ((s, a) <- nodes; (d, l) <- a) g.addArc(s, d, l)
        g
    }

    def fromString(s: String) = fromString1(s, '>', '/', false)
    def fromStringLabel(s: String) = fromString1(s, '>', '/', true)
}






