import scala99.{Digraph, Graph, MTree, Node}

/**
 * Created by josef on 22.05.2015.
 */

/**
 * Test icin prog
 * */
object Prog {
    def main(args: Array[String]) {

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
        val grph = Graph.fromString("[b-c, b-f, b-d, c-d, c-f, f-d, d-e, g]")
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
        println(KnightJump.closedtour(6, 1, 1))
        println(KnightJump.alllazytours(7, 6, 4))
        println("felan filan")

    }
}