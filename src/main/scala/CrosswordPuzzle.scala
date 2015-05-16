/**
 * Created by josef on 16.05.2015.
 */

//p99
object CrosswordPuzzle {

    val ff = "/home/josef/Masaüstü/notlar/p99a.dat"
    def valuesFile(src: String) = {
        val source = scala.io.Source.fromFile(src)
        val file = try source.mkString finally source.close()
        val lines = file.split('\n').toList
        val word = lines.takeWhile( x => x != "")
        val b1 = lines.dropWhile(x => x != "").tail.toVector
        val board = b1.map(x => x.toVector.map(y => if(y == '.') '.' else '*'))
        val max = board.maxBy(x => x.length).length
        val bboard = board.map(x =>
            if(x.length < max) x ++ (for(i <- 0 until (max - x.length)) yield '*').toVector else x)

        (word, bboard)
    }
    //| ala.collection.immutable.Vector[Char]])

    // * : kapalı kareyi temsil eder
    // . : harf girilebilecek kareyi temsil eder



    case class Site(x: Int, y: Int, length: Int, position: Char)



    def isOk1(s: String, v: Vector[Char]): Boolean = {
        if(s.size != v.size) false
        else if(s.isEmpty) true
        else {
            if(s.head == v.head || v.head == '.') isOk1(s.tail, v.tail)
            else false
        }
    }

    def isOk2(lst: List[String], v: Vector[Char]) = {
        lst.exists(x => isOk1(x, v))
    }

    def isOk3(lst: List[Site], mp: Map[Int, List[String]], mat: Vector[Vector[Char]]): Boolean = {
        if(lst.isEmpty) true
        else {
            val lst_str = mp(lst.head.length)
            val vv = getSite(mat, lst.head)
            if(isOk2(lst_str, vv)) isOk3(lst.tail, mp, mat) else false
        }
    }


    def retSites(v: Vector[Char]) = {
        def retSites1(vv: Vector[Char], accu: List[(Int, Int)], n0: Int, n1: Int): List[(Int, Int)] = {
            if(vv.isEmpty) if(n1 > 1) (n0 - n1, n1) :: accu else accu
            else {
                if(vv.head == '*') {
                    val nn0 = n0 + 1
                    val nn1 = 0
                    val aaccu = if(n1 > 1) (n0 - n1, n1) :: accu else accu
                    retSites1(vv.tail, aaccu, nn0, nn1)
                }
                else {
                    val nn0 = n0 + 1
                    val nn1 = n1 + 1
                    val aaccu = accu
                    retSites1(vv.tail, aaccu , nn0, nn1)
                }
            }
        }
        retSites1(v, Nil, 0, 0)
    }

    def allSites(mat: Vector[Vector[Char]]) = {
        val r1 = (for(i <- 0 until mat.length) yield retSites(mat(i)).
          map(x => Site(i, x._1, x._2, 'h'))).toList.flatten
        val tmat = mat.transpose
        val r2 = (for(i <- 0 until tmat.length) yield retSites(tmat(i)).
          map(x => Site(x._1, i, x._2, 'v'))).toList.flatten
        r1 ::: r2
    }


    def mapMatris[T, Z](mat: Vector[Vector[T]])(f: (Int, Int, T) => Z) = {
        (for(i <- 0 until mat.length) yield {
            (for(j <- 0 until mat(i).length) yield f(i, j, mat(i)(j))).toVector
        }).toVector
    }
    //| or[Z]]

    def setMatris(mat: Vector[Vector[Char]], x: Int, y: Int, p: Char, s: String) = {
        mapMatris(mat){(i, j, v) =>
            if(p == 'h'){
                if(x == i && j >= y && j < y + s.length) s(j - y) else v
            }
            else {
                if(y == j && i >= x && i < x + s.length) s(i - x) else v
            }
        }
    }
    //| ector[Vector[Char]]

    def getSite(mat: Vector[Vector[Char]], st:Site) = {
        if(st.position == 'h') {
            (for(i <- 0 until st.length) yield mat(st.x)(st.y + i)).toVector
        }
        else {
            (for(i <- 0 until st.length) yield mat(st.x + i)(st.y)).toVector
        }
    }

    def solution1(lst: List[Site], mp: Map[Int, List[String]],
                  mat: Vector[Vector[Char]]): Vector[Vector[Char]] = {
        def nloop(nlst: List[String]): Vector[Vector[Char]] = {
            if(nlst.isEmpty) Vector()
            else {
                val m = setMatris(mat, lst.head.x, lst.head.y, lst.head.position, nlst.head)
                val nn = lst.head.length
                val mp1 = mp - nn
                val l1 = mp(nn)
                val l2 = l1 diff List(nlst.head)
                val mp2 = mp1 + ((nn, l2))
                if(isOk3(lst.tail, mp2, m)){
                    val r1 = solution1(lst.tail, mp2, m)
                    if(r1 == Vector()) nloop(nlst.tail) else r1
                }
                else nloop(nlst.tail)
            }
        }

        if(lst.isEmpty) mat
        else {
            val lst_str1 = mp(lst.head.length)
            val vv = getSite(mat, lst.head)
            val lst_str2 = lst_str1.filter(x => isOk1(x, vv))
            nloop(lst_str2)
        }
    }

    def solution(src: String) = {
        val (words, mat) = valuesFile(src)
        val lst_sites = allSites(mat).sortWith( _.length > _.length)
        val mp = words.groupBy( _.length)
        val r = solution1(lst_sites, mp, mat)
        if(r == Vector()) println("Nothing...")
        else {
            val rr = r.map(x => x.map(y => if (y == '*') " " else y))
            val rrr = rr.map(x => x.mkString(" "))
            rrr.foreach(x => println(x))
        }
    }

}