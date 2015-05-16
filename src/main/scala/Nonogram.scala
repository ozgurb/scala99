/**
 * Created by josef on 16.05.2015.
 */
object Nonogram {
    def isRight(n1: Int, n2: Int, n: Int, v: Vector[Int]) = {
        def isright0(nn1: Int, level: Int, flag: Int, vv: Vector[Int]): Boolean = {
            if(vv.isEmpty) true
            else {
                if(flag == 0){
                    if(vv.head == 0) {
                        val nnn1   = nn1
                        val fflag  = flag
                        val llevel = level + 1
                        if(nnn1 + llevel > n) false
                        else isright0(nnn1, llevel, fflag, vv.tail)
                    }
                    else {
                        val nnn1   = nn1 - 1
                        val fflag  = flag + 1
                        val llevel = level + 1
                        if(nnn1 + llevel > n) false
                        else isright0(nnn1, llevel, fflag, vv.tail)
                    }

                }
                else if(flag == 1) {
                    if(vv.head == 0) {
                        val nnn1   = nn1
                        val fflag  = flag + 1
                        val llevel = level + 1
                        if(nnn1 != 0) false
                        else isright0(nnn1, llevel, fflag, vv.tail)
                    }
                    else {
                        val nnn1   = nn1 - 1
                        val fflag  = flag
                        val llevel = level + 1
                        if(nnn1 < 0 || nnn1 + llevel > n) false
                        else isright0(nnn1, llevel, fflag, vv.tail)
                    }
                }
                else if(flag == 2) {
                    if(vv.head == 0) {
                        val nnn1   = nn1
                        val fflag  = flag
                        val llevel = level + 1
                        isright0(nnn1, llevel, fflag, vv.tail)
                    }
                    else {
                        false
                    }
                }
                else false
            }
        }
        def isright1(nn1: Int, nn2: Int, level: Int, flag: Int, vv: Vector[Int]): Boolean = {
            if(vv.isEmpty) true
            else {
                if(flag == 0){
                    if(vv.head == 0) {
                        val nnn1   = nn1
                        val nnn2   = nn2
                        val fflag  = flag
                        val llevel = level + 1
                        if(nnn1 + nnn2 + 1 + llevel > n) false
                        else isright1(nnn1, nnn2, llevel, fflag, vv.tail)
                    }
                    else {
                        val nnn1   = nn1 - 1
                        val nnn2   = nn2
                        val fflag  = flag + 1
                        val llevel = level + 1
                        if(nnn1 + nnn2 + llevel > n) false
                        else isright1(nnn1, nnn2, llevel, fflag, vv.tail)
                    }

                }
                else if(flag == 1) {
                    if(vv.head == 0) {
                        val nnn1   = nn1
                        val nnn2   = nn2
                        val fflag  = flag + 1
                        val llevel = level + 1
                        if(nnn2 + llevel > n || nnn1 != 0) false
                        else isright1(nnn1, nnn2, llevel, fflag, vv.tail)
                    }
                    else {
                        val nnn1   = nn1 - 1
                        val nnn2   = nn2
                        val fflag  = flag
                        val llevel = level + 1
                        if(nnn1 < 0 || nnn1 + nnn2 + 1 + llevel > n) false
                        else isright1(nnn1, nnn2, llevel, fflag, vv.tail)
                    }
                }
                else if(flag == 2) {
                    if(vv.head == 0) {
                        val nnn1   = nn1
                        val nnn2   = nn2
                        val fflag  = flag
                        val llevel = level + 1
                        if(nnn2 + llevel > n) false
                        else isright1(nnn1, nnn2, llevel, fflag, vv.tail)
                    }
                    else {
                        val nnn1   = nn1
                        val nnn2   = nn2 - 1
                        val fflag  = flag + 1
                        val llevel = level + 1
                        if(nnn2 + llevel > n) false
                        else isright1(nnn1, nnn2, llevel, fflag, vv.tail)
                    }
                }
                else if(flag == 3) {
                    if(vv.head == 0) {
                        val nnn1   = nn1
                        val nnn2   = nn2
                        val fflag  = flag + 1
                        val llevel = level + 1
                        if(nnn2 != 0 ) false
                        else isright1(nnn1, nnn2, llevel, fflag, vv.tail)
                    }
                    else {
                        val nnn1   = nn1
                        val nnn2   = nn2 - 1
                        val fflag  = flag
                        val llevel = level + 1
                        if(nnn2 < 0 || nnn2 + llevel > n) false
                        else isright1(nnn1, nnn2, llevel, fflag, vv.tail)
                    }
                }
                else if(flag == 4) {
                    if(vv.head == 0) {
                        val nnn1   = nn1
                        val nnn2   = nn2
                        val fflag  = flag
                        val llevel = level + 1
                        isright1(nnn1, nnn2, llevel, fflag, vv.tail)
                    }
                    else {
                        false
                    }
                }
                else false
            }
        }

        if(n2 == 0) isright0(n1,0, 0, v)
        else isright1(n1, n2, 0, 0, v)
    }

    def combine(n1: Int, n2: Int, n: Int): Vector[Vector[Int]] = {
        def combine1(nn1: Int, nn2: Int): Vector[Vector[Int]] = {
            def uret(bs: Int, ort: Int): Vector[Int] = {
                val first = (for (i <- 1 to nn1) yield 1).toVector
                val second = (for (i <- 1 to nn2) yield 1).toVector
                val bfirst = (for (i <- 1 to bs) yield 0).toVector
                val bsecond = (for (i <- 1 to ort) yield 0).toVector
                val bson = (for (i <- 1 to (n - nn1 - nn2 - bs - ort)) yield 0).toVector
                bfirst ++ first ++ bsecond ++ second ++ bson
            }
            val bas = for (i <- 0 to (n - nn1 - nn2 - 1)) yield i
            val orta = for (i <- 1 to (n - nn1 - nn2)) yield i
            val ikili = for {
                i <- bas
                j <- orta
                if ((i + j + nn1 + nn2) < n + 1)
            } yield (i, j)

            ikili.map(x => uret(x._1, x._2)).toVector

        }
        def combine0(nn1: Int): Vector[Vector[Int]] = {
            def uret(bs: Int) = {
                val first = (for(i <- 1 to nn1) yield 1).toVector
                val bfirst = (for(i <- 1 to bs) yield 0).toVector
                val bson = (for(i <- 1 to (n - nn1 - bs)) yield 0).toVector
                bfirst ++ first ++ bson
            }
            val tekli = for(i <- 0 to (n - nn1)) yield i
            tekli.map(x => uret(x)).toVector
        }
        if(n2 == 0) combine0(n1)
        else combine1(n1, n2)

    }




    def solution(vertical: Vector[(Int, Int)], horizontal: Vector[(Int, Int)]) = {
        def sutunlar(mat: Vector[Vector[Int]]) = {
            if(mat.isEmpty) Vector()
            else (for(i <- 0 to mat(0).length - 1) yield
            (for(j <- 0 to mat.length - 1) yield mat(j)(i)).toVector
              ).toVector
        }

        def isOk(mat: Vector[Vector[Int]]) = {
            val ss = sutunlar(mat)
            val zs = ss.zip(horizontal)
            zs.foldLeft(true)((x, y) =>
                x && isRight(y._2._1, y._2._2, vertical.length, y._1))

        }
        def nonograms1(v1: Vector[(Int, Int)], accu: Vector[Vector[Int]]): Vector[Vector[Int]] = {

            def nloop(vv: Vector[Vector[Int]]): Vector[Vector[Int]] = {
                if(vv.isEmpty) Vector()
                else {
                    val aaccu = accu ++ Vector(vv.head)
                    if(isOk(aaccu)){

                        val ss = nonograms1(v1.tail, aaccu)
                        if(ss == Vector()) nloop(vv.tail) else ss
                    }
                    else nloop(vv.tail)
                }
            }

            if(v1.isEmpty) accu
            else {
                val (n1, n2) = v1.head
                val cc = combine(n1, n2, vertical.length)
                nloop(cc)
            }
        }
        nonograms1(vertical, Vector())

    }


}
