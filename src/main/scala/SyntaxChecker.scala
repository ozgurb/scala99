/**
 * Created by josef on 16.05.2015.
 */

//p96
object SyntaxChecker {

    def isIdentifier(str: String): Boolean = {
        def isLetter(ch: Char) = ch.isLetter
        def isDigit(ch: Char) = ch.isDigit
        def isUnderscores(ch: Char) = ch == '_'
        /**
         * n == 0 durumu, okunan karekterin bir alt çizgi karakteri olduğunu imler...
         * */
        def isIdentifier1(str1: String, n: Int): Boolean = {
            if(str1.isEmpty) {
                if(n == 0) false else true
            }
            else if(n == 0) {
                if(isUnderscores(str1.head)) false
                else if(isLetter(str1.head) || isDigit(str1.head)) isIdentifier1(str1.tail, 1)
                else false
            }
            else {
                if(isLetter(str1.head)  || isDigit(str1.head)) isIdentifier1(str1.tail, 1)
                else if(isUnderscores(str1.head)) isIdentifier1(str1.tail, 0)
                else false
            }
        }

        if(str.isEmpty) false
        else if(str.head == '_') false
        else isIdentifier1(str, 1)
    }


}
