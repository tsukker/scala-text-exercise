// ScalaText syntax-control
object Main extends App{
    def loopFrom0To9(): Unit = {
        var i = 0
        do {
            println(i)
            i += 1
        } while (i <= 9)
    }
    if (false) {
        loopFrom0To9()
    }

    def printPythagoreanTriple(): Unit = {
        val n = 1000
        for (a <- 1 to n; b <- a to n; c <- b to n) {
            if (a * a + b * b == c * c) {
                println(a, b, c)
            }
        }
    }
    if (false) {
        printPythagoreanTriple()
    }

    def printPseudoRandomString(): Unit = {
        for (_ <- 1 to 1000) {
            val lst = new scala.util.Random(new java.security.SecureRandom()).alphanumeric.take(4).toList
            println((lst(3) :: lst).mkString(""))
        }
    }
    if (false) {
        printPseudoRandomString()
    }
}
