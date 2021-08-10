// ScalaText function
import scala.io.Source

object Main extends App{
    val addFunc = (x: Int, y: Int) => x + y
    println(addFunc(2, 3))
    println(addFunc.apply(2, 3))

    def addMethod(x: Int, y: Int): Int = x + y
    println(addMethod(2, 5))
    //println(addMethod.apply(2, 5))  // error

    def withFile[A](filename: String)(f: Source => A): A = {
        val s = Source.fromFile(filename)
        try {
            f(s)
        } finally {
            s.close()
        }
    }
    def printFile(filename: String): Unit = {
        withFile(filename)(s => {
            for (line <- s.getLines) {
                println(line)
            }
        })
    }
    printFile("Main.scala")
}
