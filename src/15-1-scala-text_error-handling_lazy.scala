// ScalaText error-handling
object Main extends App{
    if (false) {
        val lst = List(2, 3, 5, 7, 11).map(e => Some(e))
        println(lst)
        println(lst(0).map(c1 => lst(1).map(c2 => c1 * c2)).flatten)

        val prod = lst.foldLeft(Some(1): Option[Int]){(current, next) => current.map(c1 => next.map(c2 => c1 * c2)).flatten}
        println(prod)

        val prodFlatMap = lst.foldLeft(Some(1): Option[Int]){(current, next) => current.flatMap(c1 => next.map(c2 => c1 * c2))}
        println(prodFlatMap)

        val prodFor = lst match {
            case v1 :: v2 :: v3 :: v4 :: v5 :: _ => {
                for {
                    c1 <- v1
                    c2 <- v2
                    c3 <- v3
                    c4 <- v4
                    c5 <- v5
                } yield c1 * c2 * c3 * c4 * c5
            }
        }
        println(prodFor)
    }
    if (false) {
        val v: Either[String, Int] = Right(123)
        // v: Either[String, Int] = Right(value = 123)

        println(v.map(_ * 2))
        // res20: Either[String, Int] = Right(value = 246)

        val v2: Either[String, Int] = Left("a")
        // v2: Either[String, Int] = Left(value = "a")
        println(v2.map(_ * 2))
        // v2がLeftなので実行されない
        // res21: Either[String, Int] = Left(value = "a")
    }

    if (true) {
        def f(g: => Unit): Unit = {
          println("prologue f")
          g
          println("epilogue f")
        }
        def g(): Unit = println("g")
        f(g())
    }
}
