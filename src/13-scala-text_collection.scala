// ScalaText collection
object Main extends App{
    def swapArray[T] (arr: Array[T])(i: Int, j: Int): Unit = {
        val tmp = arr(i)
        arr(i) = arr(j)
        arr(j) = tmp
    }
    if (false) {
        val arr = Array[Int](100, 200, 300)
        println(arr.mkString(" "))
        swapArray(arr)(0, 1)
        println(arr.mkString(" "))
    }

    def mkRangeList (left: Int, right: Int): List[Int] = {
        (left until right).toList
    }

    def joinByComma (start: Int, end: Int): String = {
        (start to end).mkString(",")
    }
    if (false) {
        println(joinByComma(10, 15))
    }

    val lst = List(100, 200, 300)
    def reverse[T](list: List[T]): List[T] = {
        list.foldLeft(Nil: List[T])((value, next) => {
            next :: value
        })
    }
    if (false) {
        println(reverse(lst))
    }

    def sum(list: List[Int]): Int = {
        list.foldRight(0)((next: Int, value: Int) => {
            value + next
        })
    }
    if (false) {
        println(sum(lst))
        println(sum(List()))
    }
    def mul(list: List[Int]): Int = {
        list.foldRight(1)((next: Int, value: Int) => {
            value * next
        })
    }
    if (false) {
        println(mul(lst))
        println(mul(List()))
    }
    def mkString[T](list: List[T])(sep: String): String = {
        list match {
            case a :: b => b.foldLeft(a.toString)((value, next) => value + sep + next)
            case _ => ""
        }
    }
    if (false) {
        println(mkString(lst)(" "))
        println(mkString(List())(" "))
    }

    def map[T, U](list: List[T])(f: T => U): List[U] = {
        list.foldLeft(Nil: List[U])((current, next) => f(next) :: current).reverse
    }
    if (false) {
        println(map(List(1, 2, 3))(x => x + 1))
        assert(List(2, 3, 4) == map(List(1, 2, 3))(x => x + 1))
        assert(List(2, 4, 6) == map(List(1, 2, 3))(x => x * 2))
        assert(Nil == map(List[Int]())(x => x * x))
        assert(List(0, 0, 0)  == map(List(1, 2, 3))(x => 0))
    }

    def filter[T](list: List[T])(f: T => Boolean): List[T] = {
        list.foldLeft(Nil: List[T])((current, next) => if (f(next)) next :: current else current).reverse
    }
    if (false) {
        println(filter(List(1, 2, 3))(x => x % 2 == 1))
        assert(List(2) == filter(List(1, 2, 3))(x => x % 2 == 0))
        assert(List(1, 3) == filter(List(1, 2, 3))(x => x % 2 == 1))
        assert(Nil == filter(List(1, 2, 3))(x => x > 3))
        assert(List(1) == filter(List(1))(x => x == 1))
        assert(Nil == filter(List[Int]())(x => false))
    }

    def find[T](list: List[T])(f: T => Boolean): Option[T] = {
        list match {
            case a :: b => if (f(a)) Some(a) else find(b)(f)
            case _ => None
        }
    }
    if (false) {
        println(find(List(1, 2, 3))(x => x == 2))
        println(find(List(1, 2, 3))(x => x > 3))
        assert(Some(2) == find(List(1, 2, 3))(x => x == 2))
        assert(None == find(List(1, 2, 3))(x => x > 3))
        assert(Some(1) == find(List(1))(x => x == 1))
        assert(None == find(List(1))(x => false))
        assert(None == find(List[Int]())(x => x == 1))
    }

    def takeWhile[T](list: List[T])(f: T => Boolean): List[T] = {
        list match {
            case a :: b => if (f(a)) a :: takeWhile(b)(f) else Nil
            case _ => Nil
        }
    }
    if (false) {
        println(takeWhile(List(1, 2, 3, 4, 5))(x => x <= 3))
        assert(List(1, 2, 3) == takeWhile(List(1, 2, 3, 4, 5))(x => x <= 3))
        assert(List(1) == takeWhile(List(1, 2, 3, 3, 4, 5))(x => x == 1))
        assert(List(1, 2, 3, 4)  == takeWhile(List(1, 2, 3, 4, 5))(x => x < 5))
        assert(Nil == takeWhile(List(1, 2, 3, 3, 2, 2))(x => false))
    }

    def count[T](list: List[T])(f: T => Boolean): Int = {
        list.foldLeft(0)((current, next) => if (f(next)) current + 1 else current)
    }
    if (false) {
        assert(3 == count(List(1, 2, 3, 3, 2, 2))(x => x == 2))
        assert(1 == count(List(1, 2, 3, 3, 2, 2))(x => x == 1))
        assert(2 == count(List(1, 2, 3, 3, 2, 2))(x => x == 3))
        assert(0 == count(List(1, 2, 3, 3, 2, 2))(x => x == 5))
    }

    def flatMap[T, U](list: List[T])(f: T => List[U]): List[U] = {
        list match {
            case a :: b => f(a) ++ flatMap(b)(f)
            case _ => Nil
        }
    }
    if (true) {
        assert(List(1, 2, 3) == flatMap(List(1, 2, 3))(x => List(x)))
        assert(
            List(3, 4, 6, 8) == flatMap(List(1, 2))(x =>
                map(List(3, 4))(y => x * y)
            )
        )
    }
}
